## Packages needed by this App (all listed in appspec.json, except moveapps which the SDK provides)
library("moveapps")   # logger.*(), appArtifactPath()
library("move2")
library("dplyr")      # masks stats::filter and base::lag/intersect/setdiff/union; all of these are called with their prefix below
library("keyring")    # backend for movebank_store_credentials(); exports nothing that masks
## The following packages are only called with their pkg:: prefix, to avoid maskings (e.g. lubridate::date, purrr::flatten,
## vctrs::data_frame). They must be installed, but not attached; uncomment to attach them for local testing.
# library("lubridate")  # now(), days()
# library("purrr")      # map(), map_lgl()
# library("rlang")      # is_bare_list()
# library("sf")         # st_is_empty(), st_coordinates(), st_crs(), st_transform()
# library("tidyselect") # all_of(), any_of()
# library("vctrs")      # vec_c()


### helper function for while loop to pull data from movebank

retry_with_backoff <- function(expr,
                               check_var = NULL,       # optional: name (string) of variable whose existence signals success
                               timeout = 1800,          # total time to keep trying (seconds)
                               label = "Movebank",      # human-readable name for log messages, e.g. "Movebank"
                               initial_wait = 2,        # seconds before backoff kicks in at all
                               short_backoff = 600,     # seconds after which backoff switches to long interval
                               short_sleep = 60,        # sleep interval during short backoff phase
                               long_sleep = 300,        # sleep interval during long backoff phase
                               no_retry_classes = c("move2_error_no_data_found",   # error classes that signal a data/request problem,
                                                    "move2_error_no_deployed_data", # not downtime: give up immediately
                                                    "move2_error_movebank_api_401_no_valid_credentials", # wrong username/password
                                                    "move2_error_movebank_api_403_no_access", # study without download permission
                                                    "move2_error_movebank_api_license_not_accepted", # license of the study not yet accepted
                                                    "move2_error_movebank_api_not_valid_sensor_type_id", # sensor not present in the study
                                                    "move2_error_movebank_api_multiple_rows_of_study_info", # inconsistent study information
                                                    "move2_error_movebank_api_individuals_duplicated_in_track_data", # inconsistent deployment information
                                                    "move2_error_movebank_construct_url_event_reductions_all"), # raised client-side by move2 before any request
                               no_retry_pattern = "gets\\s+too\\s+long", # unclassed client-side move2 error (URL length limit); cli may wrap the message, hence \\s+
                               envir = parent.frame()) {
  
  expr <- substitute(expr)
  time0 <- Sys.time()
  elapsed <- 0
  succeeded <- FALSE
  last_error <- NULL
  
  is_success <- function() {
    if (!is.null(check_var)) exists(check_var, envir = envir, inherits = FALSE) else succeeded
  }
  
  while (elapsed < timeout && !is_success()) {
    
    # backoff schedule based on elapsed time
    if (elapsed > initial_wait && elapsed <= short_backoff) Sys.sleep(short_sleep)
    if (elapsed > short_backoff) Sys.sleep(long_sleep)
    
    logger.info(paste0("Try ", label, " access at: ", Sys.time()))
    
    tryCatch({
      eval(expr, envir = envir)
      succeeded <- TRUE   # only reached if eval() didn't error
    }, error = function(e) {
      if (inherits(e, no_retry_classes) || grepl(no_retry_pattern, conditionMessage(e))) stop(e)  # rethrow unchanged so the caller can handle it
      last_error <<- conditionMessage(e)
    })
    
    elapsed <- as.numeric(difftime(Sys.time(), time0, units = "secs"))
  }
  
  if (!is_success()) {
    error_msg <- paste0(
      "Tried to access ", label, " for ", timeout / 60, " minutes, no successful response. ",
      label, " seems to be currently down. Try again later. ",
      "Original error: ", if (!is.null(last_error)) last_error else geterrmessage()
    )
    logger.error(error_msg)
    # classed so that the study loop can tell "Movebank is down" (abort the run) from a problem with a single study (skip it)
    stop(errorCondition(error_msg, class = "movebank_unreachable_error", call = NULL))
  }
  
  if (!is.null(check_var)) return(get(check_var, envir = envir, inherits = FALSE))
  invisible(TRUE)  # side-effect-only call succeeded; nothing meaningful to return
}


### helper: tracks grouped and time ordered, as required by move2

ensure_ordered <- function(locs, log = FALSE) {
  if (!mt_is_track_id_cleaved(locs)) {
    if (log) logger.info("Your data set was not grouped by individual/track. We regroup it for you.")
    locs <- locs |> dplyr::arrange(mt_track_id(locs))
  }
  if (!mt_is_time_ordered(locs)) {
    if (log) logger.info("Your data is not time ordered (within the individual/track groups). We reorder the locations for you.")
    locs <- locs |> dplyr::arrange(mt_track_id(locs), mt_time(locs))
  }
  locs
}


### helper: unlisting track data columns of class list

unlist_track_data_lists <- function(x) {
  if (any(sapply(mt_track_data(x), rlang::is_bare_list))) {
    ## reduce all columns were entry is the same to one (so no list anymore)
    x <- x |> mutate_track_data(across(
      where(~ rlang::is_bare_list(.x) && all(purrr::map_lgl(.x, function(y) 1 == length(unique(y))))),
      ~ do.call(vctrs::vec_c, purrr::map(.x, head, 1))))
    if (any(sapply(mt_track_data(x), rlang::is_bare_list))) {
      ## transform those that are still a list into a character string
      x <- x |> mutate_track_data(across(
        where(~ rlang::is_bare_list(.x) && any(purrr::map_lgl(.x, function(y) 1 != length(unique(y))))),
        ~ unlist(purrr::map(.x, paste, collapse = ","))))
    }
  }
  x
}


### species selection
# keep_species: character vector of taxon_canonical_name values to retain; the element "no_species" also retains
# individuals/tracks without a species.

species_is_selected <- function(species, keep_species) {
  keep_species <- as.character(unlist(keep_species))
  if (length(keep_species) == 0) return(rep(TRUE, length(species))) # no selection: everything is kept
  species <- as.character(species)
  no_species <- is.na(species) | trimws(species) == ""
  (species %in% keep_species) | (no_species & "no_species" %in% keep_species)
}

species_label <- function(keep_species) { # for log messages
  keep_species <- as.character(unlist(keep_species))
  paste(c(keep_species[keep_species != "no_species"], if ("no_species" %in% keep_species) "no species"), collapse = ", ")
}

# Select the individuals of the wanted species from the deployment metadata of a study (movebank_download_deployment()),
# so that only these are requested from Movebank. Returns a list with the animal IDs to download (NULL if all individuals
# of the study are wanted, so that no filter is needed) and the counts; or NULL if no individual matches (reason logged).
select_individuals <- function(dep, keep_species, pre = "") {
  if (nrow(dep) == 0 || !"individual_local_identifier" %in% names(dep)) { # movebank_download_deployment() drops all-NA columns, a study without deployments gives an empty table
    logger.error(paste0(pre, "The study has no deployments, i.e. no animal is associated with a tag. No data will be downloaded for this study."))
    return(NULL)
  }
  species <- if ("taxon_canonical_name" %in% names(dep)) dep$taxon_canonical_name else rep(NA_character_, nrow(dep))
  selected <- species_is_selected(species, keep_species)
  indv_all <- unique(as.character(dep$individual_local_identifier))
  if (length(indv_all) == 0) {
    logger.error(paste0(pre, "The study has no deployments, i.e. no animal is associated with a tag. No data will be downloaded for this study."))
    return(NULL)
  }
  indv_sel <- unique(as.character(dep$individual_local_identifier[selected]))
  indv_sel <- indv_sel[!is.na(indv_sel)]
  if (length(indv_sel) == 0) {
    logger.error(paste0(pre, "None of the ", length(indv_all), " individuals belongs to the selected species (", species_label(keep_species), "). No data will be downloaded for this study."))
    return(NULL)
  }
  if (length(indv_sel) == length(indv_all)) {
    logger.info(paste0(pre, "all ", length(indv_all), " individuals belong to the selected species and will be downloaded."))
    return(list(ids = NULL, n_all = length(indv_all), n_selected = length(indv_sel)))
  }
  dropped <- unique(as.character(species[!selected])); dropped[is.na(dropped) | trimws(dropped) == ""] <- "no species"
  logger.info(paste0(pre, length(indv_sel), " of ", length(indv_all), " individuals belong to the selected species and will be downloaded; the others are not requested as their species is not selected (", paste(sort(dropped), collapse = ", "), ")."))
  list(ids = indv_sel, n_all = length(indv_all), n_selected = length(indv_sel))
}

# Post-download filter of the tracks by species (track data column taxon_canonical_name). Only needed when the whole
# study had to be downloaded (see download_study_locations()). Returns NULL if no track of this study is left.
filter_species <- function(locs, keep_species, pre = "") {
  trk <- mt_track_data(locs)
  species <- if ("taxon_canonical_name" %in% names(trk)) trk$taxon_canonical_name else rep(NA_character_, nrow(trk))
  track_is_selected <- species_is_selected(species, keep_species)
  if (!any(track_is_selected)) {
    logger.error(paste0(pre, "None of the downloaded tracks belongs to the selected species (", species_label(keep_species), "), i.e. the individuals of the selected species have no data for the selected sensors and time range. No data will be downloaded for this study."))
    return(NULL)
  }
  if (all(track_is_selected)) return(locs)
  dropped <- unique(as.character(species[!track_is_selected])); dropped[is.na(dropped) | trimws(dropped) == ""] <- "no species"
  logger.info(paste0(pre, sum(!track_is_selected), " of ", length(track_is_selected), " tracks are dropped as their species is not selected (", paste(sort(dropped), collapse = ", "), ")."))
  locs |> filter_track_data(track_is_selected)
}


### summary of the downloaded data: number of individuals per species, plus the number of tracks without species name ("unknown species")

species_summary <- function(locs) {
  trk <- mt_track_data(locs)
  species <- if ("taxon_canonical_name" %in% names(trk)) as.character(trk$taxon_canonical_name) else rep(NA_character_, nrow(trk))
  no_species <- is.na(species) | trimws(species) == ""
  # an individual is identified by animal ID within its study (the same animal ID can occur in several studies)
  indv <- paste(if ("study_id" %in% names(trk)) trk$study_id else "", trk$individual_local_identifier)
  counts <- if (any(!no_species)) tapply(indv[!no_species], species[!no_species], function(x) length(unique(x))) else integer(0)
  if (length(counts) > 0) counts <- counts[order(names(counts))] # species alphabetically
  n_unknown <- length(unique(trk[[mt_track_id_column(locs)]][no_species])) # tracks (not individuals) without species name
  paste0(paste0(c(names(counts), "unknown species"), ": ", c(counts, n_unknown)), collapse = ", ")
}


### download and clean the locations of one study
# Called once per study: every call gets its own environment, which the check_var mechanism of retry_with_backoff() relies on.
# Always returns a list with the move2 object (locs) and the counts for the summary at the end of the log. If the study
# yields no data, locs is NULL and "reason" holds a short reason for the list of skipped studies at the end of the log.
# Errors are left to the caller: "movebank_unreachable_error" (retries of 30 min exhausted) aborts the whole run,
# any other error skips this study only.

download_study_locations <- function(study_id, arguments, select_sensors, minarg, keep_species, user_end) {
  
  pre <- paste0("Study ", study_id, ": ")
  arguments[["study_id"]] <- study_id
  counts <- list(n_indv_all = 0, n_indv_selected = 0, n_indv_downloaded = 0, n_downloaded = 0, n_species = 0, n_empty = 0, n_nacoord = 0)
  skip <- function(reason) c(list(locs = NULL, reason = reason), counts) # study without data: only the counts and the reason
  
  # species selection: the deployment metadata of the study tell which individuals belong to the selected species,
  # only these are requested from Movebank
  dep <- retry_with_backoff({
    dep <- movebank_download_deployment(study_id = study_id)
  }, check_var = "dep")
  indv <- select_individuals(dep, keep_species = keep_species, pre = pre)
  counts$n_indv_all <- length(unique(dep$individual_local_identifier)) # NULL column for a study without deployments, giving 0
  if (is.null(indv)) return(skip(if (counts$n_indv_all == 0) "no deployments in the study" else "no individual of the selected species"))
  counts$n_indv_selected <- indv$n_selected
  if (!is.null(indv$ids)) arguments[["individual_local_identifier"]] <- indv$ids
  
  # event reduction profiles: attributes=all does not work, a vector with the attribute names of this study is needed
  if (!is.null(arguments$event_reduction_profile) && !minarg) {
    attr_sdy <- retry_with_backoff({
      attr_sdy <- unique(unlist(lapply(select_sensors, function(x) {
        movebank_retrieve(entity_type = "study_attribute", study_id = study_id, sensor_type_id = x)$short_name
      })))
    }, check_var = "attr_sdy")
    arguments[["attributes"]] <- attr_sdy
  }
  
  no_data_reason <- "no data downloaded" # set by the handlers below, for the list of skipped studies at the end of the log
  # download; if the request with the list of individuals gets too long for the Movebank API, the whole study is downloaded and filtered afterwards
  locs <- tryCatch(
    retry_with_backoff({
      locs <- tryCatch(
        do.call(movebank_download_study, arguments),
        error = function(e) {
          if (!is.null(arguments$individual_local_identifier) && grepl("gets\\s+too\\s+long", conditionMessage(e))) { # cli may wrap the message, hence \\s+
            logger.warn(paste0(pre, "The request for the ", length(arguments$individual_local_identifier), " selected individuals is too long for the Movebank API. The whole study is downloaded instead and the individuals of other species are removed afterwards."))
            arguments[["individual_local_identifier"]] <<- NULL # outer copy, so a retry does not repeat this warning
            do.call(movebank_download_study, arguments)
          } else stop(e)
        }
      )
    }, check_var = "locs"),
    move2_error_no_data_found = function(e) {
      no_data_reason <<- "no data for the selected individuals, sensors and time range"
      fmt_ts <- function(x) { # platform passes "yyyyMMddHHmmssSSS" strings, lastXdays a POSIXct
        if (is.character(x)) x <- as.POSIXct(x, format = "%Y%m%d%H%M%S", tz = "UTC")
        format(x, "%Y-%m-%d %H:%M:%S UTC")
      }
      ## only the timestamps set by the user are mentioned, the end timestamp defaults to "now"
      start_set <- !is.null(arguments$timestamp_start)
      when <- if (start_set && user_end) paste0(" in the selected time range (start: ", fmt_ts(arguments$timestamp_start), ", end: ", fmt_ts(arguments$timestamp_end), ")")
              else if (start_set) paste0(" from the selected start timestamp (", fmt_ts(arguments$timestamp_start), ") onwards")
              else if (user_end) paste0(" up to the selected end timestamp (", fmt_ts(arguments$timestamp_end), ")")
              else ""
      logger.error(paste0(pre, "No data are available", when, " for the selected sensors. No data will be downloaded for this study. Please compare your selections with the first and last location dates shown in the Studies Tab."))
      NULL
    },
    move2_error_no_deployed_data = function(e) {
      no_data_reason <<- "no location of the study is assigned to an animal"
      logger.error(paste0(pre, "Data were downloaded but none of the records are deployed, i.e. assigned to an animal. No data will be downloaded for this study. ", conditionMessage(e)))
      NULL
    }
  )
  if (is.null(locs)) return(skip(no_data_reason))  # nothing to process; the reason has been logged above
  
  counts$n_downloaded <- nrow(locs) # counts for the summary at the end of the log
  
  # species filter on the downloaded tracks: only removes anything if the whole study had to be downloaded (see above)
  locs <- filter_species(locs, keep_species = keep_species, pre = pre)
  if (is.null(locs)) { # the selected individuals have no data; the reason has been logged above
    counts$n_species <- counts$n_downloaded
    return(skip("no data for the selected individuals, sensors and time range"))
  }
  counts$n_species <- counts$n_downloaded - nrow(locs)
  
  # quality check: cleaved, time ordered, non-empty, non-duplicated (dupl get removed further down in the code)
  locs <- ensure_ordered(locs, log = TRUE)
  
  if (!mt_has_no_empty_points(locs)) {
    emptylocs <- dplyr::filter(locs, sf::st_is_empty(locs))
    counts$n_empty <- nrow(emptylocs)
    logger.info(paste0(pre, "Your data included empty points (", counts$n_empty, "). We remove them for you."))
    locs <- dplyr::filter(locs, !sf::st_is_empty(locs))
  }
  ## for some reason, sometimes either lat or long are NA, as one still has a value it does not get removed with the excluding empty, here is what I came up with:
  crds <- sf::st_coordinates(locs)
  rem <- unique(c(which(is.na(crds[, 1])), which(is.na(crds[, 2]))))
  if (length(rem) > 0) {
    counts$n_nacoord <- length(rem)
    logger.info(paste0(pre, "Your data included locations with missing latitude or longitude (", counts$n_nacoord, "). We remove them for you."))
    locs <- locs[-rem, ]
  }
  
  # every study is put on deployment_id as track ID: one track-data row per deployment, no list columns, hence identical
  # column types across studies so that they can be stacked. The track definition chosen by the user is applied afterwards
  # to the stacked data set by set_track_definition().
  if (mt_track_id_column(locs) != "deployment_id") {
    idcolumn <- mt_track_id_column(locs) # need to get track id column before changing it
    locs <- mt_set_track_id(locs, "deployment_id")
    locs <- mt_as_track_attribute(locs, tidyselect::all_of(idcolumn)) # when changing the track_id column, the previous one stays in the event table, but gets removed from track table (which makes sense), but putting it back as in this case it will always work
    locs <- ensure_ordered(locs)
  }
  
  # individuals that were selected but have no data (Movebank simply omits them), so that they are not silently missing
  indv_downloaded <- unique(as.character(mt_track_data(locs)$individual_local_identifier))
  counts$n_indv_downloaded <- length(indv_downloaded)
  indv_nodata <- setdiff(if (is.null(indv$ids)) unique(as.character(dep$individual_local_identifier[species_is_selected(dep$taxon_canonical_name, keep_species)])) else indv$ids, indv_downloaded)
  if (length(indv_nodata) > 0) {
    logger.warn(paste0(pre, length(indv_nodata), " of the ", counts$n_indv_selected, " selected individuals have no data for the selected sensors and time range and are missing in the downloaded data: ", paste(sort(indv_nodata), collapse = ", "), "."))
  }
  
  c(list(locs = locs, reason = NA_character_), counts)
}


### apply the track definition chosen by the user to the stacked data set, then the cleaning steps that depend on the track ID
# trackid=c("indv","deploy","indv_deploy"); the input has deployment_id as track ID (see download_study_locations())

set_track_definition <- function(locs, trackid, thin, thin_numb, thin_unit) {
  
  if (trackid == "indv") { #  "individual_local_identifier"
    # the same animal name can be used in different studies; keep those apart by appending the study ID before the deployments get merged into individuals
    trk <- mt_track_data(locs)
    name_study <- unique(data.frame(name = as.character(trk$individual_local_identifier), study_id = as.character(trk$study_id)))
    dup <- unique(name_study$name[duplicated(name_study$name)])
    if (length(dup) > 0) {
      logger.info(paste0("The animal ID", if (length(dup) > 1) "s '" else " '", paste(dup, collapse = "', '"), "' occur", if (length(dup) == 1) "s", " in more than one study. To keep these animals apart, their track ID gets the study ID appended (e.g. '", dup[1], "_", name_study$study_id[match(dup[1], name_study$name)], "')."))
      locs <- locs |> mutate_track_data(individual_local_identifier = ifelse(
        as.character(individual_local_identifier) %in% dup,
        paste0(individual_local_identifier, "_", study_id),
        as.character(individual_local_identifier)))
    }
    locs <- mt_set_track_id(locs, "individual_local_identifier")
    # "deployment_id" moves to the event table, probably could somehow get it back to the track table, but not sure its worth the effort
    locs <- ensure_ordered(locs)
  }
  if (trackid == "indv_deploy") {
    locs <- locs |> mutate_track_data(individual_name_deployment_id = paste0(mt_track_data(locs)$individual_local_identifier, "_", mt_track_data(locs)$deployment_id))
    locs <- mt_set_track_id(locs, "individual_name_deployment_id")
    locs <- mt_as_track_attribute(locs, tidyselect::all_of("deployment_id")) # when changing the track_id column, the previous one stays in the event table, but gets removed from track table (which makes sense), but putting it back as in this case it will always work
    locs <- ensure_ordered(locs)
  }
  # trackid == "deploy": nothing to do, deployment_id is already the track ID
  
  # remove duplicates without user interaction, start with select most-info row
  n_dupl <- 0
  if (!mt_has_unique_location_time_records(locs)) {
    n_dupl <- length(which(duplicated(paste(mt_track_id(locs), mt_time(locs)))))
    logger.info(paste("Your data has", n_dupl, "duplicated location-time records. We removed here those with less info and then select the first if still duplicated."))
    ## this piece of code keeps the duplicated entry with least number of columns with NA values
    locs <- locs |>
      mutate(n_na = rowSums(is.na(pick(everything())))) |>
      arrange(n_na) |>
      mt_filter_unique(criterion = "first") |> # this always needs to be "first" because the duplicates get ordered according to the number of columns with NA.
      dplyr::select(-n_na) # helper column, must not end up in the output
    locs <- locs |> dplyr::arrange(mt_track_id(locs), mt_time(locs)) # mt_track_id() without argument only works with the magrittr pipe, so pass the object
  }
  
  # thinning to first location of given time windows (thus, resulting time lag can be shorter some times)
  if (isTRUE(thin)) {
    logger.info(paste("Your data will be thinned as requested to one location per", thin_numb, thin_unit))
    locs <- locs[order(mt_track_id(locs), mt_time(locs)), ]
    locs <- mt_filter_per_interval(locs, criterion = "first", unit = paste(thin_numb, thin_unit))
    locs <- locs |> group_by(trk = mt_track_id(locs)) |> slice(if (n() > 1) -1 else 1) |> ungroup() ## the thinning happens within the time window, so the 1st location is mostly off. After the 1st location the intervals are regular if the data allow for it. If track endsup only with one location, this one is retained
    locs <- locs |> dplyr::select(-trk) # helper column added by group_by()
  }
  
  # make names
  ids <- mt_track_id(locs); u <- unique(ids)
  mt_track_id(locs) <- make.names(u, allow_ = TRUE, unique = TRUE)[match(ids, u)] # unique = TRUE: ids that differ only in non-syntactic characters must not be merged
  
  locs <- unlist_track_data_lists(locs) # merging deployments into individuals creates list columns in the track data
  list(locs = locs, n_dupl = n_dupl)
}


rFunction = function(data=NULL, username, password, study, select_sensors,keep_species, incl_outliers=FALSE, minarg=FALSE, thin=FALSE, thin_numb=6, thin_unit="hours", timestamp_start=NULL, timestamp_end=NULL, event_reduc=NULL, lastXdays=NULL, trackid="indv", ...) {
  
  # fixed once at the very start, so that with lastXdays every study is downloaded for the identical time window,
  # no matter how long the previous downloads (or retries) took
  run_start <- lubridate::now(tzone = "UTC")
  
  options("keyring_backend" = "env")
  
  ## studies
  study <- unique(unlist(study))
  if (length(study) == 0) {
    msg <- "No study has been selected. No data will be downloaded by this App."
    logger.error(msg)
    stop(msg, call. = FALSE)
  }
  logger.info(paste0(length(study), " Movebank stud", if (length(study) == 1) "y" else "ies", " selected: ", paste(study, collapse = ", "), ". All individuals of each study that belong to the selected species (see below) will be downloaded."))
  
  ## species
  keep_species <- as.character(unlist(keep_species))
  if (length(keep_species) == 0) {
    logger.info("No species selection: all species of the selected studies will be downloaded.")
  } else {
    logger.info(paste0("Only individuals of the selected species will be downloaded: ", species_label(keep_species), ". For each study, the individuals of other species are identified from the study's deployment metadata and not requested."))
  }
  
  ## sensor types
  if (is.null(select_sensors)) {
    msg <- "The selected studies do not contain any location sensor data. No data will be downloaded by this App."
    logger.error(msg)
    stop(msg, call. = FALSE)
  }
  if (length(select_sensors) == 0) {
    msg <- "Either the selected studies do not contain any location sensor data or you have deselected all available location sensors. No data will be downloaded by this App."
    logger.error(msg)
    stop(msg, call. = FALSE)
  }
  
  ## credentials
  cred_ok <- tryCatch(
    retry_with_backoff({
      ## test query of our own: movebank_store_credentials() does the same internally, but masks the error class,
      ## so that wrong credentials could not be told apart from Movebank being down (and were retried for 30 min)
      movebank_retrieve(entity_type = "tag_type", handle = movebank_handle(username, password = password))
      movebank_store_credentials(username, password, force = TRUE) # force: the test query above has already been done
      cred_ok <- TRUE
    }, check_var = "cred_ok"),
    move2_error_movebank_api_401_no_valid_credentials = function(e) {
      logger.error("Your Movebank username and/or password are not valid (Movebank rejected them with 'no valid credentials'). Please correct the Movebank Login in the settings of this App.")
      NULL
    },
    error = function(e) {
      logger.error(paste0("Failed to access Movebank: ", conditionMessage(e)))
      NULL
    }
  )
  if (is.null(cred_ok)) stop("The Movebank credentials could not be verified. See the log messages above for details.", call. = FALSE)
  
  sensorInfo <- tryCatch(
    retry_with_backoff({
      sensorInfo <- movebank_retrieve(entity_type = "tag_type")
    }, check_var = "sensorInfo"),
    error = function(e) {
      logger.error(paste0("Failed to access Movebank: ", conditionMessage(e)))
      NULL
    }
  )
  if (is.null(sensorInfo)) stop("Movebank could not be reached to retrieve the sensor types. See the log messages above for details.", call. = FALSE)
  
  select_sensors_name <- sensorInfo$name[which(as.numeric(sensorInfo$id) %in% select_sensors)]
  logger.info(paste("You have selected to download locations of these selected sensor types:", paste(select_sensors_name, collapse = ", ")))
  
  ## arguments for movebank_download_study() shared by all studies; study_id (and study specific attributes) are added per study
  arguments <- list()
  arguments[["sensor_type_id"]] <- select_sensors
  
  # include outliers
  if (isTRUE(incl_outliers)) {
    logger.info("Also locations marked as outliers in Movebank (visible=FALSE) will be downloaded. Note that this may lead to unexpected results.")
  } else {
    arguments[["remove_movebank_outliers"]] <- TRUE
    logger.info("Only data that were not marked as outliers previously are downloaded (default).")
  }
  
  if (isTRUE(minarg)) {
    arguments[["attributes"]] <- c("tag_local_identifier", "individual_local_identifier", "deployment_id", "sensor_type_id")
    logger.info("You have selected to only include the minimum set of event attributes: animal ID, tag ID, deployment ID, sensor type, timestamp and location. The track attributes will be fully included.")
  }
  
  if (!is.null(timestamp_start)) {
    logger.info(paste0("timestamp_start is set and will be used: ", timestamp_start))
    arguments[["timestamp_start"]] <- timestamp_start
  } else {
    logger.info("timestamp_start not set.")
  }
  
  if (!is.null(timestamp_end)) {
    logger.info(paste0("timestamp_end is set and will be used: ", timestamp_end))
    arguments[["timestamp_end"]] <- timestamp_end
  } else {
    arguments[["timestamp_end"]] <- run_start # always set an end to prevent downloading locations with timestamps in the future
    logger.info(paste0("timestamp_end not set. The start time of this App run (", format(run_start, "%Y-%m-%d %H:%M:%S UTC"), ") is used as end timestamp for all studies to exclude locations with timestamps in the future."))
  }
  
  if (!is.null(lastXdays)) {
    timestamp_start <- run_start - lubridate::days(lastXdays)
    arguments[["timestamp_start"]] <- timestamp_start ## [[ is needed for POSIXct so its class survives
    arguments[["timestamp_end"]] <- run_start
    logger.info(paste0("data will be downloaded starting from: ", timestamp_start, ", this is ", lastXdays, " days before the start of this App run (", run_start, "), until the start of this App run, for all selected studies. If timestamp_start or timestamp_end are set, these values will be ignored"))
  }
  
  # event reduction profiles EURING: 1-quick daily location, 3-all location of the last 30 days
  # NOTE ANNE: "EURING_01" and "EURING_03" work, but the attributes have to be named, attributes="all" does  not work, nor timestamp start/end.
  if (!is.null(event_reduc) && length(event_reduc) > 0) {
    logger.info(paste("You have selected to use the event reduction profile", event_reduc, "for fast download from Movebank. EURING_01 indicates download of 1 location per day for the full selected tracks, EURING_03 download of the last 30 days of data for each selected individual track (starting at the last position)."))
    
    arguments[["event_reduction_profile"]] <- event_reduc # can have values "EURING_01" or "EURING_03"
    if (!is.null(arguments$timestamp_start)) {
      arguments[["timestamp_start"]] <- NULL
      logger.info(paste0("timestamp_start cannot be used in combination with the setting ", event_reduc))
    }
    if (!is.null(timestamp_end)) {
      logger.info(paste0("timestamp_end cannot be used in combination with the setting ", event_reduc))
    }
    arguments[["timestamp_end"]] <- NULL # profiles do not accept timestamps, this also removes the default "now"
    # the attribute names needed for the profile are study specific, they are retrieved in download_study_locations()
  } else logger.info("You have selected to NOT use a fast reduction profile download. Start and end timestamps will be used if defined above.")
  
  user_end <- !is.null(timestamp_end) && !is.null(arguments$timestamp_end) # end timestamp set by the user (not the default "now") and still in use
  
  ## download study by study
  locs_list <- list()
  skipped <- character(0) # studies without data, named by study ID, with the reason; listed at the end of the log
  counts <- list(n_indv_all = 0, n_indv_selected = 0, n_indv_downloaded = 0, n_downloaded = 0, n_species = 0, n_empty = 0, n_nacoord = 0) # summed over the studies for the summary at the end of the log
  for (i in seq_along(study)) {
    sid <- study[i]
    logger.info(paste0("Downloading study ", sid, " (", i, " of ", length(study), ")"))
    movebank_down <- FALSE
    locs_i <- tryCatch(
      download_study_locations(study_id = sid, arguments = arguments, select_sensors = select_sensors, minarg = minarg, keep_species = keep_species, user_end = user_end),
      movebank_unreachable_error = function(e) { # Movebank down: abort the run (details already logged by retry_with_backoff())
        movebank_down <<- TRUE
        NULL
      },
      error = function(e) { # any other problem concerns this study only (e.g. no data in the time range, no access): skip it and continue
        logger.error(paste0("Study ", sid, ": download failed, this study is skipped. ", conditionMessage(e)))
        list(locs = NULL, reason = paste0("download failed (", conditionMessage(e), ")"))
      }
    )
    if (movebank_down) {
      msg <- paste0("Movebank could not be reached while downloading study ", sid, ". The App stops here so that no incomplete data set is passed on; try again later.")
      logger.error(msg)
      stop(msg, call. = FALSE)
    }
    for (cnt in names(counts)) counts[[cnt]] <- counts[[cnt]] + (if (is.null(locs_i[[cnt]])) 0 else locs_i[[cnt]])
    if (is.null(locs_i$locs)) {
      skipped[as.character(sid)] <- if (is.null(locs_i$reason) || is.na(locs_i$reason)) "no data" else locs_i$reason
    } else {
      logger.info(paste0("Study ", sid, ": ", nrow(locs_i$locs), " locations of ", mt_n_tracks(locs_i$locs), " deployments downloaded."))
      locs_list[[as.character(sid)]] <- locs_i$locs
    }
  }
  
  if (length(locs_list) == 0) {
    logger.error(paste0("The following studies could not be downloaded: ", paste0(names(skipped), " (", skipped, ")", collapse = "; "), "."))
    msg <- "None of the selected studies returned data, check your settings and the logs for messages that might indicate where the problem is."
    logger.error(msg)
    stop(msg, call. = FALSE)
  }
  
  ## stack all studies into one move2 object (all on deployment_id as track ID, see download_study_locations())
  if (length(locs_list) == 1) {
    locs <- locs_list[[1]]
  } else {
    locs <- tryCatch(
      mt_stack(unname(locs_list), .track_combine = "rename", .track_id_repair = "unique_quiet"), # deployment IDs are unique in Movebank, rename is only a safety net
      error = function(e) {
        logger.error(paste0("The downloaded studies cannot be combined into one data set: ", conditionMessage(e)))
        NULL
      }
    )
    if (is.null(locs)) stop("The downloaded studies could not be combined into one data set. See the log messages above for details.", call. = FALSE)
    logger.info(paste0("The ", length(locs_list), " studies have been combined: ", nrow(locs), " locations of ", mt_n_tracks(locs), " deployments."))
  }
  
  ## track definition chosen by the user, duplicates, thinning, syntactic track IDs
  tmp <- set_track_definition(locs, trackid = trackid, thin = thin, thin_numb = thin_numb, thin_unit = thin_unit)
  locs <- tmp$locs; counts$n_dupl <- tmp$n_dupl; rm(tmp)
  logger.info(paste0("Tracks are defined by ", switch(trackid, indv = "animal ID", deploy = "deployment ID", indv_deploy = "animal ID and deployment ID"), ": ", nrow(locs), " locations of ", mt_n_tracks(locs), " tracks."))
  
  n_indv_species <- species_summary(locs) # logged at the very end
  
  ## combine with other input data (move2!)
  if (!is.null(data)) {
    if (!sf::st_crs(data) == sf::st_crs(locs)) {
      locs <- sf::st_transform(locs, sf::st_crs(data))
      logger.info(paste0("The new data sets to combine has a different projection. It has been re-projected, and now the combined data set is in the '", sf::st_crs(data)$input, "' projection."))
    }
    result <- tryCatch(
      mt_stack(data, locs, .track_combine = "rename", .track_id_repair = "unique_quiet"),
      error = function(e) { # move2 0.5.0 raises this without its intended class (typo in cli_abort), so catch any error
        if (mt_track_id_column(data) != mt_track_id_column(locs)) {
          logger.error(paste0("The downloaded data cannot be combined with the input data: the track ID column differs ('", mt_track_id_column(data), "' in the input data, '", mt_track_id_column(locs), "' in the downloaded data). Please select the same 'track ID' option in all Movebank Apps of this workflow."))
        } else {
          logger.error(paste0("The downloaded data cannot be combined with the input data: ", conditionMessage(e)))
        }
        NULL
      }
    )
    if (is.null(result)) stop("The downloaded data could not be combined with the input data. See the log messages above for details.", call. = FALSE)
    result <- unlist_track_data_lists(result)
  } else {
    result <- locs
  }
  
  ## remove all attributes that contain NAs in all rows
  na_cols <- result |>
    dplyr::select(where(~ all(is.na(.)))) |>
    select_track_data(where(~ all(is.na(.))))
  naevnt <- names(na_cols)
  natrk <- names(mt_track_data(na_cols))
  naevnt <- naevnt[!naevnt %in% c(mt_track_id_column(result), mt_time_column(result), attr(result, "sf_column"))]
  natrk <- natrk[!natrk %in% c(mt_track_id_column(result))]
  
  if (length(naevnt) >= 1) {
    logger.info(paste0("The event attributes: ", paste0(naevnt, collapse = ", "), " have been removed as they only contained NAs."))
  }
  if (length(natrk) >= 1) {
    logger.info(paste0("The track attributes: ", paste0(natrk, collapse = ", "), " have been removed as they only contained NAs."))
  }
  
  result <- result |>
    dplyr::select(where(~ !all(is.na(.)))) |>
    select_track_data(where(~ !all(is.na(.))))
  
  ## create file with metadata for download, one column per study
  attrList <- c("study_id", "name", "taxon_ids", "principal_investigator_name", "contact_person_name", "citation", "license_terms", "license_type")
  metadata <- result |> mt_track_data() |> dplyr::select(tidyselect::any_of(attrList)) |> distinct(.keep_all = TRUE)
  if (nrow(metadata) >= 1) {
    metadata$download_date <- Sys.Date()
    metadata[] <- lapply(metadata, as.character) # otherwise t() pads the numbers with blanks when the studies differ in the number of digits
    metadata_csv <- data.frame(names(metadata), t(metadata))
    write.table(metadata_csv, appArtifactPath("citation_metadata.csv"), sep = ",", qmethod = "double", row.names = FALSE, col.names = FALSE)
  }
  
  ## summary of what was downloaded and what was excluded by the App
  n_plural <- function(n, singular, plural) paste(n, if (n == 1) singular else plural)
  logger.info(paste0("Summary: ", length(locs_list), " of the ", n_plural(length(study), "selected study", "selected studies"), if (length(locs_list) == 1) " was" else " were", " downloaded, with data of ",
                     n_plural(counts$n_indv_downloaded, "individual", "individuals"), " (", counts$n_indv_selected, " of the ", counts$n_indv_all, " individuals of the checked studies belong to the selected species and were requested); ",
                     counts$n_downloaded, " locations downloaded from Movebank; excluded: ",
                     if (counts$n_species > 0) paste0(counts$n_species, " of species not selected, ") else "", counts$n_empty, " empty points, ", counts$n_nacoord, " with missing coordinates, ", counts$n_dupl, " duplicated timestamps",
                     if (isTRUE(thin)) paste0(", plus locations removed by thinning to one per ", thin_numb, " ", thin_unit) else "",
                     "; ", nrow(locs), " locations returned", if (!is.null(data)) " (in addition to the input data)" else "", "."))
  logger.info(paste0("The downloaded data of all studies contain the following number of individuals per species (and number of tracks without species name as 'unknown species'): ", n_indv_species, "."))
  if (length(skipped) > 0) { # very visible at the end of the log: which studies are missing from the result, and why
    logger.warn(paste0("ATTENTION: ", length(skipped), " of the ", length(study), " selected studies could NOT be downloaded and ", if (length(skipped) == 1) "is" else "are", " missing in the returned data: ",
                       paste0("study ", names(skipped), " (", skipped, ")", collapse = "; "), ". See the messages above for details."))
  }
  
  return(result)
}
