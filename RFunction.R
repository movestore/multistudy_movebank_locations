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
                                                    "move2_error_movebank_api_403_no_access", # study without download permission
                                                    "move2_error_movebank_api_license_not_accepted", # license of the study not yet accepted
                                                    "move2_error_movebank_api_not_valid_sensor_type_id", # sensor not present in the study
                                                    "move2_error_movebank_construct_url_event_reductions_all"), # raised client-side by move2 before any request
                               no_retry_pattern = "gets too long", # unclassed client-side move2 error (URL length limit)
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


### keep only the tracks of the selected species
# keep_species: character vector of taxon_canonical_name values to retain; the element "no_species" also retains
# tracks without a species. NULL or empty: all tracks are kept. Returns NULL if no track of this study is left.

filter_species <- function(locs, keep_species, pre = "") {
  keep_species <- as.character(unlist(keep_species))
  if (length(keep_species) == 0) return(locs)
  keep_no_species <- "no_species" %in% keep_species
  keep_species <- keep_species[keep_species != "no_species"]
  
  trk <- mt_track_data(locs)
  species <- if ("taxon_canonical_name" %in% names(trk)) as.character(trk$taxon_canonical_name) else rep(NA_character_, nrow(trk))
  no_species <- is.na(species) | trimws(species) == ""
  keep <- (species %in% keep_species) | (no_species & keep_no_species)
  
  if (!any(keep)) {
    logger.error(paste0(pre, "None of the tracks belongs to the selected species (", paste(c(keep_species, if (keep_no_species) "no species"), collapse = ", "), "). No data will be downloaded for this study."))
    return(NULL)
  }
  if (all(keep)) return(locs)
  dropped <- unique(species[!keep]); dropped[is.na(dropped) | trimws(dropped) == ""] <- "no species"
  logger.info(paste0(pre, sum(!keep), " of ", length(keep), " tracks are dropped as their species is not selected (", paste(dropped, collapse = ", "), ")."))
  locs |> filter_track_data(keep)
}


### summary of the downloaded data: number of individuals per species (and without species, if these were selected)

species_summary <- function(locs, keep_species) {
  trk <- mt_track_data(locs)
  species <- if ("taxon_canonical_name" %in% names(trk)) as.character(trk$taxon_canonical_name) else rep(NA_character_, nrow(trk))
  species[is.na(species) | trimws(species) == ""] <- "no species"
  # an individual is identified by animal ID within its study (the same animal ID can occur in several studies)
  indv <- paste(if ("study_id" %in% names(trk)) trk$study_id else "", trk$individual_local_identifier)
  counts <- tapply(indv, species, function(x) length(unique(x)))
  if (!"no_species" %in% as.character(unlist(keep_species))) counts <- counts[names(counts) != "no species"]
  if ("no_species" %in% as.character(unlist(keep_species)) && !"no species" %in% names(counts)) counts <- c(counts, "no species" = 0)
  counts <- counts[order(names(counts) == "no species", names(counts))] # species alphabetically, "no species" last
  paste0(names(counts), ": ", counts, collapse = ", ")
}


### download and clean the locations of one study
# Called once per study: every call gets its own environment, which the check_var mechanism of retry_with_backoff() relies on.
# Returns a move2 object, or NULL if this study yields no data (reason logged). Errors are left to the caller:
# "movebank_unreachable_error" (retries of 30 min exhausted) aborts the whole run, any other error skips this study only.

download_study_locations <- function(study_id, arguments, select_sensors, minarg, keep_species) {
  
  pre <- paste0("Study ", study_id, ": ")
  arguments[["study_id"]] <- study_id
  
  # event reduction profiles: attributes=all does not work, a vector with the attribute names of this study is needed
  if (!is.null(arguments$event_reduction_profile) && !minarg) {
    attr_sdy <- retry_with_backoff({
      attr_sdy <- unique(unlist(lapply(select_sensors, function(x) {
        movebank_retrieve(entity_type = "study_attribute", study_id = study_id, sensor_type_id = x)$short_name
      })))
    }, check_var = "attr_sdy")
    arguments[["attributes"]] <- attr_sdy
  }
  
  ## check timestamp end and start to be within range of data
  if (!is.null(arguments$timestamp_start) || !is.null(arguments$timestamp_end)) {
    stdyi <- retry_with_backoff({
      stdyi <- movebank_download_study_info(study_id = study_id)
    }, check_var = "stdyi")
    
    if (!is.null(arguments$timestamp_start)) {
      if (!is.na(stdyi$timestamp_last_deployed_location) && as.POSIXct(arguments$timestamp_start, format = "%Y%m%d%H%M%S", tz = "UTC") > stdyi$timestamp_last_deployed_location) {
        logger.error(paste0(pre, "Your start timestamp is set after the last deployed location of the study (", stdyi$timestamp_last_deployed_location, "). No data will be downloaded for this study."))
        return(NULL)
      }
    }
    if (!is.null(arguments$timestamp_end)) {
      if (!is.na(stdyi$timestamp_first_deployed_location) && as.POSIXct(arguments$timestamp_end, format = "%Y%m%d%H%M%S", tz = "UTC") < stdyi$timestamp_first_deployed_location) {
        logger.error(paste0(pre, "Your end timestamp is set before the first deployment location of the study (", stdyi$timestamp_first_deployed_location, "). No data will be downloaded for this study."))
        return(NULL)
      }
    }
  }
  
  # download
  locs <- tryCatch(
    retry_with_backoff({
      locs <- do.call(movebank_download_study, arguments)
    }, check_var = "locs"),
    move2_error_no_data_found = function(e) {
      if (!is.null(arguments$timestamp_start) || !is.null(arguments$timestamp_end)) {
        fmt_ts <- function(x) { # platform passes "yyyyMMddHHmmssSSS" strings, lastXdays a POSIXct
          if (is.null(x)) return("not set")
          if (is.character(x)) x <- as.POSIXct(x, format = "%Y%m%d%H%M%S", tz = "UTC")
          format(x, "%Y-%m-%d %H:%M:%S UTC")
        }
        logger.error(paste0(pre, "No data are available in the selected time range (start: ", fmt_ts(arguments$timestamp_start),
                            ", end: ", fmt_ts(arguments$timestamp_end),
                            ") for the selected sensors. No data will be downloaded for this study."))
      } else {
        logger.error(paste0(pre, "No data are available for the selected sensors. No data will be downloaded for this study. ", conditionMessage(e)))
      }
      NULL
    },
    move2_error_no_deployed_data = function(e) {
      logger.error(paste0(pre, "Data were downloaded but none of the records are deployed, i.e. assigned to an animal. No data will be downloaded for this study. ", conditionMessage(e)))
      NULL
    }
  )
  if (is.null(locs)) return(NULL)  # nothing to process; the reason has been logged above
  
  # species selection (track level, column taxon_canonical_name)
  locs <- filter_species(locs, keep_species = keep_species, pre = pre)
  if (is.null(locs)) return(NULL)  # no track of the selected species in this study; the reason has been logged above
  
  # quality check: cleaved, time ordered, non-empty, non-duplicated (dupl get removed further down in the code)
  locs <- ensure_ordered(locs, log = TRUE)
  
  if (!mt_has_no_empty_points(locs)) {
    emptylocs <- dplyr::filter(locs, sf::st_is_empty(locs))
    logger.info(paste0(pre, "Your data included empty points (", nrow(emptylocs), "). We remove them for you."))
    locs <- dplyr::filter(locs, !sf::st_is_empty(locs))
  }
  ## for some reason, sometimes either lat or long are NA, as one still has a value it does not get removed with the excluding empty, here is what I came up with:
  crds <- sf::st_coordinates(locs)
  rem <- unique(c(which(is.na(crds[, 1])), which(is.na(crds[, 2]))))
  if (length(rem) > 0) {
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
  
  locs
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
  if (thin == TRUE) {
    logger.info(paste("Your data will be thinned as requested to one location per", thin_numb, thin_unit))
    locs <- locs[order(mt_track_id(locs), mt_time(locs)), ]
    locs <- mt_filter_per_interval(locs, criterion = "first", unit = paste(thin_numb, thin_unit))
    locs <- locs |> group_by(trk = mt_track_id(locs)) |> slice(if (n() > 1) -1 else 1) |> ungroup() ## the thinning happens within the time window, so the 1st location is mostly off. After the 1st location the intervals are regular if the data allow for it. If track endsup only with one location, this one is retained
    locs <- locs |> dplyr::select(-trk) # helper column added by group_by()
  }
  
  # make names
  ids <- mt_track_id(locs); u <- unique(ids)
  mt_track_id(locs) <- make.names(u, allow_ = TRUE, unique = TRUE)[match(ids, u)] # unique = TRUE: ids that differ only in non-syntactic characters must not be merged
  
  unlist_track_data_lists(locs) # merging deployments into individuals creates list columns in the track data
}


rFunction = function(data=NULL, username, password, study, select_sensors, incl_outliers=FALSE, minarg=FALSE, thin=FALSE, thin_numb=6, thin_unit="hours", timestamp_start=NULL, timestamp_end=NULL, event_reduc=NULL, lastXdays=NULL, trackid="indv", keep_species=NULL, ...) {
  
  # fixed once at the very start, so that with lastXdays every study is downloaded for the identical time window,
  # no matter how long the previous downloads (or retries) took
  run_start <- lubridate::now(tzone = "UTC")
  
  options("keyring_backend" = "env")
  
  ## studies
  study <- unique(unlist(study))
  if (length(study) == 0) {
    logger.error("No study has been selected. No data will be downloaded (NULL output) by this App.")
    return(NULL)
  }
  logger.info(paste0(length(study), " Movebank stud", if (length(study) == 1) "y" else "ies", " selected: ", paste(study, collapse = ", "), ". All individuals of each study will be downloaded."))
  
  ## species
  keep_species <- as.character(unlist(keep_species))
  if (length(keep_species) == 0) {
    logger.info("No species selection: all species of the selected studies will be downloaded.")
  } else {
    logger.info(paste0("Only tracks of the selected species will be retained: ", paste(sub("^no_species$", "tracks without species", keep_species), collapse = ", "), ". Tracks of other species are dropped after the download of each study."))
  }
  
  ## sensor types
  if (is.null(select_sensors)) {
    logger.info("The selected studies do not contain any location sensor data. No data will be downloaded (NULL output) by this App.")
    return(NULL)
  }
  if (length(select_sensors) == 0) {
    logger.info("Either the selected studies do not contain any location sensor data or you have deselected all available location sensors. No data will be downloaded (NULL output) by this App.")
    return(NULL)
  }
  
  ## credentials
  cred_ok <- tryCatch(
    retry_with_backoff({
      movebank_store_credentials(username, password)
    }),
    error = function(e) {
      logger.error(paste0("Failed to access Movebank: ", conditionMessage(e)))
      NULL
    }
  )
  if (is.null(cred_ok)) return(NULL)  # Movebank unreachable; reason has been logged above
  
  sensorInfo <- tryCatch(
    retry_with_backoff({
      sensorInfo <- movebank_retrieve(entity_type = "tag_type")
    }, check_var = "sensorInfo"),
    error = function(e) {
      logger.error(paste0("Failed to access Movebank: ", conditionMessage(e)))
      NULL
    }
  )
  if (is.null(sensorInfo)) return(NULL)  # Movebank unreachable; reason has been logged above
  
  select_sensors_name <- sensorInfo$name[which(as.numeric(sensorInfo$id) %in% select_sensors)]
  logger.info(paste("You have selected to download locations of these selected sensor types:", paste(select_sensors_name, collapse = ", ")))
  
  ## arguments for movebank_download_study() shared by all studies; study_id (and study specific attributes) are added per study
  arguments <- list()
  arguments[["sensor_type_id"]] <- select_sensors
  
  # include outliers
  if (incl_outliers == TRUE) {
    logger.info("Also locations marked as outliers in Movebank (visible=FALSE) will be downloaded. Note that this may lead to unexpected results.")
  } else {
    arguments[["remove_movebank_outliers"]] <- TRUE
    logger.info("Only data that were not marked as outliers previously are downloaded (default).")
  }
  
  if (minarg == TRUE) {
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
    logger.info("timestamp_end not set.")
  }
  
  if (!is.null(lastXdays)) {
    timestamp_start <- run_start - lubridate::days(lastXdays)
    arguments[["timestamp_start"]] <- timestamp_start ## [[ is needed for POSIXct so its class survives
    arguments[["timestamp_end"]] <- NULL
    logger.info(paste0("data will be downloaded starting from: ", timestamp_start, ", this is ", lastXdays, " days before the start of this App run (", run_start, "), for all selected studies. If timestamp_start or timestamp_end are set, these values will be ignored"))
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
    if (!is.null(arguments$timestamp_end)) {
      arguments[["timestamp_end"]] <- NULL
      logger.info(paste0("timestamp_end cannot be used in combination with the setting ", event_reduc))
    }
    # the attribute names needed for the profile are study specific, they are retrieved in download_study_locations()
  } else logger.info("You have selected to NOT use a fast reduction profile download. Start and end timestamps will be used if defined above.")
  
  ## download study by study
  locs_list <- list()
  for (i in seq_along(study)) {
    sid <- study[i]
    logger.info(paste0("Downloading study ", sid, " (", i, " of ", length(study), ")"))
    movebank_down <- FALSE
    locs_i <- tryCatch(
      download_study_locations(study_id = sid, arguments = arguments, select_sensors = select_sensors, minarg = minarg, keep_species = keep_species),
      movebank_unreachable_error = function(e) { # Movebank down: abort the run (details already logged by retry_with_backoff())
        movebank_down <<- TRUE
        NULL
      },
      error = function(e) { # any other problem concerns this study only (e.g. no data in the time range, no access): skip it and continue
        logger.error(paste0("Study ", sid, ": download failed, this study is skipped. ", conditionMessage(e)))
        NULL
      }
    )
    if (movebank_down) {
      logger.error(paste0("Movebank could not be reached while downloading study ", sid, ". The App stops here so that no incomplete data set is passed on; try again later."))
      return(NULL)
    }
    if (!is.null(locs_i)) {
      logger.info(paste0("Study ", sid, ": ", nrow(locs_i), " locations of ", mt_n_tracks(locs_i), " tracks downloaded."))
      locs_list[[as.character(sid)]] <- locs_i
    }
  }
  
  if (length(locs_list) == 0) {
    logger.error("None of the selected studies returned data, check your settings and the logs for messages that might indicate where the problem is.")
    return(NULL)
  }
  if (length(locs_list) < length(study)) {
    logger.warn(paste0("Only ", length(locs_list), " of the ", length(study), " selected studies returned data (", paste(names(locs_list), collapse = ", "), "). See the messages above for the skipped studies."))
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
    if (is.null(locs)) return(NULL)
    logger.info(paste0("The ", length(locs_list), " studies have been combined: ", nrow(locs), " locations of ", mt_n_tracks(locs), " deployments."))
  }
  
  ## track definition chosen by the user, duplicates, thinning, syntactic track IDs
  locs <- set_track_definition(locs, trackid = trackid, thin = thin, thin_numb = thin_numb, thin_unit = thin_unit)
  logger.info(paste0("Tracks are defined by ", switch(trackid, indv = "animal ID", deploy = "deployment ID", indv_deploy = "animal ID and deployment ID"), ": ", nrow(locs), " locations of ", mt_n_tracks(locs), " tracks."))
  
  n_indv_species <- species_summary(locs, keep_species = keep_species) # logged at the very end
  
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
    if (is.null(result)) return(NULL)
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
  
  logger.info(paste0("The downloaded data of all studies contain the following number of individuals per species: ", n_indv_species, "."))
  
  return(result)
}
