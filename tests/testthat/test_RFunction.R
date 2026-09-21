# Offline tests: the Movebank calls are mocked, the sample data sets play the role of the downloaded studies.

library("move2")

studies <- list(
  "2" = test_data("input3_move2loc_LatLon.rds"), # 1 stork with 5 deployments
  "4" = test_data("input4_move2loc_LatLon.rds")  # 3 geese with 1 deployment each
)
# a third "study" with the same animal names as study 4 but another study ID and (as in Movebank) its own deployment IDs
studies[["5"]] <- studies[["4"]] |> mutate_track_data(study_id = bit64::as.integer64(5), deployment_id = deployment_id + 1L)

orig_retry <- retry_with_backoff

mock_movebank <- function(env = parent.frame()) {
  calls <- list()
  local_mocked_bindings(
    movebank_store_credentials = function(...) invisible(TRUE),
    movebank_retrieve = function(entity_type, ...) data.frame(id = c(653, 673), name = c("GPS", "Argos")),
    movebank_download_study_info = function(study_id, ...) {
      data.frame(timestamp_first_deployed_location = as.POSIXct("1990-01-01", tz = "UTC"),
                 timestamp_last_deployed_location = as.POSIXct("2030-01-01", tz = "UTC"))
    },
    movebank_download_study = function(study_id, ...) {
      calls[[length(calls) + 1]] <<- list(study_id = study_id, ...)
      if (study_id == 99) rlang::abort("no data", class = "move2_error_no_data_found")
      if (study_id == 98) rlang::abort("no access", class = "move2_error_movebank_api_403_no_access")
      if (study_id == 97) stop("Movebank down") # unclassed: gets retried until the timeout
      studies[[as.character(study_id)]]
    },
    .package = "move2", .env = env
  )
  function() calls
}

test_that("studies with different deployment structures are stacked, same lastXdays window for all", {
  get_calls <- mock_movebank()
  for (trackid in c("indv", "deploy", "indv_deploy")) {
    actual <- rFunction(data = NULL, username = "u", password = "p", study = c(2, 4), select_sensors = 653,
                        lastXdays = 30, trackid = trackid)
    expect_s3_class(actual, "move2")
    expect_equal(mt_n_tracks(actual), if (trackid == "indv") 1 + 3 else 5 + 3)
    expect_equal(mt_track_id_column(actual), switch(trackid, indv = "individual_local_identifier", deploy = "deployment_id", indv_deploy = "individual_name_deployment_id"))
    expect_false(any(sapply(mt_track_data(actual), rlang::is_bare_list)))
    expect_equal(nrow(actual), nrow(studies[["2"]]) + nrow(studies[["4"]]))
  }
  calls <- get_calls()
  expect_equal(length(calls), 6)
  expect_equal(length(unique(lapply(calls, `[[`, "timestamp_start"))), 3) # one timestamp per rFunction() call, shared by its two studies
})

test_that("the same animal name in two studies stays two tracks", {
  mock_movebank()
  actual <- rFunction(data = NULL, username = "u", password = "p", study = c(4, 5), select_sensors = 653, trackid = "indv")
  expect_equal(mt_n_tracks(actual), 6)
  expect_true(all(grepl("_(9589196|5)$", unique(mt_track_id(actual)))))
})

test_that("studies without data or access are skipped, the others are returned", {
  mock_movebank()
  actual <- rFunction(data = NULL, username = "u", password = "p", study = c(99, 2, 98), select_sensors = 653, trackid = "indv_deploy")
  expect_s3_class(actual, "move2")
  expect_equal(mt_n_tracks(actual), 5)
})

test_that("Movebank unreachable for one study aborts the whole run", {
  mock_movebank()
  # shorten the retry budget of the helper that rFunction() finds in the global environment
  fast_retry <- orig_retry
  formals(fast_retry)[c("timeout", "initial_wait", "short_sleep")] <- list(0.5, 0, 0.1)
  assign("retry_with_backoff", fast_retry, envir = globalenv())
  withr::defer(assign("retry_with_backoff", orig_retry, envir = globalenv()))
  expect_null(rFunction(data = NULL, username = "u", password = "p", study = c(2, 97, 4), select_sensors = 653, trackid = "indv_deploy"))
})

test_that("NULL when nothing can be downloaded", {
  mock_movebank()
  expect_null(rFunction(data = NULL, username = "u", password = "p", study = c(99, 98), select_sensors = 653))
  expect_null(rFunction(data = NULL, username = "u", password = "p", study = NULL, select_sensors = 653))
  expect_null(rFunction(data = NULL, username = "u", password = "p", study = 2, select_sensors = list()))
})

test_that("retry_with_backoff rethrows request problems immediately but retries other errors", {
  expect_error(retry_with_backoff(rlang::abort("x", class = "move2_error_movebank_api_403_no_access")),
               class = "move2_error_movebank_api_403_no_access")
  n <- 0
  expect_error(retry_with_backoff({ n <- n + 1; stop("down") }, timeout = 0.5, initial_wait = 0, short_sleep = 0.1),
               class = "movebank_unreachable_error")
  expect_gt(n, 1)
})
