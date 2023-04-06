.onLoad <- function(lib, pkg) {
  if (Sys.getenv("SPORTSDATAVERSE.UPLOAD.INSIST", "true") == "true") {
    pause_base <- Sys.getenv("SPORTSDATAVERSE.UPLOAD.PAUSE_BASE", 0.05) |> as.numeric()
    pause_min <- Sys.getenv("SPORTSDATAVERSE.UPLOAD.PASUE_MIN", 1) |> as.numeric()
    max_times <- Sys.getenv("SPORTSDATAVERSE.UPLOAD.MAX_TIMES", 10) |> as.numeric()
    quiet <- Sys.getenv("SPORTSDATAVERSE.UPLOAD.QUIET", TRUE) |> as.logical()

    retry_rate <- purrr::rate_backoff(
      pause_base = pause_base,
      pause_min = pause_min,
      max_times = max_times
    )

    assign(
      x = "sportsdataverse_upload",
      value = purrr::insistently(sportsdataverse_upload, rate = retry_rate, quiet = quiet),
      envir = parent.env(environment())
    )

    assign(
      x = "update_release_timestamp",
      value = purrr::insistently(update_release_timestamp, rate = retry_rate, quiet = quiet),
      envir = parent.env(environment())
    )
  }
}
