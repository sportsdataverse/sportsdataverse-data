#' Upload to sportsdataverse release
#'
#' @param files vector of filepaths to upload
#' @param tag release name
#' @param pkg_function related package function name
#' @param ... other args passed to `piggyback::pb_upload()`
#'
#' @export
sportsdataverse_upload <- function(files, tag, pkg_function = NULL, ...) {
  cli::cli_alert("Uploading {length(files)} files!")
  # upload files
  piggyback::pb_upload(files, repo = "sportsdataverse/sportsdataverse-data", tag = tag, ...)
  update_release_timestamp(tag)
  update_package_function(tag, pkg_function)
  cli::cli_alert("Uploaded {length(files)} to sportsdataverse/sportsdataverse-data @ {tag} on {Sys.time()}")
}

update_release_timestamp <- function(tag) {
  temp_dir <- tempdir(check = TRUE)

  update_time <- format(Sys.time(), tz = "America/Toronto", usetz = TRUE)
  writeLines(update_time, file.path(temp_dir, "timestamp.txt"))

  list(last_updated = update_time) |>
    jsonlite::toJSON(auto_unbox = TRUE) |>
    writeLines(file.path(temp_dir, "timestamp.json"))

  piggyback::pb_upload(file.path(temp_dir,"timestamp.txt"), repo = "sportsdataverse/sportsdataverse-data", tag = tag, overwrite = TRUE)
  piggyback::pb_upload(file.path(temp_dir,"timestamp.json"), repo = "sportsdataverse/sportsdataverse-data", tag = tag, overwrite = TRUE)

  invisible(NULL)
}


update_package_function <- function(tag, pkg_function) {
  if (!is.null(pkg_function)) {
    temp_dir <- tempdir(check = TRUE)

    package_function <- paste0(pkg_function)
    writeLines(package_function, file.path(temp_dir, "package_function.txt"))

    list(package_function = package_function) |>
      jsonlite::toJSON(auto_unbox = TRUE) |>
      writeLines(file.path(temp_dir, "package_function.json"))

    piggyback::pb_upload(file.path(temp_dir, "package_function.txt"), repo = "sportsdataverse/sportsdataverse-data", tag = tag, overwrite = TRUE)
    piggyback::pb_upload(file.path(temp_dir, "package_function.json"), repo = "sportsdataverse/sportsdataverse-data", tag = tag, overwrite = TRUE)

    invisible(NULL)
  }
}


#' Save files to sportsdataverse release
#'
#' This functions attaches sportsdataverse attributes like type and timestamp, saves
#' data to a temporary directory in all four of csv, rds, parquet, and qs formats,
#' and then uploads to sportsdataverse-data repository for a specified release tag.
#'
#' @param data_frame data_frame to save
#' @param file_name file_name to upload as, without the file extension
#' @param sportsdataverse_type metadata: name/information to add to data
#' @param release_tag name of release to upload to
#' @param pkg_function related package function name
#' @param .token a GitHub token, defaults to gh::gh_token()
#' @param file_types one or more of c("rds","csv","parquet","qs","csv.gz")
#'
#' @export
sportsdataverse_save <- function(data_frame,
                          file_name,
                          sportsdataverse_type,
                          release_tag,
                          pkg_function,
                          .token = gh::gh_token(),
                          file_types = c("rds", "csv", "parquet")
) {

  stopifnot(
    is.data.frame(data_frame),
    is.character(file_name),
    is.character(sportsdataverse_type),
    is.character(release_tag),
    is.character(pkg_function),
    is.character(.token),
    is.character(file_types),
    length(file_name) == 1,
    length(sportsdataverse_type) == 1,
    length(release_tag) == 1,
    length(pkg_function) == 1,
    length(.token) == 1,
    length(file_types) >= 1
  )

  if ("season" %in% names(data_frame)) data_frame$season <- as.integer(data_frame$season)
  if ("week" %in% names(data_frame)) data_frame$week <- as.integer(data_frame$week)

  attr(data_frame, "sportsdataverse_type") <- sportsdataverse_type
  attr(data_frame, "sportsdataverse_timestamp") <- Sys.time()

  temp_dir <- tempdir(check = TRUE)
  ft <- rlang::arg_match(file_types,
                         values = c("rds", "csv", "csv.gz", "parquet", "qs"),
                         multiple = TRUE)

  if ("rds" %in% ft) saveRDS(data_frame, file.path(temp_dir, paste0(file_name, ".rds")))
  if ("csv" %in% ft) data.table::fwrite(data_frame, file.path(temp_dir, paste0(file_name, ".csv")))
  if ("csv.gz" %in% ft) data.table::fwrite(data_frame, file.path(temp_dir, paste0(file_name, ".csv.gz")))
  if ("parquet" %in% ft) arrow::write_parquet(data_frame, file.path(temp_dir, paste0(file_name, ".parquet")))
  if ("qs" %in% ft) {
    qs::qsave(data_frame,
              file.path(temp_dir, paste0(file_name, ".qs")),
              preset = "custom",
              algorithm = "zstd_stream",
              compress_level = 22,
              shuffle_control = 15)
  }

  .filetypes <- paste0(".", ft)

  .file_names <- file.path(temp_dir, paste0(file_name, .filetypes))

  sportsdataverse_upload(.file_names, tag = release_tag, pkg_function = pkg_function, .token = .token)
}
