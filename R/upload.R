#' Upload to sportsdataverse release
#'
#' @param files vector of filepaths to upload
#' @param tag release name
#' @param pkg_function related package function name
#' @param ... currently not used
#' @param repo repository to upload to, default: `"sportsdataverse/sportsdataverse-data"`
#' @param overwrite If `TRUE` (the default) existing files will be overwritten
#'
#' @export
sportsdataverse_upload <- function(
  files,
  tag,
  pkg_function = NULL,
  ...,
  repo = "sportsdataverse/sportsdataverse-data",
  overwrite = TRUE
) {
  cli::cli_alert("Uploading {length(files)} files!")
  # create timestamp_files in a temp folder
  timestamp_files <- create_timestamp_file()

  # create package_function files in a temp folder
  pkg_function_files <- create_package_function(tag, pkg_function)

  if (!is.null(pkg_function_files)) {
    timestamp_files <- c(timestamp_files, pkg_function_files)
  }
  # append timestamp files to the actual files to upload
  # timestamp will be right BEFORE the upload begins instead of afterwards
  # but it won't be released at all if the upload breaks
  files <- c(files, timestamp_files)
  # upload files
  gh_cli_release_upload(
    files = files,
    tag = tag,
    repo = repo,
    overwrite = overwrite
  )

  cli::cli_alert(
    "Uploaded {length(files)} to sportsdataverse/sportsdataverse-data @ {tag} on {Sys.time()}"
  )
}

create_timestamp_file <- function() {
  temp_dir <- tempdir(check = TRUE)

  update_time <- format(Sys.time(), tz = "America/Toronto", usetz = TRUE)
  writeLines(update_time, file.path(temp_dir, "timestamp.txt"))

  list(last_updated = update_time) |>
    jsonlite::toJSON(auto_unbox = TRUE) |>
    writeLines(file.path(temp_dir, "timestamp.json"))

  timestamp_files <- file.path(temp_dir, c("timestamp.txt", "timestamp.json"))

  timestamp_files
}


create_package_function <- function(tag, pkg_function) {
  if (!is.null(pkg_function)) {
    temp_dir <- tempdir(check = TRUE)

    package_function <- paste0(pkg_function)
    writeLines(package_function, file.path(temp_dir, "package_function.txt"))

    list(package_function = package_function) |>
      jsonlite::toJSON(auto_unbox = TRUE) |>
      writeLines(file.path(temp_dir, "package_function.json"))

    package_function_files <- file.path(
      temp_dir,
      c("package_function.txt", "package_function.json")
    )

    package_function_files
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
#' @param repo repository to upload to, default: `"sportsdataverse/sportsdataverse-data"`

#'
#' @export
sportsdataverse_save <- function(
  data_frame,
  file_name,
  sportsdataverse_type,
  release_tag,
  pkg_function,
  .token = gh::gh_token(),
  file_types = c("rds", "csv", "parquet"),
  repo = "sportsdataverse/sportsdataverse-data"
) {
  stopifnot(
    is.data.frame(data_frame),
    is.character(file_name),
    is.character(sportsdataverse_type),
    is.character(release_tag),
    is.character(pkg_function),
    is.character(.token),
    is.character(file_types),
    is.character(repo),
    length(file_name) == 1,
    length(sportsdataverse_type) == 1,
    length(release_tag) == 1,
    length(pkg_function) == 1,
    length(.token) == 1,
    length(file_types) >= 1,
    length(repo) == 1
  )

  if ("season" %in% names(data_frame)) {
    data_frame$season <- as.integer(data_frame$season)
  }
  if ("week" %in% names(data_frame)) {
    data_frame$week <- as.integer(data_frame$week)
  }

  attr(data_frame, "sportsdataverse_type") <- sportsdataverse_type
  attr(data_frame, "sportsdataverse_timestamp") <- Sys.time()

  temp_dir <- tempdir(check = TRUE)
  ft <- rlang::arg_match(
    file_types,
    values = c("rds", "csv", "csv.gz", "parquet", "qs"),
    multiple = TRUE
  )

  if ("rds" %in% ft) {
    saveRDS(data_frame, file.path(temp_dir, paste0(file_name, ".rds")))
  }
  if ("csv" %in% ft) {
    data.table::fwrite(
      data_frame,
      file.path(temp_dir, paste0(file_name, ".csv"))
    )
  }
  if ("csv.gz" %in% ft) {
    data.table::fwrite(
      data_frame,
      file.path(temp_dir, paste0(file_name, ".csv.gz"))
    )
  }
  if ("parquet" %in% ft) {
    d <- arrow::as_arrow_table(data_frame)
    d$metadata$sportsdataverse_type <- d$metadata$r$attributes$sportsdataverse_type
    d$metadata$sportsdataverse_timestamp <- d$metadata$r$attributes$sportsdataverse_timestamp
    arrow::write_parquet(d, file.path(temp_dir, paste0(file_name, ".parquet")))
  }
  if ("qs" %in% ft) {
    qs::qsave(
      data_frame,
      file.path(temp_dir, paste0(file_name, ".qs")),
      preset = "custom",
      algorithm = "zstd_stream",
      compress_level = 22,
      shuffle_control = 15
    )
  }

  .filetypes <- paste0(".", ft)

  .file_names <- file.path(temp_dir, paste0(file_name, .filetypes))

  sportsdataverse_upload(
    .file_names,
    tag = release_tag,
    pkg_function = pkg_function,
    .token = .token,
    repo = repo
  )
}
