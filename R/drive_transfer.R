#' @title Upload a local file to Google Drive
#' 
#' @description Uploads a local file from the project's directory to its
#'   corresponding location within the project's Google Drive root folder.
#' 
#' @inheritParams doc_file
#' @inheritParams cloud_drive_ls
#'  
#' @inherit cloud_drive_find_path details
#' 
#' @return Invisibly returns a [googledrive::dribble] object representing the
#'   uploaded file on Google Drive.
#'   
#' @examplesIf interactive() 
#' # create a toy csv file
#' dir.create("toy_data")
#' write.csv(mtcars, "toy_data/mtcars.csv")
#' 
#' # uploads toy_data/mtcars.csv to 'data' subfolder of project's 
#' # Google Drive folder
#' cloud_drive_upload("toy_data/mtcars.csv")
#' 
#' # clean up
#' unlink("toy_data", recursive = TRUE)
#' 
#' @export
cloud_drive_upload <- function(file, root = NULL) {
  check_path(file)
  
  if (!file.exists(file)) {
    cli::cli_abort("File {.path {file}} does not exist.")
  }
  
  check_string(root, alt_null = TRUE)
  if (is.null(root)) root <- cloud_drive_get_root()
  
  file_dir <- dirname(file)
  if (file_dir == ".") file_dir <- ""
  file_dir_id <- cloud_drive_find_path(root, file_dir, create = TRUE)
  
  id <- cloud_drive_put(media = file, path = file_dir_id)
  cli::cli_alert_success(
    "File {.path {file}} uploaded to Google Drive."
  )
  invisible(id)
}

#' @title Download a file from Google Drive to the local project folder
#' 
#' @description Retrieves a file from the project's Google Drive folder and 
#'   saves it to the local project folder, maintaining the original folder 
#'   structure.
#' 
#' @inheritParams doc_file
#' @inheritParams cloud_drive_ls
#' 
#' @inherit cloud_drive_find_path details
#' 
#' @return Invisibly returns `NULL` after successfully downloading the file.
#' 
#' @examplesIf interactive() 
#' # downloads toy_data/demo.csv from project's Google Drive folder 
#' # (provided it exists) and saves it to local 'toy_data' folder
#' cloud_drive_download("toy_data/demo.csv")
#' 
#' # clean up
#' unlink("toy_data", recursive = TRUE)
#' 
#' @export
cloud_drive_download <- function(file, root = NULL) {
  check_path(file)
  
  check_string(root, alt_null = TRUE)
  if (is.null(root)) root <- cloud_drive_get_root()
  
  file_dir <- dirname(file)
  if (!dir.exists(file_dir)) dir.create(file_dir, recursive = TRUE)
  file_id <- cloud_drive_find_path(root, file)
  
  cloud_drive_download_by_id(
    file = file_id,
    path = file,
    overwrite = TRUE
  )
  cli::cli_alert_success(
    "File {.path {file}} downloaded from Google Drive."
  )
  invisible(NULL)
}

#' @title Write an object to Google Drive
#' 
#' @description Saves an R object to a designated location in the project's
#'   Google Drive folder. If no custom writing function is provided, the
#'   function will infer the appropriate writing method based on the file's
#'   extension.
#'   
#' @inheritParams doc_file
#' @inheritParams cloud_drive_ls
#' 
#' @param x An R object to be written to Google Drive.
#' @param fun A custom writing function. If `NULL` (default), the appropriate
#'   writing function will be inferred based on the file's extension.
#' @param ... Additional arguments to pass to the writing function `fun`.
#' @param local Logical. If `TRUE`, a local copy of the file will also be
#'   created at the specified path. Default is `FALSE`.
#' 
#' @inheritSection cloud_guess_write_fun Default writing functions
#' 
#' @return Invisibly returns a [googledrive::dribble] object representing the
#'   written file on Google Drive.
#' 
#' @examplesIf interactive() 
#' # write mtcars dataframe to mtcars.csv in data folder
#' cloud_drive_write(mtcars, "data/mtcars.csv")
#' cloud_drive_write(random_forest, "models/random_forest.rds")
#' 
#' # provide custom writing function with parameters 
#' cloud_drive_write(c("one", "two"), "text/count.txt", writeLines, sep = "\n\n")
#' 
#' @export
cloud_drive_write <- function(x, file, fun = NULL, ..., local = FALSE,
                           root = NULL) {
  check_path(file)
  check_bool(local)
  
  if (is.null(fun)) {
    fun <- cloud_guess_write_fun(file)
  }
  
  check_string(root, alt_null = TRUE)
  if (is.null(root)) root <- cloud_drive_get_root()
  
  file_dir <- dirname(file)
  if (file_dir == ".") file_dir <- ""
  file_dir_id <- cloud_drive_find_path(root, file_dir, create = TRUE)
  
  if (local) {
    local_file <- file
  } else {
    file_name <- basename(file)
    local_file <- file.path(tempdir(), file_name)
  }
  local_dir <- dirname(local_file)
  if (!dir.exists(local_dir)) dir.create(local_dir, recursive = TRUE)
  
  fun(x, local_file, ...)
  
  id <- cloud_drive_put(media = local_file, path = file_dir_id)
  
  if (!local) {unlink(local_file)}
  cli::cli_alert_success(
    "Written to {.path {file}} on Google Drive."
  )
  invisible(id)
}

#' @title Read a file from Google Drive
#' 
#' @description Retrieves and reads a file from the project's Google Drive
#'   folder. By default, the function attempts to determine the appropriate
#'   reading function based on the file's extension. However, you can specify a
#'   custom reading function if necessary.
#'   
#' @inheritParams doc_file
#' @inheritParams cloud_drive_ls
#' 
#' @param fun A custom reading function. If `NULL` (default), the appropriate
#'   reading function will be inferred based on the file's extension.
#'
#' @param ... Additional arguments to pass to the reading function `fun`.
#' 
#' @inheritSection cloud_guess_read_fun Default reading functions
#' 
#' @return The content of the file read from Google Drive, with additional 
#'   attributes containing metadata about the file.
#' 
#' @examplesIf interactive() 
#' # provided there are folders called "data" and "models" in the root of your
#' # project's main Google Drive folder and they contain the files mentioned
#' # below
#' cloud_drive_read("data/mtcars.csv")
#' cloud_drive_read("models/random_forest.rds")
#' cloud_drive_read("data/dm.sas7bdat", fun = haven::read_sas)
#' 
#' @export
cloud_drive_read <- function(file, fun = NULL, ..., root = NULL) {
  check_path(file)
  if (is.null(fun)) {
    fun <- cloud_guess_read_fun(file)
  }
  
  check_string(root, alt_null = TRUE)
  if (is.null(root)) root <- cloud_drive_get_root()
  
  file <- clean_up_file_path(file)
  file_id <- cloud_drive_find_path(root, file)
  
  file_name <- basename(file)
  local_file <- file.path(tempdir(), file_name)

  cli::cli_alert_info(
    "Trying to read {.path {file}} from Google Drive."
  )
  
  cloud_drive_download_by_id(
    file = file_id,
    path = local_file,
    overwrite = TRUE
  )
  
  res <- fun(local_file, ...)
  
  meta <- cloud_drive_get_obj_meta(file_id)
  
  for (n in names(meta)) {
    attr(res, n) <- meta[[n]]
    cli::cli_text("{.field {n}}: {.val {meta[[n]]}}")
  }
  
  res
}

#' @title Obtain relevant metadata from a Google Drive object
#' 
#' @description Given an object id returns a list with it's meta information
#'   like e.g. creation date.
#' 
#' @noRd
cloud_drive_get_obj_meta <- function(id) {
  id <- googledrive::as_id(id)
  stopifnot(length(id) == 1)
  
  obj <- googledrive::drive_get(id)
  
  lm_char <- obj$drive_resource[[1]]$modifiedTime
  lm <- as.POSIXct(lm_char, format = "%Y-%m-%dT%H:%M:%OS", tz = "EST")
  
  list(
    cloud = "Google Drive",
    mime_type = obj$drive_resource[[1]]$mimeType,
    last_modified = lm
  )
}
