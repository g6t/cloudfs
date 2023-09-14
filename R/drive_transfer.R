#' @title Upload a local file to Google Drive
#' 
#' @description Uploads a file from project's folder to the corresponding
#'   location on project's Google Drive folder
#' 
#' @inheritParams cloud_validate_file_path
#' @inheritParams cloud_drive_ls
#'  
#' @inherit cloud_drive_find_path details
#'   
#' @examples 
#' \dontrun{
#' # uploads data/demo.csv to 'data' subfolder of project's Google Drive folder
#' cloud_drive_upload("data/demo.csv")
#' }
#' 
#' @export
cloud_drive_upload <- function(file, root = NULL) {
  cloud_validate_file_path(file)
  
  if (!file.exists(file)) {
    cli::cli_abort("File {.path {file}} does not exist.")
  }
  
  check_string(root, alt_null = TRUE)
  if (is.null(root)) root <- cloud_drive_get_root()
  
  file_dir <- dirname(file)
  if (file_dir == ".") file_dir <- ""
  file_dir_id <- cloud_drive_find_path(root, file_dir, create = TRUE)
  
  cloud_drive_put(media = file, path = file_dir_id)
  cli::cli_alert_success(
    "File {.path {file}} uploaded to Google Drive."
  )
}

#' @title Download a file from Google Drive to local project folder
#' 
#' @description Downloads a file from project's Google Drive folder to local
#'   project folder and saves it preserving folder structure.
#' 
#' @inheritParams cloud_validate_file_path
#' @inheritParams cloud_drive_ls
#' 
#' @inherit cloud_drive_find_path details
#' 
#' @examples 
#' \dontrun{
#' # downloads data/demo.csv from project's Google Drive folder
#' # and saves it to local 'data' folder
#' cloud_drive_download("data/demo.csv")
#' }
#' 
#' @export
cloud_drive_download <- function(file, root = NULL) {
  cloud_validate_file_path(file)
  
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
}

#' @title Write an object to Google Drive
#' 
#' @description Writes an R object to a file in project's Google Drive folder.
#'   In most cases it is expected to be used for writing data.frames as csv, sav
#'   or any other tabular data formats to Google Drive. But it is also possible
#'   to save any R object if a proper saving function is provided. It tries to
#'   guess a suitable writing function and the output file name where possible.
#'   
#' @inheritParams cloud_validate_file_path
#' @inheritParams cloud_drive_ls
#' 
#' @param x An R object (e.g. data frame) to write to Google Drive.
#' @param fun A function to write a file to cloud location to which x and a file
#'   path will be passed (in that order). By default, if `fun = NULL`, it will
#'   be attempted to find an appropriate writing function based on the file
#'   extension.
#' @param ... Further parameters to pass to `fun`
#' @param local (logical) If `TRUE`, will additionally create a local file at
#'   the corresponding path. Default is `FALSE`.
#' 
#' @inheritSection cloud_guess_write_fun Default writing functions
#' 
#' @examples 
#' \dontrun{
#' # write mtcars dataframe to mtcars.csv in data folder
#' cloud_drive_write(mtcars, "data/mtcars.csv")
#' cloud_drive_write(random_forest, "models/random_forest.rds")
#' 
#' # provide custom writing function with parameters 
#' cloud_drive_write(c("one", "two"), "text/count.txt", writeLines, sep = "\n\n")
#' }
#' 
#' @export
cloud_drive_write <- function(x, file, fun = NULL, ..., local = FALSE,
                           root = NULL) {
  cloud_validate_file_path(file)
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
  
  cloud_drive_put(media = local_file, path = file_dir_id)
  
  if (!local) {unlink(local_file)}
  cli::cli_alert_success(
    "Written to {.path {file}} on Google Drive."
  )
}

#' @title Read an object from Google Drive
#' 
#' @description Reads a file from project's Google Drive. This function tries to
#'   guess an appropriate reading function based on the `file` name, but you can
#'   also provide a function by yourself if it's needed.
#'   
#' @inheritParams cloud_validate_file_path
#' @inheritParams cloud_drive_ls
#' 
#' @param fun Reading function. By default is `NULL` which means that it will
#'   be attempted to guess an appropriate reading function from file extension.
#' @param ... Further parameters to pass to `fun`.
#' 
#' @inheritSection cloud_guess_read_fun Default reading functions
#' 
#' @examples 
#' \dontrun{
#' cloud_drive_read("data/mtcars.csv")
#' cloud_drive_read("models/random_forest.rds")
#' cloud_drive_read("data/dm.sas7bdat", fun = haven::read_sas)
#' }
#' 
#' @export
cloud_drive_read <- function(file, fun = NULL, ..., root = NULL) {
  cloud_validate_file_path(file)
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
    "Trying to read {.path file} from the Google Drive folder of \\
    {.field project_name} project."
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
