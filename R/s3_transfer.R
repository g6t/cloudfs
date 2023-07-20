#' @title Upload a local file to S3
#' 
#' @description Uploads a file from project's folder to the corresponding
#'   location on project's S3 folder
#' 
#' @inheritParams cloud_validate_file_path
#' @inheritParams cloud_not_wd_warning
#'   
#' @examples 
#' \dontrun{
#' # uploads data/demo.csv to 'data' subfolder of project's S3 folder
#' cloud_s3_upload("data/demo.csv")
#' }
#' 
#' @export
cloud_s3_upload <- function(file, project = getwd()) {
  cloud_validate_file_path(file)
  cloud_not_wd_warning(project)
  project_name <- proj_desc_get("Name", project)
  s3_folder <- cloud_s3_get_root(project = project)
  file_path <- normalizePath(file.path(project, file), mustWork = FALSE)
  s3_file_path <- file.path(s3_folder, file)
  if (!file.exists(file_path)) {
    cli::cli_abort("Can't find {.path {file}} under {.field {project}}.")
  }
  aws.s3::put_object(
    bucket = "bucket-name",
    file = file_path,
    object = s3_file_path
  )
  cli::cli_alert_success(
    "File {.path {file}} uploaded to S3 folder of {.field {project_name}} project."
  )
}

#' @title Download a file from S3 to local project folder
#' 
#' @description Downloads a file from project's S3 folder to local project 
#' folder and saves it preserving folder structure.
#' 
#' @inheritParams cloud_validate_file_path
#' @inheritParams cloud_not_wd_warning
#' 
#' @examples 
#' \dontrun{
#' # downloads data/demo.csv from project's S3 folder
#' # and saves it to local 'data' folder
#' cloud_s3_download("data/demo.csv")
#' }
#' 
#' @export
cloud_s3_download <- function(file, project = getwd()) {
  cloud_validate_file_path(file)
  cloud_not_wd_warning(project)
  project_name <- proj_desc_get("Name", project)
  s3_folder <- cloud_s3_get_root(project = project)
  file_path <- normalizePath(file.path(project, file), mustWork = FALSE)
  s3_file_path <- file.path(s3_folder, file)
  
  aws.s3::save_object(
    bucket = "bucket-name",
    object = s3_file_path,
    file = file_path
  )
  cli::cli_alert_success(
    "File {.path {file}} downloaded from S3 folder of {.field {project_name}} \\
    project to {.path {file_path}}."
  )
}

#' @title Write an object to S3
#' 
#' @description Writes an R object to a file in project's S3 folder. In most
#'   cases it is expected to be used for writing data.frames as csv, sav or any
#'   other tabular data formats to S3. But it is also possible to save any R
#'   object if a proper saving function is provided. It tries to guess a
#'   suitable writing function and the output file name where possible.
#'   
#' @inheritParams cloud_validate_file_path
#' @inheritParams cloud_not_wd_warning
#' 
#' @param x an R object (e.g. data frame) to write to S3.
#' @param fun a function to write a file to cloud location to which x and a file
#'   path will be passed (in that order). By default, if `fun = NULL`, it will
#'   be attempted to find an appropriate writing function based on the file
#'   extension.
#' @param ... further parameters to pass to `fun`
#' @param local (logical) If `TRUE`, will additionally create a local file at
#'   the corresponding path. Default is `FALSE`.
#' 
#' @inheritSection cloud_guess_write_fun Default writing functions
#' 
#' @examples 
#' \dontrun{
#' # write mtcars dataframe to mtcars.csv in data folder
#' cloud_s3_write(mtcars, "data/mtcars.csv")
#' cloud_s3_write(random_forest, "models/random_forest.rds")
#' 
#' # provide custom writing function with parameters 
#' cloud_s3_write(c("one", "two"), "text/count.txt", writeLines, sep = "\n\n")
#' }
#' 
#' @export
cloud_s3_write <- function(x, file, fun = NULL, ..., local = FALSE,
                         project = getwd()) {
  cloud_validate_file_path(file)
  stopifnot(is.logical(local))
  project_name <- proj_desc_get("Name", project)
  if (is.null(fun)) {
    fun <- cloud_guess_write_fun(file)
  }
  s3_folder <- cloud_s3_get_root(project = project)
  s3_file_path <- file.path(s3_folder, file)
  
  if (local) {
    local_file <- file.path(project, file)
  } else {
    local_file <- tempfile(fileext = paste0(".", tools::file_ext(file)))
  }
  local_dir <- dirname(local_file)
  if (!dir.exists(local_dir)) dir.create(local_dir, recursive = TRUE)
  
  fun(x, local_file, ...)
  
  aws.s3::put_object(
    file = local_file,
    bucket = "bucket-name",
    object = s3_file_path
  )
  
  if (!local) {unlink(local_file)}
  cli::cli_alert_success(
    "Written to {.path {file}} on S3 folder of {.field {project_name}} project."
  )
}

#' @title Read an object from S3
#' 
#' @description Reads a file from project's S3. This function tries to guess an
#'   appropriate reading function based on the `file` name, but you can also
#'   provide a function by yourself if it's needed.
#'   
#' @inheritParams cloud_not_wd_warning
#' @inheritParams cloud_validate_file_path
#' 
#' @param fun Reading function. By default is `NULL` which means that it will
#'   be attempted to guess an appropriate reading function from file extension.
#' @param ... Further parameters to pass to `fun`.
#' 
#' @inheritSection cloud_guess_read_fun Default reading functions
#' 
#' @examples 
#' \dontrun{
#' cloud_s3_read("data/mtcars.csv")
#' cloud_s3_read("models/random_forest.rds")
#' cloud_s3_read("data/dm.sas7bdat", fun = haven::read_sas)
#' }
#' 
#' @export
cloud_s3_read <- function(file, fun = NULL, ..., project = getwd()) {
  cloud_validate_file_path(file)
  project_name <- proj_desc_get("Name", project)
  if (is.null(fun)) {
    fun <- cloud_guess_read_fun(file)
  }
  s3_folder <- cloud_s3_get_root(project = project)
  s3_file_path <- file.path(s3_folder, file)
  cli::cli_alert_info(
    "Trying to read {.path {file}} from S3 folder of {.field {project_name}} project."
  )
  res <- 
    aws.s3::s3read_using(
      bucket = "bucket-name",
      object = s3_file_path,
      FUN = fun,
      ...
    )

  meta <- cloud_s3_get_obj_meta(s3_file_path)
  
  for (n in names(meta)) {
    attr(res, n) <- meta[[n]]
    cli::cli_text("{.field {n}}: {.val {meta[[n]]}}")
  }
  
  res
}


#' @title Obtain relevant meta from an S3 object
#' 
#' @description Given a file path on the 'bucket-name' bucket returns a list
#' with it's meta information like e.g. creation date.
#' 
#' @noRd
cloud_s3_get_obj_meta <- function(path) {
  stopifnot(is.character("path") & length(path) == 1)
  obj <- 
    aws.s3::get_bucket(
      bucket = "bucket-name",
      prefix = path,
      delimiter = "/"
    )
  
  if (length(obj) == 0) cli::cli_abort(
    "S3 request for {.path {path}} in {.path bucket-name} bucket did not \\
    return any results."
  )

  if (length(obj) > 1) cli::cli_abort(
    "S3 request for {.path {path}} in {.path bucket-name} bucket returned \\
    more than one result."
  )
    
  lm_char <- obj[[1]]$LastModified
  lm <- as.POSIXct(lm_char, format = "%Y-%m-%dT%H:%M:%OS", tz = "EST")
  
  list(
    cloud = "S3",
    key = obj[[1]]$Key,
    last_modified = lm
  )
}
