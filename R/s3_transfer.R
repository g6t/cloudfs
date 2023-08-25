#' @title Upload a local file to S3
#' 
#' @description Uploads a file from project's folder to the corresponding
#'   location on project's S3 folder
#' 
#' @inheritParams cloud_validate_file_path
#'   
#' @examples 
#' \dontrun{
#' # uploads data/demo.csv to 'data' subfolder of project's S3 folder
#' cloud_s3_upload("data/demo.csv")
#' }
#' 
#' @export
cloud_s3_upload <- function(file, root = NULL) {
  cloud_validate_file_path(file)

  check_string(root, alt_null = TRUE)
  if (is.null(root)) root <- cloud_s3_get_root()
  full_path <- file.path(root, file)
  bucket_prefix <- s3_path_to_bucket_prefix(full_path)
  
  s3_file_path <- file.path(root, file)
  if (!file.exists(file)) {
    cli::cli_abort("Can't find {.path {file}}.")
  }
  aws.s3::put_object(
    bucket = bucket_prefix$bucket,
    file = file,
    object = bucket_prefix$prefix
  )
  cli::cli_alert_success(
    "File {.path {file}} uploaded to S3 root {.field {root}}."
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
cloud_s3_download <- function(file, root = NULL) {
  cloud_validate_file_path(file)
  
  check_string(root, alt_null = TRUE)
  if (is.null(root)) root <- cloud_s3_get_root()
  full_path <- file.path(root, file)
  bucket_prefix <- s3_path_to_bucket_prefix(full_path)
  
  aws.s3::save_object(
    bucket = bucket_prefix$bucket,
    object = bucket_prefix$prefix,
    file = file
  )
  cli::cli_alert_success(
    "File {.path {file}} downloaded from S3 root {.field {root}}."
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
                         root = NULL) {
  cloud_validate_file_path(file)
  check_bool(local)
  check_class(fun, "function", alt_null = TRUE)
  check_string(root, alt_null = TRUE)
  
  if (is.null(fun)) {
    fun <- cloud_guess_write_fun(file)
  }
  
  if (is.null(root)) root <- cloud_s3_get_root()
  full_path <- file.path(root, file)
  bucket_prefix <- s3_path_to_bucket_prefix(full_path)
  
  if (local) {
    local_file <- file
  } else {
    local_file <- tempfile(fileext = paste0(".", tools::file_ext(file)))
  }
  local_dir <- dirname(local_file)
  if (!dir.exists(local_dir)) dir.create(local_dir, recursive = TRUE)
  
  fun(x, local_file, ...)
  
  aws.s3::put_object(
    file = local_file,
    bucket = bucket_prefix$bucket,
    object = bucket_prefix$prefix
  )
  
  if (!local) {unlink(local_file)}
  cli::cli_alert_success(
    "Written to {.path {file}} in S3 root {.field {root}}."
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
cloud_s3_read <- function(file, fun = NULL, ..., root = NULL) {
  cloud_validate_file_path(file)
  check_string(root, alt_null = TRUE)
  
  if (is.null(fun)) {
    fun <- cloud_guess_read_fun(file)
  }
  
  if (is.null(root)) root <- cloud_s3_get_root()
  full_path <- file.path(root, file)
  bucket_prefix <- s3_path_to_bucket_prefix(full_path)
  
  cli::cli_alert_info(
    "Trying to read {.path {file}} from S3 root {.field {root}}."
  )
  res <- 
    aws.s3::s3read_using(
      bucket = bucket_prefix$bucket,
      object = bucket_prefix$prefix,
      FUN = fun,
      ...
    )

  meta <- cloud_s3_get_obj_meta(bucket_prefix$bucket, bucket_prefix$prefix)
  
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
cloud_s3_get_obj_meta <- function(bucket, prefix) {
  check_string(bucket)
  check_string(prefix)
  obj <- 
    aws.s3::get_bucket(
      bucket = bucket,
      prefix = prefix,
      delimiter = "/"
    )
  
  if (length(obj) == 0) cli::cli_abort(
    "S3 request for {.path {prefix}} in {.path {bucket}} bucket did not \\
    return any results."
  )

  if (length(obj) > 1) cli::cli_abort(
    "S3 request for {.path {prefix}} in {.path {bucket}} bucket returned \\
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
