#' @title Upload a local file to S3
#' 
#' @description Uploads a local file from the project's directory to its
#'   corresponding location within the project's S3 root folder.
#' 
#' @inheritParams cloud_validate_file_path
#' @inheritParams cloud_s3_ls
#'  
#' @return Invisibly returns `NULL` after successfully uploading the file.
#'
#' @examplesIf interactive() 
#' # create a toy csv file
#' dir.create("toy_data")
#' write.csv(mtcars, "toy_data/mtcars.csv")
#' 
#' # uploads toy_data/mtcars.csv to 'data' subfolder of project's S3 folder
#' cloud_s3_upload("toy_data/mtcars.csv")
#' 
#' # clean up
#' unlink("toy_data", recursive = TRUE)
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
  invisible(NULL)
}




#' @title Download a file from S3 to the local project folder
#' 
#' @description Retrieves a file from the project's S3 root folder and saves it
#'   to the local project folder, maintaining the original folder structure.
#' 
#' @inheritParams cloud_validate_file_path
#' @inheritParams cloud_s3_ls
#' 
#' @return Invisibly returns `NULL` after successfully downloading the file.
#' 
#' @examplesIf interactive() 
#' # downloads toy_data/demo.csv from project's S3 folder (provided it exists)
#' # and saves it to local 'toy_data' folder
#' cloud_s3_download("toy_data/demo.csv")
#' 
#' # clean up
#' unlink("toy_data", recursive = TRUE)
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
  invisible(NULL)
}

#' @title Write an object to S3
#' 
#' @description Saves an R object to a designated location in the project's
#'   S3 storage. If no custom writing function is specified, the function will
#'   infer the appropriate writing method based on the file's extension.
#'   
#' @inheritParams cloud_validate_file_path
#' @inheritParams cloud_s3_ls
#' 
#' @param x An R object to be written to S3.
#' @param fun A custom writing function. If `NULL` (default), the appropriate
#'   writing function will be inferred based on the file's extension.
#' @param ... Additional arguments to pass to the writing function `fun`.
#' @param local Logical. If `TRUE`, a local copy of the file will also be
#'   created at the specified path. Default is `FALSE`.
#' 
#' @inheritSection cloud_guess_write_fun Default writing functions
#'
#' @return Invisibly returns `NULL` after successfully writing the object to S3.
#' 
#' @examplesIf interactive() 
#' # write mtcars dataframe to mtcars.csv in data folder
#' cloud_s3_write(mtcars, "data/mtcars.csv")
#' cloud_s3_write(random_forest, "models/random_forest.rds")
#' 
#' # provide custom writing function with parameters 
#' cloud_s3_write(c("one", "two"), "text/count.txt", writeLines, sep = "\n\n")
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
  invisible(NULL)
}

#' @title Read a file from S3
#' 
#' @description Retrieves and reads a file from the project's S3 folder. By
#'   default, the function attempts to determine the appropriate reading
#'   function based on the file's extension. However, you can specify a custom
#'   reading function if necessary.
#'   
#' @inheritParams cloud_validate_file_path
#' @inheritParams cloud_s3_ls
#' 
#' @param fun A custom reading function. If `NULL` (default), the appropriate
#'   reading function will be inferred based on the file's extension.
#' @param ... Additional arguments to pass to the reading function `fun`.
#' 
#' @return The content of the file read from S3, with additional attributes
#'   containing metadata about the file.
#' 
#' @inheritSection cloud_guess_read_fun Default reading functions
#' 
#' @examplesIf interactive() 
#' # provided there are folders called "data" and "models" in the root of your
#' # project's main S3 folder and they contain the files mentioned below
#' cloud_s3_read("data/mtcars.csv")
#' cloud_s3_read("models/random_forest.rds")
#' cloud_s3_read("data/dm.sas7bdat", fun = haven::read_sas)
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
