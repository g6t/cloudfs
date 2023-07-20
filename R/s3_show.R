#' @description Given a path like "//my_bucket/project///data"
#' 1. Replace all repeating all slashes with single slashes.
#' 2. Remove heading slashes.
#' 3. Extract `bucket` as the first token of the path
#' 4. Extract `prefix` as the remaining part.
#' 5. Return `bucket` and `prefix` in a named list
#' 
#' @noRd
s3_path_to_bucket_prefix <- function(path) {
  check_string(path)
  path <- gsub("/+", "/", path)
  path <- gsub("^/", "", path)
  bucket <- gsub("\\/.*", "", path)
  prefix <- substring(path, nchar(bucket) + 2)
  list(bucket = bucket, prefix = prefix, path = path)
}

#' @title Browse a path on S3
#' @description For internal use
#' @examples 
#' \dontrun{
#' # opens list of buckets
#' cloud_s3_browse_path()
#' 
#' # opens bucket called "projects"
#' cloud_s3_browse_path("projects")
#' 
#' # opens folder "alpha" in bucket "projects"
#' cloud_s3_browse_path("projects/alpha")
#' }
#' 
#' @noRd
cloud_s3_browse_path <- function(path = "") {
  bucket_prefix <- s3_path_to_bucket_prefix(path)
  
  url <- "https://s3.console.aws.amazon.com/s3/buckets/"
  
  if (bucket_prefix$bucket == "") utils::browseURL(url)
  
  url <- paste0(url_head, bucket_prefix$bucket)
  if (length(prefix) != 0) 
    url <- paste0(url, "?prefix=", bucket_prefix$prefix, "/")
  utils::browseURL(url)
}

#' @title Browse project's S3 folder
#' 
#' @description Opens project's S3 folder in browser.
#' 
#' @inheritParams cloud_s3_ls
#' @param path (optional) Path inside S3 folder to open. By default, when 
#'   `path = ""`, navigates to the root level of project's S3 folder.
#' 
#' @examples 
#' \dontrun{
#' cloud_s3_browse()
#' cloud_s3_browse("data")
#' }
#' 
#' @export
cloud_s3_browse <- function(path = "", root = NULL) {
  check_string(path)
  check_string(root, alt_null = TRUE)
  if (is.null(root)) root <- cloud_s3_get_root()
  full_path <- file.path(root, path, "/")
  cloud_s3_browse_path(full_path)
}

#' @title List Contents of Project's S3 Folder
#' 
#' @description Prints names, timestamps and sizes of files and folders inside
#'   S3 folder.
#'
#' @inheritParams cloud_prep_ls
#' 
#' @param path (optional) Path inside S3 folder to list contents of a subfolder.
#'   By default, when `path = ""`, lists root-level files and folders.
#' @param root S3 path of the project root -- point relative to which to
#'   consider all relative paths. When left as `NULL`, the root is automatically
#'   derived from the `cloudfs.s3` field of the project's DESCRIPTION file.
#' 
#' @examples 
#' \dontrun{
#' # list only root-level files and folders
#' cloud_s3_ls() 
#' 
#' # list all files in all nested folders
#' cloud_s3_ls(recursive = TRUE)
#' 
#' # list contents of "plots/barplots" subfolder
#' cloud_s3_ls("plots/barplots")
#' }
#' 
#' @export
cloud_s3_ls <- function(path = "", recursive = FALSE, full_names = FALSE,
                      root = NULL) {
  check_string(path)
  check_bool(recursive)
  check_bool(full_names)
  check_string(root, alt_null = TRUE)
  
  if (is.null(root)) root <- cloud_s3_get_root()
  full_path <- file.path(root, path, "/")
  bucket_prefix <- s3_path_to_bucket_prefix(full_path)
  
  cli::cli_text("{.field S3 path}: {.path {bucket_prefix$path}}")

  # NOTE: this lists all contents recursively regardless of `recursive`
  # parameter because this way it is easier to parse the response. Shouldn't
  # work any slower anyways unless a project contains hundreds nested folders
  # with thousands of files (highly unlikely).
  resp_df <- 
    aws.s3::get_bucket_df(
      bucket = bucket_prefix$bucket,
      delimiter = "",
      prefix = bucket_prefix$prefix,
      max = Inf
    )
  
  if (nrow(resp_df) == 0) cli::cli_abort("Path does not exist.")
  
  cont_df <- tibble(.rows = nrow(resp_df))
  cont_df$short_name <- gsub(paste0("^", bucket_prefix$prefix), "", resp_df$Key)
  cont_df$last_modified <- 
    as.POSIXct(resp_df$LastModified, format = "%Y-%m-%dT%H:%M:%OS", tz = "EST")
  cont_df$size_b <- as.integer(resp_df$Size)
  
  cloud_prep_ls(
    cont_df, 
    path = path,
    recursive = recursive,
    full_names = full_names
  )
}
