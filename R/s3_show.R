#' @title Open a path specified by a prefix in bucket-name bucket
#' @description For internal use
#' @examples 
#' \dontrun{
#' # opens bucket-name root
#' cloud_s3_browse_prefix()
#' # opens the newsleter folder
#' cloud_s3_browse_prefix("newsletter/")
#' }
#' 
#' @noRd
cloud_s3_browse_prefix <- function(prefix = "") {
  # fix slashes at the beginning and duplicated slashes
  prefix <- gsub("/+", "/", prefix)
  url_head <- "https://s3.console.aws.amazon.com/s3/buckets/bucket-name?region=us-east-1&prefix="
  url_tail <- "&showversions=false"
  url <- paste0(url_head, prefix, url_tail)
  utils::browseURL(url)
}

#' @title Browse project's S3 folder
#' 
#' @description Opens project's S3 folder in browser.
#' 
#' @inheritParams cloud_not_wd_warning
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
  stopifnot(is.character(path) & length(path) == 1)
  s3_folder <- cloud_s3_get_location(project = project)
  prefix <- file.path(s3_folder, path, "/")
  cloud_s3_browse_prefix(prefix)
}

#' @title List Contents of Project's S3 Folder
#' 
#' @description Prints names, timestamps and sizes of files and folders inside
#'   S3 folder.
#'
#' @inheritParams cloud_not_wd_warning
#' @inheritParams cloud_prep_ls
#' 
#' @param path (optional) Path inside S3 folder to list contents of a subfolder.
#'   By default, when `path = ""`, lists root-level files and folders.
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
                      project = getwd()) {
  stopifnot(is.character(path) & length(path) == 1)
  stopifnot(isTRUE(recursive) | isFALSE(recursive))
  stopifnot(isTRUE(full_names) | isFALSE(full_names))
  
  s3_location <- cloud_s3_get_location(project = project)
  s3_path <- clean_file_path(s3_location, path, "/")
  cli::cli_text("{.field S3 path}: {.path {s3_path}}")
  
  s3_path_split <- strsplit(s3_path, "/")[[1]]
  bucket <- s3_path_split[[1]]
  prefix <- paste0(paste(s3_path_split[-1], collapse = "/"), "/")

  # NOTE: this lists all contents recursively regardless of `recursive`
  # parameter because this way it is easier to parse the response. Shouldn't
  # work any slower anyways unless a project contains hundreds nested folders
  # with thousands of files (highly unlikely).
  resp_df <- 
    aws.s3::get_bucket_df(
      bucket = bucket,
      delimiter = "",
      prefix = prefix,
      max = Inf
    )
  
  if (nrow(resp_df) == 0) cli::cli_abort("Path does not exist.")
  
  cont_df <- tibble(.rows = nrow(resp_df))
  cont_df$short_name <- gsub(paste0("^", prefix), "", resp_df$Key)
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
