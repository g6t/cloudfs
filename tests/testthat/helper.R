# misc ----
regex_relax_spaces <- function(x) {
  gsub("(\\s)+", "(\\\\s)+", x = x, fixed = FALSE)
}

vals_to_names <- function(x) {
  names(x) <- x
  x
}

#' @description Copied from [rlang:::set_attrs_impl] 
#' @noRd
set_attributes <- function (.x, ...) 
{
  attrs <- dots_list(...)
  set_attrs_null <- list(NULL)
  names(set_attrs_null) <- ''
  if (identical(attrs, set_attrs_null)) {
    attributes(.x) <- NULL
  }
  else {
    attributes(.x) <- c(attributes(.x), attrs)
  }
  .x
}

# Project ----
init_tmp_project <- function(description = TRUE) {
  project <- tempfile(pattern = "project_")
  dir.create(project)
  if (description) init_desc(project)
  project
}

remove_tmp_project <- function(project) {
  unlink(project, recursive = TRUE, force = TRUE)
}

# Google Drive ----
skip_if_no_drive_token <- function() {
  testthat::skip_if_not(googledrive::drive_has_token(), "No Drive token")
}

create_tmp_drive_dir <- function() {
  if (nrow(googledrive::drive_get("~/tmp/")) == 0) {
    googledrive::drive_mkdir("tmp")
  }
  dir_name <- format(Sys.time(), "cloudfs_%Y-%m-%dT%H:%M:%S")
  res_dribble <- googledrive::drive_mkdir(dir_name, path = "~/tmp/")
  res_dribble$id
}

cloud_drive_attach_tmp <- function(project = ".") {
  id <- create_tmp_drive_dir()
  desc::desc_set(cloudfs.drive = id, file = project)
}

mock_cloud_attach <- function(drive = "aaaaaa", s3 = "test-r-package/cloudfs") {
  function(project) {
    desc::desc_set(
      cloudfs.drive = drive,
      cloudfs.s3 = s3,
      file = project
    )
  }
}

# S3 ----
has_s3_access <- function() {
  tryCatch(
    {
      buckets <- aws.s3::bucketlist()
      is.data.frame(buckets)
    }, 
    error = function(e) FALSE
  )
}

skip_if_no_s3_access <- function() {
  testthat::skip_if_not(has_s3_access(), "No S3 access")
}

create_tmp_s3_dir <- function() {
  dir_name <- format(Sys.time(), "cloudfs_%Y-%m-%dT%H-%M-%S")
  aws.s3::put_folder(dir_name, bucket = "test-r-package")
  file.path("test-r-package", dir_name)
}

cloud_s3_attach_tmp <- function(project = ".") {
  path <- create_tmp_s3_dir()
  desc::desc_set(cloudfs.s3 = path, file = project)
}

#' @description Remove a folder in the monorepo-data bucket and all its
#'   contents.
#' 
#' @noRd
s3_remove_folder <- function(path) {
  bucket_prefix <- s3_path_to_bucket_prefix(path)
  bucket <- bucket_prefix$bucket
  prefix <- bucket_prefix$prefix
  
  resp_df <- 
    aws.s3::get_bucket_df(
      bucket = bucket,
      delimiter = "",
      prefix = prefix,
      max = Inf
    )
  
  for (key in resp_df$Key) {
    aws.s3::delete_object(key, bucket = bucket)
  }
  aws.s3::delete_object(prefix, bucket = bucket)
}

