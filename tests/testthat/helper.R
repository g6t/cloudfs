regex_relax_spaces <- function(x) {
  gsub("(\\s)+", "(\\\\s)+", x = x, fixed = FALSE)
}

skip_if_no_drive_token <- function() {
  testthat::skip_if_not(googledrive::drive_has_token(), "No Drive token")
}

init_tmp_project <- function(description = TRUE) {
  project <- tempfile(pattern = "project_")
  dir.create(project)
  if (description) init_desc(project)
  project
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

remove_tmp_project <- function(project) {
  unlink(project, recursive = TRUE, force = TRUE)
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
