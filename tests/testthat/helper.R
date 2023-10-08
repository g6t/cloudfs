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
