skip_if_no_drive_token <- function() {
  testthat::skip_if_not(googledrive::drive_has_token(), "No Drive token")
}
