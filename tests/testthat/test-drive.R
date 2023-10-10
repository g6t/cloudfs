test_that("Basic interactions with Google Drive, all in one go", {
  skip_if_offline()
  skip_if_no_drive_token()
  
  project <- init_tmp_project(description = TRUE)
  cloud_drive_attach_tmp(project = project)
  drive_root <- cloud_drive_get_root(project)
  
  # ls empty ----
  ls_empty <- withr::with_dir(
    project,
    cloud_drive_ls()
  )
  
  ls_ptype <- dplyr::tibble(
    name = character(),
    type = character(),
    last_modified = set_attributes(Sys.time()[NULL], tzone = ""),
    size_b = integer(),
    id = googledrive::as_id("a")[NULL]
  )
  
  expect_identical(ls_empty, ls_ptype)
  
  # upload DESCRIPTION and check ls ---
  withr::with_dir(
    project,
    cloud_drive_upload("DESCRIPTION")
  )
  
  ls_now <- withr::with_dir(project, cloud_drive_ls())
  expect_equal(
    ls_now[, c("name", "type")],
    tibble(name = vals_to_names("DESCRIPTION"), type = "")
  )
  
  expect_equal(
    googledrive::drive_ls(drive_root)$id,
    ls_now$id
  )
  
  # write a dataframe ----
  withr::with_dir(
    project,
    cloud_drive_write(mtcars, "data/mtcars.csv")
  )
  
  mtcars_from_drive <- 
    withr::with_dir(project, cloud_drive_read("data/mtcars.csv")) |> 
    set_attributes(cloud = NULL, mime_type = NULL, last_modified = NULL)
  
  expect_identical(
    dplyr::as_tibble(mtcars),
    mtcars_from_drive
  )
  
  ls_now <- withr::with_dir(project, cloud_drive_ls(recursive = TRUE))
  expect_equal(
    ls_now[, c("name", "type")],
    tibble(
      name = vals_to_names(c("DESCRIPTION", "data/mtcars.csv", "data/")),
      type = c("", "csv", "folder")
    )
  )
  
  # download from drive ----
  withr::with_dir(project, cloud_drive_download("data/mtcars.csv"))
  local_ls_now <- withr::with_dir(project, cloud_local_ls("data"))
  expect_equal(
    local_ls_now[, c("name", "type")],
    tibble(
      name = c("data/mtcars.csv" = "mtcars.csv"),
      type = c("csv")
    )
  )
  
  remove_tmp_project(project)
  googledrive::drive_rm(drive_root)
})
