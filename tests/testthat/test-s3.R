test_that("Basic interactions with S3, all in one go", {
  skip_if_offline()
  skip_if_no_s3_access()
  
  project <- init_tmp_project(description = TRUE)
  cloud_s3_attach_tmp(project = project)
  s3_root <- cloud_s3_get_root(project)
  
  # ls empty ----
  ls_empty <- withr::with_dir(
    project,
    cloud_s3_ls()
  )
  
  ls_ptype <- dplyr::tibble(
    name = character(),
    type = character(),
    last_modified = set_attributes(Sys.time()[NULL], tzone = ""),
    size_b = integer()
  )
  
  expect_identical(ls_empty, ls_ptype)
  
  # upload DESCRIPTION and check ls ---
  withr::with_dir(
    project,
    cloud_s3_upload("DESCRIPTION")
  )
  
  ls_now <- withr::with_dir(project, cloud_s3_ls())
  expect_equal(
    ls_now[, c("name", "type")],
    tibble(name = vals_to_names("DESCRIPTION"), type = "")
  )
  
  # write a dataframe ----
  withr::with_dir(
    project,
    cloud_s3_write(mtcars, "data/mtcars.csv")
  )
  
  mtcars_from_s3 <- 
    withr::with_dir(project, cloud_s3_read("data/mtcars.csv")) |> 
    set_attributes(cloud = NULL, mime_type = NULL, last_modified = NULL, key = NULL)
  
  expect_identical(
    dplyr::as_tibble(mtcars),
    mtcars_from_s3
  )
  
  ls_now <- withr::with_dir(project, cloud_s3_ls(recursive = TRUE)) |> 
    dplyr::arrange(last_modified)
  expect_equal(
    ls_now[, c("name", "type")],
    tibble(
      name = vals_to_names(c("DESCRIPTION", "data/mtcars.csv")),
      type = c("", "csv")
    )
  )
  
  # download from s3 ----
  withr::with_dir(project, cloud_s3_download("data/mtcars.csv"))
  local_ls_now <- withr::with_dir(project, cloud_local_ls("data"))
  expect_equal(
    local_ls_now[, c("name", "type")],
    tibble(
      name = c("data/mtcars.csv" = "mtcars.csv"),
      type = c("csv")
    )
  )
  
  remove_tmp_project(project)
  s3_remove_folder(s3_root)
})
