test_that("cloud_drive_get_root(): When no DESCRIPTION proposes to init", {
  project <- init_tmp_project(description = FALSE)
  
  # first reject the proposal and expect error
  expect_error(
    with_mocked_bindings(
      cloud_drive_get_root(project),
      cli_yeah = function(...) FALSE,
    ),
    regex_relax_spaces("Cannot proceed without having 'DESCRIPTION'")
  )

  # agree to the proposal and check that DESCRIPTION was created
  with_mocked_bindings(
    cloud_drive_get_root(project),
    cli_yeah = function(...) TRUE,
    cloud_drive_attach = mock_cloud_attach()
  )
  
  expect_equal(
    unname(desc::desc_get("Package", file = project)),
    "-"
  )
  
  remove_tmp_project(project)
})

test_that("cloud_s3_get_root(): When no DESCRIPTION proposes to init", {
  project <- init_tmp_project(description = FALSE)
  
  # first reject the proposal and expect error
  expect_error(
    with_mocked_bindings(
      cloud_s3_get_root(project),
      cli_yeah = function(...) FALSE,
    ),
    regex_relax_spaces("Cannot proceed without having 'DESCRIPTION'")
  )
  
  # agree to the proposal and check that DESCRIPTION was created
  with_mocked_bindings(
    cloud_s3_get_root(project),
    cli_yeah = function(...) TRUE,
    cloud_s3_attach = mock_cloud_attach()
  )
  
  expect_equal(
    unname(desc::desc_get("Package", file = project)),
    "-"
  )
  
  remove_tmp_project(project)
})

# ----
test_that("roots are retrieved correctly when in a project", {
  project <- init_tmp_project(description = TRUE)
  desc::desc_set(cloudfs.drive = "aaaaaa", file = project)
  desc::desc_set(cloudfs.s3 = "test-r-package/cloudfs", file = project)
  
  expect_equal(
    withr::with_dir(project, as.character(cloud_drive_get_root())),
    "aaaaaa"
  )
  
  expect_equal(
    withr::with_dir(project, as.character(cloud_s3_get_root())),
    "test-r-package/cloudfs"
  )
  
  remove_tmp_project(project)
})
