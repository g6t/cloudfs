#' @title Prepare S3 content dataframe to be used by bulk download/read
#'   functions
#' 
#' @description `cloud_s3_ls` returns a dataframe of contents of an S3 folder.
#'   This dataframe has `name` and `type` columns. `name` may be either full
#'   names or short names (depending on `full_names` parameter of `cloud_s3_ls`),
#'   but `names(name)` will always contain full names. This function:
#'   1. filters out folders
#'   2. extracts `names(name)` into `path` column.
#'   3. informs about the size of files that are to be downloaded/read and asks
#'     for confirmation
#'   
#' @param content (data.frame) Output of `cloud_s3_ls()`
#' @param what What will be done with content, either "read" or "download".
#'   This affects only how messages will look.
#' @param safe_size What is considered to be safe size in bytes to download in
#'   bulk. To show additional caution message in case if you accidentally run
#'   bulk reading on a folder with gigabytes of data.
#' @param quiet All caution messages may be turned off by setting this parameter
#'   to `TRUE`.
#'   
cloud_s3_prep_bulk <- function(content, what = c("read", "upload", "download"),
                             safe_size = 5e7, quiet = FALSE) {
  
  check_class(content, "data.frame")
  stopifnot(all(c("name", "type", "size_b") %in% names(content)))
  check_class(content$name, "character")
  check_class(content$type, "character")
  check_numeric(content$size_b)
  check_bool(quiet)
  what <- rlang::arg_match(what)
  cont <- 
    content %>% 
    filter(.data$type != "folder", !is.na(.data$type)) %>% 
    mutate(path = names(.data$name)) %>% 
    mutate(size_to_print = sapply(
      .data$size_b,
      function(x) format(structure(x, class = "object_size"), units = "auto")
    )) %>% 
    mutate(label = glue::glue("{path} ({size_to_print})")) %>% 
    relocate("name", "path")
  if (nrow(cont) == 0) cli::cli_abort("Nothing to {what}.")
  cli::cli_text("Attempting to {what} the following files:")
  cli::cli_text()
  cli::cli_ul()
  for (i in 1:nrow(cont)) {
    cli::cli_li("{.path {cont$path[[i]]}} ({cont$size_to_print[[i]]})")
  }
  cli::cli_end()
  total_size <- structure(sum(cont$size_b), class = "object_size")
  safe_size <- structure(safe_size, class = "object_size")
  cli::cli_text()
  cli::cli_text(
    "... with total size of {.field {format(total_size, units = 'auto')}}"
  )
  cli::cli_text()
  
  
  if (!quiet) {
    if (total_size > safe_size) {
      cli::cli_warn("This is quite a lot.")
      yeah <- cli_yeah("Do you really wish to continue?")
    } else {
      yeah <- cli_yeah("Do you wish to continue?", straight = TRUE)
    } 
    if (!yeah) cli::cli_abort("Aborting.")
  }
  cont
}

#' @title Upload files to S3 in bulk
#' 
#' @description [cloud_local_ls] function returns a dataframe of contents of
#'   local project folder. `cloud_s3_upload_bulk` can be applied to such a
#'   dataframe to upload all the listed files. [cloud_s3_upload] is used under
#'   the hood.
#'   
#'   The workflow in mind is that you would call `cloud_local_ls()`, then use
#'   dplyr verbs to keep only files that you need and then call
#'   `cloud_s3_upload_bulk` on the result.
#' 
#' @inheritParams cloud_s3_upload
#' @inheritParams cloud_s3_prep_bulk
#' 
#' @examples 
#' \dontrun{
#' cloud_local_ls("plots") %>% 
#'   filter(type == "png") %>% 
#'   cloud_s3_upload_bulk()
#' }
#'   
#' @export
cloud_s3_upload_bulk <- function(content, quiet = FALSE, root = NULL) {
  check_string(root, alt_null = TRUE)
  if (is.null(root)) root <- cloud_s3_get_root()
  
  cont <- cloud_s3_prep_bulk(content, what = "upload", quiet = quiet)
  n <- nrow(cont)
  cli::cli_progress_bar(
    format = "Uploading {cli::pb_bar} [{cli::pb_current}/{cli::pb_total}]",
    total = n
  )
  for (i in seq_along(cont$name)) {
    cli::cli_progress_update()
    cloud_s3_upload(cont$path[[i]], root = root)
  }
  cli::cli_alert_success("Done!")
}

#' @title Download S3 contents in bulk
#' 
#' @description [cloud_s3_ls] function returns a dataframe of contents of an S3
#'   folder. `cloud_s3_download_bulk` can be applied to such a dataframe to
#'   download all the listed files. [cloud_s3_download] is used under the hood.
#'   
#'   The workflow in mind is that you would call `cloud_s3_ls()`, then use dplyr
#'   verbs to keep only files that you need and then call `cloud_s3_download_bulk`
#'   on the result.
#' 
#' @inheritParams cloud_s3_download
#' @inheritParams cloud_s3_prep_bulk
#' 
#' @examples 
#' \dontrun{
#' cloud_s3_ls("data") %>% 
#'   filter(type == "csv") %>% 
#'   cloud_s3_download_bulk()
#' }
#'   
#' @export
cloud_s3_download_bulk <- function(content, quiet = FALSE, root = NULL) {
  check_string(root, alt_null = TRUE)
  if (is.null(root)) root <- cloud_s3_get_root()
  
  cont <- cloud_s3_prep_bulk(content, what = "download", quiet = quiet)
  n <- nrow(cont)
  cli::cli_progress_bar(
    format = "Downloading {cli::pb_bar} [{cli::pb_current}/{cli::pb_total}]",
    total = n
  )
  for (i in seq_along(cont$name)) {
    cli::cli_progress_update()
    cloud_s3_download(cont$path[[i]], root = root)
  }
  cli::cli_alert_success("Done!")
}

#' @title Write objects to S3 in bulk
#'
#' @description Given a named list of objects [cloud_object_ls] function returns
#'   a dataframe similar to the output of [cloud_local_ls] or [cloud_s3_ls].
#'   `cloud_s3_write_bulk` can be applied to such a dataframe to write all the
#'   listed objects to S3. [cloud_s3_write] is used under the hood. It will be
#'   attempted to guess writing function from file extensions. You can pass
#'   writing function manually by setting `fun` parameter, but it means that all
#'   the files will be written using one function. In fact, you probably
#'   shouldn't be writing multiple files of different types in bulk.
#' 
#' @inheritParams cloud_s3_write  
#' @inheritParams cloud_object_prep_bulk
#' 
#' @examples 
#' \dontrun{
#' # write two csv files: data/df_mtcars.csv and data/df_iris.csv
#' cloud_object_ls(
#'   dplyr::lst(mtcars = mtcars, iris = iris),
#'   path = "data",
#'   extension = "csv",
#'   prefix = "df_"
#' ) %>% 
#' cloud_s3_write_bulk()
#' }
#'   
#' @export
cloud_s3_write_bulk <- function(content, fun = NULL, ..., local = FALSE,
                                quiet = FALSE, root = NULL) {
  check_string(root, alt_null = TRUE)
  if (is.null(root)) root <- cloud_s3_get_root()
  
  cont <- cloud_object_prep_bulk(content, quiet = quiet)
  n <- nrow(cont)
  cli::cli_progress_bar(
    format = "Writing {cli::pb_bar} [{cli::pb_current}/{cli::pb_total}]",
    total = n
  )
  for (i in seq_along(cont$name)) {
    cli::cli_progress_update()
    cloud_s3_write(
      x = cont$object[[i]],
      file = cont$path[[i]],
      fun = fun, ...,
      local = local,
      root = root
    )
  }
  cli::cli_alert_success("Done!")
}

#' @title Read S3 contents in bulk
#' 
#' @description [cloud_s3_ls] function returns a dataframe of contents of an S3
#'   folder. `cloud_s3_read_bulk` can be applied to such a dataframe to read all
#'   the listed files into a named list. [cloud_s3_read] is used under the hood.
#'   It will be attempted to guess reading function from file extensions. You
#'   can pass reading function manually by setting `fun` parameter, but it means
#'   that all the files will be read using one function. In fact, you probably
#'   shouldn't be reading multiple files of different types in bulk.
#'   
#'   The workflow in mind is that you would call `cloud_s3_ls()`, then use dplyr
#'   verbs to keep only files that you need and then call `cloud_s3_read_bulk`
#'   on the result. Note that you don't need to filter out folders -- it is done
#'   automatically.
#' 
#' @inheritParams cloud_s3_read  
#' @inheritParams cloud_s3_prep_bulk
#' 
#' @examples 
#' \dontrun{
#' data_lst <- 
#'   cloud_s3_ls("data") %>% 
#'   filter(type == "csv") %>% 
#'   cloud_s3_read_bulk()
#' }
#'   
#' @export
cloud_s3_read_bulk <- function(content, fun = NULL, ..., quiet = FALSE,
                             root = NULL) {
  check_string(root, alt_null = TRUE)
  if (is.null(root)) root <- cloud_s3_get_root()
  
  cont <- cloud_s3_prep_bulk(content, what = "read", quiet = quiet)
  n <- nrow(cont)
  res <- list()
  cli::cli_progress_bar(
    format = "Reading {cli::pb_bar} [{cli::pb_current}/{cli::pb_total}]",
    total = n
  )
  for (i in seq_along(cont$name)) {
    cli::cli_progress_update()
    res[[cont$name[[i]]]] <- 
      cloud_s3_read(cont$path[[i]], fun, ..., root = root)
  }
  cli::cli_alert_success("Done!")
  res
}
