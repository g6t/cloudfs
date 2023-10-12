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
#' @return Transformed `content` dataframe.
#'   
#' @keywords internal
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

#' @title Bulk Upload Files to S3
#' 
#' @description This function facilitates the bulk uploading of multiple files
#'   from the local project folder to the project's designated S3 folder. By
#'   using [cloud_local_ls], you can obtain a dataframe detailing the contents
#'   of the local folder. Applying `cloud_s3_upload_bulk` to this dataframe
#'   allows you to upload all listed files to S3.
#' 
#' @inheritParams cloud_s3_upload
#' @inheritParams cloud_s3_prep_bulk
#' 
#' @return Invisibly returns the input `content` dataframe.
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
  invisible(content)
}

#' @title Bulk Download Contents from S3
#' 
#' @description Downloads multiple files from an S3 folder based on the output 
#'   dataframe from [cloud_s3_ls]. This function streamlines the process of 
#'   downloading multiple files by allowing you to filter and select specific 
#'   files from the S3 listing and then download them in bulk.
#' 
#' @inheritParams cloud_s3_download
#' @inheritParams cloud_s3_prep_bulk
#' 
#' @return Invisibly returns the input `content` dataframe.
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
  invisible(content)
}

#' @title Write multiple objects to S3 in bulk
#'
#' @description This function allows for the bulk writing of multiple R objects
#'   to the project's designated S3 folder. To prepare a list of objects for
#'   writing, use [cloud_object_ls], which generates a dataframe listing the
#'   objects and their intended destinations in a format akin to the output of
#'   [cloud_s3_ls]. By default, the function determines the appropriate writing
#'   method based on each file's extension. However, if a specific writing
#'   function is provided via the `fun` parameter, it will be applied to all
#'   files, which may not be ideal if dealing with a variety of file types.
#' 
#' @inheritParams cloud_s3_write  
#' @inheritParams cloud_object_prep_bulk
#'
#' @return Invisibly returns the input `content` dataframe.
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
  invisible(content)
}

#' @title Bulk Read Contents from S3
#' 
#' @description This function facilitates the bulk reading of multiple files
#'   from the project's designated S3 folder. By using [cloud_s3_ls], you can
#'   obtain a dataframe detailing the contents of the S3 folder. Applying
#'   `cloud_s3_read_bulk` to this dataframe allows you to read all listed files
#'   into a named list. The function will, by default, infer the appropriate
#'   reading method based on each file's extension. However, if a specific
#'   reading function is provided via the `fun` parameter, it will be applied
#'   uniformly to all files, which may not be suitable for diverse file types.
#' 
#' @inheritParams cloud_s3_read  
#' @inheritParams cloud_s3_prep_bulk
#' 
#' @return A named list where each element corresponds to the content of a file
#'   from S3. The names of the list elements are derived from the file names.
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
