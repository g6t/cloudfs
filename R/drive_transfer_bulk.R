#' @title Prepare Google Drive content dataframe to be used by bulk
#'   download/read functions
#' 
#' @description `cloud_drive_ls` returns a dataframe of contents of a Google
#'   Drive folder. This dataframe has `name`, `type` and `id` columns. `name`
#'   may be either full names or short names (depending on `full_names`
#'   parameter of `cloud_drive_ls`), but `names(name)` will always contain full
#'   names. This function:
#'   1. filters out folders
#'   2. extracts `names(name)` into `path` column.
#'   2. informs about the size of files that are to be downloaded/read and asks
#'     for confirmation 
#'   
#' @param content (data.frame) Output of `cloud_drive_ls()`
#' @param what What will be done with content, either "read" or "download".
#'   This affects only how messages will look.
#' @param safe_size What is considered to be safe size in bytes to download in
#'   bulk. To show additional caution message in case if you accidentally run
#'   bulk reading on a folder with gigabytes of data.
#' @param quiet All caution messages may be turned off by setting this parameter
#'   to `TRUE`.
#'   
cloud_drive_prep_bulk <- function(content, what = c("read", "download"),
                               safe_size = 5e7, quiet = FALSE) {
  stopifnot(is.data.frame(content))
  stopifnot(all(c("name", "type", "size_b", "id") %in% names(content)))
  check_string(content$name)
  check_string(content$type)
  check_numeric(content$size_b)
  check_string(content$id)
  check_bool(quiet)
  what <- what[[1]]
  cont <- 
    content %>% 
    filter(.data$type != "folder", !is.na(.data$type)) %>% 
    mutate(path = names(.data$name)) %>% 
    mutate(size_to_print = sapply(
      .data$size_b,
      function(x) format(structure(x, class = "object_size"), units = "auto")
    )) %>% 
    mutate(label = glue::glue("{path} ({size_to_print})"))
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


#' @title Prepare content dataframe for a bulk write/upload to Google Drive
#' 
#' @description When we upload something to Google Drive, we need to know ids of
#'   the folders we upload to. The process of finding a folder id takes some
#'   time, so finding destination folder id for each file separately is not
#'   optimal. This function identifies the list of all destination directories,
#'   finds their ids and merges to the content dataframe accordingly.
#' 
#' @noRd   
cloud_drive_content_find_dirs <- function(cont, root = NULL) {
  if (nrow(cont) == 0) return(cont)
  cont$dir <- dirname(cont$path)
  dir_df <- tibble(dir = unique(cont$dir))
  dir_df$dir_id <- googledrive::as_id(NA_character_)
  
  check_string(root, alt_null = TRUE)
  if (is.null(root)) root <- cloud_drive_get_root()
  
  for (i in seq_along(dir_df$dir)) {
    dir_df$dir_id[[i]] <- 
      cloud_drive_find_path(root, dir_df$dir[[i]], create = TRUE)
  }
  
  left_join(cont, dir_df, by = "dir")
}


#' @title Upload files to Google Drive in bulk
#' 
#' @description [cloud_local_ls] function returns a dataframe of contents of
#'   local project folder. `cloud_drive_upload_bulk` can be applied to such a
#'   dataframe to upload all the listed files.
#'   
#'   The workflow in mind is that you would call `cloud_local_ls()`, then use
#'   dplyr verbs to keep only files that you need and then call
#'   `cloud_drive_upload_bulk` on the result.
#' 
#' @inheritParams cloud_drive_upload
#' @inheritParams cloud_s3_prep_bulk
#' 
#' @examples 
#' \dontrun{
#' cloud_local_ls("plots") %>% 
#'   filter(type == "png") %>% 
#'   cloud_drive_upload_bulk()
#' }
#'   
#' @export
cloud_drive_upload_bulk <- function(content, quiet = FALSE, root = NULL) {
  # Yes, S3 function. Google Drive prep function differs from the S3 only by
  # that it additionally checks the id column. When we list local files we don't
  # know GD ids initially.
  cont <- cloud_s3_prep_bulk(content, what = "upload", quiet = quiet)
  cont$local_path <- clean_file_path(cont$path)
  cont <- cloud_drive_content_find_dirs(cont, root = root)

  n <- nrow(cont)
  cli::cli_progress_bar(
    format = "Uploading {cli::pb_bar} [{cli::pb_current}/{cli::pb_total}]",
    total = n
  )
  for (i in seq_along(cont$name)) {
    cli::cli_progress_update()
    cloud_drive_put(media = cont$local_path[[i]], path = cont$dir_id[[i]])
    cli::cli_alert_success(
      "Local file {.path {cont$local_path[[i]]}} uploaded to \\
      {.path {cont$path[[i]]}} on Google Drive."
    )
  }
  cli::cli_alert_success("Done!")
}


#' @title Download Google Drive contents in bulk
#' 
#' @description [cloud_drive_ls] function returns a dataframe of contents of a
#'   Google Drive folder. `cloud_drive_download_bulk` can be applied to such a
#'   dataframe to download all the listed files.
#'   
#'   The workflow in mind is that you would call `cloud_drive_ls()`, then use
#'   dplyr verbs to keep only files that you need and then call
#'   `cloud_drive_download_bulk` on the result.
#' 
#' @inheritParams cloud_drive_download
#' @inheritParams cloud_drive_prep_bulk
#' 
#' @examples 
#' \dontrun{
#' cloud_drive_ls("data") %>% 
#'   filter(type == "csv") %>% 
#'   cloud_drive_download_bulk()
#' }
#'   
#' @export
cloud_drive_download_bulk <- function(content, quiet = FALSE) {
  cont <- cloud_drive_prep_bulk(content, what = "download", quiet = quiet)
  cont$local_path <- clean_file_path(cont$path)
  n <- nrow(cont)
  cli::cli_progress_bar(
    format = "Downloading {cli::pb_bar} [{cli::pb_current}/{cli::pb_total}]",
    total = n
  )
  for (i in seq_along(cont$name)) {
    cli::cli_progress_update()
    current_file_dir <- dirname(cont$local_path[[i]])
    if (!dir.exists(current_file_dir))
      dir.create(current_file_dir, recursive = TRUE)
    
    cloud_drive_download_by_id(
      file = cont$id[[i]],
      path = cont$local_path[[i]],
      overwrite = TRUE
    )
    cli::cli_alert_success(
      "File {.path {cont$path[[i]]}} downloaded from Google Drive to \\
      {.path {cont$local_path[[i]]}}."
    )
  }
  cli::cli_alert_success("Done!")
}

#' @title Write objects to Google Drive in bulk
#'
#' @description Given a named list of objects [cloud_object_ls] function returns
#'   a dataframe similar to the output of [cloud_local_ls] or [cloud_drive_ls].
#'   `cloud_drive_write_bulk` can be applied to such a dataframe to write all
#'   the listed objects to S3. It will be attempted to guess writing function
#'   from file extensions. You can pass writing function manually by setting
#'   `fun` parameter, but it means that all the files will be written using one
#'   function. In fact, you probably shouldn't be writing multiple files of
#'   different types in bulk.
#' 
#' @inheritParams cloud_drive_write  
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
#' cloud_drive_write_bulk()
#' }
#'   
#' @export
cloud_drive_write_bulk <- function(content, fun = NULL, ..., local = FALSE,
                                   quiet = FALSE, root = NULL) {
  cont <- cloud_object_prep_bulk(content, quiet = quiet)
  cont <- cloud_drive_content_find_dirs(cont, root = root)
  
  n <- nrow(cont)
  cli::cli_progress_bar(
    format = "Writing {cli::pb_bar} [{cli::pb_current}/{cli::pb_total}]",
    total = n
  )
  for (i in seq_along(cont$name)) {
    cli::cli_progress_update()
    
    if (!is.null(fun)) {
      current_fun <- fun
    } else {
      current_fun <- cloud_guess_write_fun(cont$name[[i]])
    }
    
    if (local) {
      local_file <- cont$path[[i]]
    } else {
      file_name <- basename(cont$path[[i]])
      local_file <- file.path(tempdir(), file_name)
    }
    
    current_fun(cont$object[[i]], local_file, ...)
    cloud_drive_put(media = local_file, path = cont$dir_id[[i]])
    
    if (!local) {unlink(local_file)}
  }
  cli::cli_alert_success("Done!")
}

#' @title Read Google Drive contents in bulk
#' 
#' @description [cloud_drive_ls] function returns a dataframe of contents of a
#'   Google Drive folder. `cloud_drive_read_bulk` can be applied to such a
#'   dataframe to read all the listed files into a named list. It will be
#'   attempted to guess reading function from file extensions. You can pass
#'   reading function manually by setting `fun` parameter, but it means that all
#'   the files will be read using one function. In fact, you probably shouldn't
#'   be reading multiple files of different types in bulk.
#'   
#'   The workflow in mind is that you would call `cloud_drive_ls()`, then use
#'   dplyr verbs to keep only files that you need and then call
#'   `cloud_drive_read_bulk` on the result. Note that you don't need to filter
#'   out folders -- it is done automatically.
#' 
#' @inheritParams cloud_drive_read  
#' @inheritParams cloud_drive_prep_bulk
#' 
#' @examples 
#' \dontrun{
#' data_lst <- 
#'   cloud_drive_ls("data") %>% 
#'   filter(type == "csv") %>% 
#'   cloud_drive_read_bulk()
#' }
#'   
#' @export
cloud_drive_read_bulk <- function(content, fun = NULL, ..., quiet = FALSE) {
  cont <- cloud_s3_prep_bulk(content, what = "read", quiet = quiet)
  n <- nrow(cont)
  res <- list()
  cli::cli_progress_bar(
    format = "Reading {cli::pb_bar} [{cli::pb_current}/{cli::pb_total}]",
    total = n
  )
  for (i in seq_along(cont$name)) {
    cli::cli_progress_update()
    if (!is.null(fun)) {
      current_fun <- fun
    } else {
      current_fun <- cloud_guess_read_fun(cont$name[[i]])
    }
    local_file <- file.path(tempdir(), basename(cont$name[[i]]))
    
    cloud_drive_download_by_id(
      file = cont$id[[i]],
      path = local_file,
      overwrite = TRUE
    )
    
    res[[cont$name[[i]]]] <- current_fun(local_file, ...)
    
    unlink(local_file)
  }
  cli::cli_alert_success("Done!")
  res
}
