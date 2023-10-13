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
#' @return Transformed `content` dataframe.
#'   
#' @keywords internal
cloud_drive_prep_bulk <- function(content, what = c("read", "download"),
                               safe_size = 5e7, quiet = FALSE) {
  check_class(content, "data.frame")
  stopifnot(all(c("name", "type", "size_b", "id") %in% names(content)))
  check_class(content$name, "character")
  check_class(content$type, "character")
  check_numeric(content$size_b)
  check_class(content$id, "character")
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


#' @title Bulk Upload Files to Google Drive
#' 
#' @description This function streamlines the process of uploading multiple
#'   files from the local project folder to the project's designated Google
#'   Drive folder. By using [cloud_local_ls], you can obtain a dataframe
#'   detailing the contents of the local folder. Applying
#'   `cloud_drive_upload_bulk` to this dataframe allows you to upload all listed
#'   files to Google Drive.
#' 
#' @inheritParams cloud_drive_upload
#' @inheritParams cloud_s3_prep_bulk
#' 
#' @return Invisibly returns the input `content` dataframe.
#' 
#' @examplesIf interactive() 
#' # create toy plots: 2 png's and 1 jpeg
#' dir.create("toy_plots")
#' png("toy_plots/plot1.png"); plot(rnorm(100)); dev.off()
#' png("toy_plots/plot2.png"); plot(hist(rnorm(100))); dev.off()
#' png("toy_plots/plot3.jpeg"); plot(hclust(dist(USArrests), "ave")); dev.off()
#' 
#' # upload only the two png's
#' cloud_local_ls("toy_plots")  |> 
#'   dplyr::filter(type == "png")  |> 
#'   cloud_drive_upload_bulk()
#' 
#' # clean up
#' unlink("toy_plots", recursive = TRUE)
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
  invisible(content)
}

#' @title Bulk download contents from Google Drive
#' 
#' @description Downloads multiple files from a Google Drive folder based on 
#'   the output dataframe from [cloud_drive_ls]. This function streamlines 
#'   the process of downloading multiple files by allowing you to filter and 
#'   select specific files from the Google Drive listing and then download 
#'   them in bulk.
#' 
#' @inheritParams cloud_drive_download
#' @inheritParams cloud_drive_prep_bulk
#' 
#' @return Invisibly returns the input `content` dataframe.
#' 
#' @examplesIf interactive() 
#' # provided there's a folder called "data" in the root of your project's
#' # Google Drive folder, and this folder contains "csv" files
#' cloud_drive_ls("data") |> 
#'   filter(type == "csv") |> 
#'   cloud_drive_download_bulk()
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
  invisible(content)
}

#' @title Write multiple objects to Google Drive in bulk
#'
#' @description This function allows for the bulk writing of multiple R objects
#'   to the project's designated Google Drive folder. To prepare a list of
#'   objects for writing, use [cloud_object_ls], which generates a dataframe
#'   listing the objects and their intended destinations in a format akin to the
#'   output of [cloud_drive_ls]. By default, the function determines the
#'   appropriate writing method based on each file's extension. However, if a
#'   specific writing function is provided via the `fun` parameter, it will be
#'   applied to all files, which may not be ideal if dealing with a variety of
#'   file types.
#' 
#' @inheritParams cloud_drive_write  
#' @inheritParams cloud_object_prep_bulk
#' 
#' @return Invisibly returns the input `content` dataframe.
#' 
#' @examplesIf interactive() 
#' # write two csv files: data/df_mtcars.csv and data/df_iris.csv
#' cloud_object_ls(
#'   dplyr::lst(mtcars = mtcars, iris = iris),
#'   path = "data",
#'   extension = "csv",
#'   prefix = "df_"
#' ) |> 
#' cloud_drive_write_bulk()
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
  invisible(content)
}

#' @title Bulk Read Contents from Google Drive
#' 
#' @description This function facilitates the bulk reading of multiple files
#'   from the project's designated Google Drive folder. By using
#'   [cloud_drive_ls], you can obtain a dataframe detailing the contents of the
#'   Google Drive folder. Applying `cloud_drive_read_bulk` to this dataframe
#'   allows you to read all listed files into a named list. The function will,
#'   by default, infer the appropriate reading method based on each file's
#'   extension. However, if a specific reading function is provided via the
#'   `fun` parameter, it will be applied uniformly to all files, which may not
#'   be suitable for diverse file types.
#' 
#' @inheritParams cloud_drive_read  
#' @inheritParams cloud_drive_prep_bulk
#' 
#' @return A named list where each element corresponds to the content of a file
#'   from Google Drive. The names of the list elements are derived from the file
#'   names.
#' 
#' @examplesIf interactive()
#' # provided there's a folder called "data" in the root of the project's main
#' # Google Drive folder, and it contains csv files
#' data_lst <- 
#'   cloud_drive_ls("data") |> 
#'   filter(type == "csv") |> 
#'   cloud_drive_read_bulk()
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
