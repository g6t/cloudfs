#' @title List Contents of local project folder
#' 
#' @description Prints names, timestamps and sizes of files and folders inside
#'   local project folder.
#'   
#' @inheritParams validate_desc
#' @inheritParams cloud_prep_ls
#' 
#' @param path (optional) Path inside local project folder to list contents of
#'   a subfolder. By default, when `path = ""`, lists root-level files and
#'   folders.
#' @param root Local path relative to which to consider all paths.
#' @param ignore (logical) Currently just ignores the "renv" folder if `TRUE`.
#'   The main reason for this parameter is that "renv" folder usually contains
#'   thousands of files and it takes a lot of time to calculate its size. But
#'   potentially we may use something like global or project-level cloud ignore
#'   files akin to .gitignore.
#'   
#' @examples 
#' \dontrun{
#' # list only root-level files and folders
#' cloud_local_ls() 
#' 
#' # list all files in all nested folders
#' cloud_local_ls(recursive = TRUE)
#' 
#' # list contents of "plots/barplots" subfolder
#' cloud_local_ls("plots/barplots")
#' }
#' 
#' @export
cloud_local_ls <- function(path = "", root = ".", recursive = FALSE,
                           full_names = FALSE, ignore = TRUE) {
  check_bool(full_names)
  check_bool(recursive)
  check_bool(ignore)
  check_string(root)
  if (!dir.exists(root)) 
    cli::cli_abort("Local root {.path {root}} does not exist.")
  path <- clean_file_path(path)
  local_path <- file.path(root, path)
  if (!file.exists(local_path)) {
    cli::cli_abort(
      "Path {.path {path}} under {.field {root}} project does not exist."
    )
  }
  if (!dir.exists(local_path)) {
    cli::cli_abort(
      "Path {.path {path}} under {.field {project}} project does not \\
      correspond to a directory."
    )
  }
  
  if (ignore & (path == "")) {
    all_dirs <- list.dirs(local_path, full.names = FALSE, recursive = FALSE)
    dirs <- setdiff(all_dirs, "renv")
    root_files <- setdiff(list.files(local_path), all_dirs)
    dir_content_lst <- lapply(
      dirs,
      function(x) file.path(
        x,
        list.files(
          file.path(local_path, x),
          recursive = TRUE
        )
      )
    )
    dir_content <- unlist(dir_content_lst)
    file_names <- c(root_files, dir_content)
  } else {
    file_names <- list.files(local_path, full.names = FALSE, recursive = TRUE)
  }
  
  file_full_names <- file.path(local_path, file_names)
  file_info <- file.info(file_full_names)
  
  cont_df <- 
    tibble(
      short_name = file_names,
      last_modified = file_info$mtime,
      size_b = as.integer(file_info$size)
    )
  
  cloud_prep_ls(
    cont_df, 
    path = path,
    recursive = recursive,
    full_names = full_names
  )
}

#' @title Prepare an ls dataframe for a list of objects
#' 
#' @description `cloud_*_ls` functions for cloud locations (e.g.
#'   [`cloud_s3_ls`]) return content dataframes which can then be passed to
#'   `cloud_*_read_bulk` and `cloud_*_download_bulk` functions to read/download
#'   multiple files at once. In a similar manner, this function takes a list of
#'   objects as an input and produces a dataframe which can then be passed to
#'   `cloud_*_write_bulk` functions to write multiple files at once.
#'   
#' @param x A **named** list. Names may contain letters, digits, spaces, '.', 
#' '-', '_' symbols and cannot contain trailing or leading spaces.
#' @param path A directory to write objects to.
#' @param extension File extension (string).
#' @param prefix,suffix (optional) strings to attach at the beginning or at the
#'   end of file names.
#'   
#' @return A tibble with two columns.
#' 
#' - `object` - objects you've provided
#' 
#' - `name` - contains paths where objects are meant to be written.
#'   
#' @examples
#' cloud_object_ls(
#'   dplyr::lst(mtcars = mtcars, iris = iris),
#'   path = "data",
#'   extension = "csv",
#'   prefix = "df_"
#' )
#' 
#' @export
cloud_object_ls <- function(x, path, extension, prefix = "", suffix = "") {
  check_class(x, "list")
  check_string(path)
  check_string(extension)
  check_string(prefix)
  check_string(suffix)
  
  if (!grepl("^([A-Za-z]|[0-9]|-|_|\\.|/)+$", path)) {
    cli::cli_abort(c(
      "Directory path {.path {path}} is not valid. A valid directory path may \\
      consist of:",
      "*" = "uppercase/lowercase letters",
      "*" = "digits",
      "*" = "'/' symbols to describe its location inside project's folder",
      "*" = "'_', '-', '.' symbols or spaces."
    ))
  }
  
  if (!grepl("^([A-Za-z]|[0-9])+$", extension)) {
    cli::cli_abort(c(
      "{.arg extension} {.val {extension}} is not valid. A valid extension path \\
      may consist of:",
      "*" = "uppercase/lowercase letters",
      "*" = "digits"
    ))
  }
  
  nms <- names(x)
  if (is.null(nms)) {
    cli::cli_abort("Please provide a {.emph named} list.")
  }
  cloud_validate_file_names(nms)
  short_names <- paste0(prefix, nms, suffix, ".", extension)
  short_names <- clean_up_file_path(short_names)
  full_names <- file.path(path, short_names)
  tibble(
    object = x,
    name = stats::setNames(full_names, full_names),
    type = extension
  )
}

#' @title Prepare object content dataframe to be used by bulk download/read
#'   functions
#' 
#' @description `cloud_object_ls` returns an ls-like dataframe for a named list
#'   of objects. This function is used mainly to inform the user about which
#'   files are going to be written and to ask for confirmation
#'   
#' @param content (data.frame) output of `cloud_object_ls()`
#' @param quiet all caution messages may be turned off by setting this parameter
#'   to `TRUE`.
#'   
cloud_object_prep_bulk <- function(content, quiet = FALSE) {
  check_class(content, "data.frame")
  stopifnot(all(c("object", "name", "type") %in% names(content)))
  check_class(content$name, "character")
  check_class(content$type, "character")
  check_bool(quiet)
  cont <- 
    content %>% 
    filter(.data$type != "folder", !is.na(.data$type)) %>% 
    mutate(path = names(.data$name))
  if (nrow(cont) == 0) cli::cli_abort("Nothing to write.")
  cli::cli_text("Attempting to write objects to the following files:")
  cli::cli_text()
  cli::cli_ul()
  for (i in 1:nrow(cont)) {
    cli::cli_li("{.path {cont$path[[i]]}}")
  }
  cli::cli_end()
  cli::cli_text()
  
  if (!quiet) {
    yeah <- cli_yeah("Do you wish to continue?", straight = TRUE)
    if (!yeah) cli::cli_abort("Aborting.")
  }
  cont
}
