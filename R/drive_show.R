#' @title Browse project's Google Drive folder
#' 
#' @description Opens project's Google Drive folder in browser.
#' 
#' @inheritParams cloud_not_wd_warning
#' @param path (optional) Path inside Google Drive folder to open. By default,
#'   when `path = ""`, navigates to the root level of project's folder.
#'   
#' @inherit cloud_drive_find_path details
#' 
#' @examples 
#' \dontrun{
#' cloud_drive_browse()
#' cloud_drive_browse("models/kmeans")
#' }
#' 
#' @export
cloud_drive_browse <- function(path = "", project = getwd()) {
  root_id <- cloud_drive_get_location(project = project)
  id <- cloud_drive_find_path(root_id, path)
  googledrive::drive_browse(id)
}

#' @title List Contents of Project's Google Drive Folder
#' 
#' @description Prints names, timestamps and sizes of files and folders inside
#'   Google Drive folder.
#'
#' @inheritParams cloud_not_wd_warning
#' @inheritParams cloud_prep_ls
#' 
#' @param path (optional) Path inside Google Drive folder to list contents of
#'   subfolders. By default, when `path = ""`, lists root-level files and
#'   folders.
#'   
#' @inherit cloud_drive_find_path details
#' 
#' @examples 
#' \dontrun{
#' # list only root-level files and folders
#' cloud_drive_ls() 
#' 
#' # list all files in all nested folders
#' cloud_drive_ls(recursive = TRUE)
#' 
#' # list contents of "plots/barplots" subfolder
#' cloud_drive_ls("plots/barplots")
#' }
#' 
#' @export
cloud_drive_ls <- function(path = "", recursive = FALSE, full_names = FALSE,
                        project = getwd()) {
  root_id <- cloud_drive_get_location(project = project)
  stopifnot(is.character(path) & length(path) == 1)
  path <- clean_up_file_path(path)
  cli::cli_text("{.field path}: {.path {path}}")
  head_id <- cloud_drive_find_path(root_id, path)
  stopifnot(isTRUE(recursive) | isFALSE(recursive))
  stopifnot(isTRUE(full_names) | isFALSE(full_names))
  
  # This lists all contents of a folder recursively, but shows only basenames.
  # We need to get relative paths instead. To do this for each object in the
  # response we'll find it's direct parent and append its name to the object
  # name to start forming relative path. Then repeat this until we get to the
  # head - folder located at `path`.
  response <- googledrive::drive_ls(head_id, recursive = TRUE)
  
  if (nrow(response) == 0) {
    return(tibble(
      name = character(),
      type = character(),
      last_modified = as.POSIXct(character()),
      size_b = integer(),
      id = structure(character(), class = c("drive_id", "vctrs_vctr", "character"))
    ))
  }
  # stop if any names contain slash
  which_name_slash <- which(grepl("/", response$name))
  if (length(which_name_slash) != 0) {
    cli::cli_text("Objects with names containing slash (/): ")
    cli::cli_ul()
    for (i in which_name_slash) {
      cli::cli_li("{.field {response[i, 'name']}} (id: {.val {response[i, 'id']}})")
    }
    cli::cli_end()
    cli::cli_abort("Object with names containing slash will cause problems.")
  }

  parents <- lapply(response$drive_resource, function(x) x$parent)
  parents_df <- 
    tibble(
      id = rep(response$id, sapply(parents, length)),
      parent_id = googledrive::as_id(unlist(parents))
    ) %>% 
    left_join(
      select(response, parent_id = "id", parent_name = "name"),
      by = "parent_id"
    ) %>% 
    filter(!is.na(.data$parent_name))

  # construct file paths step-by-step using parent information
  content <- 
    response %>% 
    mutate(prefix_id = .data$id) %>% 
    mutate(name = if_else(
      googledrive::is_folder(response),
      paste0(.data$name, "/"),
      .data$name
    ))
  repeat {
    content <- 
      content %>% 
      left_join(parents_df, by = c(prefix_id = "id")) %>% 
      mutate(name = case_when(
        is.na(.data$parent_name) ~ .data$name,
        TRUE ~ file.path(.data$parent_name, .data$name)
      )) %>% 
      select(-c("prefix_id", "parent_name")) %>% 
      rename(prefix_id = "parent_id")
    if (all(is.na(content$prefix_id))) break
  }
  
  # stop if duplicated names
  duplicated_names <- content$name[duplicated(content$name)]
  if (length(duplicated_names) != 0) {
    cli::cli_abort(
      "Found objects with duplicated names: \\
      {.val {duplicated_names}}."
    )
  }
  
  cont_lm_lst <- lapply(
    content$drive_resource,
    function(x) x$modifiedTime %||% NA_character_
  )
  cont_lm_char <- as.character(cont_lm_lst)
  cont_lm <- as.POSIXct(cont_lm_char, format = "%Y-%m-%dT%H:%M:%OS", tz = "EST")
  
  cont_size_lst <- lapply(
    content$drive_resource,
    function(x) x$size %||% "0"
  )
  cont_size <- as.numeric(cont_size_lst)
  
  cont_df <- tibble(
    short_name = content$name,
    last_modified = cont_lm,
    size_b = cont_size,
    id = content$id
  )
  
  res <- cloud_prep_ls(
    cont_df, 
    path = path,
    recursive = recursive,
    full_names = full_names
  )
  
  # join google drive ids
  left_join(
    res,
    cont_df[, c("short_name", "id")],
    by = c("name" = "short_name")
  )
}
