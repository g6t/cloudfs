#' @title Attach Google Drive folder to project
#'
#' @description The `cloud_drive_attach()` function is designed to add a field
#'   to the DESCRIPTION file of a project, which uniquely identifies the
#'   location of the project's folder in Google Drive. The function prompts the
#'   user to visit the Google Drive website (https://drive.google.com/) where
#'   they can find or create a dedicated folder for the project. Once the user
#'   has located or created the desired folder, they can copy the URL of the
#'   folder from the web browser and paste it into the R console. The function
#'   then parses the URL and populates the corresponding field (cloudfs.drive) in
#'   the DESCRIPTION file with a string that represents the location of the
#'   project in Google Drive.
#'
#' @inheritParams proj_desc_get
#'
#' @examples
#' \dontrun{cloud_drive_attach()}
#'
#' @export
cloud_drive_attach <- function(project = ".") {
  check_string(project)
  
  name <- proj_desc_get("Name", project)
  drive_desc <- proj_desc_get("cloudfs.drive", project)
  
  if (is.na(drive_desc)) {
    cli::cli_alert_info(
      "For {.code cloud_drive_*} functions to work, project's \\
      {.path DESCRIPTION} file needs to contain ID of a dedicated \\
      Google Drive folder."
    )
  } else {
    cli::cli_alert_info(
      "Project's {.path DESCRIPTION} file already contains a link to a \\
      Google Drive folder."
    )
    if (!cli_yeah("Do you want to update it?", straight = TRUE)) {
      return(invisible(TRUE))
    }
  }
  
  yeah <- cli_yeah(
    "Do you wish to visit Google Drive to find/create a folder?",
    straight = TRUE
  )
  
  if (yeah) utils::browseURL("https://drive.google.com/")
  
  repeat {
    ok <- TRUE
    cli::cli_text("Paste folder URL below")
    url <- readline("URL: ")
    id <- googledrive::as_id(url)
    drbl <- tryCatch(
      googledrive::as_dribble(id),
      error = function(e) e
    )
    if (inherits(drbl, "error")) {
      cli::cli_warn(drbl$message)
      ok <- FALSE
    } else if (!googledrive::is_folder(drbl)) {
      cli::cli_warn("Provided URL does not correspond to a folder.")
      ok <- FALSE
    }
    
    if (ok) {
      desc::desc_set(cloudfs.drive = id, file = file.path(project, "DESCRIPTION"))
      folder_name <- drbl$name
      cli::cli_alert_success(
        "Attached Google Drive folder {.val {folder_name}} to \\
        {.field {name}} project. {.field cloudfs.drive} field in \\
        {.path DESCRIPTION} has been updated sucessfully."
      )
      return(invisible(TRUE))
    } else {
      if (!cli_yeah("Try again?", straight = TRUE)) {
        cli::cli_text("Aborting ...")
        break
      }
    }
  }
}

#' @title Get Project's Google Drive Location
#' 
#' @description Tries to read `GoogleDrive` field from project's DESCRIPTION
#'   file. If it's absent, proposes to attach it with [cloud_drive_attach].
#' 
#' @noRd
cloud_drive_get_root <- function(project = ".") {
  loc <- proj_desc_get("cloudfs.drive", project)
  if (is.na(loc)) {
    cloud_drive_attach(project = project)
    loc <- proj_desc_get("cloudfs.drive", project)
  }
  googledrive::as_id(loc)
}

#' @title Find Google Drive folder based on a path
#' 
#' @description Given a Google Drive id pointing to a folder and a relative path
#'   inside this folder, returns id of the object (file or folder) corresponding
#'   to this path.
#'   
#' @param root ID of the folder to start search at
#' @param path Relative location with respect to the root folder
#' @param create Create folders describing path if they do not exist? Default is
#'   `FALSE` so by default the function throws an error if path was not found.
#'   If `TRUE`, the function will create all missing subdirectories. Note that
#'   the object on the deepest level will always be created as a folder. E.g.
#'   if `path = "models/kmeans/model.Rds"` and `"model.Rds"` is missing, this
#'   function will create a folder with such name.
#' 
#' @details Google Drive file structure is different from the usual file
#'   structure like e.g. on Linux or Windows. A folder on Google Drive can have
#'   two or more child folders with the same name. Google Drive marks files and
#'   folders with so-called id values to distinguish between them. These values
#'   are always unique. You can see them in browser URL for example. The concept
#'   of "name" is in the first place for convenience of the end user.
#'
#'   In such a setup a relative file path may correspond to multiple files or
#'   folders. This function however works under assumption that the relative
#'   path you pass to it defines strictly one object. If there's any ambiguity
#'   it throws an error.
#'   
#' @examples 
#' \dontrun{
#' cloud_drive_find_path("1ul0MYeHb0nJtnuaPinKV1WtH0n3igmN2", "models/kmeans")
#' }
#' 
cloud_drive_find_path <- function(root, path = "", create = FALSE) {
  check_string(path)
  check_string(root)
  check_bool(create)
  
  path <- clean_up_file_path(path)
  root <- googledrive::as_id(root)
  
  if (path == "") return(root)
  
  path_seq <- strsplit(path, "/")[[1]]
  current_id <- root
  for (i in seq_along(path_seq)) {
    current_target <- path_seq[[i]]
    current_content <- googledrive::drive_ls(current_id)
    current_where <- paste(path_seq[0:(i - 1)], collapse = "/")
    if (current_where == "") {
      current_where_print <- "the root level"
    } else {
      current_where_print <- glue::glue("{{.val {current_where}}}")
    }
    n_hits <- sum(current_target == current_content$name)
    if (!is.numeric(n_hits) | is.na(n_hits)) {
      cli::cli_abort(
        "Something is wrong. Searching for {.val {current_target}} among \\
        [{.val {current_content$name}}] at {.val {current_id}}"
      )
    }
    if (n_hits == 1) {
      current_id <- current_content[current_target == current_content$name,]$id
    } else if (n_hits > 1) {
      cli::cli_abort(glue::glue(
        "Found more than one {{.val {current_target}}} at {current_where_print}."
      ))
    } else if (n_hits == 0) {
      if (!create) {
        cli::cli_abort(glue::glue(
          "Couldn't find {{.val {current_target}}} at {current_where_print}."
        ))
      }
      repeat {
        current_id <- googledrive::drive_mkdir(current_target, current_id)$id
        i <- i + 1
        if (i > length(path_seq)) return(current_id)
        current_target <- path_seq[[i]]
      }
    }
  }
  current_id
}
