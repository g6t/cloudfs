# --- copied from googledrive package sources ---
.drive <- new.env(parent = emptyenv())

.drive$translate_mime_types <-
  system.file(
    "extdata", "data", "translate_mime_types.csv",
    package = "googledrive",
    mustWork = TRUE
  ) %>%
  utils::read.csv(stringsAsFactors = FALSE) %>%
  dplyr::as_tibble()


## get the default export MIME type for a native Google MIME type
## examples:
##    Google Doc --> MS Word
##  Google Sheet --> MS Excel
## Google Slides --> MS PowerPoint
get_export_mime_type <- function(mime_type) {
  m <- .drive$translate_mime_types$mime_type_google == mime_type &
    sapply(.drive$translate_mime_types$default, isTRUE)
  if (!any(m)) {
    cli::cli_abort(c(
      "Not a recognized Google MIME type:",
      "x" = mime_type
    ))
  }
  .drive$translate_mime_types$mime_type_local[m]
}
# ------

#' @description Like `googledrive::drive_download()` but does not change `path`.
#'   When you download e.g. a googlesheet using `googledrive::drive_download()`
#'   with `path = "summary"` (without extension), it will save the result into
#'   "summary.xlsx", so in other words, it'll attach an appropriate extension.
#'   In this particular case not changing file name does not sound like an
#'   improvement, but this is needed to ensure consistency in naming between
#'   local files, google drive and S3 and to avoid having more complex logic
#'   in `cloud_drive_read()` and `cloud_drive_download_bulk()`.
#'   
#' @noRd
cloud_drive_download_by_id <- function(file, path, overwrite = FALSE) {
  file <- googledrive::as_dribble(file)
  file <- googledrive::confirm_single_file(file)
  mime_type <- file$drive_resource[[1]]$mimeType
  
  if (grepl("google", mime_type)) {
    export_type <- get_export_mime_type(mime_type)
    request <- googledrive::request_generate(
      endpoint = "drive.files.export", 
      params = list(fileId = file$id, mimeType = export_type)
    )
  }
  else {
    request <- googledrive::request_generate(
      endpoint = "drive.files.get", 
      params = list(fileId = file$id, alt = "media")
    )
  }
  
  response <- googledrive::request_make(
    request,
    httr::write_disk(path, overwrite = overwrite)
  )
  success <- httr::status_code(response) == 200 && file.exists(path)
  if (success) {
    cli::cli_alert_success("File downloaded: {.path {path}}")
  }
  else {
    cli::cli_abort("Download failed.")
  }
  invisible(path)
}


#' @description Guess Google Drive type by file name. Currently is used
#'   only to detect excel files to know to convert them to Google Sheets.
#' 
#' @noRd
cloud_drive_guess_type <- function(file) {
  cloud_validate_file_path(file)
  ext <- tolower(tools::file_ext(file))
  if (ext == "") cli::cli_abort("Missing file extension, unable to guess reading function.")
  switch (
    ext,
    "xls" = "spreadsheet",
    "xlsx" = "spreadsheet",
    NULL
  )
}

#' @description Like `googledrive::drive_put` but:
#'   - uses appropriate `type` where possible (via `cloud_drive_guess_type()`)
#'   - `name` is automatically derived from local file path (`media`)
#'   - makes sure that original file extension is kept in place.
#' 
#' @noRd
cloud_drive_put <- function(media, path) {
  name <- basename(media)
  type <- cloud_drive_guess_type(name)
  id <- googledrive::drive_put(
    media = media,
    path = path,
    name = name,
    type = type
  )
  googledrive::local_drive_quiet()
  googledrive::drive_rename(id, name)
}

#' @title Automatically resize all columns in a google spreadsheet
#' @description Finds the spreadsheet by path relative to a project root.
#'   Applies [googlesheets4::range_autofit()] to each sheet.
#' 
#' @inheritParams cloud_validate_file_path
#' @inheritParams cloud_drive_ls
#' 
#' @examples 
#' \dontrun{
#' cloud_drive_write(mtcars, "results/mtcars.xlsx")
#' cloud_drive_spreadsheet_autofit("results/mtcars.xlsx")
#' }
#' 
#' @export 
cloud_drive_spreadsheet_autofit <- function(file, root = NULL) {
  cloud_validate_file_path(file)
  check_string(root, alt_null = TRUE)
  
  if (is.null(root)) root <- cloud_drive_get_root()
  file <- clean_up_file_path(file)
  file_id <- cloud_drive_find_path(root, file)
  
  file_dribble <- googledrive::as_dribble(file_id)
  file_mime_type <- file_dribble$drive_resource[[1]]$mimeType
  if (file_mime_type != googledrive::drive_mime_type("spreadsheet")) {
    cli::cli_abort("{.path {file}} is not a spreadsheet.")
  }
  
  sheet_names <- googlesheets4::sheet_names(file_id)
  for (sheet in sheet_names) {
    googlesheets4::range_autofit(file_id, sheet = sheet)
  }
}