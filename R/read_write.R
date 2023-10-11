#' @title Guess writing function based on file extensions
#' 
#' @description Take a look at the switch call. That's basically it. Returns an
#'   appropriate function or throws an error if wasn't able to find one.
#'   
#' @inheritParams cloud_validate_file_path
#'   
#' @section Default writing functions:
#' 
#' Here's how we identify a writing function based on file extension
#' 
#' - `.csv`: [readr::write_csv]
#' - `.json`: [jsonlite::write_json]
#' - `.rds`: [base::saveRDS]
#' - `.xls`: [writexl::write_xlsx]
#' - `.xlsx`: [writexl::write_xlsx]
#' - `.sav`: [haven::write_sav]
#' - `.xml`: [xml2::write_xml]
#' 
#' @keywords internal
cloud_guess_write_fun <- function(file) {
  cloud_validate_file_path(file)
  ext <- tolower(tools::file_ext(file))
  if (ext == "") stop("Missing file extension, unable to guess writing function.")
  fun <- switch (
    ext,
    "csv" = readr::write_csv,
    "json" = jsonlite::write_json,
    "rds" = saveRDS,
    "sav" = haven::write_sav,
    "xls" = writexl::write_xlsx,
    "xlsx" = writexl::write_xlsx,
    "xml" = xml2::write_xml,
    NULL
  )
  if (is.null(fun)) 
    cli::cli_abort(
      "Unable to guess writing function from file extension: {.val {ext}}."
    )
  fun
}

#' @title Guess reading function based on file extensions
#' 
#' @description Take a look at the switch call. That's basically it. Returns an
#'   appropriate function or throws an error if wasn't able to find one.
#'   
#' @inheritParams cloud_validate_file_path
#' 
#' @section Default reading functions:
#' 
#' Here's how we identify a reading function based on file extension
#' - `.csv`: [readr::read_csv]
#' - `.json`: [jsonlite::read_json]
#' - `.rds`: [base::readRDS]
#' - `.sav`: [haven::read_sav]
#' - `.xls`: [cloud_read_excel]
#' - `.xlsx`: [cloud_read_excel]
#' - `.xml`: [xml2::read_xml]
#' 
#' @keywords internal
cloud_guess_read_fun <- function(file) {
  cloud_validate_file_path(file)
  ext <- tolower(tools::file_ext(file))
  if (ext == "") stop("Missing file extension, unable to guess reading function.")
  fun <- switch (
    ext,
    "csv" = readr::read_csv,
    "json" = jsonlite::read_json,
    "rds" = readRDS,
    "sav" = haven::read_sav,
    "xls" = cloud_read_excel,
    "xlsx" = cloud_read_excel,
    "xml" = xml2::read_xml,
    NULL
  )
  if (is.null(fun))
    cli::cli_abort(
      "Unable to guess reading function from file extension: {.val {ext}}."
    )
  fun
}

#' @title Read excel file as a list of dataframes
#' 
#' @description Uses [readxl::read_excel] under the hood, reads all sheets and
#'   returns them as a named list of dataframes.
#'   
#' @param path Path to the xlsx/xls file.
#' 
#' @return A named list of dataframes, where each dataframe corresponds to a
#'   sheet in the Excel file. The names of the list elements are derived from
#'   the sheet names.
#'
#' @examples 
#' \dontrun{
#' data_lst <- cloud_read_excel("my_project/data.xlsx")
#' }
#' 
#' @export
cloud_read_excel <- function(path) {
  stopifnot(is.character(path) & length(path) == 1)
  stopifnot(file.exists(path))
  sheet_names <- readxl::excel_sheets(path)
  res <- list()
  for (n in sheet_names) {
    res[[n]] <- readxl::read_excel(path, sheet = n)
  }
  res
}
