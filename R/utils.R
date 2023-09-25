#' @title Construct (Clean) Path to File
#' 
#' @description Like [file.path], but also removes all duplicated slashes (/).
#' 
#' @noRd
clean_file_path <- function(...) {
  res <- file.path(...)
  res <- gsub("/+", "/", res)
  res
}

#' @title Clean up a file path
#' @description Given a vector of file paths, removes all leading and trailing
#'   slashes (/) and duplicated slashes inside.
#' @examples
#' clean_up_file_path("///adasd///asdasd//fg/asd///")
#' #> [1] "adasd/asdasd/fg/asd"
#' @noRd
clean_up_file_path <- function(path) {
  stopifnot(is.character(path))
  path <- gsub("^/+", "", path)
  path <- gsub("/+$", "", path)
  path <- gsub("/+", "/", path)
  path
}

#' @description Test if `path` corresponds to the current wd.
#' @noRd
is.wd <- function(path) {
  normalized_path <- normalizePath(path, mustWork = FALSE)
  normalized_wd <- normalizePath(getwd())
  return(normalized_path == normalized_wd)
}

cli_format_path <- function(path) {
  if (is.wd(path)) return("the working directory")
  glue::glue("{{.path {path}}}")
}
