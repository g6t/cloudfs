#' @description Base version of [stringr::str_subset] for internal use.
#' @noRd
str_subset_ <- function(x, pattern) {
  x[grepl(x = x, pattern = pattern)]
}

#' @description Base version of [stringr::str_subset] for internal use.
#' @noRd
str_remove_ <- function(x, pattern) {
  gsub(x = x, pattern = pattern, replacement = "")
}