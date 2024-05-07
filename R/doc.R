#' @title Package-wide description of `file` parameter
#' @description A dummy function to be referred by `@inheritParams` for a
#'   parameter documentation.
#' 
#' @param file Path to a file relative to project folder root. Can contain only
#'   letters, digits, '-', '_', '.', spaces and '/' symbols.
#'
#' @keywords internal
doc_file <- function(file) {}



#' @title Package-wide description of `local` parameter
#' @description A dummy function to be referred by `@inheritParams` for a
#'   parameter documentation.
#' 
#' @param local Logical, defaulting to `FALSE`. If `TRUE`, the function will
#'   also create a local copy of the file at the specified path. Note that some
#'   writing functions might not overwrite existing files unless explicitly
#'   allowed. Typically, such functions have a parameter (often named
#'   `overwrite`) to control this behavior. Check the documentation of the
#'   writing function used to determine the exact parameter name and pass it
#'   through the `...` argument if necessary. Alternatively, you can define an
#'   anonymous function for `fun` that calls a writing function with the
#'   overwriting option enabled.
#'   
#' @keywords internal
doc_local <- function(local) {}
