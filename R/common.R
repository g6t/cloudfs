#' @title Get cloud roots of a project
#' @description Returns a list with all `cloudfs.*` roots defined in a project's
#'   DESCRIPTION.
#' 
#' @inheritParams validate_desc
#' @return A named list where each element corresponds to a `cloudfs.*` root
#'   defined in the project's DESCRIPTION file. The names of the list elements
#'   are derived from the `cloudfs.*` fields by removing the `cloudfs.` prefix.
#' 
#' @examples 
#' # create a temp. folder, and put DESCRIPTION file with cloudfs.* fields into it
#' tmp_project <- file.path(tempdir(), "cloudfs")
#' if (!dir.exists(tmp_project)) dir.create(tmp_project)
#' tmp_project_desc <- file.path(tmp_project, "DESCRIPTION")
#' desc_content <- c(
#'   "Package: -",
#'   "cloudfs.s3: my_bucket/my_project",
#'   "cloudfs.drive: aaaaaa"
#' )
#' writeLines(desc_content, tmp_project_desc)
#' 
#' roots <- cloud_get_roots(tmp_project)
#' roots
#' 
#' @export
cloud_get_roots <- function(project = ".") {
  fields <- desc::desc_fields(project)
  cloudfs_fiels <- str_subset_(fields, "^cloudfs\\.")
  res <- as.list(desc::desc_get(cloudfs_fiels, file = project))
  names(res) <- str_remove_(names(res), "^cloudfs\\.")
  res
}


#' @title Extract values from DESCRIPTION file
#' 
#' @inheritParams validate_desc
#' @param key Character. What field to search for in DESCRIPTION file.
#' 
#' @return A string value extracted from the DESCRIPTION field.
#' 
#' @keywords internal
proj_desc_get <- function(key, project = ".") {
  check_string(key)
  check_string(project)
  validate_desc(project)
  desc_file <- file.path(project, "DESCRIPTION")
  value <- desc::desc_get(key, desc_file)
  unname(value)
}

#' @title Validate file path for cloud functions
#' 
#' @description Makes sure that file path passed to a cloud function is in the
#'   right format.
#' 
#' @param file Path to a file relative to project folder root. Can contain only
#'   letters, digits, '-', '_', '.', spaces and '/' symbols.
#' @param error if `TRUE` (default), throws an error if `file` is not a valid 
#'   file path.
#'   
#' @return Either `TRUE` or `FALSE` if `error` is `FALSE`. Either `TRUE` or
#' an error if `error` is `TRUE`.
#'
#' @keywords internal
cloud_validate_file_path <- function(file, error = TRUE) {
  check_string(file)
  res <- grepl("^([A-Za-z]|[0-9]|-|_|\\.| |/)+$", file)
  if (error) {
    if (file == "") cli::cli_abort("A valid file name should not be empty.")
    if (!res) cli_abort(c(
      "File name '{file}' is not valid",
      "A valid file name may consist of:",
      "*" = "uppercase/lowercase letters",
      "*" = "digits",
      "*" = "spaces",
      "*" = "'/' symbols to describe its location inside project's folder",
      "*" = "'_', '-', '.' symbols"
    ))
  }
  res
}

#' @title Validate file names
#' 
#' @description Given a character vector of filenames checks that all names pass
#' for file names. If any one of the names does not pass, throws an error.
#' 
#' @noRd
cloud_validate_file_names <- function(x) {
  check_class(x, arg_class = "character")
  bad_na <- is.na(x)
  bad_symbols <- !grepl("^([A-Za-z]|[0-9]|-|_| |\\.)+$", x)
  x_trimmed <- gsub("^[ ]+", "", gsub("[ ]+$", "", x))
  bad_spaces <- x_trimmed != x
  bad <- bad_na | bad_symbols | bad_spaces
  x_bad <- x[bad]
  if (any(bad)) {
    cli::cli_abort(c(
      "Invalid names: {.val {x_bad}}.",
      "File names can contain only letters, digits, spaces, '.', '_', \\
      '-' symbols and cannot have leading or trailing spaces."
    ))
  }
  return(invisible(TRUE))
}

#' @title Validate project's DESCRIPTION file
#' 
#' @description Checks that DESCRIPTION file exists in a project folder. If it's
#' not the case, proposes to create a DESCRIPTION file from template.
#'   
#' @param project Character. Path to a project. By default it is current working
#'   directory.
#'   
#' @return Either `TRUE` or an error.
#'
#' @keywords internal
validate_desc <- function(project = ".") {
  
  desc_path <- normalizePath(file.path(project, "DESCRIPTION"), mustWork = FALSE)
  
  if (!file.exists(desc_path)) {
    
    yeah <- cli_yeah(glue::glue(
      "Cannot find {{.path DESCRIPTION}} file in {cli_format_path(project)}.
      Do you want to generate it automatically?"
    ), straight = TRUE)
    
    if (yeah) {
      init_desc(project = project)
      return(invisible(TRUE))
    } else {
      cli::cli_abort("Cannot proceed without having {.path DESCRIPTION} file")
    }
  }
  invisible(TRUE)
}

#' @description Inserts the template DESCRIPTION into a project.
#' 
#' @noRd
init_desc <- function(project = ".") {
  desc_path <- normalizePath(file.path(project, "DESCRIPTION"), mustWork = FALSE)
  desc_content <- c(
    "Package: -",
    "Name: [Project Name]",
    "Title: [Project Title]",
    "Description: [Project Description]"
  )
  
  if (interactive()) {
    cli::cli_bullets(c(
      "v" = "A sample DESCRIPTION file has been created at \\
          {.path {desc_path}}.",
      " " = "Feel free to edit the {.field Name}, {.field Title} and \\
          {.field Description} fields as needed to reflect your current project \\
          (optional).",
      " " = "Please don't change the {.field Package} field."
    ))
  }
  
  writeLines(con = desc_path, desc_content)
}

#' @title Prepare ls output
#' 
#' @description Under the hood all ls functions (s3, drive, local) obtain
#'   information about folder content recursively regardless of `recursive`
#'   parameter. This is needed to be able to calculate last modified time and
#'   size for folders in case if `recursive` is set to `FALSE`. The content is
#'   presented in a form of a dataframe similar to what you'd see if you run an
#'   ls function with `recursive = TRUE` and `full_names = FALSE`.
#'   
#'   This function takes such a dataframe from this point and:
#'   1. Summarizes it to non-recursive output if `recursive` is `FALSE`.
#'   2. Appends `path` to names if `full_names` is `TRUE`.
#'   3. Writes full names to names of the `name` column regardless of the
#'   `full_names` parameter.
#'   4. Evaluates the `type` column.
#'
#' @param data ls dataframe assembled internally by a cloud_ls_* function
#' @param path path that was used in a cloud_ls_* function
#' @param recursive (logical) If `TRUE`, lists contents recursively in all
#'   nested subfolders. Default is `FALSE`.
#' @param full_names (logical) If `TRUE`, folder path is appended to object
#'   names to give a relative file path.
#'   
#' @return Transformed `data`.   
#' 
#' @keywords internal
cloud_prep_ls <- function(data, path, recursive, full_names) {
  check_class(data, arg_class = "data.frame")
  required_cols <- c("short_name", "last_modified", "size_b")
  if (!all(required_cols %in% names(data)))
    cli::cli_abort("{.arg data} must contain the following column names: \\
      {.val {required_cols}}")
  
  data <- data[data$short_name != "", ]
  
  if (nrow(data) == 0) {
    return(tibble(
      name = character(),
      type = character(),
      last_modified = as.POSIXct(character()),
      size_b = integer()
    ))
  }
  
  # if not recursive, will replace all names like 'data/mtcars.csv'
  # with 'data/' , so basically removing everything after the first '/'
  # and summarize at the end to take the last modified time
  if (!recursive) {
    data <- 
      data %>% 
      mutate(short_name = gsub("/.+", "/", .data$short_name)) %>% 
      group_by(.data$short_name) %>% 
      summarise(
        across("last_modified", max),
        across("size_b", sum),
        .groups = "drop"
      )
  }
  
  data$type <- if_else(
    grepl("/$", data$short_name),
    "folder",
    tools::file_ext(data$short_name)
  )
  
  data$full_name <- `if`(
    path == "",
    data$short_name,
    clean_file_path(path, data$short_name)
  )
  
  data$name <- `if`(
    full_names,
    data$full_name,
    data$short_name
  )
  
  # always keep full as names of `name` column to be able to find files
  # if bulk read/download is requested
  names(data$name) <- data$full_name
  
  data %>% 
    select(-c("short_name", "full_name")) %>% 
    relocate("name", "type", "last_modified", "size_b")
}
