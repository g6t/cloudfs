#' @title Turn cloud functions dialogues on/off
#'
#' @description [cloud_talk_on], [cloud_talk_off] set dialogue mode on/off,
#'   [cloud_talk] gets the current state.
#' 
#' @noRd 
cloud_talk_on <- function() {
  options(cloud.talk = TRUE)
}
cloud_talk_off <- function() {
  options(cloud.talk = FALSE)
}
cloud_talk <- function() {
  opt <- getOption("cloud.talk")
  if (is.null(opt)) return(FALSE)
  stopifnot(is.logical(opt))
  stopifnot(!is.na(opt))
  opt
}

#' @title Extract values from DESCRUPTION file
#' 
#' @param key Character. What field to search for in DESCRIPTION file.
#' @param project Character. Path to a project. By default it is current working
#'   directory.
#' 
proj_desc_get <- function(key, project = getwd()) {
  stopifnot(is.character(key) & length(key) == 1)
  stopifnot(is.character(project) & length(project) == 1)
  validate_desc(project)
  desc_file <- file.path(project, "DESCRIPTION")
  value <- desc::desc_get(key, desc_file)
  unname(value)
}

#' @title Warn if cloud function is used not for current working directory
#' 
#' @description Functions for uploading/downloading files from project cloud
#'   locations are designed to synchronize local and cloud folder structures.
#'   That is e.g. when you call `cloud_s3_upload` with `file` parameter set to
#'   "data/demo.csv" and `project` parameter set to something different from the
#'   current working directory it is always assumed that "data/demo.csv" from
#'   **the project's folder** and not from the current working directory needs
#'   to be uploaded to S3. But for development purposes it is handy to be able
#'   to call the functions not only for the current wd. This function checks
#'   that project is set to the current wd. If not, it throws a warning and asks
#'   if user wants to continue.
#'   
#' @param project Path to a project. By default it is current working directory.
#'
cloud_not_wd_warning <- function(project) {
  stopifnot(is.character(project) & length(project) == 1)
  if (cloud_talk()) {
    wd <- normalizePath(getwd())
    project <- normalizePath(project)
    if (wd != project) {
      cli::cli_warn(
        "This function is meant to be used without changing the {.arg project} parameter."
      )
      yeah <- cli_yeah("Do you want to continue?")
      if (!yeah) {
        cli::cli_abort("Aborting")
      }
    }
  }
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
cloud_validate_file_path <- function(file, error = TRUE) {
  stopifnot(is.character(file))
  res <- grepl("^([A-Za-z]|[0-9]|-|_|\\.| |/)+$", file)
  if (error) {
    if (file == "") stop("A valid file name should not be empty.")
    if (!res) stop(
      "File name '", file, "' is not valid\n\n",
      "A valid file name may consist of\n",
      "  * uppercase/lowercase letters\n",
      "  * digits\n",
      "  * spaces",
      "  * '/' symbols to describe its location inside project's folder\n",
      "  * '_', '-', '.' symbols"
    )
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
  stopifnot(is.character(x))
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

#' @title Extract the project name and base package from a path
#' 
#' @description given a path to a directory first checks that it's a subdir
#' of vignettes dir of either packages/g6tr.projects or packages/g6tr.voice.
#' If this condition is satisfied, returns a list with two fields:
#' - `name` - project's name derived as basename(path)
#' - `base_pkg` - project's base package. So either "g6tr.projects" or 
#' "g6tr.voice"
#' If the condition is not satisfied, returns a list with same names, filled
#' with NA values.
#' 
#' @inheritParams cloud_not_wd_warning
#' 
#' @examples
#' \dontrun{
#' g6tr_get_project_meta_from_path(g6tr_here(
#'   "packages/g6tr.projects/vignettes/2021-11-04_acai/"
#' ))
#' #> $name
#' #> [1] "acai"
#' #>
#' #> $base_pkg
#' #> [1] "g6tr.projects" 
#' }
#' 
#' @noRd
g6tr_get_project_meta_from_path <- function(project = getwd()) {
  stopifnot(is.character(project))
  stopifnot(dir.exists(project))
  project <- normalizePath(project)
  name_with_date <- basename(project)
  name <- gsub("^[0-9]{4}-[0-9]{2}-[0-9]{2}_", "", name_with_date)
  # will be returned like this if project is not in a standard location
  res <- list(name = name, base_pkg = NA_character_)
  # if 3 levels up is not "packages"
  if (dirname(dirname(dirname(project))) != g6tr_here("packages")) return(res)
  # if 1 level up is not vignettes
  if (basename(dirname(project)) != "vignettes") base_pkg <- return(res)
  # remove ISO date at the beginning
  base_pkg <- basename(dirname(dirname(project)))
  if (!(base_pkg %in% c("g6tr.projects", "g6tr.voice"))) return(res)
  list(
    name = name,
    base_pkg = base_pkg
  )
}

#' @title Assert that a key in project's DESCRIPTION file has a certain value
#' 
#' @description Given a path do DESCRIPTION file or to a project containing such
#' file makes sure that field `key` in it has value `value`. 
#' - If this field is absent, proposes to populate it with `value`.
#' - If this field exists, but is populated with a different value, throws an
#'   error.
#' - If this field exists and is populated with the right value, silently 
#'   returns TRUE.
#'   
#' @param key field name, character
#' @param value required field value, character
#' @param file path to DESCRIPTION file
#'  
#' @noRd
assert_desc_field <- function(key, value, file) {
  stopifnot(is.character(key) & length(key) == 1)
  stopifnot(is.character(value) & length(value) == 1)
  desc_value <- desc::desc_get(keys = key, file = file)
  if (is.na(desc_value)) {
    cli::cli_warn("Field {.field key} does not exist in {.path DESCRIPTION}.")
    yeah <- cli_yeah("Fill it with {.val value}?", straight = TRUE)
    if (yeah) {
      desc::desc_set_list(key, value, file = file)
      return(invisible(TRUE))
    } else {
      cli::cli_abort("Stopping")
    }
  }
  if (desc_value != value) 
    cli::cli_abort(
      "Value found in {.path DESCRIPTION}, {.val {desc_value}}, is different \\
      from what should be there - {.val {value}}."
    )
  return(invisible(TRUE))
}

#' @title Validate project's DESCRIPTION file
#' 
#' @description Given a path to a project, figures out project name and base 
#' package. Checks that `Name` and `BasePkg` fields in project's 
#' DESCRIPTION file have corresponding values.
#' - If DESCRIPTION file is not found, proposes to create one and populate all
#'   the main fields (including `Name` and `BasePkg`) automatically.
#' - If DESCRIPTION exists but `Name` and/or `BasePkg` are not populated,
#'   proposes to populate these fields.
#' - If applied to a package folder, throws a warning.
#'   
#' @inheritParams cloud_not_wd_warning
#'
#' @noRd   
validate_desc <- function(project = getwd()) {
  
  desc_path <- file.path(project, "DESCRIPTION")
  
  if (!file.exists(desc_path)) {
    
    yeah <- cli_yeah("Cannot find {.path DESCRIPTION} file in {.field {project}}.\n
                     Do you want to generate it automatically?", straight = TRUE)
    if (yeah) {
      desc_content <- c("Name: [Project Name]",
                        "Title: [Description about the project]")
      
      writeLines(con = desc_path, desc_content)
      desc::desc_print(desc_path)
      
      cli::cli_alert_success("A sample DESCRIPTION file has been created in {.field {project}}.\n
                             Kindly edit the {.field Name} and {.field Title} fields to reflect your current project.\n")
      return(invisible(TRUE))
    } else {
      cli::cli_abort("Cannot proceed without having DESCRIPTION file")
    }
  }
  invisible(TRUE)
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
cloud_prep_ls <- function(data, path, recursive, full_names) {
  stopifnot(is.data.frame(data))
  stopifnot(all(c("short_name", "last_modified", "size_b") %in% names(data)))
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
