#' @title List project folders on S3
#' 
#' @description Lists project folders in "bucket-name" S3 bucket
#' 
#' @param newsletter If `FALSE` (default) lists regular project's S3 folders, 
#'   i.e located in the root of "bucket-name". If `TRUE`, returns all 
#'   subfolders of "newsletter" folder in "bucket-name".
#' 
#' @noRd
cloud_s3_list_projects <- function(newsletter = FALSE) {
  stopifnot(is.logical(newsletter) & !is.na(newsletter))
  prefix <- ifelse(newsletter, "newsletter/", "")
  response <- 
    aws.s3::s3HTTP(
      verb = "GET",
      bucket = "bucket-name",
      query = list(
        delimiter = "/",
        prefix = prefix
      )
    )
  
  res1 <- response[names(response) == "CommonPrefixes"]
  res2 <- as.character(res1)
  res <- basename(res2)
  if (!newsletter) res <- setdiff(res, "newsletter")
  res
}

#' @title Check that a project folder exists on S3
#' 
#' @description If `newsletter` is `FALSE` (default), returns `TRUE` is `name`
#'   is among first-level folders of "bucket-name". If `newsletter` is `TRUE`,
#'   returns `TRUE` if `name` is among subfolders of "newsletter" folder in
#'   "bucket-name".
#' 
#' @noRd
cloud_s3_project_exists <- function(name, newsletter = FALSE) {
  check_scalar(name, arg_class = "character")
  name %in% cloud_s3_list_projects(newsletter = newsletter)
}


#' @title Create S3 folder for a project
#'
#' @description Creates a new folder with a given name inside bucket-name
#'   bucket. Puts empty `data` and `models` subfolders into it.
#'
#' @param name name of the project
#'
#' @noRd
s3_create_project_folder <- function(name, .check = TRUE) {
  cli::cli_alert_info("Creating needed S3 buckets on AWS.")
  bucket_name <- "bucket-name"
  aws.s3::put_folder(bucket = bucket_name, folder = paste0(name, "/data"))
  aws.s3::put_folder(bucket = bucket_name, folder = paste0(name, "/models"))
  cli::cli_alert_success("Created needed S3 buckets on AWS!")
}

#' @title Attach S3 folder to project
#' 
#' @description A regular (client) project with a name `name` is normally
#'   located in monorepo at
#'   "packages/g6tr.projects/vignettes/{iso-date}_{name}". A newsletter project
#'   is located at "packages/g6tr.voice/vignettes/{iso-date}_{name}". The right
#'   S3 folder for a regular project would be "{name}" folder in the
#'   "bucket-name" bucket. The right S3 folder for a newsletter project would
#'   be "newsletter/{year}-{month}". If a project is set up using [g6tr_setup]
#'   or [g6tr::g6tr_newsletter_setup] with `s3_bucket` set to `TRUE`, a
#'   folder will be created following the conventions described above and
#'   project DESCRIPTION will contain the address of the project S3 folder. If
#'   you happen to run an `cloud_s3_*` function in a project where DESCRIPTION
#'   does not contain an address of an S3 folder, you'll be asked to provide a
#'   link to a folder. This link will then be processed and put to DESCRIPTION.
#'   You can trigger this process manually by calling `cloud_s3_attach()`.
#'   
#' @inheritParams cloud_not_wd_warning
#' @param prefix It is possible to suppress the dialogue where you are asked to
#'   provide a folder URL by passing a prefix to this parameter. Although this
#'   is not recommended. This option exists mainly to be used by other
#'   functions.
#' 
#' @examples 
#' \dontrun{cloud_s3_attach()}
#' 
#' @export
cloud_s3_attach <- function(prefix = NULL, project = getwd()) {
  g6tr_validate_desc(project)
  
  if (!is.null(prefix)) {
    if (!is.character(prefix) | length(prefix) > 1) {
      cli::cli_abort(
        "{.arg prefix} should be either {.code NULL} or a character \\
        vector of length 1."
      )
    }
    res <- grepl("^([A-Za-z]|[0-9]|-|_|\\.| |/)+$", prefix)
    if (prefix == "") stop("A valid folder prefix should not be empty.")
    if (!res) cli::cli_abort(c(
      "Prefix {.path {prefix}} is not valid. A prefix may consist of:",
      "*" = "uppercase/lowercase letters",
      "*" = "digits",
      "*" = "spaces",
      "*" = "'/' symbols to describe its location inside project's folder",
      "*" = "'_', '-', '.' symbols"
    ))
    s3_create_project_folder(prefix)
    desc::desc_set(S3 = prefix, file = project)
    cli::cli_alert_success("Added S3 folder {.path {prefix}} to {.path DESCRIPTION}.")
    return(invisible(TRUE))
  }
  
  name <- proj_desc_get("Name", project)
  base_pkg <- proj_desc_get("BasePkg", project)
  s3_desc <- proj_desc_get("S3", project)
  
  if (is.na(s3_desc)) {
    cli::cli_alert_info(
      "For {.code cloud_s3_*} functions to work, project's {.path DESCRIPTION} \\
      file needs to contain a path to a dedicated S3 folder."
    )
  } else {
    cli::cli_alert_info(
      "Project's {.path DESCRIPTION} file alread contains a path to an S3 folder."
    )
    if (!g6tr.ui::cli_yeah("Update it?", straight = TRUE)) {
      return(invisible(TRUE))
    }
  }
  
  yeah <- g6tr.ui::cli_yeah("Do you wish to visit S3 to find/create a folder?", straight = TRUE)
  if (yeah) {
    if (!is.na(base_pkg) & base_pkg == "g6tr.voice") {
      cloud_s3_browse_prefix("newsletter/")
    } else {
      cloud_s3_browse_prefix()
    }
  }
  
  repeat {
    ok <- TRUE
    cli::cli_text("Paste folder URL below")
    url <- readline("URL: ")
    prefix <- tryCatch(
      cloud_s3_get_prefix_from_url(url),
      error = function(e) e
    )

    if (inherits(prefix, "error")) {
      cli::cli_warn(prefix$message)
      ok <- FALSE
    }
    
    if (ok) {
      desc::desc_set(S3 = prefix, file = project)
      cli::cli_alert_success(
        "Attached S3 folder {.path {prefix}} to {.field {name}} project. \\
        {.field S3} field in {.path DESCRIPTION} updated."
      )
      return(invisible(TRUE))
    } else {
      if (!g6tr.ui::cli_yeah("Try again?", straight = TRUE)) {
        cli::cli_text("Aborting ...")
        break
      }
    }
  }
}

#' @title Get Project's S3 Location
#' 
#' @description Tries to read `S3` field from project's DESCRIPTION file. If
#'   it's absent, tries to attach it with [cloud_s3_attach].
#' 
#' @noRd
cloud_s3_get_location <- function(project = getwd()) {
  loc <- proj_desc_get("S3", project)
  if (is.na(loc)) {
    cloud_s3_attach(project = project)
    loc <- proj_desc_get("S3", project)
  }
  loc
}

#' @title Extract S3 prefix from URL
#' @description First makes sure that provided url points to a folder inside the
#'   `bucket-name` bucket. If not, throws an error. Then returns the folder
#'   prefix
#'   
#' @examples 
#' url <- "https://s3.console.aws.amazon.com/s3/buckets/bucket-name?region=us-east-1&prefix=alpha/&showversions=false"
#' cloud_s3_get_prefix_from_url(url)
#' #> [1] "alpha"
#' 
#' @noRd
cloud_s3_get_prefix_from_url <- function(url) {
  # if (gre)
  if (!grepl("s3/buckets/bucket-name", url)) {
    cli::cli_abort(
      "Project's S3 folder must be a subfolder of the {.path bucket-name} bucket."
    )
  }
  prefix <- stringr::str_extract(url, "(?<=prefix=).*(?=&)")
  prefix <- stringr::str_remove(prefix, "/+$")
  prefix
}
