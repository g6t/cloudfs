
#' @title Attach S3 folder to project
#'
#' @description The `cloud_s3_attach()` function is used to add a field to the
#'   DESCRIPTION file of a project, which uniquely identifies the location of
#'   the project's folder in an S3 cloud storage. It prompts the user to visit
#'   the S3 cloud storage website (https://s3.console.aws.amazon.com/) where
#'   they can find or create a dedicated folder for the project. After the user
#'   has selected or created the desired folder, they can copy the URL of the
#'   folder from the web browser and paste it into the R console. The function
#'   then parses the URL and populates the corresponding field (CloudS3) in the
#'   DESCRIPTION file with a string that represents the location of the project
#'   in the S3 cloud storage.
#'
#' @inheritParams proj_desc_get
#'
#' @examples
#' \dontrun{cloud_s3_attach()}
#'
#' @export
cloud_s3_attach <- function(project = getwd()) {
  validate_desc(project)
  
  name <- proj_desc_get("Name", project)
  s3_desc <- proj_desc_get("CloudS3", project)
  
  if (is.na(s3_desc)) {
    cli::cli_alert_info(
      "For {.code cloud_s3_*} functions to work, project's {.path DESCRIPTION} \\
      file needs to contain a path to a dedicated S3 folder."
    )
  } else {
    cli::cli_alert_info(
      "Project's {.path DESCRIPTION} file already contains a path to an S3 folder."
    )
    if (!cli_yeah("Do you want to update it?", straight = TRUE)) {
      return(invisible(TRUE))
    }
  }
  
  yeah <- cli_yeah("Do you wish to visit S3 to find/create a folder?", straight = TRUE)
  
  if (yeah) { utils::browseURL("https://s3.console.aws.amazon.com/") }
  
  repeat {
    ok <- TRUE
    cli::cli_text("Paste folder URL below")
    url <- readline("URL: ")
    info <- tryCatch(
      cloud_s3_get_info_from_url(url),
      error = function(e) e
    )

    if (inherits(info, "error")) {
      cli::cli_warn(info$message)
      ok <- FALSE
    }
    
    if (ok) {
      desc::desc_set(CloudS3 = info, file = file.path(project, "DESCRIPTION"))
      cli::cli_alert_success(
        "Attached S3 folder {.path {info}} to {.field {name}} project.\n
        {.field CloudS3} field in {.path DESCRIPTION} has been updated sucessfully."
      )
      return(invisible(TRUE))
    } else {
      if (cli_yeah("Try again?", straight = TRUE)) {
        cli::cli_text("Aborting ...")
        break
      }
    }
  }
}

#' @title List project folders on S3
#' 
#' @description Lists project folders in a S3 bucket
#' 
#' @param bucket S3 Bucket name
#' @param prefix sub folder in `bucket`
#' 
#' @noRd
cloud_s3_list_projects <- function(bucket, prefix = "") {
  check_scalar(bucket, arg_class = "character")
  check_scalar(prefix, arg_class = "character")
  
  response <- 
    aws.s3::s3HTTP(
      verb = "GET",
      bucket = bucket,
      query = list(
        delimiter = "/",
        prefix = prefix
      )
    )
  
  res1 <- response[names(response) == "CommonPrefixes"]
  res2 <- as.character(res1)
  res <- basename(res2)
  
  res
}

#' @title Check that a project folder exists on S3
#' 
#' @noRd
cloud_s3_project_exists <- function(name, bucket, prefix = "") {
  check_scalar(name, arg_class = "character")
  name %in% cloud_s3_list_projects(bucket = bucket, prefix = prefix)
}


#' @title Create S3 folder for a project
#'
#' @description Creates new folder[s] with a given name inside a bucket.
#'
#' @param bucket_name The name of the bucket in S3.
#' @param folder_name The name of the main folder in the S3 bucket.
#' @param sub_folders A character vector specifying additional subfolder names
#'   to be created inside the main folder.
#'
#' @noRd
s3_create_project_folder <-
  function(bucket_name,
           folder_name,
           sub_folders) {
    
    check_scalar(bucket_name, arg_class = "character")
    check_scalar(folder_name, arg_class = "character")
    check_class(sub_folders, arg_class = "character")
    
    cli::cli_alert_info("Creating needed S3 buckets on AWS.")
    
    for (value in seq_along(sub_folders)) {
      aws.s3::put_folder(bucket = bucket_name,
                         folder = paste(folder_name, sub_folders[value], sep = "/"))
    }
    
    cli::cli_alert_success("Created needed S3 buckets on AWS!")
  }

#' @title Get Project's S3 Location
#' 
#' @description Tries to read `S3` field from project's DESCRIPTION file. If
#'   it's absent, tries to attach it with [cloud_s3_attach].
#' 
#' @noRd
cloud_s3_get_location <- function(project = getwd()) {
  loc <- proj_desc_get("CloudS3", project)
  if (is.na(loc)) {
    cloud_s3_attach(project = project)
    loc <- proj_desc_get("CloudS3", project)
  }
  loc
}

#' @title Extract S3 info from URL
#' @description First makes sure that provided url is an S3 link.
#'  If not, throws an error. Then returns bucket-name|prefix.
#'   
#' @examples 
#' url <- "https://s3.console.aws.amazon.com/s3/buckets/bucket-name?region=us-east-1&prefix=alpha/&showversions=false"
#' cloud_s3_get_info_from_url(url)
#' #> [1] "bucket-name|alpha"
#' 
#' @noRd
cloud_s3_get_info_from_url <- function(url) {
  
  if (!grepl("s3/buckets/[^/?]+", url)) {
    
    url_sample <- "https://s3.console.aws.amazon.com/s3/buckets/bucket-name/..."
    
    cli::cli_abort(c(
      "Project's S3 URL is invalid:",
      "i" = "URL should be of the format {.path {url_sample}}",
      "x" = "URL provided doesn't meet that specification."
    ))
  }
  
  # Extract bucket
  bucket_match <- regmatches(url, regexpr("buckets/([^?]+)", url))
  bucket <- substring(bucket_match[[1]], 9)
  
  # Extract prefix
  prefix_match <- regmatches(url, regexpr("prefix=([^&]+)", url))
  
  if (length(prefix_match) == 0) {
    prefix <- NA
  } else {
    prefix <- substring(prefix_match[[1]], 8)
    prefix <- sub("/+$", "", prefix)
  }
  
  if (is.na(prefix)) {
    cli::cli_alert_info("Project's S3 bucket has no subfolder.")
  }
  
  paste(bucket, prefix, sep = "|")
  
}

cloud_s3_split_info <- function(info) {
  strsplit(info, split = "|", fixed = T)[[1]]
}
