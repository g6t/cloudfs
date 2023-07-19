path <- "~/monorepos/g6tr/packages/g6tr.projects/vignettes/2023-01-17_lodge/"

Cloud_Project <- R6::R6Class("Cloud_Project",
  public = list(
    Name = NULL,
    Drive = NULL,
    S3 = NULL,
    # print = function(...) {
    #   cat(
    #     "Project:", self$Name, "\n",
    #     "S3:", self$S3, "\n",
    #     "Drive:", self$Drive
    #   )
    # },
    initialize = function(Name, Drive = NULL, S3 = NULL) {
      self$Name <- Name
      self$Drive <- Drive
      self$S3 <- S3
    },
    get_location = function(what) {
      # either S3 or Drive
      # this should throw error when location is empty
      # to be called by all the transfer/show functions
    }
  )
)


cloud_project_from_desc <- function(path) {
  Cloud_Project$new(
    Name = desc::desc_get("Name", file = path),
    S3 = desc::desc_get("S3", file = path),
    Drive = desc::desc_get("Drive", file = path)
  )
}

cloud_project_from_opts <- function() {
  Cloud_Project$new(
    Name = "dunno",
    S3 = getOption("cloud.s3"),
    Drive = getOption("cloud.drive")
  )
}

cloud_project_from_desc(path)
cloud_project_from_opts()


# what should cloud_project() do ?

cloud_project <- function(path = NULL, s3 = NULL, drive = NULL) {
  
}

