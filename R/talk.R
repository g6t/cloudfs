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
