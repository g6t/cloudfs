#' @import rlang
#' @importFrom cli cli_abort
#' 
#' @title User Interface: Ask a Yes/No question
#' 
#' @description This function is inspired by (if not mostly copied from) 
#' [usethis::ui_yeah] function. It's purpose is to ask user a yes/no question.
#' The differences are:
#' 1. It is more limited in answer options customization. This is done on
#' purpose to standardize command line dialogues in our code.
#' 2. It uses `cli` package under the hood, so `cli` rich text formatting is
#' possible.
#' 
#' @inheritParams cli::cli_text
#' @param x Question to display.
#' @param straight (logical) Ask a straight Yes/No question? By default (when
#'   `FALSE`), two different "no" options and one "yes" option are sampled from
#'   a pool of variants. In other words it behaves just like [usethis::ui_yeah]
#'   with default parameter setup. When `straight = TRUE`, it only shows "Yes"
#'   and "No", literally.
#' 
#' @return (logical) Returns `TRUE` when the user selects a "yes" option and
#'   `FALSE` otherwise, i.e. when user selects a "no" option or refuses to make
#'   a selection (cancels).
#'
#' @examples
#' \dontrun{
#' cli_yeah("Is this {.strong true}?: {.code 2+2 == 4}")
#' cli_yeah("{.field Yes} or {.field No}?", straight = TRUE)
#' }
#' 
#' @noRd 
cli_yeah <- function(x, straight = FALSE, .envir = parent.frame()) {
  check_scalar(x, arg_class = "character")
  check_scalar(straight, arg_class = "logical")
  if (!interactive()) {
    cli::cli_abort(c(
      "User input required, but session is not interactive. Query:",
      x
    ), .envir = .envir)
  }
  
  yes <- c("Yes", "Definitely", "For sure", "Yup", "Yeah", "I agree", "Absolutely")
  no <- c("No", "No way", "Not now", "Negative", "Nope", "Absolutely not")
  
  # answer options to be shown
  if (straight) {
    qs <- c("Yes", "No")
  } else {
    qs <- c(sample(yes, 1), sample(no, 2))
    qs <- sample(qs)
  }
  
  cli::cli_text(x, .envir = .envir)
  out <- utils::menu(qs)
  out != 0L && qs[[out]] %in% yes
}

#' @title Check if Function Argument is Scalar
#' 
#' @description A function to check that argument is of proper class and of
#'   length 1.
#'
#' @param ... Function argument that is being asserted.
#' @param arg_class Class name. Usually "character", "numeric", 
#'   "data.frame", etc.
#' @param alt_null Logical. Should argument accept NULL value.
#'
#' @return Invisible `NULL` if assertion is `TRUE`, otherwise an error message.
#'
#' @examples
#' # Variables values to test
#' char_s <- "test"
#' char_v <- c("test", "variable")
#' num_s <- 1.5
#' num_v <- c(2, 1.5, 3.33)
#' logical_s <- TRUE
#' logical_v <- c(FALSE, FALSE)
#' int_s <- 1L
#' int_v <- c(3L, 33L)
#' \dontrun{
#' # Assert Scalar Character
#' check_scalar(char_s, arg_class = "character")
#' check_scalar(char_v, arg_class = "character")
#' check_scalar(num_s, arg_class = "character")
#' check_scalar(logical_s, arg_class = "character")
#' 
#' # Assert Scalar Numeric
#' check_scalar(num_s, arg_class = "numeric")
#' check_scalar(num_v, arg_class = "numeric")
#' check_scalar(int_s, arg_class = "numeric", alt_null = TRUE)
#' check_scalar(logical_v, arg_class = "numeric")
#' 
#' # Assert Scalar Logical
#' check_scalar(logical_s, arg_class = "logical")
#' check_scalar(logical_v, arg_class = "logical")
#' check_scalar(char_s, arg_class = "logical", alt_null = TRUE)
#' check_scalar(int_v, arg_class = "logical")
#' 
#' #' # Assert Scalar Integer
#' check_scalar(int_s, arg_class = "integer")
#' check_scalar(int_v,  arg_class = "integer", alt_null = TRUE)
#' check_scalar(num_s, arg_class = "integer")
#' check_scalar(logical_v, arg_class = "integer", alt_null = TRUE)
#' }
#' @noRd 
check_scalar <- function(..., arg_class, alt_null = FALSE) {
  
  .args <- check_args(...)
  
  if (length(.args) != 1) {
    cli::cli_abort("Cannot pass more than one variable name to {.var ...}")
  }
  
  scalar_msg <- paste0(
    "{.arg {x_name}} should be ", 
    "{.cls ", arg_class, "} ",
    "of length {.val {1}}"
  )
  
  if(isTRUE(alt_null)) {
    scalar_msg <- paste0(scalar_msg, " or {.cls NULL}")
  }
  
  for (q in .args) {
    # check class
    cl <- rlang::call2(
      check_class, rlang::quo_squash(q), arg_class, alt_null, add_msg = scalar_msg
    )
    rlang::eval_tidy(cl, env = rlang::quo_get_env(q))
    # check length
    cl <- rlang::call2(
      check_length, rlang::quo_squash(q), arg_length = 1L, alt_null, 
      add_msg = scalar_msg
    )
    rlang::eval_tidy(cl, env = rlang::quo_get_env(q))
  }
}

#' @description [check_scalar] for `arg_class` equal to "character".
#' @noRd
check_string <- function(..., alt_null = FALSE) {
  check_scalar(..., arg_class = "character", alt_null = alt_null)
}


check_numeric <- function(x) {
  if (!is.numeric(x)) {
    x_name <- deparse(substitute(x))
    cli::cli_abort("{.arg {x_name}} must be numeric.")
  }
}

#' @title Check Argument's Class
#' 
#' @description Check if argument is of proper class.
#'
#' @param x Function argument that is being asserted.
#' @param arg_class Class name. Usually "character", "numeric", "data.frame", 
#'   etc.
#' @param alt_null Logical. Should argument accept NULL value.
#' @param add_msg Is an additional message that can be printed over the standard
#'   function error message. You can:
#'   * pass the names of the arguments that failed the test by using
#'   `{x_names}` in the message body (e.g. "What are the \{x_names\}");
#'   * pass the tested class by using `{arg_class}` in the message body (e.g.
#'   "I want them to be \{arg_class\})"
#'   * pass the classes of the arguments that failed the test by using 
#'   `{wrong_class}` in the message body (e.g. "\{wrong_class\} is wrong")
#'
#' @return If argument `class` is same as `arg_class` it returns invisible
#'   `NULL`. Otherwise the function throws an error.
#'
#' @examples
#' c1 <- c("x", "y")
#' n1 <- c(1,3,4)
#' n2 <- c(1.5, 2.5)
#' i1 <- 1L
#' df1 <- data.frame(x = 1:5, y = 6:10)
#' new_class <- structure("new class", class= c("character", "new class"))
#' nl1 <- NULL
#' \dontrun{
#' check_class(c1, arg_class = "character")
#' check_class(c1, arg_class = "numeric")
#' check_class(df1, arg_class = "data.frame")
#' check_class(
#'   new_class, arg_class = "tbl_df",
#'   add_msg = "{.arg {x_name}} with {.cls {wrong_class}} not {.cls {arg_class}}"
#' )
#' check_class(nl1, arg_class = "character")
#' check_class(nl1, arg_class = "character", alt_null = TRUE)
#' check_class(n2, arg_class = "character", alt_null = TRUE)
#' }
#' @noRd 
check_class <- function(x, arg_class, alt_null = FALSE, add_msg = NULL) {
  if(!(inherits(arg_class, "character") & length(arg_class) == 1)) {
    cli::cli_abort(
      "{.arg arg_class} needs to be {.cls character} of length {.val 1}"
    )
  }
  
  if(!(inherits(add_msg, "character") | is.null(add_msg))){
    cli::cli_abort(
      "!" = "{.arg add_msg} needs to be {.cls character} or {.cls NULL}",
      "x" = "{.arg add_msg} is {.cls class(add_msg)}"
    )
  }
  
  check_null <- check_null_cond(x = x, alt_null = alt_null)
  
  if (!(inherits(x, arg_class) | check_null)) {
    x_name <- deparse(substitute(x))
    wrong_class <- class(x)
    
    warn_msg <- ifelse(
      alt_null,
      "{.arg {x_name}} should be of class {.cls {arg_class}} or {.cls NULL}",
      "{.arg {x_name}} should be of class {.cls {arg_class}}"
    )
    
    cli::cli_abort(c(
      "i" = add_msg,
      "!" = warn_msg, 
      "x" = "{.arg {x_name}} is {.cls {wrong_class}}"
    ))
  }
}

#' @title Check if Argument is of Proper Length
#' 
#' @description TODO.
#'
#' @param x Function arguments that are being asserted.
#' @param arg_length Integer. Length of argument, for scalars it should take 
#'   value `1` (default).
#' @param alt_null Logical. Should argument accept NULL value.
#' @param add_msg Is an additional message that can be printed over the standard
#'   function error message. You can:
#'   * pass the names of the arguments that failed the test by using
#'   `{x_name}` in the message body (e.g. "What are the \{wrong_names\}");
#'   * pass the tested length by using `{arg_length}` in the message body (e.g.
#'   "I want them to be \{arg_length\})"
#'   * pass the lengths of the arguments that failed the test by using 
#'   `{wrong_length}` in the message body (e.g. "\{wrong_lengths\} are wrong")
#'
#' @return Returns invisible `NULL` when argument is of asserted length,
#'   otherwise it will throw an error.
#'
#' @examples
#' x1 <- 2
#' x2 <- c("x", "y")
#' nl1 <- NULL
#' \dontrun{
#' check_length(x1, arg_length = 1L)
#' check_length(x2, arg_length = 1L)
#' check_length(x1, arg_length = 2L)
#' check_length(
#'   x1, arg_length = 2L, add_msg = "{.arg {x_name}} should be short"
#' )
#' check_length(
#'   x1, arg_length = 2L, alt_null = TRUE, add_msg = "{.arg {x_name}} should be short"
#' )
#' check_length(
#'   nl1, arg_length = 2L, alt_null = TRUE, add_msg = "{.arg {x_name}} should be short"
#' )
#' }
#' @noRd 
check_length <- function(x, arg_length = 1L, alt_null = FALSE, add_msg = NULL) {
  if(!inherits(arg_length, "integer") | length(arg_length) != 1) {
    cli::cli_abort(
      "{.arg arg_length} should be an {.cls integer} of length {.val {1}}"
    )
  }
  
  check_class(add_msg, arg_class ="character", alt_null = TRUE)
  
  check_null <- check_null_cond(x = x, alt_null = alt_null)
  
  if (!(length(x) == arg_length | check_null)) {
    # parsing is time consuming so should run only if needed
    x_name <- deparse(substitute(x)) 
    # wrong_lengths stores lengths of wrong arguments, to be reused in messages
    wrong_length <- length(x)
    
    warn_msg <- ifelse(
      alt_null,
      "{.arg {x_name}} should be of length {.val {arg_length}} or {.cls NULL}",
      "{.arg {x_name}} should be of length {.val {arg_length}}"
    )
    
    cli::cli_abort(c(
      "i" = add_msg,
      "!" = warn_msg, 
      "x" = "{.arg {x_name}} is of length {.val {wrong_length}}"
    ))
  }
}


#' @title Capture Arguments
#' 
#' @description Helper to catch arguments.
#' 
#' @param ... unqouted arguments names
#' 
#' @noRd 
check_args <- function(...) {
  rlang::quos(...)
}

#' @title Return check_null Value
#' 
#' @description Check if `alt_null` argument is TRUE or FALSE. If it is `FALSE`
#'   it will return `FALSE`. If the argument is `TRUE` it will check if the
#'   x argument is `NULL` and return logical value.
#'   
#' @param x Argument to check if is NULL.
#' @param alt_null Logical. If `TRUE` it will check if `x` is `NULL`.
#' 
#' @noRd 
check_null_cond <- function(x, alt_null){
  if(!(isTRUE(alt_null) | isFALSE(alt_null) | length(alt_null) != 1)) {
    cli::cli_abort(
      "{.arg alt_null} should be either {.val TRUE} or {.val FALSE} and length 1"
    )
  }
  
  if(!alt_null) {
    check_null <- FALSE
  } else {
    check_null <- is.null(x)
  }
  check_null
}

#' @title Check if Argument is Single TRUE or FALSE
#' 
#' @description Check if an argument is single TRUE or FALSE. As an option it is
#'   possible to allow `NULL` value when `alt_null = TRUE`.
#'
#' @param x Function argument that is being asserted.
#' @param alt_null Logical. Should argument accept `NULL` value.
#' @param add_msg Is an additional message that can be printed over the standard
#'   function error message. You can:
#'   * pass the names of the arguments that failed the test by using
#'   `{x_name}` in the message body (e.g. "What are the \{x_name\}");
#'   * pass the class of the arguments that failed the test by using 
#'   `{wrong_class}` in the message body (e.g. "\{wrong_class\} is wrong")
#'
#' @return If argument is single `TRUE` or `FALSE` (optionally `NULL`) it 
#'   returns invisible `NULL`. Otherwise the function throws an error.
#'
#' @examples
#' c1 <- c("x", "y")
#' n1 <- c(1,3,4)
#' n2 <- c(1.5, 2.5)
#' i1 <- 1L
#' nl1 <- NULL
#' l1 <- FALSE
#' l2 <- c(FALSE, TRUE)
#' \dontrun{
#' check_bool(c1)
#' check_bool(nl1)
#' check_bool(nl1, alt_null = TRUE)
#' check_bool(n2, alt_null = TRUE)
#' check_bool(i1)
#' check_bool(l1)
#' check_bool(l2)
#' }
#' @export
check_bool <- function(x, alt_null = FALSE, add_msg = NULL) {
  
  check_class(add_msg, arg_class = "character", alt_null = TRUE)
  check_null <- check_null_cond(x = x, alt_null = alt_null)
  
  if(!(isTRUE(x) | isFALSE(x) | check_null)) {
    x_name <- deparse(substitute(x))
    # wrong_class stores class of wrong argument, to be reused in messages
    wrong_class <- class(x)
    if(wrong_class == "logical") {
      if (length(x) > 1) {
        error_msg <- paste0(
          "{.arg {x_name}} is a {.cls logical} vector,",
          " but should be a scalar (single value)"
        )
      } else {
        error_msg <- "{.arg {x_name}} is {.code {x}}"
      }
    } else {
      error_msg <- "{.arg {x_name}} is {.cls {wrong_class}}"
    }
    
    warn_msg <- ifelse(
      alt_null,
      "{.arg {x_name}} should be either {.code TRUE} or {.code FALSE} or {.cls NULL}",
      "{.arg {x_name}} should be either {.code TRUE} or {.code FALSE}"
    )
    
    cli_abort(c(
      "i" = add_msg,
      "!" = warn_msg, 
      "x" = error_msg
    ))
  }
}
