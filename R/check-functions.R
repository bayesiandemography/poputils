
## HAS_TESTS
#' Check a logical flag
#'
#' @param x TRUE or FALSE
#'
#' @returns TRUE, invisibly
#' 
#' @noRd
check_flag <- function(x) {
    nm <- deparse1(substitute(x))
    if (!identical(length(x), 1L))
        cli::cli_abort(c("{.arg {nm}} does not have length 1",
                         i = "{.arg {nm}} has length {length(x)}."))
    if (!is.logical(x))
        cli::cli_abort(c("{.arg {nm}} does not have class {.cls logical}.",
                         i = "{.arg {nm}} has class {.cls {class(x)}}"))
    if (is.na(x))
        cli::cli_abort("{.arg {nm}} is {.val {NA}}")
    invisible(TRUE)
}


## HAS_TESTS
#' Check that sex variable fits the requirements
#' of life expectancy function
#'
#' Check that 'sex' variable is a character
#' vector composed entirely of (one of)
#' "Female" or "Male".
#'
#' @param sex A character vector
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_lifeexp_sex <- function(sex) {
    valid <- c("Female", "Male")
    if (!is.character(sex))
        cli::cli_abort(c("{.arg sex} is not a character vector.",
                         i = "{.arg sex} has class {.cls {class(sex)}}."))
    if (length(sex) == 0L)
        cli::cli_abort("{.arg sex} has length 0.")
    is_valid <- sex %in% valid
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L)
        cli::cli_abort(c("{.arg sex} has invalid value.",
                         i = "Element {i_invalid} of {.arg sex} is {.val {sex[[i_invalid]]}}.",
                         i = "Valid values are: {.val {valid}}."))
    if (length(sex) > 1L) {
        is_same <- sex[-1L] == sex[[1L]]
        i_diff <- match(FALSE, is_same, nomatch = 0L)
        if (i_diff > 0L)
            cli::cli_abort(c("Values for {.arg sex} not all the same.",
                             i = "Element 1 is {.val {sex[[1L]]}}.",
                             i = "Element {i_diff + 1} is {.val {sex[[i_diff + 1L]]}}."))
    }
    invisible(TRUE)
}



check_lifeexp_method <- function(method) {
    valid <- c("const", "mid", "CD", "HMD")
    if (is.null(method))
        return(invisible(TRUE))
    if (!is.character(method))
        cli::cli_abort(c("{.arg method} is not a character vector.",
                         i = "{.arg method} has class {.cls {class(method)}}."))
    if (length(method) == 0L)
        cli::cli_abort("{.arg method} has length 0.")
    is_valid <- method %in% valid
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L)
        cli::cli_abort(c("{.arg method} has invalid value.",
                         i = "Element {i_invalid} of {.arg method} is {.val {method[[i_invalid]]}}.",
                         i = "Valid values are: {.val {valid}}."))
    if (length(method) > 1L) {
        is_same <- method[-1L] == method[[1L]]
        i_diff <- match(FALSE, is_same, nomatch = 0L)
        if (i_diff > 0L)
            cli::cli_abort(c("Values for {.arg method} not all the same.",
                             i = "Element 1 is {.val {method[[1L]]}}.",
                             i = "Element {i_diff + 1} is {.val {method[[i_diff + 1L]]}}."))
    }
    invisible(TRUE)
}


## HAS_TESTS
#' Check that an rvec of mortality rates is valid
#'
#' Check that rvec is double and
#' all non-negative. NAs are allowed.
#'
#' @param mx An rvec_dbl.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_mx_rvec <- function(mx) {
    if (!(inherits(mx, "rvec_dbl") || inherits(mx, "rvec_int")))
        cli::cli_abort(c("{.arg mx} is non-numeric.",
                         i = "{.arg mx} has class {.cls {class(mx)}}."))
    mx <- as.matrix(mx)
    if (any(mx < 0L, na.rm = TRUE))
        cli::cli_abort("{.arg mx} has negative value(s).")
    invisible(TRUE)
}
        

## HAS_TESTS
#' Check that a vector of mortality rates is valid
#'
#' Check that vector is double and
#' all non-negative. NAs are allowed.
#'
#' @param mx A numeric vector.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_mx_vec <- function(mx) {
    if (!is.numeric(mx))
        cli::cli_abort(c("{.arg mx} is non-numeric.",
                         i = "{.arg mx} has class {.cls {class(mx)}}."))
    if (any(mx < 0, na.rm = TRUE))
        cli::cli_abort("{.arg mx} has negative value(s).")
    invisible(TRUE)
}

## HAS_TESTS
#' Check a number
#'
#' Check that `x` is valid scalar integer or double.
#'
#' @param x A number.
#' @param x_arg Name for `x` to be
#' used in error messages.
#' @param is_positive Whether 'x' must be positive.
#' @param is_nonneg Whether 'x' can be non-negative.
#' @param is_whole Whether 'x' is a whole number.
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_number <- function(x, x_arg, is_positive, is_nonneg, is_whole) {
    if (!is.numeric(x))
        cli::cli_abort(c("{.arg {x_arg}} is non-numeric.",
                         i = "{.arg {x_arg}} has class {.cls {class(x)}}."))
    if (length(x) != 1L)
        cli::cli_abort(c("{.arg {x_arg}} does not have length 1.",
                         i = "{.arg {x_arg}} has length {.val {length(x)}}."))
    if (is.na(x))
        cli::cli_abort("{.arg {x_arg}} is {.val {NA}}.")
    if (is.infinite(x))
        cli::cli_abort("{.arg {x_arg}} is infinite.")
    if (is_positive) {
        if (x <= 0)
            cli::cli_abort(c("{.arg {x_arg}} is non-positive.",
                             i = "{.arg {x_arg}} equals {.val {x}}."))
    }
    if (is_nonneg) {
        if (x < 0)
            cli::cli_abort(c("{.arg {x_arg}} is negative.",
                             i = "{.arg {x_arg}} equals {.val {x}}."))
    }
    if (is_whole) {
        if (x != round(x))
            cli::cli_abort(c("{.arg {x_arg}} is not a whole number.",
                             i = "{.arg {x_arg}} is {.val {x}}."))
    }
    invisible(TRUE)
}
