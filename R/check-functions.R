
## HAS_TESTS
#' Check that colnums vectors, as produced by
#' tidyselect::eval_select(), each point
#' to 0 or 1 columns
#'
#' @param Named list of named integer vectors
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_at_most_one_colnum <- function(x) {
    check_valid_colnum_list(x)
    nms <- names(x)
    for (i in seq_along(x)) {
        n_col <- length(x[[i]])
        if (n_col > 1L) {
            nm_arg <- nms[[i]]
            nms_cols <- names(x[[i]])
            cli::cli_abort(c("{n_col} variables specified for {.arg {nm_arg}}.",
                             i = "{.arg {nm_arg}} should be a single variable."))
        }
    }
    invisible(TRUE)
}


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
#' Check that infant and child arguments are only
#' supplied if the relevant age groups exist in the data
#'
#' @param infant_supplied Whether user supplied a value for
#' the `infant` argument
#' @param child_supplied Whether user supplied a value for
#' the `child` argument
#' @param age Vector of age group labels
#' @param methods Named vector of methods
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_infant_child_age_compatible <- function(infant_supplied, child_supplied, age, methods) {
    age_group_type <- age_group_type(age)
    if (infant_supplied && (age_group_type == "five")) {
        zero <- "0"
        infant <- methods[["infant"]]
        cli::cli_abort(c(paste("Value supplied for {.arg infant}, but {.arg age}",
                               "does not include age group {.val {zero}}."),
                         i = "{.arg infant}: {.val {infant}}.",
                         i = "{.arg age}: {.val {age}}."))
    }
    if (child_supplied && (age_group_type != "lt")) {
        one_four <- "1-4"
        child <- methods[["child"]]
        cli::cli_abort(c(paste("Value supplied for {.arg child}, but {.arg age}",
                               "does not include age group {.val {one_four}}."),
                         i = "{.arg child}: {.val {child}}.",
                         i = "{.arg age}: {.val {age}}."))
    }
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
check_mx <- function(mx) {
    if (!is.numeric(mx))
        cli::cli_abort(c("{.arg mx} is non-numeric.",
                         i = "{.arg mx} has class {.cls {class(mx)}}."))
    if (!rvec::is_rvec(mx) && !is.atomic(mx))
        cli::cli_abort(c("{.arg mx} is not an rvec or an ordinary vector.",
                         i = "{.arg mx} has class {.cls {class(mx)}}."))
    mx <- as.numeric(mx)
    n_neg <- sum(mx < 0, na.rm = TRUE)
    if (n_neg > 0L)
        cli::cli_abort("{.arg mx} has negative {cli::qty(n_neg)} value{?s}.")
    invisible(TRUE)
}


## HAS_TESTS
#' Check that column vectors do not overlap
#'
#' Given a named list of colnum vectors, like those
#' produced by [tidyselect::eval_select()],
#' throw an error if there is an overlap.
#'
#' @param A named list of integer vectors.
#'
#' @return `TRUE`, invisibly
#'
#' @seealso [tidyselect::eval_select()]
#'
#' @examples
#' x <- list(arg1 = c(age = 1L),
#'           arg2 = c(gender = 4L, region = 5L),
#'           arg3 = integer())
#' check_no_overlap_colnums(x)
#' @export
check_no_overlap_colnums <- function(x) {
    check_valid_colnum_list(x)
    n <- length(x)
    if (n >= 2L) {
        i_pairs <- combn(x = n, m = 2L, simplify = FALSE)
        for (i_pair in i_pairs)
            check_no_overlap_colnums_pair(pair = x[i_pair])
    }
    invisible(TRUE)
}

## HAS_TESTS
#' Check that a pair of colnum vectors do not overlap
#'
#' Check that a pair of colnum vectors, with the format
#' produced by tidyselect::eval_select(), do not have
#' any elements in common.
#'
#' @param pair A named list of two colnum vectors.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_no_overlap_colnums_pair <- function(pair) {
    intersection <- vctrs::vec_set_intersect(x = pair[[1L]],
                                             y = pair[[2L]])
    n <- length(intersection)
    if (n > 0L) {
        nms <- names(pair)
        nm1 <- nms[[1L]]
        nm2 <- nms[[2L]]
        cli::cli_abort(c("{.arg {nm1}} and {.arg {nm2}} use the same {cli::qty(n)} variable{?s}.",
                         i = "{.arg {nm1}}: {.val {names(pair[[1L]])}}.",
                         i = "{.arg {nm2}}: {.val {names(pair[[2L]])}}.",
                         i = "Overlap: {.val {names(intersection)}}."))
    }
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


## HAS_TESTS
#' Given that specified methods do not need
#' a sex variable to be supplied
#'
#' @param methods
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_sex_not_needed <- function(methods) {
    method_needs_sex <- c("CD", "HMD")
    nms_arg <- names(methods)
    for (i in seq_along(methods)) {
        method <- methods[[i]]
        if (method %in% method_needs_sex) {
            nm_arg <- nms_arg[[i]]
            cli::cli_abort("{.arg {nm_arg}} is {.val {method}} but no value supplied for {.arg sex}.")
        }
    }
    invisible(TRUE)
}


## HAS_TESTS
#' Check that input is character, of length 1,
#' non-NA, at least one char,
#' with no blanks
#'
#' @param x A string
#' @param x_arg Name to be used in error messages
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_string <- function(x, x_arg) {
    if (!is.character(x))
        cli::cli_abort(c("{.arg {x_arg}} is non-character.",
                         i = "{.arg {x_arg}} has class {.cls {class(x)}}."))
    n <- length(x)
    if (n != 1L)
        cli::cli_abort(c("{.arg {x_arg}} does not have length 1.",
                         i = "{.arg {x_arg}} has length {n}."))
    if (is.na(x))
        cli::cli_abort("{.arg {x_arg}} is {.val {NA_character_}}.")
    if (nchar(x) == 0L)
        cli::cli_abort("{.arg {x_arg}} is blank.")
    if (grepl("[[:blank:]]", x))
        cli::cli_abort("{.arg {x_arg}} contains blanks.")
    invisible(TRUE)
}                       


## HAS_TESTS
#' Check that a list of colnum vectors, as produced by
#' tidyselect::eval_select(), is valid
#'
#' @param x A named list of named integer vectors
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_valid_colnum_list <- function(x) {
    if (!is.list(x))
        cli::cli_abort("Internal error: {.arg x} is not a list.")
    if (length(x) > 0L) {
        nms <- names(x)
        if (is.null(nms))
            cli::cli_abort("Internal error: {.arg x} does not have names.")
        if (any(duplicated(nms)))
            cli::cli_abort("Internal error: names for {.arg x} have duplicates.")
        if (!all(vapply(x, is.integer, TRUE)))
            cli::cli_abort("Internal error: elements of {.arg x} are not all integer vectors.")
        has_names <- function(y) !is.null(names(y))
        if (!all(vapply(x, has_names, TRUE)))
            cli::cli_abort("Internal error: elements of {.arg x} are not all named.")
    }
    invisible(TRUE)
}
