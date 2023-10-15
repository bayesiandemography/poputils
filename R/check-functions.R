
    
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
#' Check that 'ax' is valid
#'
#' Check that 'ax' is numeric, non-negative, less than
#' corresponding 'nx'. NAs are allowed.
#'
#' @param ax Numeric vector
#' @param ax Character vector of age group labels
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_ax <- function(ax, age) {
    if (!is.numeric(ax))
        cli::cli_abort(c("{.arg ax} is non-numeric.",
                         i = "{.arg ax} has class {.cls {class(ax)}}."))
    is_neg <- !is.na(ax) & ax < 0
    i_neg <- match(TRUE, is_neg, nomatch = 0L)
    if (i_neg > 0L) {
        age_neg <- age[[i_neg]]
        ax_neg <- ax[[i_neg]]
        cli::cli_abort(c("{.arg ax} has negative value.",
                         i = "Value of {.arg ax} for age group {.val {age_neg}} is {.val {ax_neg}}."))
    }
    age_group_categ <- age_group_categ(age)
    is_ax_le_nx <- is_ax_le_nx(ax, age_group_categ)
    i_gt <- match(FALSE, is_ax_le_nx, nomatch = 0L)
    if (i_gt > 0L) {
        age_gt <- age[[i_gt]]
        ax_gt <- ax[[i_gt]]
        width <- age_upper(age_gt) - age_lower(age_gt)
        cli::cli_abort(c("{.arg ax} larger than width of corresponding age group.",
                         i = "Value of {.arg ax} for age group {.val {age_gt}} is {.val {ax_gt}}.",
                         i = "Age group {.val {age_gt}} has width {.val {width}}."))
    }
    invisible(TRUE)
}


#' Check for duplicated age labels
#'
#' Check to see if age labels are repeated,
#' with each label repeated the same number
#' of times - which suggests that the 'by'
#' variables are incomplete
#'
#' @param age Character vector of age labels
#'
#' @returns TRUE, invisibly
#' 
#' @noRd
check_duplicated_age <- function(age) {
    tab <- table(age)
    n_tab <- length(tab)
    is_all_length_1 <- all(tab == 1L)
    is_varying_counts <- (n_tab > 1L) && (any(tab[-1L] != tab[[1L]]))
    if (is_all_length_1 || is_varying_counts)
        invisible(TRUE)
    else
        cli::cli_abort(c("Age labels duplicated.",
                         i = "Do you need to modify `sex` or `by`?"))
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
#' Check that no columns of 'data' are used more than once,
#' and that required arguments are supplied
#'
#' @param mx_colnum Named integer vector
#' @param age_colnum Named integer vector
#' @param sex_colnum Named integer vector
#' @param ax_colnum Named integer vector
#' @param by_colnums Named integer vector
#' @param groups_colnums Named integer vector
#'
#' @returns TRUE, invisibly
#'
#' @noRd        
check_life_colnums <- function(mx_colnum,
                                  age_colnum,
                                  sex_colnum,
                                  ax_colnum,
                                  by_colnums,
                                  groups_colnums) {
    has_mx <- length(mx_colnum) > 0L
    has_age <- length(age_colnum) > 0L
    has_sex <- length(sex_colnum) > 0L
    has_by <- length(by_colnums) > 0L
    has_groups <- length(groups_colnums) > 0L
    if (!has_mx)
        cli::cli_abort("No value supplied for {.arg mx}.")
    if (!has_age)
        cli::cli_abort("No value supplied for {.arg age}.")
    if (has_by && has_groups)
        cli::cli_abort("Can't supply {.arg by} when {.arg data} is a grouped data
  frame.")
    at_most_one <- list(mx = mx_colnum,
                        age = age_colnum,
                        sex = sex_colnum,
                        ax = ax_colnum)
    check_at_most_one_colnum(at_most_one)
    no_overlap <- list(mx = mx_colnum,
                       age = age_colnum,
                       sex = sex_colnum,
                       ax = ax_colnum)
    check_no_overlap_colnums(no_overlap)
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
    if (rvec::is_rvec(mx))
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
#' @param x A named list of integer vectors.
#'
#' @return `TRUE`, invisibly
#'
#' @seealso [tidyselect::eval_select()]
#'
#' @examples
#' x <- list(arg1 = c(age = 1L),
#'           arg2 = c(gender = 4L, region = 5L))
#' check_no_overlap_colnums(x)
#' @export
check_no_overlap_colnums <- function(x) {
    check_valid_colnum_list(x)
    n <- length(x)
    if (n >= 2L) {
        i_pairs <- utils::combn(x = n, m = 2L, simplify = FALSE)
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
#' @param check_positive Whether 'x' must be positive.
#' @param check_nonneg Whether 'x' can be non-negative.
#' @param check_whole Whether 'x' is a whole number.
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_number <- function(x,
                         x_arg,
                         check_positive,
                         check_nonneg,
                         check_whole) {
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
    if (check_positive) {
        if (x <= 0)
            cli::cli_abort(c("{.arg {x_arg}} is non-positive.",
                             i = "{.arg {x_arg}} equals {.val {x}}."))
    }
    if (check_nonneg) {
        if (x < 0)
            cli::cli_abort(c("{.arg {x_arg}} is negative.",
                             i = "{.arg {x_arg}} equals {.val {x}}."))
    }
    if (check_whole) {
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
