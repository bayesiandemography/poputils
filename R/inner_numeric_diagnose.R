## Internal engine for the count_*, prob_*, and rate_* families of
## diagnose and assert functions. Each condition is a function that
## takes a matrix of values (one row per element of 'x', one column
## per draw) and returns a logical matrix flagging problem values.


## HAS_TESTS
#' Diagnose Numeric Values Against a Set of Conditions
#'
#' @param x A numeric vector or an rvec.
#' @param x_arg Name of `x`, for messages.
#' @param conditions Character vector of condition names,
#' in the order they are reported.
#' @param call Environment named in error messages.
#'
#' @returns A list with elements `ok` (a logical flag)
#' and `details` (a tibble with columns `condition`,
#' `passed`, and `comment`).
#'
#' @noRd
inner_numeric_diagnose <- function(x, x_arg, conditions, call) {
  m <- numeric_values_matrix(x = x, x_arg = x_arg, call = call)
  is_rvec <- rvec::is_rvec(x)
  rows <- lapply(conditions, function(condition) {
    fun <- numeric_condition_fun(condition)
    is_bad <- fun(m)
    passed <- !any(is_bad)
    comment <- if (passed) {
      NA_character_
    } else {
      numeric_example(m = m, is_bad = is_bad, x_arg = x_arg, is_rvec = is_rvec)
    }
    tibble::tibble_row(condition = condition,
                       passed = passed,
                       comment = comment)
  })
  details <- if (length(rows) == 0L) {
    tibble::tibble(condition = character(),
                   passed = logical(),
                   comment = character())
  } else {
    vctrs::vec_rbind(!!!rows)
  }
  list(ok = all(details$passed),
       details = details)
}


## HAS_TESTS
#' Assert that Numeric Values Meet a Set of Conditions
#'
#' @inheritParams inner_numeric_diagnose
#'
#' @returns `x`, invisibly, or raises an error.
#'
#' @noRd
inner_numeric_assert <- function(x, x_arg, conditions, call) {
  val <- inner_numeric_diagnose(x = x,
                                x_arg = x_arg,
                                conditions = conditions,
                                call = call)
  if (!val$ok) {
    details <- val$details[!val$details$passed, , drop = FALSE]
    failed <- details$condition
    comments <- paste0(details$comment, ".")
    n <- length(failed)
    msg <- "{.arg {x_arg}} fails {cli::qty(n)}check{?s} {.arg {failed}}."
    bullets <- paste0("{.arg {failed[[", seq_len(n), "]]}}: {comments[[",
                      seq_len(n), "]]}")
    names(bullets) <- rep("x", times = n)
    cli::cli_abort(c(msg, bullets), call = call)
  }
  invisible(x)
}


## HAS_TESTS
#' Convert Numeric Vector or Rvec to Matrix of Values
#'
#' Raises an error if `x` is not numeric.
#'
#' @inheritParams inner_numeric_diagnose
#'
#' @returns A numeric matrix with one row per element
#' of `x`, and one column per draw (one column if
#' `x` is not an rvec).
#'
#' @noRd
numeric_values_matrix <- function(x, x_arg, call) {
  if (rvec::is_rvec(x)) {
    m <- as.matrix(x)
  } else {
    m <- matrix(x, ncol = 1L)
  }
  if (!is.numeric(m) || is.factor(x)) {
    cli::cli_abort(c("{.arg {x_arg}} is non-numeric.",
                     i = "{.arg {x_arg}} has class {.cls {class(x)}}."),
                   call = call)
  }
  m
}


## HAS_TESTS
#' Get the Function Used to Check a Condition
#'
#' Each function takes a numeric matrix and returns a logical
#' matrix of the same dimensions, `TRUE` where a value fails
#' the condition. Apart from `"no_na"`, `NA`s never fail.
#'
#' @param condition Name of a condition.
#'
#' @returns A function.
#'
#' @noRd
numeric_condition_fun <- function(condition) {
  switch(condition,
         no_na = function(m) is.na(m),
         finite = function(m) !is.na(m) & is.infinite(m),
         nonneg = function(m) !is.na(m) & m < 0,
         le_one = function(m) !is.na(m) & m > 1,
         no_zero = function(m) !is.na(m) & m == 0,
         no_one = function(m) !is.na(m) & m == 1,
         whole = function(m) !is.na(m) & is.finite(m) & (m != round(m)),
         cli::cli_abort("Internal error: Invalid condition {.val {condition}}."))
}


## HAS_TESTS
#' Describe the First Value that Fails a Condition
#'
#' The first value is the one with the lowest element index,
#' and, within that element, the lowest draw.
#'
#' @param m Numeric matrix of values.
#' @param is_bad Logical matrix flagging problem values.
#' @param x_arg Name of `x`, for messages.
#' @param is_rvec Whether `x` is an rvec.
#'
#' @returns A string.
#'
#' @noRd
numeric_example <- function(m, is_bad, x_arg, is_rvec) {
  i_bad <- which(t(is_bad))[[1L]]
  n_draw <- ncol(m)
  i_elem <- (i_bad - 1L) %/% n_draw + 1L
  i_draw <- (i_bad - 1L) %% n_draw + 1L
  val <- format(m[i_elem, i_draw])
  if (is_rvec) {
    sprintf("Example: draw %d of %s[%d] is %s", i_draw, x_arg, i_elem, val)
  } else {
    sprintf("Example: %s[%d] is %s", x_arg, i_elem, val)
  }
}


## HAS_TESTS
#' Assemble Conditions from Fixed Conditions and Flags
#'
#' @param fixed Conditions that are always checked.
#' @param flags Named logical vector of optional conditions.
#' @param order Order in which conditions are reported.
#' @param call Environment named in error messages.
#'
#' @returns A character vector.
#'
#' @noRd
numeric_conditions <- function(fixed, flags, order, call) {
  for (nm in names(flags)) {
    check_flag_named(x = flags[[nm]], nm_x = nm, call = call)
  }
  conditions <- c(fixed, names(flags)[unlist(flags)])
  order[order %in% conditions]
}


## HAS_TESTS
#' Check a Flag, with the Name Supplied Explicitly
#'
#' @param x Value to check.
#' @param nm_x Name of argument, for messages.
#' @param call Environment named in error messages.
#'
#' @returns `TRUE`, invisibly.
#'
#' @noRd
check_flag_named <- function(x, nm_x, call) {
  if (!identical(length(x), 1L))
    cli::cli_abort(c("{.arg {nm_x}} does not have length 1",
                     i = "{.arg {nm_x}} has length {length(x)}."),
                   call = call)
  if (!is.logical(x))
    cli::cli_abort(c("{.arg {nm_x}} does not have class {.cls logical}.",
                     i = "{.arg {nm_x}} has class {.cls {class(x)}}"),
                   call = call)
  if (is.na(x))
    cli::cli_abort("{.arg {nm_x}} is {.val {NA}}", call = call)
  invisible(TRUE)
}
