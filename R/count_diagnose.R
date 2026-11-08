## HAS_TESTS
#' Check Counts
#'
#' Check that values are valid counts, such as population, births, or deaths.
#' Values must be finite and non-negative. Further conditions
#' can be checked using the remaining arguments.
#'
#' `count_diagnose()` reports on each condition.
#' `count_assert()` raises an error if any condition is not met,
#' and otherwise returns `x` invisibly, so that it can be
#' used in a pipeline.
#'
#' @section Checking values:
#'
#' poputils has three families of functions for checking numeric
#' values: `count_*()` for counts, `prob_*()` for probabilities,
#' and `rate_*()` for rates. Each family has a `diagnose` function,
#' which reports on each condition, and an `assert` function, which
#' raises an error if any condition is not met.
#' Some conditions are always checked, and others are
#' switched on through arguments.
#' All functions accept ordinary numeric vectors and [rvecs][rvec::rvec()].
#'
#' Apart from `no_na`, conditions ignore `NA`s.
#'
#' | Condition | Fails if a value is... |
#' |-----------|---------------------------|
#' | `no_na`   | `NA` |
#' | `finite`  | `Inf` or `-Inf` |
#' | `nonneg`  | less than 0 |
#' | `le_one`  | greater than 1 |
#' | `no_zero` | equal to 0 |
#' | `no_one`  | equal to 1 |
#' | `whole`   | not a whole number |
#'
#' @return
#' - `count_diagnose()` returns a list with a logical flag
#'   called `ok` and a [tibble][tibble::tibble()] called `details`,
#'   with one row per condition.
#' - `count_assert()` returns `x` invisibly, or raises an error.
#'
#' @param x A numeric vector or an [rvec][rvec::rvec()].
#' @param no_na Check that there are no `NA`s.
#' Default is `FALSE` (don't check).
#' @param no_zero Check that there are no zeros, as is needed, for instance, for counts used as denominators.
#' Default is `FALSE` (don't check).
#' @param whole Check that all values are whole numbers.
#' Default is `FALSE` (don't check).
#' @param x_arg Name of `x`, used in messages.
#' Defaults to the expression supplied for `x`.
#' Functions that call `count_assert()` or `count_diagnose()`
#' can supply their own name.
#' @param call Execution environment of the function
#' to be named in error messages. The default is the
#' function itself, so that errors name, for instance,
#' `count_assert()`. A function that calls `count_assert()`
#' or `count_diagnose()` can supply `call = rlang::current_env()`,
#' so that errors name that function instead.
#'
#' @seealso
#' - [count_diagnose()], [prob_diagnose()], [rate_diagnose()]
#'
#' @examples
#' x <- c(10, 0, 3.5, NA)
#' count_diagnose(x)
#' count_diagnose(x, no_na = TRUE, no_zero = TRUE, whole = TRUE)
#'
#' ## use within a pipeline
#' df <- data.frame(age = c("0-4", "5-9"), popn = c(120, 85))
#' df$popn <- count_assert(df$popn, no_na = TRUE, whole = TRUE)
#'
#' ## supply a name, as a calling function would
#' try(count_assert(c(1, -2), x_arg = "popn"))
#' @export
count_diagnose <- function(x,
                          no_na = FALSE,
                          no_zero = FALSE,
                          whole = FALSE,
                          x_arg = rlang::caller_arg(x),
                          call = rlang::current_env()) {
  conditions <- numeric_conditions(
    fixed = c("finite", "nonneg"),
    flags = list(no_na = no_na, no_zero = no_zero, whole = whole),
    order = c("no_na", "finite", "nonneg", "le_one", "no_zero", "no_one", "whole"),
    call = call
  )
  inner_numeric_diagnose(x = x,
                         x_arg = x_arg,
                         conditions = conditions,
                         call = call)
}


## HAS_TESTS
#' @rdname count_diagnose
#' @export
count_assert <- function(x,
                        no_na = FALSE,
                        no_zero = FALSE,
                        whole = FALSE,
                        x_arg = rlang::caller_arg(x),
                        call = rlang::current_env()) {
  conditions <- numeric_conditions(
    fixed = c("finite", "nonneg"),
    flags = list(no_na = no_na, no_zero = no_zero, whole = whole),
    order = c("no_na", "finite", "nonneg", "le_one", "no_zero", "no_one", "whole"),
    call = call
  )
  inner_numeric_assert(x = x,
                       x_arg = x_arg,
                       conditions = conditions,
                       call = call)
}
