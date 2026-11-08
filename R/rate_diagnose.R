## HAS_TESTS
#' Check Rates
#'
#' Check that values are valid rates, such as mortality rates (`mx`) or age-specific fertility rates.
#' Values must be finite and non-negative. Further conditions
#' can be checked using the remaining arguments.
#'
#' `rate_diagnose()` reports on each condition.
#' `rate_assert()` raises an error if any condition is not met,
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
#' - `rate_diagnose()` returns a list with a logical flag
#'   called `ok` and a [tibble][tibble::tibble()] called `details`,
#'   with one row per condition.
#' - `rate_assert()` returns `x` invisibly, or raises an error.
#'
#' @param x A numeric vector or an [rvec][rvec::rvec()].
#' @param no_na Check that there are no `NA`s.
#' Default is `FALSE` (don't check).
#' @param no_zero Check that there are no zeros, as is needed, for instance, before taking logs.
#' Default is `FALSE` (don't check).
#' @param x_arg Name of `x`, used in messages.
#' Defaults to the expression supplied for `x`.
#' Functions that call `rate_assert()` or `rate_diagnose()`
#' can supply their own name.
#' @param call Execution environment of the function
#' to be named in error messages. Only needed when
#' the function is called from inside another function.
#' Default is [rlang::caller_env()].
#'
#' @seealso
#' - [count_diagnose()], [prob_diagnose()], [rate_diagnose()]
#'
#' @examples
#' x <- c(0.01, 0, 0.2)
#' rate_diagnose(x)
#' rate_diagnose(x, no_zero = TRUE)
#' try(rate_assert(c(0.01, -0.2)))
#' @export
rate_diagnose <- function(x,
                         no_na = FALSE,
                         no_zero = FALSE,
                         x_arg = rlang::caller_arg(x),
                         call = rlang::caller_env()) {
  conditions <- numeric_conditions(
    fixed = c("finite", "nonneg"),
    flags = list(no_na = no_na, no_zero = no_zero),
    order = c("no_na", "finite", "nonneg", "le_one", "no_zero", "no_one", "whole"),
    call = call
  )
  inner_numeric_diagnose(x = x,
                         x_arg = x_arg,
                         conditions = conditions,
                         call = call)
}


## HAS_TESTS
#' @rdname rate_diagnose
#' @export
rate_assert <- function(x,
                       no_na = FALSE,
                       no_zero = FALSE,
                       x_arg = rlang::caller_arg(x),
                       call = rlang::caller_env()) {
  conditions <- numeric_conditions(
    fixed = c("finite", "nonneg"),
    flags = list(no_na = no_na, no_zero = no_zero),
    order = c("no_na", "finite", "nonneg", "le_one", "no_zero", "no_one", "whole"),
    call = call
  )
  inner_numeric_assert(x = x,
                       x_arg = x_arg,
                       conditions = conditions,
                       call = call)
}
