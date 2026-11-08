## HAS_TESTS
#' Check Probabilities
#'
#' Check that values are valid probabilities, such as probabilities of dying (`qx`) or coverage ratios.
#' Values must be finite, and between 0 and 1. Further conditions
#' can be checked using the remaining arguments.
#'
#' `prob_diagnose()` reports on each condition.
#' `prob_assert()` raises an error if any condition is not met,
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
#' - `prob_diagnose()` returns a list with a logical flag
#'   called `ok` and a [tibble][tibble::tibble()] called `details`,
#'   with one row per condition.
#' - `prob_assert()` returns `x` invisibly, or raises an error.
#'
#' @param x A numeric vector or an [rvec][rvec::rvec()].
#' @param no_na Check that there are no `NA`s.
#' Default is `FALSE` (don't check).
#' @param no_zero Check that there are no zeros, as is needed, for instance, before taking logs or logits.
#' Default is `FALSE` (don't check).
#' @param no_one Check that there are no ones, as is needed, for instance, before taking logits.
#' Default is `FALSE` (don't check).
#' @param x_arg Name of `x`, used in messages.
#' Defaults to the expression supplied for `x`.
#' Functions that call `prob_assert()` or `prob_diagnose()`
#' can supply their own name.
#' @param call Execution environment of the function
#' to be named in error messages. The default is the
#' function itself, so that errors name, for instance,
#' `prob_assert()`. A function that calls `prob_assert()`
#' or `prob_diagnose()` can supply `call = rlang::current_env()`,
#' so that errors name that function instead.
#'
#' @seealso
#' - [count_diagnose()], [prob_diagnose()], [rate_diagnose()]
#'
#' @examples
#' x <- c(0.1, 0, 1, 0.5)
#' prob_diagnose(x)
#' prob_diagnose(x, no_zero = TRUE, no_one = TRUE)
#' try(prob_assert(c(0.2, 1.3)))
#' @export
prob_diagnose <- function(x,
                         no_na = FALSE,
                         no_zero = FALSE,
                         no_one = FALSE,
                         x_arg = rlang::caller_arg(x),
                         call = rlang::current_env()) {
  conditions <- numeric_conditions(
    fixed = c("finite", "nonneg", "le_one"),
    flags = list(no_na = no_na, no_zero = no_zero, no_one = no_one),
    order = c("no_na", "finite", "nonneg", "le_one", "no_zero", "no_one", "whole"),
    call = call
  )
  inner_numeric_diagnose(x = x,
                         x_arg = x_arg,
                         conditions = conditions,
                         call = call)
}


## HAS_TESTS
#' @rdname prob_diagnose
#' @export
prob_assert <- function(x,
                       no_na = FALSE,
                       no_zero = FALSE,
                       no_one = FALSE,
                       x_arg = rlang::caller_arg(x),
                       call = rlang::current_env()) {
  conditions <- numeric_conditions(
    fixed = c("finite", "nonneg", "le_one"),
    flags = list(no_na = no_na, no_zero = no_zero, no_one = no_one),
    order = c("no_na", "finite", "nonneg", "le_one", "no_zero", "no_one", "whole"),
    call = call
  )
  inner_numeric_assert(x = x,
                       x_arg = x_arg,
                       conditions = conditions,
                       call = call)
}
