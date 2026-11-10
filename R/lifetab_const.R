
## Constant-hazard life tables: developer functions -------------------------

#' Constant-Hazard Life Tables, for Developers
#'
#' Fast life-table calculations, assuming that the hazard (the
#' mortality rate) is constant within each age interval. The
#' functions work on matrices, with one row per age interval and one
#' column per life table (for instance, per draw). They are designed
#' for developers who need many life tables, and do only minimal
#' checking of inputs.
#'
#' - `.mx_to_lifetab_const()` calculates a life table from mortality
#'   rates.
#' - `.lx_to_mx_const()` calculates mortality rates from
#'   survivorship. The open age interval is given the rate of the
#'   last closed interval, since survivorship says nothing about
#'   mortality beyond the last age.
#' - `.brass_logit_lx()` calculates survivorship using a Brass logit
#'   model,
#'   \deqn{\text{logit}(1 - l_x) = \alpha + \beta \text{logit}(1 - l_x^{\text{std}}),}
#'   with \eqn{l_0 = 1}.
#'
#' With a constant hazard \eqn{m} in an interval of width \eqn{n},
#' the probability of dying is \eqn{q = 1 - e^{-nm}}, and person-years
#' are \eqn{L = l(1 - e^{-nm}) / m}, or \eqn{L = l / m} for the open
#' interval. These formulas are vectorised over age intervals and
#' life tables, which makes them much faster than the general
#' methods in [lifetab()].
#'
#' The calculations are done by C++ functions in the header file
#' `poputils/lifetab_const.hpp`, which other packages can use
#' directly, for instance in TMB templates, by adding `poputils` to
#' `LinkingTo` in their `DESCRIPTION` file and using
#' `#include <poputils/lifetab_const.hpp>`.
#'
#' @section Inputs:
#'
#' - Rows are age intervals, in order of age, and the last interval
#'   is open. There must be at least two intervals.
#' - `width` gives the widths of the intervals. The value for the
#'   open interval must be `Inf`.
#' - A numeric vector is treated as a matrix with one column.
#' - Values are not checked: `mx` must be non-negative; `lx` and
#'   `lx_std` must be positive, strictly decreasing, and equal to 1
#'   in the first row. Zero rates are allowed in
#'   `.mx_to_lifetab_const()`. Results for invalid values are
#'   undefined.
#'
#' @param mx Matrix of mortality rates.
#' @param lx Matrix of survivorship values.
#' @param width Numeric vector with the width of each age interval.
#' Last element must be `Inf`.
#' @param lx_std Survivorship in the standard. A vector, or a matrix
#' with one column per element of `alpha`.
#' @param alpha Numeric vector. Level parameter in the Brass logit
#' model.
#' @param beta Numeric vector, the same length as `alpha` or length
#' 1. Slope parameter in the Brass logit model. Default is `1`.
#'
#' @returns
#' - `.mx_to_lifetab_const()`: A named list of matrices, `qx`, `lx`,
#'   `dx`, `Lx`, and `ex`, each the same size as `mx`, with `lx`
#'   equal to 1 in the first row.
#' - `.lx_to_mx_const()`: A matrix the same size as `lx`.
#' - `.brass_logit_lx()`: A matrix with one row per row of `lx_std`
#'   and one column per element of `alpha`.
#'
#' @seealso [lifetab()] and [lifeexp()] for life tables from data
#' frames, with a choice of methods.
#'
#' @examples
#' width <- c(1, 4, rep(5, 18), Inf)
#' mx <- c(0.05, 0.01, 0.002, 0.001, exp(-7.5 + 0.09 * seq(10, 90, by = 5)))
#' tab <- .mx_to_lifetab_const(mx, width = width)
#' tab$ex[1, ]
#'
#' ## recover mortality rates from survivorship
#' .lx_to_mx_const(tab$lx, width = width)[1:5, ]
#'
#' ## Brass logit life tables at three levels
#' lx <- .brass_logit_lx(tab$lx, alpha = c(-0.5, 0, 0.5))
#' lx[1:5, ]
#' @name lifetab_const
NULL


#' @rdname lifetab_const
#' @export
.mx_to_lifetab_const <- function(mx, width) {
  mx <- as_matrix_const(mx, nm = "mx")
  check_width_const(width = width, n_age = nrow(mx))
  mx_to_lifetab_const_mat(mx = mx, width = as.double(width))
}


#' @rdname lifetab_const
#' @export
.lx_to_mx_const <- function(lx, width) {
  lx <- as_matrix_const(lx, nm = "lx")
  check_width_const(width = width, n_age = nrow(lx))
  lx_to_mx_const_mat(lx = lx, width = as.double(width))
}


#' @rdname lifetab_const
#' @export
.brass_logit_lx <- function(lx_std, alpha, beta = 1) {
  lx_std <- as_matrix_const(lx_std, nm = "lx_std")
  if (!is.numeric(alpha) || length(alpha) == 0L)
    cli::cli_abort("{.arg alpha} must be a numeric vector with at least one element.")
  if (!is.numeric(beta) || !(length(beta) %in% c(1L, length(alpha))))
    cli::cli_abort("{.arg beta} must be a numeric vector with length 1 or the length of {.arg alpha}.")
  if (!(ncol(lx_std) %in% c(1L, length(alpha))))
    cli::cli_abort("{.arg lx_std} must have one column or one column per element of {.arg alpha}.")
  beta <- rep_len(as.double(beta), length(alpha))
  brass_logit_lx_mat(lx_std = lx_std, alpha = as.double(alpha), beta = beta)
}


## Helpers -------------------------------------------------------------------

#' Coerce input to a numeric matrix, for constant-hazard functions
#'
#' @param x A numeric vector or matrix
#' @param nm Name of the argument, for messages
#'
#' @returns A double matrix
#'
#' @noRd
as_matrix_const <- function(x, nm) {
  if (!is.numeric(x))
    cli::cli_abort("{.arg {nm}} must be numeric.", call = rlang::caller_env())
  if (is.null(dim(x)))
    x <- matrix(x, ncol = 1L)
  if (length(dim(x)) != 2L)
    cli::cli_abort("{.arg {nm}} must be a vector or a matrix.", call = rlang::caller_env())
  if (nrow(x) < 2L)
    cli::cli_abort("{.arg {nm}} must have at least two age intervals.", call = rlang::caller_env())
  storage.mode(x) <- "double"
  x
}


#' Check widths, for constant-hazard functions
#'
#' @param width Numeric vector
#' @param n_age Number of age intervals
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_width_const <- function(width, n_age) {
  call <- rlang::caller_env()
  if (!is.numeric(width) || length(width) != n_age)
    cli::cli_abort("{.arg width} must be a numeric vector with one element per age interval ({n_age}).",
                   call = call)
  closed <- width[-n_age]
  if (anyNA(closed) || any(!is.finite(closed)) || any(closed <= 0))
    cli::cli_abort("Widths of closed intervals must be finite and positive.", call = call)
  if (!identical(as.double(width[[n_age]]), Inf))
    cli::cli_abort("Last element of {.arg width} must be {.code Inf} (the open interval).",
                   call = call)
  invisible(TRUE)
}
