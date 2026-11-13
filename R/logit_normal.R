
## HAS_TESTS
#' Mean of a Logit-Normal Distribution, and the Location Giving a
#' Required Mean
#'
#' If \eqn{X \sim \text{N}(\mu, \sigma^2)}, then
#' \eqn{P = \text{logit}^{-1}(X)} has a logit-normal distribution.
#'
#' - `.logit_normal_mean()` calculates \eqn{E[P]}, given
#'   \eqn{\mu} and \eqn{\sigma}.
#' - `.logit_normal_solve_mu()` calculates the \eqn{\mu} that gives
#'   a required \eqn{E[P]}, given \eqn{\sigma}.
#'
#' The mean has no closed form. It is calculated by Gauss-Hermite
#' quadrature with 30 nodes. The relative error is below
#' \eqn{10^{-7}} for \eqn{\sigma \le 2} and below \eqn{10^{-3}} for
#' \eqn{\sigma \le 4}. `.logit_normal_solve_mu()` uses Newton's
#' method on the logit scale, starting from the probit
#' approximation \eqn{\mu_0 = \text{logit}(E[P]) \sqrt{1 + \pi
#' \sigma^2 / 8}}, and gives an error if it does not converge.
#'
#' The calculations are done by C++ functions in the header
#' `poputils/logit_normal.hpp`, which other packages, including
#' those with TMB templates, can use via `LinkingTo: poputils`, so
#' that R code and compiled code give identical results.
#'
#' Arguments are recycled to a common length. A length-1 argument
#' is recycled; other lengths must be equal.
#'
#' @param mu Mean of \eqn{X} (the location of the logit-normal).
#' A numeric vector.
#' @param sigma Standard deviation of \eqn{X}. A non-negative
#' numeric vector.
#' @param mean Required mean of \eqn{P}. A numeric vector, with
#' values strictly between 0 and 1.
#'
#' @returns A numeric vector.
#'
#' @examples
#' .logit_normal_mean(mu = 2, sigma = c(0, 0.5, 1))
#' mu <- .logit_normal_solve_mu(mean = 0.9, sigma = c(0, 0.5, 1))
#' mu
#' .logit_normal_mean(mu = mu, sigma = c(0, 0.5, 1))
#' @export
.logit_normal_mean <- function(mu, sigma) {
  check_logit_normal_arg(x = mu, x_arg = "mu", check_nonneg = FALSE)
  check_logit_normal_arg(x = sigma, x_arg = "sigma", check_nonneg = TRUE)
  args <- recycle_logit_normal(list(mu = mu, sigma = sigma))
  logit_normal_mean_cpp11(mu = args$mu, sigma = args$sigma)
}


#' @rdname dot-logit_normal_mean
#' @export
.logit_normal_solve_mu <- function(mean, sigma) {
  check_logit_normal_arg(x = mean, x_arg = "mean", check_nonneg = FALSE)
  n_out <- sum(mean <= 0 | mean >= 1)
  if (n_out > 0L)
    cli::cli_abort("{.arg mean} has {n_out} value{?s} not strictly between 0 and 1.")
  check_logit_normal_arg(x = sigma, x_arg = "sigma", check_nonneg = TRUE)
  args <- recycle_logit_normal(list(mean = mean, sigma = sigma))
  logit_normal_solve_mu_cpp11(mean = args$mean, sigma = args$sigma, n_iter = 10L)
}


## Helpers --------------------------------------------------------------------

## HAS_TESTS
#' Check an Argument to the Logit-Normal Functions
#'
#' @param x Argument
#' @param x_arg Name of argument
#' @param check_nonneg Whether to require non-negative values
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_logit_normal_arg <- function(x, x_arg, check_nonneg) {
  check_not_rvec(x, nm_x = x_arg)
  check_numeric(x = x,
                x_arg = x_arg,
                check_na = TRUE,
                check_positive = FALSE,
                check_nonneg = check_nonneg,
                check_whole = FALSE)
  invisible(TRUE)
}


## HAS_TESTS
#' Recycle Arguments to a Common Length
#'
#' Length-1 arguments are recycled; other lengths must be equal.
#'
#' @param args Named list of numeric vectors
#'
#' @returns Named list of double vectors, all the same length
#'
#' @noRd
recycle_logit_normal <- function(args) {
  lens <- lengths(args)
  n <- max(lens)
  is_bad <- lens != 1L & lens != n
  if (any(is_bad)) {
    nms <- names(args)
    cli::cli_abort(c("Lengths of {.arg {nms[[1L]]}} and {.arg {nms[[2L]]}} not compatible.",
                     i = "{.arg {nms[[1L]]}} has length {lens[[1L]]}.",
                     i = "{.arg {nms[[2L]]}} has length {lens[[2L]]}.",
                     i = "Lengths must be equal, or one must be 1."))
  }
  lapply(args, function(x) rep_len(as.double(x), n))
}
