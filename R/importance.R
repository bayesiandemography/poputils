
## Importance sampling: developer functions -------------------------------------

#' Importance Sampling and Resampling, for Developers
#'
#' Building blocks for correcting draws from an approximation to a
#' posterior distribution by importance sampling.
#'
#' - `.psis()` applies Pareto smoothing to importance weights
#'   (PSIS: Vehtari, Simpson, Gelman, Yao and Gabry, 2024) and returns
#'   normalised log weights and the diagnostic \eqn{\hat{k}}. The
#'   largest weights are replaced by quantiles of a generalised Pareto
#'   distribution fitted to them (Zhang and Stephens' estimator, with
#'   a weakly informative prior), so a few extreme weights do not
#'   dominate. Values of \eqn{\hat{k}} below 0.5 indicate reliable
#'   weights, 0.5 to 0.7 usable ones, and above 0.7 unreliable ones.
#'   The calculations follow `loo::psis()`.
#' - `.resample_systematic()` draws indices by systematic
#'   resampling: one uniform number and evenly spaced points along the
#'   cumulative weights, so that element `i` is selected
#'   `floor(n * w[i])` or `ceiling(n * w[i])` times.
#' - `.rmvt()` and `.dmvt()` draw from, and evaluate the density of,
#'   a multivariate t distribution.
#'
#' @param log_w Log importance weights (unnormalised). A numeric
#' vector. Values of `-Inf` (zero weight) are allowed.
#' @param weight Weights, non-negative, not all zero. Need not sum
#' to 1.
#' @param n Number of draws or indices.
#' @param x Matrix of points, one column per point.
#' @param mean Location, a numeric vector.
#' @param scale Scale matrix, symmetric positive definite.
#' @param df Degrees of freedom, a positive number.
#' @param log Whether to return the log density. Default is `TRUE`.
#'
#' @returns
#' - `.psis()`: A list with elements `log_weight` (smoothed, with
#'   `sum(exp(log_weight))` equal to 1), `k_hat`, and `ess` (effective
#'   sample size of the smoothed weights).
#' - `.resample_systematic()`: An integer vector of length `n`, in
#'   increasing order.
#' - `.rmvt()`: A matrix with one row per element of `mean` and `n`
#'   columns.
#' - `.dmvt()`: A numeric vector, one element per column of `x`.
#'
#' @references
#' Vehtari, A., Simpson, D., Gelman, A., Yao, Y. and Gabry, J. (2024).
#' Pareto smoothed importance sampling. *Journal of Machine Learning
#' Research* 25(72), 1-58.
#'
#' Zhang, J. and Stephens, M. A. (2009). A new and efficient
#' estimation method for the generalized Pareto distribution.
#' *Technometrics* 51(3), 316-325.
#'
#' @examples
#' set.seed(0)
#' x <- .rmvt(n = 1000, mean = 0, scale = matrix(1), df = 5)
#' log_w <- dnorm(x[1, ], log = TRUE) - .dmvt(x, mean = 0, scale = matrix(1), df = 5)
#' ans <- .psis(log_w)
#' ans$k_hat
#' i <- .resample_systematic(exp(ans$log_weight), n = 100)
#' @name importance
NULL


## HAS_TESTS
#' @rdname importance
#' @export
.psis <- function(log_w) {
  if (!is.numeric(log_w) || length(log_w) == 0L)
    cli::cli_abort("{.arg log_w} must be a non-empty numeric vector.")
  if (anyNA(log_w) || any(log_w == Inf))
    cli::cli_abort("{.arg log_w} must not contain {.val {NA}}, {.val {NaN}}, or {.val {Inf}}.")
  is_pos <- is.finite(log_w)
  if (!any(is_pos))
    cli::cli_abort("All weights in {.arg log_w} are zero.")
  lw <- log_w[is_pos]
  lw_max <- max(lw)
  lw <- lw - lw_max
  S <- length(lw)
  tail_len <- ceiling(min(0.2 * S, 3 * sqrt(S)))
  k_hat <- Inf
  if (tail_len >= 5L) {
    ord <- order(lw)
    tail_ids <- seq.int(S - tail_len + 1L, S)
    lw_tail <- lw[ord[tail_ids]]
    if (abs(max(lw_tail) - min(lw_tail)) >= .Machine$double.eps / 100) {
      cutoff <- lw[ord[min(tail_ids) - 1L]]
      smoothed <- psis_smooth_tail(lw_tail, cutoff = cutoff)
      k_hat <- smoothed$k
      lw[ord[tail_ids]] <- smoothed$tail
    }
  }
  lw[lw > 0] <- 0
  lw <- lw - log_sum_exp(lw)
  ans <- rep(-Inf, length(log_w))
  ans[is_pos] <- lw
  list(log_weight = ans,
       k_hat = k_hat,
       ess = 1 / sum(exp(2 * lw)))
}


## HAS_TESTS
#' @rdname importance
#' @export
.resample_systematic <- function(weight, n) {
  check_n(n = n, nm_n = "n", min = 1L, max = NULL, divisible_by = NULL)
  if (!is.numeric(weight) || length(weight) == 0L || anyNA(weight) ||
        any(weight < 0) || any(!is.finite(weight)) || sum(weight) <= 0)
    cli::cli_abort("{.arg weight} must be non-negative finite numbers, not all zero.")
  n <- as.integer(n)
  cum <- cumsum(weight) / sum(weight)
  cum[length(cum)] <- 1
  points <- (stats::runif(1L) + seq.int(0L, n - 1L)) / n
  findInterval(points, cum, left.open = TRUE) + 1L
}


## HAS_TESTS
#' @rdname importance
#' @export
.rmvt <- function(n, mean, scale, df) {
  check_n(n = n, nm_n = "n", min = 1L, max = NULL, divisible_by = NULL)
  check_mvt_args(mean = mean, scale = scale, df = df)
  n <- as.integer(n)
  p <- length(mean)
  R <- chol(scale)
  z <- matrix(stats::rnorm(p * n), nrow = p)
  w <- stats::rchisq(n, df = df) / df
  ans <- crossprod(R, z)
  ans <- sweep(ans, 2L, sqrt(w), "/") + mean
  dimnames(ans) <- NULL
  ans
}


## HAS_TESTS
#' @rdname importance
#' @export
.dmvt <- function(x, mean, scale, df, log = TRUE) {
  check_mvt_args(mean = mean, scale = scale, df = df)
  if (!is.matrix(x) || nrow(x) != length(mean))
    cli::cli_abort("{.arg x} must be a matrix with one row per element of {.arg mean}.")
  p <- length(mean)
  R <- chol(scale)
  z <- backsolve(R, x - mean, transpose = TRUE)
  q <- colSums(z^2)
  ans <- lgamma((df + p) / 2) - lgamma(df / 2) - (p / 2) * log(df * pi) -
    sum(log(diag(R))) - ((df + p) / 2) * log1p(q / df)
  if (log) ans else exp(ans)
}


## Helpers ----------------------------------------------------------------------

#' Pareto-smooth the tail of log weights (as loo's psis_smooth_tail)
#'
#' @param x Log weights in the tail, in increasing order, stabilised
#' (maximum log weight 0)
#' @param cutoff Largest log weight below the tail
#'
#' @returns List with `tail` (smoothed log weights) and `k`
#'
#' @noRd
psis_smooth_tail <- function(x, cutoff) {
  len <- length(x)
  exp_cutoff <- exp(cutoff)
  fit <- gpd_fit(exp(x) - exp_cutoff)
  k <- fit$k
  if (is.na(k))
    k <- Inf
  if (is.finite(k)) {
    p <- (seq_len(len) - 0.5) / len
    sigma <- fit$sigma
    q <- if (k == 0) -sigma * log1p(-p) else sigma * expm1(-k * log1p(-p)) / k
    tail <- log(q + exp_cutoff)
  }
  else
    tail <- x
  list(tail = tail, k = k)
}


## HAS_TESTS
#' Fit a generalised Pareto distribution (Zhang and Stephens, with a
#' weakly informative prior on k), as posterior::gpdfit()
#'
#' @param x Exceedances, in increasing order
#'
#' @returns List with `k` and `sigma`
#'
#' @noRd
gpd_fit <- function(x) {
  N <- length(x)
  prior <- 3
  M <- 30 + floor(sqrt(N))
  jj <- seq_len(M)
  xstar <- x[floor(N / 4 + 0.5)]
  if (!(xstar > x[1L]))
    return(list(k = NA_real_, sigma = NA_real_))
  theta <- 1 / x[N] + (1 - sqrt(M / (jj - 0.5))) / prior / xstar
  k <- rowMeans(log1p(-theta %o% x))
  l_theta <- N * (log(-theta / k) - k - 1)
  w_theta <- exp(l_theta - log_sum_exp(l_theta))
  theta_hat <- sum(theta * w_theta)
  k_hat <- mean(log1p(-theta_hat * x))
  sigma_hat <- -k_hat / theta_hat
  k_hat <- (k_hat * N + 0.5 * 10) / (N + 10)
  if (is.na(k_hat))
    return(list(k = Inf, sigma = NaN))
  list(k = k_hat, sigma = sigma_hat)
}


#' Log of the sum of exponentials, stably
#'
#' @param x Numeric vector
#'
#' @returns A number
#'
#' @noRd
log_sum_exp <- function(x) {
  m <- max(x)
  m + log(sum(exp(x - m)))
}


#' Check arguments for the multivariate t
#'
#' @param mean,scale,df As for .rmvt()
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_mvt_args <- function(mean, scale, df) {
  if (!is.numeric(mean) || length(mean) == 0L || anyNA(mean))
    cli::cli_abort("{.arg mean} must be a non-empty numeric vector with no NAs.")
  if (!is.matrix(scale) || !is.numeric(scale) || any(dim(scale) != length(mean)))
    cli::cli_abort("{.arg scale} must be a square numeric matrix with one row per element of {.arg mean}.")
  if (!is.numeric(df) || length(df) != 1L || is.na(df) || df <= 0)
    cli::cli_abort("{.arg df} must be a single positive number.")
  invisible(TRUE)
}
