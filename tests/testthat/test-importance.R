## '.psis' -----------------------------------------------------------------------

test_that("'.psis' matches loo::psis()", {
  skip_if_not_installed("loo")
  set.seed(0)
  cases <- list(stats::rnorm(1000),
                stats::rnorm(4000, sd = 3),
                stats::rt(2000, df = 2),
                log(stats::rexp(500)))
  for (log_w in cases) {
    ans <- .psis(log_w)
    ref <- suppressWarnings(loo::psis(log_w, r_eff = 1))
    expect_equal(ans$k_hat, unname(loo::pareto_k_values(ref)))
    lw_ref <- as.numeric(weights(ref, log = TRUE, normalize = TRUE))
    expect_equal(ans$log_weight, lw_ref)
  }
})

test_that("'.psis' gives normalised weights, handles zero weights, and reports ESS", {
  set.seed(1)
  log_w <- c(stats::rnorm(999, sd = 0.3), -Inf)
  ans <- .psis(log_w)
  expect_equal(sum(exp(ans$log_weight)), 1)
  expect_identical(ans$log_weight[[1000]], -Inf)
  expect_true(ans$ess > 500 && ans$ess <= 999)
  expect_lt(.psis(rep(0, 100))$ess, 100 + 1e-8)
})

test_that("'.psis' throws expected errors", {
  expect_error(.psis(numeric()), "non-empty numeric")
  expect_error(.psis(c(1, NA)), "must not contain")
  expect_error(.psis(c(1, Inf)), "must not contain")
  expect_error(.psis(c(-Inf, -Inf)), "All weights")
})


## '.resample_systematic' ---------------------------------------------------------

test_that("'.resample_systematic' selects each element floor or ceiling of n w times", {
  set.seed(2)
  w <- stats::rexp(50)
  w <- w / sum(w)
  for (n in c(10, 100, 1000)) {
    i <- .resample_systematic(w, n = n)
    expect_length(i, n)
    expect_false(is.unsorted(i))
    counts <- tabulate(i, nbins = 50)
    expect_true(all(counts >= floor(n * w) & counts <= ceiling(n * w)))
  }
  expect_identical(.resample_systematic(c(0, 1, 0), n = 5), rep(2L, 5))
})

test_that("'.resample_systematic' is unbiased", {
  set.seed(3)
  w <- c(0.1, 0.25, 0.65)
  counts <- rowMeans(replicate(5000, tabulate(.resample_systematic(w, n = 7), nbins = 3)))
  expect_equal(counts, 7 * w, tolerance = 0.02)
})

test_that("'.resample_systematic' throws expected errors", {
  expect_error(.resample_systematic(c(0, 0), n = 2), "not all zero")
  expect_error(.resample_systematic(c(-1, 2), n = 2), "non-negative")
})


## '.rmvt', '.dmvt' --------------------------------------------------------------

test_that("'.rmvt' has the right mean and covariance", {
  set.seed(4)
  S <- matrix(c(2, 0.5, 0.5, 1), 2)
  x <- .rmvt(n = 100000, mean = c(1, -1), scale = S, df = 6)
  expect_equal(rowMeans(x), c(1, -1), tolerance = 0.02)
  expect_equal(stats::cov(t(x)), S * 6 / 4, tolerance = 0.03)
})

test_that("'.dmvt' matches the univariate t, and integrates to 1 in two dimensions", {
  x <- matrix(c(-2, 0, 1.5), nrow = 1)
  expect_equal(.dmvt(x, mean = 0.5, scale = matrix(4), df = 3),
               stats::dt((x[1, ] - 0.5) / 2, df = 3, log = TRUE) - log(2))
  S <- matrix(c(1, 0.3, 0.3, 2), 2)
  g <- as.matrix(expand.grid(seq(-30, 30, 0.1), seq(-30, 30, 0.1)))
  dens <- .dmvt(t(g), mean = c(0, 0), scale = S, df = 4, log = FALSE)
  expect_equal(sum(dens) * 0.01, 1, tolerance = 0.01)
})


## '.tmb_outer_is', '.draws_outer_is' ---------------------------------------------

test_that("'.tmb_outer_is' and '.draws_outer_is' work on a TMB model", {
  f <- make_tmb_test_f()
  fit <- .fit_tmb(f)
  set.seed(5)
  is <- .tmb_outer_is(f, est = fit$est, is_free = fit$is_free, prec = fit$prec, n = 4000)
  expect_identical(dim(is$outer), c(3L, 4000L))
  expect_identical(sum(!is$is_random), 3L)
  expect_lt(is$k_hat, 0.7)
  expect_equal(sum(exp(is$log_weight)), 1)
  draws <- .draws_outer_is(est = fit$est, is_free = fit$is_free, is_random = is$is_random,
                           prec = fit$prec, outer = is$outer, log_weight = is$log_weight, n = 4000)
  expect_identical(dim(draws), c(length(unlist(fit$est)), 4000L))
  ## close to the Gaussian approximation for this model
  gauss <- .draws_free_fixed(est = fit$est, is_free = fit$is_free, chol = fit$chol, n = 4000)
  expect_equal(rowMeans(draws), rowMeans(gauss), tolerance = 0.05)
})

test_that("'.draws_outer_is' gives the Gaussian conditional of the inner parameters", {
  ## with equal weights and proposals equal to the mode, inner draws are N(u_hat, Q_uu^-1)
  f <- make_tmb_test_f()
  fit <- .fit_tmb(f)
  is_random <- seq_along(unlist(fit$est)[fit$is_free]) %in% f$env$random
  theta_hat <- unlist(fit$est)[fit$is_free][!is_random]
  outer <- matrix(theta_hat, nrow = length(theta_hat), ncol = 10)
  set.seed(6)
  draws <- .draws_outer_is(est = fit$est, is_free = fit$is_free, is_random = is_random,
                           prec = fit$prec, outer = outer, log_weight = rep(log(0.1), 10), n = 20000)
  u <- draws[fit$is_free, ][is_random, ]
  Q_uu <- as.matrix(fit$prec[is_random, is_random])
  expect_equal(unname(rowMeans(u)), unname(unlist(fit$est)[fit$is_free][is_random]), tolerance = 0.02)
  expect_equal(unname(stats::cov(t(u))), unname(solve(Q_uu)), tolerance = 0.05)
})
