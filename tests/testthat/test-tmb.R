test_that(".tmb_is_free() identifies free parameters", {
  est <- list(a = 1, b = c(2, 3, 4), c = 5)
  expect_identical(.tmb_is_free(est), rep(TRUE, 5))
  expect_identical(.tmb_is_free(est, map = list()), rep(TRUE, 5))
  map <- list(a = factor(NA), b = factor(c(1, NA, 2)))
  expect_identical(.tmb_is_free(est, map), c(FALSE, TRUE, FALSE, TRUE, TRUE))
  expect_identical(.tmb_is_free(est, map), !bage_make_is_fixed(est, map) |> unname())
})

test_that(".tmb_is_free() checks arguments", {
  est <- list(a = 1, b = c(2, 3))
  expect_error(.tmb_is_free(list(1)), "named list")
  expect_error(.tmb_is_free(est, map = 1), "named list")
  expect_error(.tmb_is_free(est, map = list(z = factor(NA))), "not found")
  expect_error(.tmb_is_free(est, map = list(b = factor(NA))), "same length")
  expect_error(.tmb_is_free(est, map = list(b = c(NA, 1))), "factor")
  expect_error(.tmb_is_free(est, map = list(b = factor(c(1, 1)))), "share values")
})

test_that(".tmb_est_prec() is exact for a Gaussian model", {
  map <- list(log_sd_u = factor(NA), log_sd_y = factor(NA))
  f <- make_tmb_test_f(map = map)
  ignore <- .optimize(f$par, fn = f$fn, gr = f$gr)
  ans <- .tmb_est_prec(f)
  ## exact posterior of (mu, u): negative log density is quadratic
  data <- tmb_test_data()
  n_i <- tabulate(data$group + 1L, nbins = 4)
  sum_i <- as.double(tapply(data$y, data$group, sum))
  a <- 1 / 0.8^2
  b <- 1 / 0.5^2
  H <- matrix(0, 5, 5)
  H[1, 1] <- 4 * a
  H[1, -1] <- H[-1, 1] <- -a
  diag(H)[-1] <- n_i * b + a
  rhs <- c(0, b * sum_i)
  mean <- solve(H, rhs)
  expect_equal(c(ans$est$mu, ans$est$u), mean, tolerance = 1e-6)
  expect_equal(as.matrix(ans$prec), H, ignore_attr = TRUE, tolerance = 1e-6)
  expect_identical(ans$est$log_sd_u, log(0.8))
})

test_that(".tmb_est_prec() works without random effects", {
  f <- make_tmb_test_f(random = NULL)
  ignore <- .optimize(f$par, fn = f$fn, gr = f$gr)
  ans <- .tmb_est_prec(f)
  expect_identical(dim(ans$prec), c(7L, 7L))
  expect_equal(ans$prec %*% TMB::sdreport(f)$cov.fixed, diag(7), ignore_attr = TRUE, tolerance = 1e-6)
})

test_that("check_est_prec() gives an error if an estimate or variance is NA", {
  est <- list(mu = 1, u = c(1, NA))
  expect_error(check_est_prec(est, diag(3)), "Estimate of \"u\" is")
  est <- list(mu = 1, u = c(1, 2))
  prec <- diag(c(1, NA, 1))
  rownames(prec) <- c("mu", "u", "u")
  expect_error(check_est_prec(est, prec), "Variance of \"u\" is")
  expect_true(check_est_prec(est, diag(3)))
})

test_that(".fit_tmb() returns estimates, free parameters, factor and optimizer details", {
  map <- list(log_sd_y = factor(NA))
  f <- make_tmb_test_f(map = map)
  ans <- .fit_tmb(f, map = map)
  expect_identical(names(ans), c("est", "is_free", "chol", "prec", "optimizer", "times"))
  expect_identical(ans$is_free, c(TRUE, TRUE, FALSE, rep(TRUE, 4)))
  expect_s4_class(ans$chol, "CHMfactor")
  expect_true(ans$optimizer$converged)
  expect_identical(names(ans$times), c("optimize", "sdreport", "chol"))
  set.seed(0)
  draws <- .draws_free_fixed(ans$est, is_free = ans$is_free, chol = ans$chol, n = 50000)
  expect_equal(rowMeans(draws), unlist(ans$est), tolerance = 0.02)
  expect_identical(unname(draws[3, 1:3]), rep(log(0.5), 3))
})

test_that(".fit_tmb() warns when the optimizer does not converge", {
  f <- make_tmb_test_f()
  local_mocked_bindings(.optimize = function(par, fn, gr, ...) {
    out <- stats::nlminb(par, objective = fn, gradient = gr)
    list(par = out$par, value = out$objective, converged = FALSE, iter = 1L,
         message = "stopped", optimizer = "nlminb")
  })
  expect_warning(.fit_tmb(f), "did not converge")
})

test_that("restarting BFGS on the same TMB object reaches the same optimum", {
  ## review point: bage rebuilds the TMB object before BFGS
  f1 <- make_tmb_test_f()
  full <- .optimize(f1$par, fn = f1$fn, gr = f1$gr, optimizer = "nlminb")
  f2 <- make_tmb_test_f()
  restart <- optimize_multi(f2$par, fn = f2$fn, gr = f2$gr, quiet = TRUE, iter_max_nlminb = 1L)
  expect_identical(restart$optimizer, "nlminb + BFGS")
  expect_equal(restart$par, full$par, tolerance = 1e-4)
  expect_equal(.tmb_est_prec(f2)$est, .tmb_est_prec(f1)$est, tolerance = 1e-4)
})

test_that(".fit_tmb() and .tmb_est_prec() check f", {
  expect_error(.fit_tmb(list()), "created by")
  expect_error(.tmb_est_prec(list()), "created by")
})
