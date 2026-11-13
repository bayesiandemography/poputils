## Reference: mean by numerical integration
logit_normal_mean_ref <- function(mu, sigma) {
  if (sigma == 0)
    return(stats::plogis(mu))
  f <- function(z) stats::plogis(mu + sigma * z) * stats::dnorm(z)
  stats::integrate(f, -Inf, Inf, rel.tol = 1e-12)$value
}


## '.logit_normal_mean' -------------------------------------------------------

test_that("'.logit_normal_mean' matches numerical integration", {
  grid <- expand.grid(mu = c(-12, -6, -2, 0, 1.5, 4, 9), sigma = c(0, 0.1, 0.5, 1, 1.5, 2))
  ans <- .logit_normal_mean(mu = grid$mu, sigma = grid$sigma)
  ref <- mapply(logit_normal_mean_ref, grid$mu, grid$sigma)
  expect_equal(ans, ref, tolerance = 1e-7)
})

test_that("'.logit_normal_mean' is accurate to 1e-3 for sigma up to 4", {
  grid <- expand.grid(mu = c(-8, -3, 0, 3, 8), sigma = c(3, 4))
  ans <- .logit_normal_mean(mu = grid$mu, sigma = grid$sigma)
  ref <- mapply(logit_normal_mean_ref, grid$mu, grid$sigma)
  expect_equal(ans, ref, tolerance = 1e-3)
})

test_that("'.logit_normal_mean' is symmetric, and 0.5 at mu = 0", {
  mu <- c(-3, -0.5, 2)
  sigma <- c(0.3, 1, 2)
  expect_equal(.logit_normal_mean(mu, sigma) + .logit_normal_mean(-mu, sigma), rep(1, 3))
  expect_equal(.logit_normal_mean(0, sigma), rep(0.5, 3))
})

test_that("'.logit_normal_mean' recycles arguments", {
  expect_equal(.logit_normal_mean(mu = 1, sigma = c(0, 1)),
               c(stats::plogis(1), .logit_normal_mean(1, 1)))
  expect_equal(.logit_normal_mean(mu = c(-1, 1), sigma = 0), stats::plogis(c(-1, 1)))
  expect_identical(.logit_normal_mean(mu = numeric(), sigma = numeric()), double())
})

test_that("'.logit_normal_mean' throws expected errors", {
  expect_error(.logit_normal_mean(mu = "a", sigma = 1), "`mu` is non-numeric")
  expect_error(.logit_normal_mean(mu = NA_real_, sigma = 1), "`mu` has NA")
  expect_error(.logit_normal_mean(mu = Inf, sigma = 1), "`mu` has non-finite")
  expect_error(.logit_normal_mean(mu = 1, sigma = -1), "`sigma` has negative")
  expect_error(.logit_normal_mean(mu = 1:3, sigma = 1:2), "Lengths of `mu` and `sigma` not compatible")
  expect_error(.logit_normal_mean(mu = rvec::rvec(matrix(1)), sigma = 1), "`mu` is an rvec")
})


## '.logit_normal_solve_mu' ---------------------------------------------------

test_that("'.logit_normal_solve_mu' inverts '.logit_normal_mean'", {
  grid <- expand.grid(mean = c(1e-6, 0.001, 0.05, 0.3, 0.5, 0.9, 0.999, 1 - 1e-6),
                      sigma = c(0, 0.2, 1, 2, 4))
  mu <- .logit_normal_solve_mu(mean = grid$mean, sigma = grid$sigma)
  expect_equal(.logit_normal_mean(mu = mu, sigma = grid$sigma), grid$mean, tolerance = 1e-10)
})

test_that("'.logit_normal_solve_mu' gives qlogis when sigma is 0", {
  mean <- c(0.01, 0.5, 0.8)
  expect_equal(.logit_normal_solve_mu(mean = mean, sigma = 0), stats::qlogis(mean))
})

test_that("'.logit_normal_solve_mu' throws expected errors", {
  expect_error(.logit_normal_solve_mu(mean = 0, sigma = 1), "not strictly between 0 and 1")
  expect_error(.logit_normal_solve_mu(mean = c(0.5, 1), sigma = 1), "1 value not strictly between")
  expect_error(.logit_normal_solve_mu(mean = 0.5, sigma = -1), "`sigma` has negative")
  expect_error(.logit_normal_solve_mu(mean = c(0.1, 0.2), sigma = c(1, 2, 3)), "not compatible")
})


## Compiled code: header with TMB ---------------------------------------------

test_that("'logit_normal.hpp' works with TMB's automatic differentiation", {
  dll <- tmb_test_dll("logitnorm")
  f <- TMB::MakeADFun(data = list(), parameters = list(mean = 0.1, sigma = 0.8),
                      DLL = dll, silent = TRUE)
  x <- c(0.1, 0.8)
  rep <- f$report(x)
  expect_equal(rep$mu, .logit_normal_solve_mu(mean = 0.1, sigma = 0.8))
  expect_equal(rep$mean_check, .logit_normal_mean(mu = rep$mu, sigma = 0.8))
  ## objective = mu; gradient against finite differences
  h <- 1e-6
  fd <- sapply(1:2, function(i) {
    e <- replace(c(0, 0), i, h)
    (f$fn(x + e) - f$fn(x - e)) / (2 * h)
  })
  expect_equal(as.numeric(f$gr(x)), fd, tolerance = 1e-6)
})
