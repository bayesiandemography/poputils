test_that("intrinsic_growth_rate_cpp11 solves Lotka equation (basic case)", {
  age_mid <- c(17.5, 22.5, 27.5, 32.5, 37.5)
  Lx      <- c(4.2, 4, 3.4, 3.2, 3)
  mx      <- c(0.02, 0.10, 0.12, 0.08, 0.02)
  r <- intrinsic_growth_rate_cpp11(mx, Lx, age_mid, n_iter = 50L)
  expect_true(is.finite(r))
  f <- sum(exp(-r * age_mid) * mx * Lx) - 1
  expect_lt(abs(f), 1e-12)
})

test_that("intrinsic_growth_rate_cpp11 returns near zero when NRR is 1", {
  age_mid <- c(20, 30, 40)
  Lx      <- c(1.0, 1.0, 1.0)
  mx <- c(0.2, 0.3, 0.5)
  r <- intrinsic_growth_rate_cpp11(mx, Lx, age_mid, n_iter = 50L)
  expect_lt(abs(r), 1e-12)
})

test_that("intrinsic_growth_rate_cpp11 gives negative r when NRR < 1", {
  age_mid <- c(20, 30, 40)
  Lx      <- c(1.0, 1.0, 1.0)
  mx      <- c(0.1, 0.2, 0.3)
  r <- intrinsic_growth_rate_cpp11(mx, Lx, age_mid, n_iter = 50L)
  expect_lt(r, 0)
  f <- sum(exp(-r * age_mid) * mx * Lx) - 1
  expect_lt(abs(f), 1e-12)
})

test_that("intrinsic_growth_rate_cpp11 gives positive r when NRR > 1", {
  age_mid <- c(20, 30, 40)
  Lx      <- c(1.0, 1.0, 1.0)
  mx      <- c(0.4, 0.4, 0.4)
  r <- intrinsic_growth_rate_cpp11(mx, Lx, age_mid, n_iter = 50L)
  expect_gt(r, 0)
  f <- sum(exp(-r * age_mid) * mx * Lx) - 1
  expect_lt(abs(f), 1e-12)
})

test_that("intrinsic_growth_rate_cpp11 converges in 10 steps for a wide range of schedules", {
  age_mid <- seq(17.5, 47.5, by = 5)
  Lx <- c(4.8, 4.7, 4.6, 4.5, 4.4, 4.3, 4.2)
  shape <- c(0.05, 0.2, 0.25, 0.22, 0.15, 0.1, 0.03)
  for (tfr in c(0.8, 1.5, 2.1, 5, 8)) {
    mx <- tfr * shape / 5 / 2.05
    r <- intrinsic_growth_rate_cpp11(mx, Lx, age_mid, n_iter = 10L)
    f <- sum(exp(-r * age_mid) * mx * Lx) - 1
    expect_lt(abs(f), 1e-12)
  }
})

test_that("intrinsic_growth_rate_cpp11 errors when nrr is non-positive", {
  age_mid <- c(20, 30, 40)
  Lx      <- c(1.0, 1.0, 1.0)
  expect_error(intrinsic_growth_rate_cpp11(c(0, 0, 0), Lx, age_mid, 50L),
               "nrr = sum\\(mx \\* Lx\\) must be positive and finite\\.")
  expect_error(intrinsic_growth_rate_cpp11(c(-0.1, 0, 0), Lx, age_mid, 50L),
               "nrr = sum\\(mx \\* Lx\\) must be positive and finite\\.")
})

test_that("intrinsic_growth_rate_cpp11 errors when mean age is non-positive", {
  expect_error(intrinsic_growth_rate_cpp11(c(0.2, 0.3, 0.5), c(1, 1, 1), c(0, 0, 0), 50L),
               "mean age of reproduction must be positive and finite\\.")
})

test_that("intrinsic_growth_rate_cpp11 errors when too few steps to converge", {
  age_mid <- c(17.5, 22.5, 27.5, 32.5, 37.5)
  Lx      <- c(0.98, 0.97, 0.96, 0.94, 0.90)
  mx      <- c(0.02, 0.10, 0.12, 0.08, 0.02)
  expect_error(intrinsic_growth_rate_cpp11(mx, Lx, age_mid, n_iter = 0L),
               "failed to converge\\.")
})
