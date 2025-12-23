
test_that("intrinsic_growth_rate_cpp11 solves Lotka equation (basic case)", {
  # A small, well-behaved fertility schedule
  age_mid <- c(17.5, 22.5, 27.5, 32.5, 37.5)
  Lx      <- c(4.2, 4, 3.4, 3.2, 3)
  mx      <- c(0.02, 0.10, 0.12, 0.08, 0.02)     
  r <- intrinsic_growth_rate_cpp11(mx, Lx, age_mid,
                                   max_iter = 100L,
                                   tol = 1e-12,
                                   deriv_tol = 1e-14)
  expect_true(is.finite(r))
  # check the defining equation: sum(exp(-r a) mx Lx) == 1
  f <- sum(exp(-r * age_mid) * mx * Lx) - 1
  expect_lt(abs(f), 1e-10)
})

test_that("intrinsic_growth_rate_cpp11 returns near zero when NRR is 1", {
  age_mid <- c(20, 30, 40)
  Lx      <- c(1.0, 1.0, 1.0)
  # Choose mx so that nrr = sum(mx*Lx) = 1 exactly
  mx <- c(0.2, 0.3, 0.5)
  r <- intrinsic_growth_rate_cpp11(mx, Lx, age_mid,
                                   max_iter = 100L,
                                   tol = 1e-12,
                                   deriv_tol = 1e-14)
  expect_true(is.finite(r))
  # r should be very close to 0 because f(0) = nrr - 1 = 0 and fp != 0
  expect_lt(abs(r), 1e-10)
})

test_that("intrinsic_growth_rate_cpp11 gives negative r when NRR < 1", {
  age_mid <- c(20, 30, 40)
  Lx      <- c(1.0, 1.0, 1.0)
  mx      <- c(0.1, 0.2, 0.3)  # nrr = 0.6 < 1
  r <- intrinsic_growth_rate_cpp11(mx, Lx, age_mid,
                                   max_iter = 100L,
                                   tol = 1e-12,
                                   deriv_tol = 1e-14)
  expect_true(is.finite(r))
  expect_lt(r, 0)
  f <- sum(exp(-r * age_mid) * mx * Lx) - 1
  expect_lt(abs(f), 1e-10)
})

test_that("intrinsic_growth_rate_cpp11 gives positive r when NRR > 1", {
  age_mid <- c(20, 30, 40)
  Lx      <- c(1.0, 1.0, 1.0)
  mx      <- c(0.4, 0.4, 0.4)  # nrr = 1.2 > 1

  r <- intrinsic_growth_rate_cpp11(mx, Lx, age_mid,
                                   max_iter = 100L,
                                   tol = 1e-12,
                                   deriv_tol = 1e-14)
  expect_true(is.finite(r))
  expect_gt(r, 0)
  f <- sum(exp(-r * age_mid) * mx * Lx) - 1
  expect_lt(abs(f), 1e-10)
})

test_that("intrinsic_growth_rate_cpp11 errors when nrr is non-positive", {
  age_mid <- c(20, 30, 40)
  Lx      <- c(1.0, 1.0, 1.0)
  mx0 <- c(0, 0, 0)
  expect_error(
    intrinsic_growth_rate_cpp11(mx0, Lx, age_mid, 50L, 1e-10, 1e-14),
    "nrr = sum\\(mx \\* Lx\\) must be positive and finite\\."
  )
  mx_neg <- c(-0.1, 0.0, 0.0)
  expect_error(
    intrinsic_growth_rate_cpp11(mx_neg, Lx, age_mid, 50L, 1e-10, 1e-14),
    "nrr = sum\\(mx \\* Lx\\) must be positive and finite\\."
  )
})

test_that("intrinsic_growth_rate_cpp11 errors when mean age is non-positive", {
  # nrr positive but mean_age <= 0
  age_mid <- c(0, 0, 0)
  Lx      <- c(1.0, 1.0, 1.0)
  mx      <- c(0.2, 0.3, 0.5)  # nrr = 1
  expect_error(
    intrinsic_growth_rate_cpp11(mx, Lx, age_mid, 50L, 1e-10, 1e-14),
    "mean age of reproduction must be positive and finite\\."
  )
})

test_that("intrinsic_growth_rate_cpp11 errors when deriv_tol is set too large", {
  # For typical schedules, fp is on the order of -mean_age (at r near 0),
  # so setting deriv_tol huge should force the derivative-too-small error.
  age_mid <- c(20, 30, 40)
  Lx      <- c(1.0, 1.0, 1.0)
  mx      <- c(0.2, 0.3, 0.5)  # nrr = 1 => r ~ 0, fp ~ -mean_age

  expect_error(
    intrinsic_growth_rate_cpp11(mx, Lx, age_mid,
                                max_iter = 50L,
                                tol = 1e-12,
                                deriv_tol = 1e6),
    "failed to converge: derivative too small / non-finite\\."
  )
})

test_that("intrinsic_growth_rate_cpp11 errors when max_iter is too small to converge", {
  age_mid <- c(17.5, 22.5, 27.5, 32.5, 37.5)
  Lx      <- c(0.98, 0.97, 0.96, 0.94, 0.90)
  mx      <- c(0.02, 0.10, 0.12, 0.08, 0.02)
  expect_error(
    intrinsic_growth_rate_cpp11(mx, Lx, age_mid,
                                max_iter = 1L,
                                tol = 1e-16,
                                deriv_tol = 1e-14),
    "failed to converge: max_iter reached\\."
  )
})
