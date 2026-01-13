
test_that(".intrinsic_growth_rate works with valid inputs - mx, Lx numeric", {
  age_mid <- c(17.5, 22.5, 27.5, 32.5, 37.5)
  Lx      <- c(4.2, 4, 3.4, 3.2, 3)
  mx      <- c(0.02, 0.10, 0.12, 0.08, 0.02)     
  r <- .intrinsic_growth_rate(mx, Lx, age_mid)
  expect_true(is.finite(r))
  f <- sum(exp(-r * age_mid) * mx * Lx) - 1
  expect_lt(abs(f), 1e-10)
})

test_that(".intrinsic_growth_rate works with valid inputs - mx rvec, Lx numeric", {
  age_mid <- c(17.5, 22.5, 27.5, 32.5, 37.5)
  Lx      <- c(4.2, 4, 3.4, 3.2, 3)
  mx      <- rvec::rvec(matrix(c(0.02, 0.10, 0.12, 0.08, 0.02,
                                 0.01, 0.05, 0.33, 0.02, 0.05),
                               ncol = 2))
  r <- .intrinsic_growth_rate(mx, Lx, age_mid)
  expect_true(rvec::is_rvec(r))
  f <- sum(exp(-r * age_mid) * mx * Lx) - 1
  expect_true(rvec::draws_all(abs(f) < 1e-10))
})

test_that(".intrinsic_growth_rate works with valid inputs - mx numeric, Lx rvec", {
  age_mid <- c(17.5, 22.5, 27.5, 32.5, 37.5)
  Lx      <- rvec::rvec(matrix(c(4.2, 4, 3.4, 3.2, 3,
                                 3.1, 3, 2.9, 2.2, 1.3),
                               ncol = 2))
  mx      <- c(0.02, 0.10, 0.12, 0.08, 0.02)
  r <- .intrinsic_growth_rate(mx, Lx, age_mid)
  expect_true(rvec::is_rvec(r))
  f <- sum(exp(-r * age_mid) * mx * Lx) - 1
  expect_true(rvec::draws_all(abs(f) < 1e-10))
})

test_that(".intrinsic_growth_rate works with valid inputs - mx rvec, Lx rvec", {
  age_mid <- c(17.5, 22.5, 27.5, 32.5, 37.5)
  Lx      <- rvec::rvec(matrix(c(4.2, 4, 3.4, 3.2, 3,
                                 3.1, 3, 2.9, 2.2, 1.3),
                               ncol = 2))
  mx      <- rvec::rvec(matrix(c(0.02, 0.10, 0.12, 0.08, 0.02,
                                 0.01, 0.05, 0.33, 0.02, 0.05),
                               ncol = 2))
  r <- .intrinsic_growth_rate(mx, Lx, age_mid)
  expect_true(rvec::is_rvec(r))
  f <- sum(exp(-r * age_mid) * mx * Lx) - 1
  expect_true(rvec::draws_all(abs(f) < 1e-10))
})

test_that(".intrinsic_growth_rate works with valid inputs - mx rvec 1, Lx rvec", {
  age_mid <- c(17.5, 22.5, 27.5, 32.5, 37.5)
  Lx      <- rvec::rvec(matrix(c(4.2, 4, 3.4, 3.2, 3,
                                 3.1, 3, 2.9, 2.2, 1.3),
                               ncol = 2))
  mx      <- rvec::rvec(matrix(c(0.02, 0.10, 0.12, 0.08, 0.02),
                               ncol = 1))
  r <- .intrinsic_growth_rate(mx, Lx, age_mid)
  expect_true(rvec::is_rvec(r))
  f <- sum(exp(-r * age_mid) * mx * Lx) - 1
  expect_true(rvec::draws_all(abs(f) < 1e-10))
})

test_that(".intrinsic_growth_rate works with valid inputs - mx rvec, Lx rvec 1", {
  age_mid <- c(17.5, 22.5, 27.5, 32.5, 37.5)
  Lx      <- rvec::rvec(matrix(c(4.2, 4, 3.4, 3.2, 3),
                               ncol = 1))
  mx      <- rvec::rvec(matrix(c(0.02, 0.10, 0.12, 0.08, 0.02,
                                 0.01, 0.05, 0.33, 0.02, 0.05),
                               ncol = 2))
  r <- .intrinsic_growth_rate(mx, Lx, age_mid)
  expect_true(rvec::is_rvec(r))
  f <- sum(exp(-r * age_mid) * mx * Lx) - 1
  expect_true(rvec::draws_all(abs(f) < 1e-10))
})

test_that(".intrinsic_growth_rate throws correct error with non-monitonic age", {
  age_mid <- c(17.5, 22.5, 27.5, 32.5, 30.5)
  Lx      <- c(4.2, 4, 3.4, 3.2, 3)
  mx      <- c(0.02, 0.10, 0.12, 0.08, 0.02)     
  expect_error(.intrinsic_growth_rate(mx, Lx, age_mid),
               "`age_mid` not monotonically increasing.")
})

test_that(".intrinsic_growth_rate throws correct error when rvecs have different n_draw", {
  age_mid <- c(17.5, 22.5, 27.5, 32.5, 37.5)
  Lx      <- rvec::rvec(matrix(c(4.2, 4, 3.4, 3.2, 3,
                                 3.1, 3, 2.9, 2.2, 1.3,
                                 4, 3, 2.6, 2.5, 2),
                               ncol = 3))
  mx      <- rvec::rvec(matrix(c(0.02, 0.10, 0.12, 0.08, 0.02,
                                 0.01, 0.05, 0.33, 0.02, 0.05),
                               ncol = 2))
  expect_error(.intrinsic_growth_rate(mx, Lx, age_mid),
               "`mx` and `Lx` have different numbers of draws.")
})



