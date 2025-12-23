
test_that(".intrinsic_growth_rate works with valid inputs", {
  age_mid <- c(17.5, 22.5, 27.5, 32.5, 37.5)
  Lx      <- c(4.2, 4, 3.4, 3.2, 3)
  mx      <- c(0.02, 0.10, 0.12, 0.08, 0.02)     
  r <- .intrinsic_growth_rate(mx, Lx, age_mid)
  expect_true(is.finite(r))
  f <- sum(exp(-r * age_mid) * mx * Lx) - 1
  expect_lt(abs(f), 1e-10)
})

test_that(".intrinsic_growth_rate throws correct error with non-monitonic age", {
  age_mid <- c(17.5, 22.5, 27.5, 32.5, 30.5)
  Lx      <- c(4.2, 4, 3.4, 3.2, 3)
  mx      <- c(0.02, 0.10, 0.12, 0.08, 0.02)     
  expect_error(.intrinsic_growth_rate(mx, Lx, age_mid),
               "`age_mid` not monotonically increasing.")
})


