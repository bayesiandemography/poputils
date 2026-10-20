
## 'rr3' ----------------------------------------------------------------------

test_that("rr3 rounds appropriately", {
  set.seed(0)
  x <- round(rnorm(n = 1000, mean = 0, sd = 3))
  x[6] <- NA
  ans <- rr3(x)
  expect_true(all(ans[-6] %% 3 == 0))
  expect_true(is.na(ans[6]))
  is_mod3 <- !is.na(x) & (x %% 3 == 0)
  expect_true(all(ans[is_mod3] == x[is_mod3]))
  is_le_0 <- x <= 0
  expect_true(all(ans[is_le_0] <= 0, na.rm = TRUE))
  is_ge_0 <- x >= 0
  expect_true(all(ans[is_ge_0] >= 0, na.rm = TRUE))
})

test_that("rr3 is unbiased, and rounds to the nearest multiples of 3 with correct probabilities", {
  set.seed(0)
  n <- 100000
  for (x in c(-2L, -1L, 1L, 2L, 4L, 5L)) {
    ans <- rr3(rep(x, n))
    lower <- 3L * (x %/% 3L)
    upper <- lower + 3L
    expect_true(all(ans %in% c(lower, upper)))
    p_upper <- (x - lower) / 3
    se <- sqrt(p_upper * (1 - p_upper) / n)
    expect_lt(abs(mean(ans == upper) - p_upper), 4 * se)
    expect_lt(abs(mean(ans) - x), 4 * 3 * se)
  }
})

test_that("rr3 leaves type unchanged", {
  expect_true(is.integer(rr3(c(1:5, NA))))
  expect_true(is.double(rr3(c(1:5, NA_real_))))
})

test_that("rr3 throws correct error with non-integer", {
  expect_error(rr3(c(1, 2, 1.1)),
               "`x` has non-integer values.")
})

test_that("rr3 throws correct error with non-finite", {
  expect_error(rr3(c(1, 2, Inf, -3)),
               "`x` has non-finite values.")
})

test_that("rr3 throws correct error with non-numeric", {
  expect_error(rr3(c("a", 2, 1.1)),
               "`x` must be numeric or integer.")
  expect_error(rr3(rvec::rvec(matrix("a", nr = 1))),
               "`x` must be numeric or integer.")
})

test_that("rr3 throws correct error with value too large", {
  expect_error(rr3(c(1, 2, .Machine$integer.max + 1)),
               "Maximum value in `x` greater than largest representable integer.")
})

test_that("rr3 errors when rounding leaves the integer range", {
  hi <- as.integer(.Machine$integer.max)
  lo <- -hi
  set.seed(0)
  expect_error(rr3(hi),
               "Randomly rounded value is outside the range of an integer.")
  set.seed(1)
  expect_identical(rr3(hi), hi - 1L)
  set.seed(1)
  expect_error(rr3(lo),
               "Randomly rounded value is outside the range of an integer.")
  set.seed(0)
  expect_identical(rr3(lo), lo + 1L)
  expect_true(all(rr3(c(1L, 3L, NA)) %% 3L == 0L, na.rm = TRUE))
})

test_that("rr3 works with rvec", {
  x <- matrix(round(rnorm(n = 1000, mean = 5, sd = 3)), nrow = 10)
  x <- rvec::rvec(x)
  ans <- rr3(x)
  ans <- as.numeric(ans)
  expect_true(all(ans %% 3 == 0))
  x[2] <- NA
  ans <- rr3(x)
  ans <- as.numeric(ans)
  expect_true(all(ans %% 3 == 0, na.rm = TRUE))
})
