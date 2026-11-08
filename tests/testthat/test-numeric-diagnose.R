## 'count_diagnose', 'count_assert' -------------------------------------------

test_that("count_diagnose() passes valid counts, with fixed conditions only by default", {
  ans <- count_diagnose(c(0, 3, 10))
  expect_true(ans$ok)
  expect_identical(ans$details$condition, c("finite", "nonneg"))
  expect_identical(ans$details$comment, c(NA_character_, NA_character_))
})

test_that("count_diagnose() reports every failed condition, in standard order", {
  popn <- c(10, -3, 2.5, NA, Inf, 0)
  ans <- count_diagnose(popn, whole = TRUE, no_zero = TRUE, no_na = TRUE)
  expect_false(ans$ok)
  expect_identical(ans$details$condition,
                   c("no_na", "finite", "nonneg", "no_zero", "whole"))
  expect_identical(
    ans$details$comment,
    c("Example: popn[4] is NA", "Example: popn[5] is Inf",
      "Example: popn[2] is -3", "Example: popn[6] is 0",
      "Example: popn[3] is 2.5")
  )
})

test_that("count_diagnose() ignores NAs unless no_na is TRUE", {
  expect_true(count_diagnose(c(1, NA), whole = TRUE, no_zero = TRUE)$ok)
  expect_false(count_diagnose(c(1, NA), no_na = TRUE)$ok)
})

test_that("count_diagnose() passes empty input", {
  expect_true(count_diagnose(numeric(), no_na = TRUE, whole = TRUE)$ok)
})

test_that("count_diagnose() works with integers and rvecs", {
  expect_true(count_diagnose(1:3, whole = TRUE)$ok)
  x <- rvec::rvec(matrix(c(1, 2, 3, -1), nrow = 2))
  ans <- count_diagnose(x)
  expect_identical(ans$details$comment[[2L]], "Example: draw 2 of x[2] is -1")
})

test_that("count_assert() returns x invisibly when valid", {
  x <- c(1, 2)
  expect_invisible(count_assert(x))
  expect_identical(count_assert(x, no_na = TRUE, whole = TRUE), x)
  x_rvec <- rvec::rvec(matrix(1:4, nrow = 2))
  expect_identical(count_assert(x_rvec, whole = TRUE), x_rvec)
})

test_that("count_assert() throws error listing failed conditions", {
  popn <- c(10, -3, 2.5)
  expect_snapshot(count_assert(popn, whole = TRUE), error = TRUE)
})

test_that("count_assert() names itself by default", {
  popn <- -1
  err <- expect_error(count_assert(popn))
  expect_identical(err$call, quote(count_assert(popn)))
  f <- function(data) count_assert(data$popn, x_arg = "popn")
  err <- expect_error(f(data.frame(popn = -1)))
  expect_identical(err$call[[1L]], quote(count_assert))
})

test_that("count_assert() uses 'x_arg' and 'call' supplied by calling function", {
  f <- function(data) {
    count_assert(data$popn, x_arg = "popn", call = rlang::current_env())
  }
  err <- expect_error(f(data.frame(popn = -1)))
  expect_identical(err$call, quote(f(data.frame(popn = -1))))
  expect_match(conditionMessage(err), "`popn` fails check `nonneg`")
  g <- function(x) count_diagnose(x, whole = NA, call = rlang::current_env())
  err <- expect_error(g(1), "`whole` is")
  expect_identical(err$call, quote(g(1)))
})

test_that("count_diagnose() and count_assert() throw error with non-numeric input", {
  expect_error(count_diagnose("a"), "is non-numeric")
  expect_error(count_assert(factor("1")), "is non-numeric")
  expect_error(count_assert(TRUE), "is non-numeric")
})

test_that("count_diagnose() checks flags", {
  expect_error(count_diagnose(1, whole = NA), "`whole` is")
  expect_error(count_diagnose(1, no_na = "yes"), "`no_na` does not have class")
  expect_error(count_diagnose(1, no_zero = c(TRUE, FALSE)), "`no_zero` does not have length 1")
})


## 'prob_diagnose', 'prob_assert' ---------------------------------------------

test_that("prob_diagnose() checks limits, with optional no_zero and no_one", {
  x <- c(0, 0.5, 1)
  ans <- prob_diagnose(x)
  expect_true(ans$ok)
  expect_identical(ans$details$condition, c("finite", "nonneg", "le_one"))
  ans <- prob_diagnose(x, no_zero = TRUE, no_one = TRUE, no_na = TRUE)
  expect_identical(ans$details$condition,
                   c("no_na", "finite", "nonneg", "le_one", "no_zero", "no_one"))
  expect_identical(ans$details$passed, c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_identical(ans$details$comment[5:6],
                   c("Example: x[1] is 0", "Example: x[3] is 1"))
})

test_that("prob_assert() throws error when values outside [0, 1]", {
  qx <- c(0.2, 1.3, -0.1)
  expect_snapshot(prob_assert(qx), error = TRUE)
  expect_identical(prob_assert(c(0.2, 0.3)), c(0.2, 0.3))
})


## 'rate_diagnose', 'rate_assert' ---------------------------------------------

test_that("rate_diagnose() checks rates, with optional no_zero", {
  ans <- rate_diagnose(c(0.01, 0, 2))
  expect_true(ans$ok)
  expect_identical(ans$details$condition, c("finite", "nonneg"))
  ans <- rate_diagnose(c(0.01, 0, 2), no_zero = TRUE)
  expect_false(ans$ok)
})

test_that("rate_assert() throws error with negative rate", {
  expect_error(rate_assert(c(0.01, -0.2)), "fails check `nonneg`")
})


## helpers --------------------------------------------------------------------

test_that("numeric_values_matrix() returns one column, or one per draw", {
  expect_identical(numeric_values_matrix(1:2, "x", NULL), matrix(1:2, ncol = 1L))
  x <- rvec::rvec(matrix(c(1, 2, 3, 4), nrow = 2))
  expect_identical(dim(numeric_values_matrix(x, "x", NULL)), c(2L, 2L))
})

test_that("numeric_condition_fun() returns working functions", {
  m <- matrix(c(-1, 0, 0.5, 1, 2, NA, Inf))
  expect_identical(as.vector(numeric_condition_fun("no_na")(m)),
                   c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE))
  expect_identical(as.vector(numeric_condition_fun("finite")(m)),
                   c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_identical(as.vector(numeric_condition_fun("nonneg")(m)),
                   c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(as.vector(numeric_condition_fun("le_one")(m)),
                   c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE))
  expect_identical(as.vector(numeric_condition_fun("no_zero")(m)),
                   c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(as.vector(numeric_condition_fun("no_one")(m)),
                   c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))
  expect_identical(as.vector(numeric_condition_fun("whole")(m)),
                   c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_error(numeric_condition_fun("wrong"), "Internal error")
})

test_that("numeric_example() finds first element, then first draw", {
  m <- matrix(c(1, -2, -3, -4), nrow = 2)
  is_bad <- m < 0
  expect_identical(numeric_example(m, is_bad, "x", is_rvec = TRUE),
                   "Example: draw 2 of x[1] is -3")
  expect_identical(numeric_example(m[, 1, drop = FALSE], is_bad[, 1, drop = FALSE],
                                   "x", is_rvec = FALSE),
                   "Example: x[2] is -2")
})

test_that("numeric_conditions() combines fixed conditions and flags in order", {
  expect_identical(
    numeric_conditions(fixed = c("finite", "nonneg"),
                       flags = list(whole = TRUE, no_na = TRUE, no_zero = FALSE),
                       order = c("no_na", "finite", "nonneg", "no_zero", "whole"),
                       call = NULL),
    c("no_na", "finite", "nonneg", "whole")
  )
})

test_that("inner_numeric_diagnose() handles no conditions", {
  ans <- inner_numeric_diagnose(1, "x", character(), NULL)
  expect_true(ans$ok)
  expect_identical(nrow(ans$details), 0L)
})
