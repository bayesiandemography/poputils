
## Check functions not visible to end-user

## 'check_flag' ---------------------------------------------------------------

test_that("'check_flag' returns TRUE with valid inputs", {
    x <- TRUE
    expect_true(check_flag(x))
    x <- FALSE
    expect_true(check_flag(x))
})

test_that("'check_flag' throws expected error non-length-1", {
    y <- logical()
    expect_error(check_flag(y),
                 "`y` does not have length 1")
    z <- c(TRUE, TRUE)
    expect_error(check_flag(z),
                 "`z` does not have length 1")
})

test_that("'check_flag' throws expected error non-logical", {
    x <- "hello"
    expect_error(check_flag(x),
                 "`x` does not have class <logical>")
})

test_that("'check_flag' throws expected error NA", {
    x <- NA
    expect_error(check_flag(x),
                 "`x` is NA")
})


## 'check_lifeexp_sex' --------------------------------------------------------

test_that("'check_lifeexp_sex' returns TRUE with valid inputs", {
    sex <- "Female"
    expect_true(check_lifeexp_sex(sex))
    sex <- c("Male", "Male", "Male")
    expect_true(check_lifeexp_sex(sex))
})

test_that("'check_lifeexp_sex' throws correct error with non-character", {
    sex <- c(TRUE, FALSE)
    expect_error(check_lifeexp_sex(sex),
                 "`sex` is not a character vector.")
})

test_that("'check_lifeexp_sex' throws correct error with invalid value", {
    sex <- c("F", "F")
    expect_error(check_lifeexp_sex(sex),
                 "`sex` has invalid value.")
})

test_that("'check_lifeexp_sex' throws correct error when values vary", {
    sex <- c("Female", "Male")
    expect_error(check_lifeexp_sex(sex),
                 "Values for `sex` not all the same.")
})


## 'check_mx_rvec' ---------------------------------------------------------------

test_that("'check_mx_rvec' returns TRUE with valid inputs", {
    x <- rvec::rvec_dbl()
    expect_true(check_mx_rvec(x))
    x <- rvec::rvec_dbl(matrix(1:6))
    expect_true(check_mx_rvec(x))
    x <- rvec::rvec_int(matrix(1:6))
    expect_true(check_mx_rvec(x))
})

test_that("'check_mx_rvec' throws correct error with non-numeric", {
    x <- rvec::rvec_lgl()
    expect_error(check_mx_rvec(x),
                 "`mx` is non-numeric")
    x <- NULL
    expect_error(check_mx_rvec(x),
                 "`mx` is non-numeric")
})

test_that("'check_mx_rvec' throws correct error with negative value", {
    x <- rvec::rvec_dbl(matrix(c(1, 0, NA, -0.1), nrow = 1))
    expect_error(check_mx_rvec(x),
                 "`mx` has negative value\\(s\\).")
})


## 'check_mx_vec' -------------------------------------------------------------

test_that("'check_mx_vec' returns TRUE with valid inputs", {
    x <- 1:3
    expect_true(check_mx_vec(x))
    x <- c(0.2, 0.1, NA)
    expect_true(check_mx_vec(x))
    x <- double()
    expect_true(check_mx_vec(x))
})

test_that("'check_mx_vec' throws correct error with non-numeric", {
    x <- c(TRUE, FALSE)
    expect_error(check_mx_vec(x),
                 "`mx` is non-numeric")
    x <- NULL
    expect_error(check_mx_vec(x),
                 "`mx` is non-numeric")
})

test_that("'check_mx_rvec' throws correct error with negative value", {
    x <- rvec::rvec_dbl(matrix(c(1, 0, NA, -0.1), nrow = 1))
    expect_error(check_mx_rvec(x),
                 "`mx` has negative value\\(s\\).")
})


## 'check_number' --------------------------------------------------------------

test_that("'check_number' returns TRUE with valid inputs", {
    expect_true(check_number(1L, x_arg = "x", is_positive = TRUE, is_nonneg = TRUE, is_whole = TRUE))
    expect_true(check_number(0.001, x_arg = "x", is_positive = TRUE,
                             is_nonneg = TRUE, is_whole = FALSE))
    expect_true(check_number(0, x_arg = "x", is_positive = FALSE, is_nonneg = TRUE, is_whole = TRUE))
    expect_true(check_number(-1, x_arg = "x", is_positive = FALSE, is_nonneg = FALSE, is_whole = TRUE))
})

test_that("'check_number' returns correct error with non-numeric", {
    expect_error(check_number("1", x_arg = "x", is_positive = TRUE, is_nonneg = TRUE, is_whole = TRUE),
                 "`x` is non-numeric.")
})

test_that("'check_number' returns correct error with wrong length", {
    expect_error(check_number(1:2, x_arg = "x", is_positive = TRUE, is_nonneg = TRUE, is_whole = TRUE),
                 "`x` does not have length 1.")
})

test_that("'check_number' returns correct error with NA", {
    expect_error(check_number(NA_real_, x_arg = "x", is_positive = TRUE,
                              is_nonneg = TRUE, is_whole = TRUE),
                 "`x` is NA.")
})

test_that("'check_number' returns correct error with Inf", {
    expect_error(check_number(Inf, x_arg = "x", is_positive = TRUE, is_nonneg = TRUE, is_whole = TRUE),
                 "`x` is infinite.")
})

test_that("'check_number' returns correct error with is_positive", {
    expect_error(check_number(0, x_arg = "x", is_positive = TRUE, is_nonneg = TRUE, is_whole = TRUE),
                 "`x` is non-positive.")
    expect_error(check_number(-1, x_arg = "x", is_positive = TRUE, is_nonneg = TRUE, is_whole = TRUE),
                 "`x` is non-positive.")
})

test_that("'check_number' returns correct error with is_nonneg", {
    expect_error(check_number(-1, x_arg = "x", is_positive = FALSE, is_nonneg = TRUE, is_whole = TRUE),
                 "`x` is negative.")
})

test_that("'check_number' returns correct error with is_whole", {
    expect_error(check_number(0.5, x_arg = "x", is_positive = FALSE,
                              is_nonneg = TRUE, is_whole = TRUE),
                 "`x` is not a whole number.")
})

