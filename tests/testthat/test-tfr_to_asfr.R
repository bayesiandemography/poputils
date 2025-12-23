
## 'tfr_to_asfr_scale' ------------------------------------------------------

test_that("'tfr_to_asfr_scale' works with valid inputs", {
  set.seed(0)
  target <- tibble::tibble(tfr = c(1, 3),
                           region = c("a", "b"))
  standard <- tibble::tibble(age = 15:49,
                             value = runif(35))
  ans <- tfr_to_asfr_scale(target = target,
                           standard = standard)
  expect_true(is.data.frame(ans))
  expect_setequal(names(ans), c("age", "region", "asfr"))
  expect_equal(sum(ans$asfr[1:35]), 1)
  expect_equal(sum(ans$asfr[36:70]), 3)
})

test_that("'tfr_to_asfr_scale' works with valid inputs - no bu variables", {
    target <- tibble::tibble(tfr = 2.3)
    standard <- tibble::tibble(age = 15:49,
                               value = 1:35)
    ans <- tfr_to_asfr_scale(target = target,
                             standard = standard,
                             suffix = "xx")
    expect_true(is.data.frame(ans))
    expect_setequal(names(ans), c("age", "asfr.xx"))
    expect_equal(sum(ans$asfr.xx), 2.3)
})

test_that("'tfr_to_asfr_scale' works with valid inputs - tfr is rvec", {
  target <- tibble::tibble(tfr = rvec::rvec(list(c(7, 7.5), c(7.1, 7.6))),
                           reg = 1:2)
  age <- age_labels(type = "five", min = 15, max = 50)
  standard <- tibble::tibble(value = 7:1,
                             age = age)
  ans <- tfr_to_asfr_scale(target = target,
                           standard = standard)
  expect_true(is.data.frame(ans))
  expect_setequal(names(ans), c("reg", "age", "asfr"))
  expect_true(rvec::is_rvec(ans$asfr))
  expect_equal(tfr(ans, by = reg), target[2:1])
})


## 'combine_target_standard_tfr_to_asfr' --------------------------------------

test_that("'combine_target_standard_tfr_to_asfr' works with valid inputs - beta not provided", {
    target <- data.frame(reg = c("F", "M"),
                         tfr = 2:3)
    standard <- data.frame(reg = rep(c("F", "M"), each = 3),
                           age = rep(c("0", "1-4", "5+"), times = 2),
                           value = c(1, 0.5, 0.2, 1, 0.4, 0.1))
    ans_obtained <- combine_target_standard_tfr_to_asfr(target = target,
                                                        standard = standard)
    ans_expected <- vctrs::vec_split(data.frame(tfr = rep(2:3, each = 3),
                                                age = rep(c("0", "1-4", "5+"), times = 2),
                                                value = c(1, 0.5, 0.2, 1, 0.4, 0.1)),
                                     data.frame(reg = rep(c("F", "M"), each = 3)))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'combine_target_standard_tfr_to_asfr' throws correct error when standard missing rows", {
  target <- data.frame(reg = c("F", "M", "D"),
                       tfr = 1:3)
  standard <- data.frame(reg = rep(c("F", "M"), each = 3),
                         age = rep(c("0", "1-4", "5+"), times = 2),
                         value = c(1, 0.5, 0.2, 1, 0.4, 0.1))
  expect_error(combine_target_standard_tfr_to_asfr(target = target,
                                                   standard = standard),
               "`standard` does not have values for case where `reg`=\"D\"")
})


## 'tfr_to_asfr_scale_one' ----------------------------------------------------

test_that("'tfr_to_asfr_scale_one' works with valid inputs - not rvec", {
  val <- tibble::tibble(tfr = 3,
                        age = age_labels(type = "five",
                                         min = 15,
                                         max = 50),
                        value = 1:7)
  ans <- tfr_to_asfr_scale_one(val = val,
                               suffix = "y")
  expect_true(is.data.frame(ans))
  expect_setequal(names(ans), c("age", "asfr.y"))
  expect_equal(sum(ans$asfr.y), 3/5)
  expect_equal(nrow(ans), nrow(val))
})

test_that("'tfr_to_asfr_scale_one' works with valid inputs - is rvec", {
  val <- tibble::tibble(tfr = rvec::rvec_dbl(matrix(1:3, nr = 1)),
                        age = 15:49,
                        value = 1:35)
  suffix <- NULL
  ans <- tfr_to_asfr_scale_one(val = val,
                               suffix = suffix)
  expect_true(is.data.frame(ans))
  expect_setequal(names(ans), c("age", "asfr"))
  expect_true(rvec::is_rvec(ans$asfr))
  expect_equal(sum(ans$asfr), val$tfr[[1]])
  expect_equal(nrow(ans), nrow(val))
})
