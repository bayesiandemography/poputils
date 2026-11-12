test_that("is_fitted() and unfit() dispatch to methods", {
  x <- structure(list(fit = 1), class = "poputils_test_model")
  is_fitted.poputils_test_model <- function(x, ...) !is.null(x$fit)
  unfit.poputils_test_model <- function(x, ...) {
    x["fit"] <- list(NULL)
    x
  }
  registerS3method("is_fitted", "poputils_test_model", is_fitted.poputils_test_model,
                   envir = asNamespace("poputils"))
  registerS3method("unfit", "poputils_test_model", unfit.poputils_test_model,
                   envir = asNamespace("poputils"))
  expect_true(is_fitted(x))
  x <- unfit(x)
  expect_false(is_fitted(x))
  expect_identical(unfit(x), x)
})

test_that("is_fitted() and unfit() give errors when there is no method", {
  expect_error(is_fitted(1), "no applicable method")
  expect_error(unfit(1), "no applicable method")
})
