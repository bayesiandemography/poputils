test_that("replicate_data() dispatches to methods", {
  x <- structure(list(), class = "poputils_test_model")
  replicate_data.poputils_test_model <- function(x, n = 19, ...) n
  registerS3method("replicate_data", "poputils_test_model",
                   replicate_data.poputils_test_model,
                   envir = asNamespace("poputils"))
  expect_identical(replicate_data(x), 19)
  expect_identical(replicate_data(x, n = 5), 5)
})

test_that("replicate_data() gives an error when there is no method", {
  expect_error(replicate_data(1), "no applicable method")
})
