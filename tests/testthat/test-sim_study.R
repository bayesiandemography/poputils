test_that("sim_study() dispatches to methods", {
  x <- structure(list(), class = "poputils_test_model")
  sim_study.poputils_test_model <- function(mod_est, n_sim = 100, ...) n_sim
  registerS3method("sim_study", "poputils_test_model",
                   sim_study.poputils_test_model,
                   envir = asNamespace("poputils"))
  expect_identical(sim_study(x), 100)
  expect_identical(sim_study(x, n_sim = 5), 5)
})

test_that("sim_study() gives an error when there is no method", {
  expect_error(sim_study(1), "no applicable method")
})
