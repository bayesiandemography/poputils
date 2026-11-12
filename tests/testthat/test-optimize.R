rosenbrock <- function(x) 100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2
rosenbrock_gr <- function(x)
  c(-400 * x[1] * (x[2] - x[1]^2) - 2 * (1 - x[1]), 200 * (x[2] - x[1]^2))
quad <- function(x) sum((x - c(1, 2, 3))^2)
quad_gr <- function(x) 2 * (x - c(1, 2, 3))

test_that(".optimize() finds minimum with each optimizer", {
  for (optimizer in c("multi", "nlminb", "BFGS", "CG")) {
    ans <- .optimize(c(0, 0, 0), fn = quad, gr = quad_gr, optimizer = optimizer)
    expect_equal(ans$par, c(1, 2, 3), tolerance = 1e-4)
    expect_true(ans$converged)
    expect_identical(names(ans), c("par", "value", "converged", "iter", "message", "optimizer"))
  }
  ans <- .optimize(c(-1.2, 1), fn = rosenbrock, gr = rosenbrock_gr)
  expect_equal(ans$par, c(1, 1), tolerance = 1e-6)
  expect_identical(ans$optimizer, "nlminb")
})

test_that("'multi' continues with BFGS when nlminb does not converge", {
  ans <- optimize_multi(c(-1.2, 1), fn = rosenbrock, gr = rosenbrock_gr,
                        quiet = TRUE, iter_max_nlminb = 2L)
  expect_identical(ans$optimizer, "nlminb + BFGS")
  expect_match(ans$iter, "^2 \\+ ")
  expect_match(ans$message, " \\+ ")
  expect_equal(ans$par, c(1, 1), tolerance = 1e-3)
  ans <- optimize_multi(c(0, 0, 0), fn = quad, gr = quad_gr, quiet = TRUE, force_fallback = TRUE)
  expect_identical(ans$optimizer, "nlminb + BFGS")
  expect_equal(ans$par, c(1, 2, 3), tolerance = 1e-4)
})

test_that(".optimize() reports progress when quiet is FALSE", {
  expect_message(
    utils::capture.output(.optimize(c(0, 0, 0), fn = quad, gr = quad_gr, quiet = FALSE)),
    "Output from")
  expect_message(
    utils::capture.output(optimize_multi(c(-1.2, 1), fn = rosenbrock, gr = rosenbrock_gr,
                                         quiet = FALSE, iter_max_nlminb = 2L)),
    "continuing with")
  expect_silent(.optimize(c(0, 0, 0), fn = quad, gr = quad_gr, quiet = TRUE))
})

test_that(".optimize() checks arguments", {
  expect_error(.optimize("a", fn = quad, gr = quad_gr), "numeric vector")
  expect_error(.optimize(numeric(), fn = quad, gr = quad_gr), "numeric vector")
  expect_error(.optimize(0, fn = 1, gr = quad_gr), "function")
  expect_error(.optimize(0, fn = quad, gr = 1), "function")
  expect_error(.optimize(0, fn = quad, gr = quad_gr, optimizer = "wrong"), "should be one of")
  expect_error(.optimize(0, fn = quad, gr = quad_gr, quiet = NA), "TRUE")
})
