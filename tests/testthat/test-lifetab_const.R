## Constant-hazard developer functions --------------------------------------

## Compare with the general path of lifetab(), which uses constant
## hazards by default
compare_with_lifetab <- function(age, mx, width) {
  d <- data.frame(age = age, mx = mx)
  expected <- lifetab(d, mx = mx, radix = 1)
  ans <- .mx_to_lifetab_const(mx, width = width)
  for (nm in c("qx", "lx", "dx", "Lx", "ex"))
    expect_equal(ans[[nm]][, 1L], expected[[nm]], tolerance = 1e-12, label = nm)
}

test_that(".mx_to_lifetab_const() agrees with lifetab(): abridged, single, five-year", {
  mx_fun <- function(x) 0.0002 + exp(-9 + 0.09 * x)
  age <- age_labels(type = "lt", max = 95)
  compare_with_lifetab(age, mx = c(0.05, 0.01, mx_fun(seq(5, 95, by = 5))),
                       width = c(1, 4, rep(5, 18), Inf))
  age <- age_labels(type = "single", max = 100)
  compare_with_lifetab(age, mx = c(0.05, mx_fun(1:100)), width = c(rep(1, 100), Inf))
  age <- age_labels(type = "five", max = 85)
  compare_with_lifetab(age, mx = c(0.03, mx_fun(seq(5, 85, by = 5))), width = c(rep(5, 17), Inf))
})

test_that(".mx_to_lifetab_const() handles zero hazards, as lifetab() does", {
  age <- age_labels(type = "lt", max = 10)
  compare_with_lifetab(age, mx = c(0, 0.01, 0, 0.1), width = c(1, 4, 5, Inf))
  ans <- .mx_to_lifetab_const(c(0.1, 0), width = c(5, Inf))
  expect_identical(ans$Lx[2, 1], Inf)
  expect_identical(ans$ex[2, 1], Inf)
})

test_that(".mx_to_lifetab_const() works column by column, including rvec draws", {
  set.seed(0)
  width <- c(1, 4, rep(5, 18), Inf)
  mx <- matrix(c(0.05, 0.01, 0.0002 + exp(-9 + 0.09 * seq(5, 95, by = 5))), 21, 3) *
    exp(matrix(stats::rnorm(63, sd = 0.2), 21, 3))
  ans <- .mx_to_lifetab_const(mx, width = width)
  for (j in 1:3)
    expect_identical(ans$ex[, j], .mx_to_lifetab_const(mx[, j], width = width)$ex[, 1])
  d <- data.frame(age = age_labels(type = "lt", max = 95))
  d$mx <- rvec::rvec(mx)
  expected <- lifetab(d, mx = mx, radix = 1)
  expect_equal(ans$ex, as.matrix(expected$ex), tolerance = 1e-12)
})

test_that(".lx_to_mx_const() inverts .mx_to_lifetab_const(); open interval gets last closed rate", {
  width <- c(1, 4, rep(5, 18), Inf)
  mx <- c(0.05, 0.01, 0.0002 + exp(-9 + 0.09 * seq(5, 95, by = 5)))
  lx <- .mx_to_lifetab_const(mx, width = width)$lx
  ans <- .lx_to_mx_const(lx, width = width)
  expect_equal(ans[-21, 1], mx[-21], tolerance = 1e-12)
  expect_identical(ans[21, 1], ans[20, 1])
})

test_that(".brass_logit_lx() follows the Brass logit model", {
  lx_std <- c(1, 0.9, 0.85, 0.6, 0.2, 0.01)
  expect_equal(.brass_logit_lx(lx_std, alpha = 0)[, 1], lx_std, tolerance = 1e-14)
  ans <- .brass_logit_lx(lx_std, alpha = c(-0.5, 0.3), beta = c(1, 1.2))
  expect_identical(dim(ans), c(6L, 2L))
  expect_identical(ans[1, ], c(1, 1))
  expect_equal(logit(1 - ans[-1, 1]), -0.5 + logit(1 - lx_std[-1]))
  expect_equal(logit(1 - ans[-1, 2]), 0.3 + 1.2 * logit(1 - lx_std[-1]))
  expect_true(all(diff(ans[, 1]) < 0))
  ## higher alpha means higher mortality
  expect_true(all(.brass_logit_lx(lx_std, 0.5)[-1] < .brass_logit_lx(lx_std, 0)[-1]))
})

test_that(".brass_logit_lx() accepts one standard per alpha", {
  lx_std <- cbind(c(1, 0.9, 0.5, 0.1), c(1, 0.95, 0.6, 0.2))
  ans <- .brass_logit_lx(lx_std, alpha = c(0, 0))
  expect_equal(ans, lx_std, tolerance = 1e-14)
})

test_that("constant-hazard functions check their inputs", {
  expect_snapshot(.mx_to_lifetab_const("a", width = c(1, Inf)), error = TRUE)
  expect_snapshot(.mx_to_lifetab_const(0.1, width = Inf), error = TRUE)
  expect_snapshot(.mx_to_lifetab_const(c(0.1, 0.2), width = c(1, 2, Inf)), error = TRUE)
  expect_snapshot(.mx_to_lifetab_const(c(0.1, 0.2), width = c(1, 5)), error = TRUE)
  expect_snapshot(.lx_to_mx_const(c(1, 0.5), width = c(0, Inf)), error = TRUE)
  expect_error(.lx_to_mx_const(array(1, c(2, 2, 2)), width = c(1, Inf)), "vector or a matrix")
  expect_snapshot(.brass_logit_lx(c(1, 0.5), alpha = c(0, 1), beta = c(1, 1, 1)), error = TRUE)
  expect_snapshot(.brass_logit_lx(cbind(c(1, 0.5), c(1, 0.6)), alpha = c(0, 1, 2)), error = TRUE)
  expect_error(.brass_logit_lx(c(1, 0.5), alpha = numeric()), "at least one element")
})


## .e0_to_alpha_brass() -----------------------------------------------------

e0_brass <- function(lx_std, alpha, width, beta = 1) {
  lx <- .brass_logit_lx(lx_std, alpha = alpha, beta = beta)
  .mx_to_lifetab_const(.lx_to_mx_const(lx, width = width), width = width)$ex[1, ]
}

test_that(".e0_to_alpha_brass() finds alpha giving the target e0", {
  width <- c(1, 4, rep(5, 18), Inf)
  mx <- c(0.05, 0.01, 0.0002 + exp(-9 + 0.09 * seq(5, 95, by = 5)))
  lx_std <- .mx_to_lifetab_const(mx, width = width)$lx[, 1]
  e0 <- c(20, 35, 50, 65, 80, 95)
  alpha <- .e0_to_alpha_brass(e0, lx_std = lx_std, width = width)
  expect_equal(e0_brass(lx_std, alpha, width), e0, tolerance = 1e-10)
  expect_true(all(diff(alpha) < 0))
  e0_std <- e0_brass(lx_std, 0, width)
  expect_equal(.e0_to_alpha_brass(e0_std, lx_std = lx_std, width = width), 0, tolerance = 1e-10)
})

test_that(".e0_to_alpha_brass() works with beta, and one standard per target", {
  width <- c(1, 4, 5, 5, Inf)
  lx_std <- cbind(c(1, 0.9, 0.8, 0.5, 0.1), c(1, 0.95, 0.9, 0.7, 0.3))
  e0 <- c(12, 15)
  alpha <- .e0_to_alpha_brass(e0, lx_std = lx_std, width = width, beta = c(0.8, 1.3))
  expect_equal(e0_brass(lx_std[, 1], alpha[[1]], width, beta = 0.8), 12, tolerance = 1e-10)
  expect_equal(e0_brass(lx_std[, 2], alpha[[2]], width, beta = 1.3), 15, tolerance = 1e-10)
})

test_that(".e0_to_alpha_brass() checks inputs, and reports unreachable targets", {
  width <- c(1, 4, 5, 5, Inf)
  lx_std <- c(1, 0.9, 0.8, 0.5, 0.1)
  expect_snapshot(.e0_to_alpha_brass(c(10, 1e-6), lx_std = lx_std, width = width), error = TRUE)
  for (bad in list(NA_real_, -1, 0, Inf, "10", numeric()))
    expect_error(.e0_to_alpha_brass(bad, lx_std = lx_std, width = width), "positive, finite")
  expect_error(.e0_to_alpha_brass(10, lx_std = lx_std, width = width, beta = 0), "`beta` must be positive")
  expect_error(.e0_to_alpha_brass(10, lx_std = cbind(lx_std, lx_std), width = width),
               "one column per element")
})
