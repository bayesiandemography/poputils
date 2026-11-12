## Precision matrix of a random walk, plus a little on the diagonal
prec_rw <- function(n) {
  D <- diff(diag(n))
  crossprod(D) + diag(0.1, n)
}

test_that(".chol_prec() reproduces the precision matrix, dense and sparse input", {
  prec <- prec_rw(6)
  for (p in list(prec, Matrix::Matrix(prec, sparse = TRUE), Matrix::Matrix(prec, sparse = FALSE))) {
    ch <- .chol_prec(p)
    expect_s4_class(ch, "CHMfactor")
    rebuilt <- as.matrix(Matrix::expand1(ch, "P1.") %*% Matrix::expand1(ch, "L") %*%
                           Matrix::expand1(ch, "L.") %*% Matrix::expand1(ch, "P1"))
    expect_equal(rebuilt, prec, ignore_attr = TRUE)
  }
})

test_that(".chol_prec() gives the same factor as bage's safe_chol_prec()", {
  prec <- prec_rw(8)
  ours <- .chol_prec(prec)
  bage <- bage_safe_chol_prec(prec, max_jitter = 1e-4)
  expect_equal(as.matrix(Matrix::expand1(ours, "L")), as.matrix(Matrix::expand1(bage, "L")))
  expect_identical(ours@perm, bage@perm)
})

test_that(".chol_prec() checks symmetry", {
  prec <- prec_rw(4)
  prec[1, 2] <- prec[1, 2] + 1e-9
  expect_warning(.chol_prec(prec), "moderately asymmetric")
  prec[1, 2] <- 5
  expect_error(.chol_prec(prec), "severely asymmetric")
})

test_that("symmetry_grade() agrees with bage's", {
  prec <- prec_rw(4)
  for (delta in c(0, 1e-12, 1e-9, 1e-6)) {
    p <- prec
    p[1, 2] <- p[1, 2] + delta
    expect_identical(symmetry_grade(p), bage_symmetry_grade(p))
    ps <- Matrix::Matrix(p, sparse = TRUE)
    expect_identical(symmetry_grade(ps), bage_symmetry_grade(ps))
  }
})

test_that(".chol_prec() adds jitter when needed, and errors when it is not enough", {
  prec <- matrix(c(1, 1, 1, 1), 2, 2) # singular
  expect_warning(ch <- .chol_prec(prec), "small quantity to the diagonal")
  expect_s4_class(ch, "CHMfactor")
  prec <- matrix(c(1, 2, 2, 1), 2, 2) # indefinite
  expect_error(suppressWarnings(.chol_prec(prec)), "factorisation failed")
  expect_error(.chol_prec(prec, max_jitter = 0), "factorisation failed")
})

test_that(".chol_prec() checks arguments", {
  expect_error(.chol_prec("a"), "must be a matrix")
  expect_error(.chol_prec(matrix("a")), "numeric")
  expect_error(.chol_prec(matrix(1, 2, 3)), "square")
  expect_error(.chol_prec(matrix(NA_real_, 1, 1)), "NAs")
  expect_error(.chol_prec(diag(2), max_jitter = -1), "non-negative")
})

test_that(".rmvnorm_prec_chol() gives draws with the right mean and covariance", {
  prec <- prec_rw(5)
  mean <- c(1, -1, 0, 2, 5)
  ch <- .chol_prec(prec)
  set.seed(0)
  draws <- .rmvnorm_prec_chol(n = 100000, mean = mean, chol = ch)
  expect_identical(dim(draws), c(5L, 100000L))
  expect_equal(rowMeans(draws), mean, tolerance = 0.02)
  expect_equal(stats::cov(t(draws)), solve(prec), tolerance = 0.02)
})

test_that(".rmvnorm_prec_chol() gives the same draws as sparseMVN", {
  testthat::skip_if_not_installed("sparseMVN")
  prec <- prec_rw(6) + diag(c(5, 0, 0, 0, 0, 1))
  for (p in list(prec, Matrix::Matrix(prec, sparse = TRUE))) {
    ch <- .chol_prec(p)
    set.seed(3)
    ours <- .rmvnorm_prec_chol(n = 4, mean = 1:6, chol = ch)
    set.seed(3)
    theirs <- t(sparseMVN::rmvn.sparse(n = 4, mu = 1:6, CH = ch, prec = TRUE))
    expect_equal(ours, theirs, ignore_attr = TRUE)
  }
})

test_that("bage's dense fallback ignores the permutation, unlike .rmvnorm_prec_chol()", {
  ## A precision matrix where the fill-reducing permutation is not
  ## the identity: an 'arrow' matrix, dense first row and column
  n <- 6
  prec <- diag(4, n)
  prec[1, ] <- prec[, 1] <- 1
  prec[1, 1] <- 10
  ch <- .chol_prec(prec)
  expect_false(identical(ch@perm, seq_len(n) - 1L))
  set.seed(0)
  ours <- .rmvnorm_prec_chol(n = 100000, mean = rep(0, n), chol = ch)
  set.seed(0)
  bage <- as.matrix(bage_rmvnorm_chol(n = 100000, mean = rep(0, n), CH = ch))
  expect_equal(stats::cov(t(ours)), solve(prec), tolerance = 0.02)
  expect_gt(max(abs(stats::cov(t(bage)) - solve(prec))), 0.05)
})

test_that(".rmvnorm_prec_chol() gives the same draws after saveRDS() and readRDS()", {
  ch <- .chol_prec(prec_rw(4))
  path <- tempfile(fileext = ".rds")
  saveRDS(ch, path)
  ch2 <- readRDS(path)
  set.seed(1)
  a <- .rmvnorm_prec_chol(n = 3, mean = rep(0, 4), chol = ch)
  set.seed(1)
  b <- .rmvnorm_prec_chol(n = 3, mean = rep(0, 4), chol = ch2)
  expect_identical(a, b)
})

test_that(".rmvnorm_prec_chol() checks arguments", {
  ch <- .chol_prec(diag(2))
  expect_error(.rmvnorm_prec_chol(n = 0, mean = c(0, 0), chol = ch))
  expect_error(.rmvnorm_prec_chol(n = 2, mean = c(0, 0), chol = diag(2)), "created by")
  expect_error(.rmvnorm_prec_chol(n = 2, mean = 0, chol = ch), "one element per row")
  expect_error(.rmvnorm_prec_chol(n = 2, mean = c(0, NA), chol = ch), "one element per row")
})

test_that(".draws_free_fixed() copies fixed values and draws free ones", {
  est <- list(a = 1, b = c(10, 20), c = 3)
  is_free <- c(FALSE, TRUE, TRUE, FALSE)
  ch <- .chol_prec(diag(c(100, 100)))
  set.seed(0)
  draws <- .draws_free_fixed(est, is_free = is_free, chol = ch, n = 5000)
  expect_identical(rownames(draws), c("a", "b1", "b2", "c"))
  expect_identical(unname(draws[1, ]), rep(1, 5000))
  expect_identical(unname(draws[4, ]), rep(3, 5000))
  expect_equal(rowMeans(draws[2:3, ]), c(b1 = 10, b2 = 20), tolerance = 0.01)
  set.seed(0)
  free <- .rmvnorm_prec_chol(n = 5000, mean = c(10, 20), chol = ch)
  expect_equal(unname(draws[2:3, ]), free)
})

test_that(".draws_free_fixed() works with no free parameters", {
  draws <- .draws_free_fixed(list(a = 1:2), is_free = c(FALSE, FALSE), chol = NULL, n = 2)
  expect_identical(unname(draws), matrix(c(1, 2, 1, 2), 2))
})

test_that(".draws_free_fixed() checks arguments", {
  ch <- .chol_prec(diag(1))
  expect_error(.draws_free_fixed(list(a = "x"), TRUE, ch, 1), "only numbers")
  expect_error(.draws_free_fixed(list(a = 1), c(TRUE, TRUE), ch, 1), "one element per value")
  expect_error(.draws_free_fixed(list(a = 1), TRUE, ch, 0))
})
