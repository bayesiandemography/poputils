
## 'check_ax_le_nx' -----------------------------------------------------------

test_that("'check_ax_le_nx' works with valid inputs", {
    expect_true(check_ax_le_nx(0, "0"))
    expect_true(check_ax_le_nx(1, "single"))
    expect_true(check_ax_le_nx(0.5, "five"))
    expect_false(check_ax_le_nx(33, "five"))
    expect_true(check_ax_le_nx(33, "open"))
    expect_true(check_ax_le_nx(NA_real_, "single"))
    expect_true(check_ax_le_nx(-1, "1-4"))
    expect_identical(check_ax_le_nx(c(-1, 1), c("1-4", "1-4")), c(TRUE, TRUE))
})


## 'lx_to_dx' -----------------------------------------------------------------

test_that("'lx_to_dx' works with no NAs", {
    set.seed(0)
    qx <- rbind(matrix(runif(12), nr = 4), 1)
    lx <- qx_to_lx(qx)
    ans_obtained <- lx_to_dx(lx)
    ans_expected <- rbind(lx[-5,] - lx[-1,], lx[5,])
    expect_identical(ans_obtained, ans_expected)
})

test_that("'lx_to_dx' works with no NAs", {
    set.seed(0)
    qx <- rbind(matrix(runif(12), nr = 4), 1)
    lx <- qx_to_lx(qx)
    lx[2, 1] <- NA
    lx[4, 2] <- NA
    ans_obtained <- lx_to_dx(lx)
    ans_expected <- rbind(lx[-5,] - lx[-1,], lx[5,])
    expect_identical(ans_obtained, ans_expected)
})

test_that("'lx_to_qx' works with single row", {
    lx <- matrix(1, nr = 1, nc = 3)
    ans_obtained <- lx_to_dx(lx)
    ans_expected <- lx
    expect_equal(ans_obtained, ans_expected)
})


## 'lx_to_qx' -----------------------------------------------------------------

test_that("'lx_to_qx' works with no NAs", {
    set.seed(0)
    qx <- rbind(matrix(runif(12), nr = 4), 1)
    lx <- qx_to_lx(qx)
    expect_identical(lx_to_qx(lx), qx)
})

test_that("'lx_to_qx' works with NAs", {
    set.seed(0)
    qx <- rbind(matrix(runif(12), nr = 4), 1)
    qx[1, 3] <- NA_real_
    qx[4, 2] <- NA_real_
    lx <- qx_to_lx(qx)
    ans_obtained <- lx_to_qx(lx)
    ans_expected <- rbind(1 - (lx[-1,] / lx[-5,]), 1)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'lx_to_qx' works with single row", {
    lx <- matrix(1, nr = 1, nc = 3)
    ans_obtained <- lx_to_qx(lx)
    ans_expected <- lx
    expect_equal(ans_obtained, ans_expected)
})


## 'qx_to_lx' -----------------------------------------------------------------

test_that("'qx_to_lx' works with no NAs", {
    set.seed(0)
    qx <- rbind(matrix(runif(12), nr = 4), 1)
    ans_obtained <- qx_to_lx(qx)
    px <- 1 - qx
    ans_expected <- rbind(1,
                          px[1,],
                          px[1,] * px[2,],
                          px[1,] * px[2,] * px[3,],
                          px[1,] * px[2,] * px[3,] * px[4,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_lx' works with NAs", {
    set.seed(0)
    qx <- rbind(matrix(runif(12), nr = 4), 1)
    qx[1, 3] <- NA_real_
    qx[4, 2] <- NA_real_
    ans_obtained <- qx_to_lx(qx)
    px <- 1 - qx
    ans_expected <- rbind(1,
                          px[1,],
                          px[1,] * px[2,],
                          px[1,] * px[2,] * px[3,],
                          px[1,] * px[2,] * px[3,] * px[4,])
    expect_equal(ans_obtained, ans_expected)
    expect_identical(which(is.na(ans_obtained)),
                     c(10L, 12:15L))
})

test_that("'qx_to_lx' works with single row", {
    qx <- matrix(1, nr = 1, nc = 3)
    ans_obtained <- qx_to_lx(qx)
    ans_expected <- qx
    expect_equal(ans_obtained, ans_expected)
})


