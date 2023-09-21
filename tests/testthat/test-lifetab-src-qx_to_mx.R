

## 'qx_to_mx_const' -----------------------------------------------------------

test_that("'qx_to_mx_const' gives correct answer - life table age groups, no ax", {
    qx <- cbind(c(0.011, 0.01, 0.05, 1),
                c(0.012, 0.015, 0.06, 1))
    ans_obtained <- qx_to_mx_const(qx = qx,
                                   age_group_categ = c("0", "1-4", "five", "open"),
                                   ax = c(NA_real_, NA_real_, NA_real_, NA_real_))
    ans_expected <- -log(1 - qx) / c(1, 4, 5, Inf)
    ans_expected[4,]  <- ans_expected[3,]
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_mx_const' gives correct answer - life table age groups, with ax", {
    qx <- cbind(c(0.011, 0.01, 0.05, 1),
                c(0.012, 0.015, 0.06, 1))
    ans_obtained <- qx_to_mx_const(qx = qx,
                                   age_group_categ = c("0", "1-4", "five", "open"),
                                   ax = c(0.3, NA_real_, NA_real_, NA_real_))
    ans_expected <- -log(1 - qx) / c(1, 4, 5, Inf)
    ans_expected[1,] <- qx[1,] / (1-qx[1,] + 0.3 * qx[1,])
    ans_expected[4,]  <- ans_expected[3,]
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_mx_const' gives correct answer - life table age groups, with 1s", {
    qx <- cbind(c(0.011, 1, 0.05, 1),
                c(0.012, 0.015, 0.06, 1))
    ans_obtained <- qx_to_mx_const(qx = qx,
                                   age_group_categ = c("0", "1-4", "five", "open"),
                                   ax = c(NA_real_, NA_real_, NA_real_, NA_real_))
    ans_expected <- -log(1 - qx) / c(1, 4, 5, Inf)
    ans_expected[4,]  <- ans_expected[3,]
    expect_equal(ans_obtained, ans_expected)
    expect_identical(ans_obtained[2,1], Inf)
})

test_that("'qx_to_mx_const' gives correct answer - life table age groups, with 0s", {
    qx <- cbind(c(0.011, 0, 0.05, 1),
                c(0.012, 0.015, 0.06, 1))
    ans_obtained <- qx_to_mx_const(qx = qx,
                                   age_group_categ = c("0", "1-4", "five", "open"),
                                   ax = c(NA_real_, NA_real_, NA_real_, NA_real_))
    ans_expected <- -log(1 - qx) / c(1, 4, 5, Inf)
    ans_expected[4,]  <- ans_expected[3,]
    expect_equal(ans_obtained, ans_expected)
    expect_identical(ans_obtained[2,1], 0)
})

test_that("'qx_to_mx_const' gives correct answer - life table age groups, with NAs", {
    qx <- cbind(c(0.011, 0.02, 0.05, 1),
                c(0.012, 0.015, NA_real_, 1))
    ans_obtained <- qx_to_mx_const(qx = qx,
                                   age_group_categ = c("0", "1-4", "five", "open"),
                                   ax = c(NA_real_, NA_real_, NA_real_, NA_real_))
    ans_expected <- -log(1 - qx) / c(1, 4, 5, Inf)
    ans_expected[4,]  <- ans_expected[3,]
    expect_equal(ans_obtained, ans_expected)
    expect_identical(ans_obtained[3:4, 2], c(NA_real_, NA_real_))
})

test_that("'qx_to_mx_const' inverts results from 'mx_to_lx_const'", {
    set.seed(0)
    mx0 <- matrix(runif(100, max = 0.8),
                  nrow = 10)
    mx0[c(1, 4)] <- 0
    age_group_categ <- c("0", "1-4", rep("five", 7), "open")
    ax <- rep(NA_real_, 10)
    lx <- mx_to_lx_const(mx = mx0,
                         age_group_categ = age_group_categ,
                         ax = ax)
    px <- rbind(lx[-1,] / lx[-10,], 0)
    qx <- 1 - px
    mx1 <- qx_to_mx_const(qx,
                          age_group_categ = age_group_categ,
                          ax = ax)
    expect_equal(mx1[-10,], mx0[-10,])
})
