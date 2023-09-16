
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




## 'mx_to_lx_cd' --------------------------------------------------------------

test_that("'mx_to_lx_cd' gives correct answer - Female, m0 >= 0.107, no ax supplied", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- mx_to_lx_cd(mx = mx,
                                age_group_type = c("0", "single", "open"),
                                sex = c("Female", "Female", "Female"),
                                ax = c(NA_real_, NA_real_, NA_real_))
    a0 <- 0.35
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                mx[2, ] / (1 + 0.5 * mx[2, ]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx_cd' gives correct answer - Female, m0 < 0.107, no ax supplied", {
    mx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- mx_to_lx_cd(mx = mx,
                                age_group_type = c("0", "single", "open"),
                                sex = c("Female", "Female", "Female"),
                                ax = c(NA_real_, NA_real_, NA_real_))
    a0 <- 0.053 + 2.8 * mx[1, ]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                1 * mx[2, ] / (1 + 0.5 * mx[2, ]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx_cd' gives correct answer - Female, m0 < 0.107, a0 supplied", {
    mx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- mx_to_lx_cd(mx = mx,
                                age_group_type = c("0", "single", "open"),
                                sex = c("Female", "Female", "Female"),
                                ax = c(0.5, NA_real_, NA_real_))
    a0 <- 0.5
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                1 * mx[2, ] / (1 + 0.5 * mx[2, ]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx_cd' gives correct answer - Female, m0 >= 0.107, no ax, life table age groups", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- mx_to_lx_cd(mx = mx,
                                age_group_type = c("0", "1-4", "open"),
                                sex = c("Female", "Female", "Female"),
                                ax = c(NA_real_, NA_real_, NA_real_))
    a0 <- 0.35
    a1 <- 1.361
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx_cd' gives correct answer with NA", {
    mx <- cbind(c(NA_real_, 0.01, 0.25),
                c(0.11, NA_real_, 0.4))
    ans_obtained <- mx_to_lx_cd(mx = mx,
                                age_group_type = c("0", "1-4", "open"),
                                sex = c("Female", "Female", "Female"),
                                ax = c(NA_real_, NA_real_, NA_real_))
    a0 <- 0.35
    a1 <- 1.361
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    ans_expected[2:3,1] <- NA_real_
    ans_expected[3,2] <- NA_real_
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx_cd' gives correct answer with Inf", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, Inf, 0.4))
    ans_obtained <- mx_to_lx_cd(mx = mx,
                                age_group_type = c("0", "1-4", "open"),
                                sex = c("Female", "Female", "Female"),
                                ax = c(NA_real_, NA_real_, NA_real_))
    a0 <- 0.35
    a1 <- 1.361
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    ans_expected[3,2] <- 0
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx_cd' gives correct answer with 0", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0, 0.4))
    ans_obtained <- mx_to_lx_cd(mx = mx,
                                age_group_type = c("0", "1-4", "open"),
                                sex = c("Female", "Female", "Female"),
                                ax = c(NA_real_, NA_real_, NA_real_))
    a0 <- 0.35
    a1 <- 1.361
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx_cd' gives correct answer - Male, m0 >= 0.107, no ax, single year", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- mx_to_lx_cd(mx = mx,
                                age_group_type = c("0", "single", "open"),
                                sex = c("Male", "Male", "Male"),
                                ax = c(NA_real_, NA_real_, NA_real_))
    a0 <- 0.33
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                mx[2, ] / (1 + 0.5 * mx[2, ]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx_cd' gives correct answer - Male, m0 >= 0.107, no ax, life table age groups", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- mx_to_lx_cd(mx = mx,
                                age_group_type = c("0", "1-4", "open"),
                                sex = c("Male", "Male", "Male"),
                                ax = c(NA_real_, NA_real_, NA_real_))
    a0 <- 0.33
    a1 <- 1.352
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx_cd' gives correct answer - Female, m0 < 0.107, no ax, life table age groups", {
    mx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- mx_to_lx_cd(mx = mx,
                                age_group_type = c("0", "1-4", "open"),
                                sex = c("Female", "Female", "Female"),
                                ax = c(NA_real_, NA_real_, NA_real_))
    a0 <- 0.053 + 2.8 * mx[1,]
    a1 <- 1.522 - 1.518 * mx[1,]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx_cd' gives correct answer - Male, m0 < 0.107, no ax, life table age groups", {
    mx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- mx_to_lx_cd(mx = mx,
                                age_group_type = c("0", "1-4", "open"),
                                sex = c("Male", "Male", "Male"),
                                ax = c(NA_real_, NA_real_, NA_real_))
    a0 = 0.045 + 2.684 * mx[1,]
    a1 <- 1.651 - 2.816 * mx[1,]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})


## 'mx_to_Lx' -----------------------------------------------------------------

test_that("'mx_to_Lx_const' agrees with example from MEASURE Evaulation", {
    ## Mortality rates and life expectancy taken from https://www.measureevaluation.org/resources/training/online-courses-and-resources/non-certificate-courses-and-mini-tutorials/multiple-decrement-life-tables/lesson-3.html
    mx <- c(0.07505,
            0.00701,
            0.00171,
            0.00128,
            0.00129,
            0.00181,
            0.00163,
            0.00198,
            0.00302,
            0.00442,
            0.00645,
            0.00923,
            0.01344,
            0.02364,
            0.03633,
            0.05182,
            0.07644,
            0.13520,
            0.33698)
    mx <- matrix(mx, ncol = 1)
    age_group_type <- c("0", "1-4", rep("five", length(mx) - 3), "open")
    ax <- rep(NA_real_, times = length(age_group_type))
    ans_obtained <- sum(mx_to_Lx_const(mx = mx,
                                   age_group_type = age_group_type,
                                   ax = ax))
    ans_expected <- 62.97331
    expect_equal(ans_obtained, ans_expected, tolerance = 0.0001) ## MEASURE calculations involve rounding
})

test_that("'mx_to_Lx_const' gives correct answer, with ax supplied", {
    mx <- cbind(c(0.011, 0.01, 0.05, 0.25),
                c(0.012, 0.015, 0.06, 0.4))
    ans_obtained <- mx_to_Lx_const(mx = mx,
                                   age_group = c("0", "1-4", "five", "open"),
                                   ax = c(NA_real_, NA_real_, 3, NA_real_))
    px <- rbind(exp(-mx[1, ]), exp(-4 * mx[2, ]), 1 - (5 * mx[3,])/(1 + 2 * mx[3,]))
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ], px[1, ] * px[2, ] * px[3, ])
    ans_expected <- rbind((lx[-4,] / mx[-4,]) - (lx[-1,] / mx[-4,]), (lx[4, ] / mx[4, ]))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx_const' gives correct answer, with mx of 0", {
    mx <- cbind(c(0.011, 0.01, 0.05, 0.25),
                c(0.012, 0.015, 0, 0.4))
    ans_obtained <- mx_to_Lx_const(mx = mx,
                                   age_group = c("0", "1-4", "five", "open"),
                                   ax = c(NA_real_, NA_real_, NA_real_, NA_real_))
    px <- rbind(exp(-mx[1, ]), exp(-4 * mx[2, ]), exp(-5 * mx[3, ]))
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ], px[1, ] * px[2, ] * px[3, ])
    ans_expected <- rbind((lx[-4,] / mx[-4,]) - (lx[-1,] / mx[-4,]), (lx[4, ] / mx[4, ]))
    ans_expected[3,2] <- 5
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx_const' gives correct answer, with mx with single row", {
    mx <- matrix(c(0.2, 0.3), nr = 1)
    ans_obtained <- mx_to_Lx_const(mx = mx,
                                   age_group = "open",
                                   ax = NA_real_)
    ans_expected <- 1 / mx
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx_const' gives correct answer, with mx including NZ", {
    mx <- cbind(c(0.011, 0.01, 0.05, 0.25),
                c(0.012, 0.015, NA_real_, 0.4))
    ans_obtained <- mx_to_Lx_const(mx = mx,
                                   age_group = c("0", "1-4", "five", "open"),
                                   ax = c(NA_real_, NA_real_, NA_real_, NA_real_))
    px <- rbind(exp(-mx[1, ]), exp(-4 * mx[2, ]), exp(-5 * mx[3, ]))
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ], px[1, ] * px[2, ] * px[3, ])
    ans_expected <- rbind((lx[-4,] / mx[-4,]) - (lx[-1,] / mx[-4,]), (lx[4, ] / mx[4, ]))
    expect_equal(ans_obtained, ans_expected)
    expect_identical(is.na(ans_obtained),
                     matrix(c(F, F, F, F, F, F, T, T),
                            nrow = 4))
})


## 'qx_to_Lx_const' -----------------------------------------------------------

test_that("'qx_to_Lx_const' gives correct answer - life table age groups, no ax", {
    qx <- cbind(c(0.011, 0.01, 0.3, 1),
                c(0.012, 0.015, 0.4, 1))
    ans_obtained <- qx_to_Lx_const(qx = qx,
                                   age_group_type = c("0", "1-4", "five", "open"),
                                   ax = c(NA_real_, NA_real_, NA_real_, NA_real_))
    mx <- -log(1 - qx) / c(1, 4, 5, Inf)
    mx[4,] <- mx[3,]
    lx <- qx_to_lx(qx)
    dx <- lx_to_dx(lx)
    ans_expected <- dx / mx
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_Lx_const' gives correct answer - life table age groups, with ax", {
    qx <- cbind(c(0.011, 0.01, 0.05, 1),
                c(0.012, 0.015, 0.06, 1))
    ans_obtained <- qx_to_Lx_const(qx = qx,
                                   age_group_type = c("0", "1-4", "five", "open"),
                                   ax = c(0.3, NA_real_, NA_real_, NA_real_))
    mx <- -log(1 - qx) / c(1, 4, 5, Inf)
    mx[4,] <- mx[3,]
    lx <- qx_to_lx(qx)
    dx <- lx_to_dx(lx)
    ans_expected <- dx / mx
    ans_expected[1,] <- lx[2,] + 0.3 * dx[1,]
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_Lx_const' gives correct answer - life table age groups, with 1s", {
    qx <- cbind(c(0.011, 1, 0.05, 1),
                c(0.012, 0.015, 0.06, 1))
    ans_obtained <- qx_to_Lx_const(qx = qx,
                                   age_group_type = c("0", "1-4", "five", "open"),
                                   ax = c(NA_real_, NA_real_, NA_real_, NA_real_))
    mx <- -log(1 - qx) / c(1, 4, 5, Inf)
    mx[4,] <- mx[3,]
    lx <- qx_to_lx(qx)
    dx <- lx_to_dx(lx)
    ans_expected <- dx / mx
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_Lx_const' gives correct answer - life table age groups, with 0s", {
    qx <- cbind(c(0.011, 0, 0.05, 1),
                c(0.012, 0.015, 0.06, 1))
    ans_obtained <- qx_to_Lx_const(qx = qx,
                                   age_group_type = c("0", "1-4", "five", "open"),
                                   ax = c(NA_real_, NA_real_, NA_real_, NA_real_))
    mx <- -log(1 - qx) / c(1, 4, 5, Inf)
    mx[4,] <- mx[3,]
    lx <- qx_to_lx(qx)
    dx <- lx_to_dx(lx)
    ans_expected <- dx / mx
    ans_expected[mx == 0] <- 4
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_Lx_const' gives correct answer - life table age groups, with NAs", {
    qx <- cbind(c(0.011, 0.02, 0.05, 1),
                c(0.012, 0.015, NA_real_, 1))
    ans_obtained <- qx_to_Lx_const(qx = qx,
                                   age_group_type = c("0", "1-4", "five", "open"),
                                   ax = c(NA_real_, NA_real_, NA_real_, NA_real_))
    mx <- -log(1 - qx) / c(1, 4, 5, Inf)
    mx[4,] <- mx[3,]
    lx <- qx_to_lx(qx)
    dx <- lx_to_dx(lx)
    ans_expected <- dx / mx
    expect_equal(ans_obtained, ans_expected)
})
                         












## 'qx_to_mx_const' -----------------------------------------------------------

test_that("'qx_to_mx_const' gives correct answer - life table age groups, no ax", {
    qx <- cbind(c(0.011, 0.01, 0.05, 1),
                c(0.012, 0.015, 0.06, 1))
    ans_obtained <- qx_to_mx_const(qx = qx,
                                   age_group_type = c("0", "1-4", "five", "open"),
                                   ax = c(NA_real_, NA_real_, NA_real_, NA_real_))
    ans_expected <- -log(1 - qx) / c(1, 4, 5, Inf)
    ans_expected[4,]  <- ans_expected[3,]
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_mx_const' gives correct answer - life table age groups, with ax", {
    qx <- cbind(c(0.011, 0.01, 0.05, 1),
                c(0.012, 0.015, 0.06, 1))
    ans_obtained <- qx_to_mx_const(qx = qx,
                                   age_group_type = c("0", "1-4", "five", "open"),
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
                                   age_group_type = c("0", "1-4", "five", "open"),
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
                                   age_group_type = c("0", "1-4", "five", "open"),
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
                                   age_group_type = c("0", "1-4", "five", "open"),
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
    age_group_type <- c("0", "1-4", rep("five", 7), "open")
    ax <- rep(NA_real_, 10)
    lx <- mx_to_lx_const(mx = mx0,
                         age_group_type = age_group_type,
                         ax = ax)
    px <- rbind(lx[-1,] / lx[-10,], 0)
    qx <- 1 - px
    mx1 <- qx_to_mx_const(qx,
                          age_group_type = age_group_type,
                          ax = ax)
    expect_equal(mx1[-10,], mx0[-10,])
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


