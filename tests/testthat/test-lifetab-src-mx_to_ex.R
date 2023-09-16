
## 'mx_to_ex_const' -----------------------------------------------------------

test_that("'mx_to_ex_const' agrees with example from MEASURE Evaluation - const", {
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
    ans_obtained <- mx_to_ex(mx = mx,
                             age_group_type = age_group_type,
                             sex = "none",
                             ax = ax,
                             method = "const")
    ans_expected <- 62.97331
    expect_equal(ans_obtained, ans_expected, tolerance = 0.0001) ## MEASURE calculations involve rounding
})

test_that("'mx_to_ex_const' handles NA as expected", {
    mx <- c(0.07505,
            0.00701,
            0.00171,
            0.00128,
            0.00129,
            0.00181,
            0.00163,
            0.00198,
            0.00302,
            NA_real_,
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
    ax <- rep(-1, times = length(age_group_type))
    ans_obtained <- mx_to_ex(mx = mx,
                             age_group_type = age_group_type,
                             sex = "none",
                             ax = ax,
                             method = "const")
    ans_expected <- NA_real_
    expect_identical(ans_obtained, ans_expected)
})

test_that("'mx_to_ex_const' gives correct answer, with ax supplied", {
    mx <- cbind(c(0.011, 0.01, 0.05, 0.25),
                c(0.012, 0.015, 0.06, 0.4))
    ans_obtained <- mx_to_ex(mx = mx,
                             age_group = c("0", "1-4", "five", "open"),
                             sex = "none",
                             ax = c(NA_real_, NA_real_, 3, NA_real_),
                             method = "const")
    px <- rbind(exp(-mx[1, ]), exp(-4 * mx[2, ]), 1 - (5 * mx[3,])/(1 + 2 * mx[3,]))
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ], px[1, ] * px[2, ] * px[3, ])
    Lx <- rbind((lx[-4,] / mx[-4,]) - (lx[-1,] / mx[-4,]), (lx[4, ] / mx[4, ]))
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex_const' gives correct answer, with mx of 0", {
    mx <- cbind(c(0.011, 0.01, 0.05, 0.25),
                c(0.012, 0.015, 0, 0.4))
    ans_obtained <- mx_to_ex(mx = mx,
                             age_group = c("0", "1-4", "five", "open"),
                             sex = "none",
                             ax = c(NA_real_, NA_real_, NA_real_, NA_real_),
                             method = "const")
    px <- rbind(exp(-mx[1, ]), exp(-4 * mx[2, ]), exp(-5 * mx[3, ]))
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ], px[1, ] * px[2, ] * px[3, ])
    Lx <- rbind((lx[-4,] / mx[-4,]) - (lx[-1,] / mx[-4,]), (lx[4, ] / mx[4, ]))
    Lx[3,2] <- lx[3,2] * 5
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex_const' gives correct answer, with mx with single row", {
    mx <- matrix(c(0.2, 0.3), nr = 1)
    ans_obtained <- mx_to_ex(mx = mx,
                             age_group_type = "open",
                             sex = "none",
                             ax = NA_real_,
                             method = "const")
    ans_expected <- as.double(1 / mx)
    expect_equal(ans_obtained, ans_expected)
})


## 'mx_to_ex_cd' --------------------------------------------------------------

test_that("'mx_to_ex_cd' gives correct answer - Female, m0 >= 0.107", {
    mx <- cbind(c(0.107, 0.023, 0.25),
                c(0.2, 0.04, 0.43))
    ans_obtained <- mx_to_ex(mx,
                             age_group_type = c("0", "1-4", "open"),
                             sex = rep("Female", 3),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "CD")
    a0 <- 0.35
    a1 <- 1.361
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                4 * lx[3,] + a1 * (lx[2,] - lx[3,]),
                lx[3, ] / mx[3, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex_cd' gives correct answer - Female, m0 < 0.107", {
    mx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- mx_to_ex(mx,
                             age_group_type = c("0", "1-4", "open"),
                             sex = rep("Female", 3),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "CD")
    a0 <- 0.053 + 2.8 * mx[1,]
    a1 <- 1.522 - 1.518 * mx[1,]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                4 * lx[3,] + a1 * (lx[2,] - lx[3,]),
                lx[3, ] / mx[3, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex_cd' gives correct answer - Male, m0 >= 0.107", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- mx_to_ex(mx,
                             age_group_type = c("0", "1-4", "open"),
                             sex = rep("Male", 3),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "CD")
    a0 <- 0.33
    a1 <- 1.352
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                4 * lx[3,] + a1 * (lx[2,] - lx[3,]),
                lx[3, ] / mx[3, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex_cd' gives correct answer - Male, m0 < 0.107", {
    mx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- mx_to_ex(mx,
                             age_group_type = c("0", "1-4", "open"),
                             sex = rep("Male", 3),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "CD")
    a0 <- 0.045 + 2.684 * mx[1, ]
    a1 <- 1.651 - 2.816 * mx[1, ]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                4 * lx[3,] + a1 * (lx[2,] - lx[3,]),
                lx[3, ] / mx[3, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex_cd' gives correct answer - has NA", {
    mx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, NA, 0.4))
    ans_obtained <- mx_to_ex(mx,
                             age_group_type = c("0", "1-4", "open"),
                             sex = rep("Male", 3),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "CD")
    a0 <- 0.045 + 2.684 * mx[1, ]
    a1 <- 1.651 - 2.816 * mx[1, ]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                4 * lx[3,] + a1 * (lx[2,] - lx[3,]),
                lx[3, ] / mx[3, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
    expect_identical(ans_expected[[2]], NA_real_)
})


## 'mx_to_ex_hmd' -------------------------------------------------------------

test_that("'mx_to_ex_hmd' gives correct answer - Female, m0 >= 0.06891", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- mx_to_ex(mx,
                             age_group_type = c("0", "single", "open"),
                             sex = rep("Female", 3),
                             ax = rep(NA_real_, 3),
                             method = "HMD")
    a0 <- 0.31411
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                mx[2, ] / (1 + 0.5 * mx[2, ]),
                mx[3, ] / (1 + 0.5 * mx[3, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                0.5 * (lx[2, ] + lx[3, ]),
                lx[3, ] / mx[3, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex_hmd' gives correct answer - Female, 0.01724 <= m0 < 0.06891", {
    mx <- cbind(c(0.01724, 0.01, 0.25),
                c(0.02, 0.015, 0.4))
    ans_obtained <- mx_to_ex(mx,
                             age_group_type = c("0", "single", "open"),
                             sex = rep("Female", 3),
                             ax = rep(NA_real_, 3),
                             method = "HMD")
    a0 <- 0.04667 + 3.88089 * mx[1, ]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                mx[2, ] / (1 + 0.5 * mx[2, ]),
                mx[3, ] / (1 + 0.5 * mx[3, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                0.5 * (lx[2, ] + lx[3, ]),
                lx[3, ] / mx[3, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex_hmd' gives correct answer - Female, m0 < 0.01724", {
    mx <- cbind(c(0.01723, 0.01, 0.25),
                c(0.001, 0.015, 0.4))
    ans_obtained <- mx_to_ex(mx,
                             age_group_type = c("0", "single", "open"),
                             sex = rep("Female", 3),
                             ax = rep(NA_real_, 3),
                             method = "HMD")
    a0 <- 0.14903 - 2.05527 * mx[1, ]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                mx[2, ] / (1 + 0.5 * mx[2, ]),
                mx[3, ] / (1 + 0.5 * mx[3, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                0.5 * (lx[2, ] + lx[3, ]),
                lx[3, ] / mx[3, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex_hmd' gives correct answer - 3+ age groups, ax = HMD-Male, m0 >= 0.08307", {
    mx <- cbind(c(0.08307, 0.01, 0.25),
                c(0.25, 0.015, 0.4))
    ans_obtained <- mx_to_ex(mx,
                             age_group_type = c("0", "single", "open"),
                             sex = rep("Male", 3),
                             ax = rep(NA_real_, 3),
                             method = "HMD")
    a0 <- 0.29915
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                mx[2, ] / (1 + 0.5 * mx[2, ]),
                mx[3, ] / (1 + 0.5 * mx[3, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                0.5 * (lx[2, ] + lx[3, ]),
                lx[3, ] / mx[3, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex_hmd' gives correct answer - Male, 0.023 <= m0 < 0.08307", {
    mx <- cbind(c(0.023, 0.01, 0.25),
                c(0.05, 0.015, 0.4))
    ans_obtained <- mx_to_ex(mx,
                             age_group_type = c("0", "single", "open"),
                             sex = rep("Male", 3),
                             ax = rep(NA_real_, 3),
                             method = "HMD")
    a0 <- 0.02832 + 3.26021 * mx[1, ]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                mx[2, ] / (1 + 0.5 * mx[2, ]),
                mx[3, ] / (1 + 0.5 * mx[3, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                0.5 * (lx[2, ] + lx[3, ]),
                lx[3, ] / mx[3, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex_hmd' gives correct answer Male, m0 < 0.0230", {
    mx <- cbind(c(0.02299, 0.01, 0.25),
                c(0.001, 0.015, 0.4))
    ans_obtained <- mx_to_ex(mx,
                             age_group_type = c("0", "single", "open"),
                             sex = rep("Male", 3),
                             ax = rep(NA_real_, 3),
                             method = "HMD")
    a0 <- 0.14929 - 1.99545 * mx[1, ]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                mx[2, ] / (1 + 0.5 * mx[2, ]),
                mx[3, ] / (1 + 0.5 * mx[3, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                0.5 * (lx[2, ] + lx[3, ]),
                lx[3, ] / mx[3, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex_hmd' gives correct answer - has NA", {
    mx <- cbind(c(0.02299, 0.01, 0.25),
                c(0.001, 0.015, NA))
    ans_obtained <- mx_to_ex(mx,
                             age_group_type = c("0", "single", "open"),
                             sex = rep("Male", 3),
                             ax = rep(NA_real_, 3),
                             method = "HMD")
    a0 <- 0.14929 - 1.99545 * mx[1, ]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                mx[2, ] / (1 + 0.5 * mx[2, ]),
                mx[3, ] / (1 + 0.5 * mx[3, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                0.5 * (lx[2, ] + lx[3, ]),
                lx[3, ] / mx[3, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})


## 'mx_to_ex_mid' -------------------------------------------------------------

test_that("'mx_to_ex_mid' gives correct answer, with ax supplied", {
    mx <- cbind(c(0.011, 0.01, 0.05, 0.25),
                c(0.012, 0.015, 0.06, 0.4))
    ans_obtained <- mx_to_ex(mx = mx,
                             age_group = c("0", "1-4", "five", "open"),
                             sex = "none",
                             ax = c(NA_real_, NA_real_, 3, NA_real_),
                             method = "mid")
    ax <- matrix(rep(c(0.5, 2, 3, Inf), each = 2), nr = 4, byrow = TRUE)
    qx <- rbind(mx[1,] / (1 + 0.5 * mx[1,]),
                4 * mx[2,] / (1 + 2 * mx[2,]),
                5 * mx[3,] / (1 + 2 * mx[3,]),
                1)
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ], px[1, ] * px[2, ] * px[3, ])
    dx <- rbind(lx[-4,] - lx[-1,], lx[4,])
    Lx <- rbind(ax[-4,] * dx[-4,] + lx[-1,] * c(1, 4, 5), lx[4,]/mx[4,])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex_mid' gives correct answer, with mx of 0", {
    mx <- cbind(c(0.011, 0.01, 0, 0.25),
                c(0.012, 0.015, 0.06, 0.4))
    ans_obtained <- mx_to_ex(mx = mx,
                             age_group = c("0", "1-4", "five", "open"),
                             sex = "none",
                             ax = c(NA_real_, NA_real_, 3, NA_real_),
                             method = "mid")
    ax <- matrix(rep(c(0.5, 2, 3, Inf), each = 2), nr = 4, byrow = TRUE)
    qx <- rbind(mx[1,] / (1 + 0.5 * mx[1,]),
                4 * mx[2,] / (1 + 2 * mx[2,]),
                5 * mx[3,] / (1 + 2 * mx[3,]),
                1)
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ], px[1, ] * px[2, ] * px[3, ])
    dx <- rbind(lx[-4,] - lx[-1,], lx[4,])
    Lx <- rbind(ax[-4,] * dx[-4,] + lx[-1,] * c(1, 4, 5), lx[4,]/mx[4,])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex_mid' gives correct answer, with mx of NA", {
    mx <- cbind(c(0.011, 0.01, 0.2, 0.25),
                c(0.012, NA, 0.06, 0.4))
    ans_obtained <- mx_to_ex(mx = mx,
                             age_group = c("0", "1-4", "five", "open"),
                             sex = "none",
                             ax = c(NA_real_, NA_real_, 3, NA_real_),
                             method = "mid")
    ax <- matrix(rep(c(0.5, 2, 3, Inf), each = 2), nr = 4, byrow = TRUE)
    qx <- rbind(mx[1,] / (1 + 0.5 * mx[1,]),
                4 * mx[2,] / (1 + 2 * mx[2,]),
                5 * mx[3,] / (1 + 2 * mx[3,]),
                1)
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ], px[1, ] * px[2, ] * px[3, ])
    dx <- rbind(lx[-4,] - lx[-1,], lx[4,])
    Lx <- rbind(ax[-4,] * dx[-4,] + lx[-1,] * c(1, 4, 5), lx[4,]/mx[4,])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex_mid' gives correct answer, with mx with single row", {
    mx <- matrix(c(0.2, 0.3), nr = 1)
    ans_obtained <- mx_to_ex(mx = mx,
                             age_group = "open",
                             sex = "none",
                             ax = NA_real_,
                             method = "mid")
    ans_expected <- as.double(1 / mx)
    expect_equal(ans_obtained, ans_expected)
})
