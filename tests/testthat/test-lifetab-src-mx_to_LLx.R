
## 'mx_to_Lx_cd' --------------------------------------------------------------

test_that("'mx_to_Lx' gives correct answer - cd, Female, m0 >= 0.107, no ax supplied", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "CD")
    a0 <- 0.35
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                mx[2, ] / (1 + 0.5 * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx * c(a0, 0.5) + lx[-1, ],
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx_cd' gives correct answer - Female, m0 < 0.107, no ax supplied", {
    mx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "CD")
    a0 <- 0.053 + 2.8 * mx[1, ]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                1 * mx[2, ] / (1 + 0.5 * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * 0.5 + lx[3,],
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx_cd' gives correct answer - Female, m0 < 0.107, a0 supplied", {
    mx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(0.5, NA_real_, NA_real_),
                             method = "CD")
    a0 <- 0.5
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                1 * mx[2, ] / (1 + 0.5 * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * 0.5 + lx[3,],
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx_cd' gives correct answer - Female, m0 >= 0.107, no ax, life table age groups", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "CD")
    a0 <- 0.35
    a1 <- 1.361
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * a1 + 4 * lx[3,],
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx_cd' gives correct answer with NA", {
    mx <- cbind(c(NA_real_, 0.01, 0.25),
                c(0.11, NA_real_, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "CD")
    a0 <- 0.35
    a1 <- 1.361
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    lx[2:3,1] <- NA_real_
    lx[3,2] <- NA_real_
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * 0.5 + lx[3,],
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx' gives correct answer with Inf", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, Inf, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "CD")
    a0 <- 0.35
    a1 <- 1.361
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    qx[2,2] <- 1
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * a1 + lx[3,] * 4,
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx_cd' gives correct answer with 0", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "CD")
    a0 <- 0.35
    a1 <- 1.361
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * a1 + lx[3,] * 4,
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx_cd' gives correct answer - Male, m0 >= 0.107, no ax, single year", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Male", "Male", "Male"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "CD")
    a0 <- 0.33
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                mx[2, ] / (1 + 0.5 * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * 0.5 + lx[3,],
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx_cd' gives correct answer - Male, m0 >= 0.107, no ax, life table age groups", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Male", "Male", "Male"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "CD")
    a0 <- 0.33
    a1 <- 1.352
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * a1 + lx[3,] * 4,
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx_cd' gives correct answer - Female, m0 < 0.107, no ax, life table age groups", {
    mx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "CD")
    a0 <- 0.053 + 2.8 * mx[1,]
    a1 <- 1.522 - 1.518 * mx[1,]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * a1 + lx[3,] * 4,
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx_cd' gives correct answer - Male, m0 < 0.107, no ax, life table age groups", {
    mx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Male", "Male", "Male"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "CD")
    a0 = 0.045 + 2.684 * mx[1,]
    a1 <- 1.651 - 2.816 * mx[1,]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * a1 + lx[3,] * 4,
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})


## 'mx_to_Lx_const' -----------------------------------------------------------

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
    age_group_categ <- c("0", "1-4", rep("five", length(mx) - 3), "open")
    ax <- rep(NA_real_, times = length(age_group_categ))
    ans_obtained <- sum(mx_to_Lx(mx = mx,
                                 age_group_categ = age_group_categ,
                                 sex = "none",
                                 ax = ax,
                                 method = "const"))
    ans_expected <- 62.97331
    expect_equal(ans_obtained, ans_expected, tolerance = 0.0001) ## MEASURE calculations involve rounding
})


test_that("'mx_to_Lx_const' gives correct answer, with ax supplied", {
    mx <- cbind(c(0.011, 0.01, 0.05, 0.25),
                c(0.012, 0.015, 0.06, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group = c("0", "1-4", "five", "open"),
                             sex = "none",
                             ax = c(NA_real_, NA_real_, 3, NA_real_),
                             method = "const")
    px <- rbind(exp(-mx[1, ]), exp(-4 * mx[2, ]), 1 - (5 * mx[3,])/(1 + 2 * mx[3,]))
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ], px[1, ] * px[2, ] * px[3, ])
    ans_expected <- rbind((lx[-4,] / mx[-4,]) - (lx[-1,] / mx[-4,]), (lx[4, ] / mx[4, ]))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx_const' gives correct answer, with mx of 0", {
    mx <- cbind(c(0.011, 0.01, 0.05, 0.25),
                c(0.012, 0.015, 0, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group = c("0", "1-4", "five", "open"),
                             sex = "none",
                             ax = c(NA_real_, NA_real_, NA_real_, NA_real_),
                             method = "const")
    px <- rbind(exp(-mx[1, ]), exp(-4 * mx[2, ]), exp(-5 * mx[3, ]))
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ], px[1, ] * px[2, ] * px[3, ])
    ans_expected <- rbind((lx[-4,] / mx[-4,]) - (lx[-1,] / mx[-4,]), (lx[4, ] / mx[4, ]))
    ans_expected[3,2] <- lx[3,2] * 5
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx_const' gives correct answer, with mx with single row", {
    mx <- matrix(c(0.2, 0.3), nr = 1)
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = "open",
                             sex = "none",
                             ax = NA_real_,
                             method = "const")
    ans_expected <- 1 / mx
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx_const' gives correct answer, with mx including NA", {
    mx <- cbind(c(0.011, 0.01, 0.05, 0.25),
                c(0.012, 0.015, NA_real_, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group = c("0", "1-4", "five", "open"),
                             sex = "none",
                             ax = c(NA_real_, NA_real_, NA_real_, NA_real_),
                             method = "const")
    px <- rbind(exp(-mx[1, ]), exp(-4 * mx[2, ]), exp(-5 * mx[3, ]))
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ], px[1, ] * px[2, ] * px[3, ])
    ans_expected <- rbind((lx[-4,] / mx[-4,]) - (lx[-1,] / mx[-4,]), (lx[4, ] / mx[4, ]))
    expect_equal(ans_obtained, ans_expected)
    expect_identical(is.na(ans_obtained),
                     matrix(c(F, F, F, F, F, F, T, T),
                            nrow = 4))
})


## 'mx_to_Lx_hmd' -------------------------------------------------------------

test_that("'mx_to_Lx' gives correct answer - hmd, Female, m0 >= 0.06891, no ax supplied", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "HMD")
    a0 <-  0.31411
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                mx[2, ] / (1 + 0.5 * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * 0.5 + lx[3,],
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx_hmd' gives correct answer - Female, m0 <  0.01724, no ax supplied", {
    mx <- cbind(c(0.015, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "HMD")
    a0 <- 0.14903 - 2.05527 * mx[1, ]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                1 * mx[2, ] / (1 + 0.5 * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * 0.5 + lx[3,],
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx_hmd' gives correct answer - Female, a0 supplied", {
    mx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(0.4, NA_real_, NA_real_),
                             method = "HMD")
    a0 <- 0.4
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                1 * mx[2, ] / (1 + 0.5 * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * 0.5 + lx[3,],
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx_hmd' gives correct answer with NA", {
    mx <- cbind(c(NA_real_, 0.01, 0.25),
                c(0.2, NA_real_, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Male", "Male", "Male"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "HMD")
    a0 <- 0.29915
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                1 * mx[2, ] / (1 + 0.5 * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * 0.5 + lx[3,],
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx_hmd' gives correct answer with Inf", {
    mx <- cbind(c(0.25, 0.01, 0.25),
                c(0.2, Inf, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Male", "Male", "Male"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "HMD")
    a0 <- 0.29915
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                1 * mx[2, ] / (1 + 0.5 * mx[2, ]))
    qx[2,2] <- 1
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * 0.5 + lx[3,],
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx_hmd' gives correct answer with Inf", {
    mx <- cbind(c(0.25, 0.01, 0.25),
                c(0.2, 0, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Male", "Male", "Male"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "HMD")
    a0 <- 0.29915
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                1 * mx[2, ] / (1 + 0.5 * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * 0.5 + lx[3,],
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})


## 'mx_to_Lx_mid' -------------------------------------------------------------

test_that("'mx_to_Lx_mid' gives correct answer - no ax supplied", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "mid")
    qx <- rbind(mx[1,] / (1 + 0.5 * mx[1,]),
                mx[2,] / (1 + 0.5 * mx[2,]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * 0.5 + lx[2, ],
                          dx[2,] * 0.5 + lx[3,],
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx_mid' gives correct answer - a0 supplied", {
    mx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(0.3, NA_real_, NA_real_),
                             method = "mid")
    qx <- rbind(mx[1,] / (1 + 0.7 * mx[1,]),
                mx[2,] / (1 + 0.5 * mx[2,]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * 0.3 + lx[2, ],
                          dx[2,] * 0.5 + lx[3,],
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx_mid' gives correct answer, no ax, life table age groups", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "mid")
    qx <- rbind(mx[1,] / (1 + 0.5 * mx[1,]),
                4 * mx[2,] / (1 + 2 * mx[2,]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * 0.5 + lx[2, ],
                          dx[2,] * 2 + lx[3,] * 4,
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx_mid' gives correct answer with NA", {
    mx <- cbind(c(NA_real_, 0.01, 0.25),
                c(0.11, NA_real_, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "mid")
    qx <- rbind(mx[1,] / (1 + 0.5 * mx[1,]),
                4 * mx[2,] / (1 + 2 * mx[2,]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * 0.5 + lx[2, ],
                          dx[2,] * 2 + lx[3,] * 4,
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
    expect_identical(is.na(ans_obtained), matrix(c(T, T, T, F, T, T), nr = 3))
})

test_that("'mx_to_Lx' gives correct answer with Inf", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, Inf, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "mid")
    qx <- rbind(mx[1,] / (1 + 0.5 * mx[1,]),
                4 * mx[2,] / (1 + 2 * mx[2,]))
    qx[2,2] <- 1
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * 0.5 + lx[2, ],
                          dx[2,] * 2 + lx[3,] * 4,
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx_mid' gives correct answer with 0", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "mid")
    qx <- rbind(mx[1,] / (1 + 0.5 * mx[1,]),
                4 * mx[2,] / (1 + 2 * mx[2,]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * 0.5 + lx[2, ],
                          dx[2,] * 2 + lx[3,] * 4,
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})
