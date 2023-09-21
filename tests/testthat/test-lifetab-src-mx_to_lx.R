

## 'mx_to_lx_cd' --------------------------------------------------------------

test_that("'mx_to_lx' gives correct answer - cd, Female, m0 >= 0.107, no ax supplied", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "CD")
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
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "CD")
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
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(0.5, NA_real_, NA_real_),
                             method = "CD")
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
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "CD")
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
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "CD")
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

test_that("'mx_to_lx' gives correct answer with Inf", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, Inf, 0.4))
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "CD")
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
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "CD")
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
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Male", "Male", "Male"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "CD")
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
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Male", "Male", "Male"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "CD")
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
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "CD")
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
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Male", "Male", "Male"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "CD")
    a0 = 0.045 + 2.684 * mx[1,]
    a1 <- 1.651 - 2.816 * mx[1,]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})



## 'mx_to_lx_const' -----------------------------------------------------------

test_that("'mx_to_lx_const' gives correct answer - no ax supplied", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "const")
    px <- rbind(exp(-mx[1,]), exp(-mx[2,]))
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx_const' gives correct answer - a0 supplied", {
    mx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(0.3, NA_real_, NA_real_),
                             method = "const")
    px <- rbind(1 - mx[1,] / (1 + 0.7 * mx[1,]), exp(-mx[2,]))
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx_const' gives correct answer, no ax, life table age groups", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "const")
    px <- rbind(exp(-mx[1,]), exp(-4 * mx[2,]))
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx_const' gives correct answer with NA", {
    mx <- cbind(c(NA_real_, 0.01, 0.25),
                c(0.11, NA_real_, 0.4))
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "const")
    px <- rbind(exp(-mx[1,]), exp(-4 * mx[2,]))
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
    expect_identical(is.na(ans_obtained), matrix(c(F, T, T, F, F, T), nr = 3))
})

test_that("'mx_to_lx' gives correct answer with Inf", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, Inf, 0.4))
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "const")
    px <- rbind(exp(-mx[1,]), exp(-4 * mx[2,]))
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx_const' gives correct answer with 0", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0, 0.4))
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "const")
    px <- rbind(exp(-mx[1,]), exp(-4 * mx[2,]))
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})


## 'mx_to_lx_hmd' -------------------------------------------------------------

test_that("'mx_to_lx' gives correct answer - hmd, Female, m0 >= 0.06891, no ax supplied", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "HMD")
    a0 <-  0.31411
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                mx[2, ] / (1 + 0.5 * mx[2, ]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx_hmd' gives correct answer - Female, m0 <  0.01724, no ax supplied", {
    mx <- cbind(c(0.015, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "HMD")
    a0 <- 0.14903 - 2.05527 * mx[1, ]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                1 * mx[2, ] / (1 + 0.5 * mx[2, ]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx_hmd' gives correct answer - Female, a0 supplied", {
    mx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(0.4, NA_real_, NA_real_),
                             method = "HMD")
    a0 <- 0.4
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                1 * mx[2, ] / (1 + 0.5 * mx[2, ]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx_hmd' gives correct answer with NA", {
    mx <- cbind(c(NA_real_, 0.01, 0.25),
                c(0.2, NA_real_, 0.4))
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Male", "Male", "Male"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "HMD")
    a0 <- 0.29915
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                1 * mx[2, ] / (1 + 0.5 * mx[2, ]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx_hmd' gives correct answer with Inf", {
    mx <- cbind(c(0.25, 0.01, 0.25),
                c(0.2, Inf, 0.4))
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Male", "Male", "Male"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "HMD")
    a0 <- 0.29915
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                1 * mx[2, ] / (1 + 0.5 * mx[2, ]))
    qx[2,2] <- 1
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx_hmd' gives correct answer with Inf", {
    mx <- cbind(c(0.25, 0.01, 0.25),
                c(0.2, 0, 0.4))
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Male", "Male", "Male"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "HMD")
    a0 <- 0.29915
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                1 * mx[2, ] / (1 + 0.5 * mx[2, ]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})


## 'mx_to_lx_mid' -------------------------------------------------------------

test_that("'mx_to_lx_mid' gives correct answer - no ax supplied", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "mid")
    qx <- rbind(mx[1,] / (1 + 0.5 * mx[1,]),
                mx[2,] / (1 + 0.5 * mx[2,]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx_mid' gives correct answer - a0 supplied", {
    mx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(0.3, NA_real_, NA_real_),
                             method = "mid")
    qx <- rbind(mx[1,] / (1 + 0.7 * mx[1,]),
                mx[2,] / (1 + 0.5 * mx[2,]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx_mid' gives correct answer, no ax, life table age groups", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "mid")
    qx <- rbind(mx[1,] / (1 + 0.5 * mx[1,]),
                4 * mx[2,] / (1 + 2 * mx[2,]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx_mid' gives correct answer with NA", {
    mx <- cbind(c(NA_real_, 0.01, 0.25),
                c(0.11, NA_real_, 0.4))
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "mid")
    qx <- rbind(mx[1,] / (1 + 0.5 * mx[1,]),
                4 * mx[2,] / (1 + 2 * mx[2,]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
    expect_identical(is.na(ans_obtained), matrix(c(F, T, T, F, F, T), nr = 3))
})

test_that("'mx_to_lx' gives correct answer with Inf", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, Inf, 0.4))
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "mid")
    qx <- rbind(mx[1,] / (1 + 0.5 * mx[1,]),
                4 * mx[2,] / (1 + 2 * mx[2,]))
    qx[2,2] <- 1
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx_mid' gives correct answer with 0", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0, 0.4))
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             method = "mid")
    qx <- rbind(mx[1,] / (1 + 0.5 * mx[1,]),
                4 * mx[2,] / (1 + 2 * mx[2,]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})
