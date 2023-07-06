
## C++ functions

## 'le' -----------------------------------------------------------------------

test_that("'le' gives answer with valid combination of inputs, and throws error with invalid", {
    ## tests of whether the answer given is the right answer are found
    ## in the tests for the helper functions
    mx <- matrix(c(0.02, 0.01, 0.5), ncol = 1)
    expect_true(le(mx, age_groups = "lt", method = "const") > 0)
    expect_true(le(mx, age_groups = "lt", method = "mid") > 0)
    expect_true(le(mx, age_groups = "lt", method = "CD-Female") > 0)
    expect_true(le(mx, age_groups = "lt", method = "CD-Male") > 0)
    expect_error(le(mx, age_groups = "lt", method = "HMD-Female"),
                 "unexpected combination of 'age_groups' \\[\"lt\"\\] and 'method' \\[\"HMD-Female\"\\]")
    expect_error(le(mx, age_groups = "lt", method = "HMD-Male"),
                 "unexpected combination of 'age_groups' \\[\"lt\"\\] and 'method' \\[\"HMD-Male\"\\]")
    expect_true(le(mx, age_groups = "single", method = "const") > 0)
    expect_true(le(mx, age_groups = "single", method = "mid") > 0)
    expect_true(le(mx, age_groups = "single", method = "CD-Female") > 0)
    expect_true(le(mx, age_groups = "single", method = "CD-Male") > 0)
    expect_true(le(mx, age_groups = "single", method = "HMD-Female") > 0)
    expect_true(le(mx, age_groups = "single", method = "HMD-Male") > 0)
    expect_true(le(mx, age_groups = "five", method = "const") > 0)
    expect_true(le(mx, age_groups = "five", method = "mid") > 0)
    expect_error(le(mx, age_groups = "five", method = "CD-Female"),
                 "unexpected combination of 'age_groups' \\[\"five\"\\] and 'method' \\[\"CD-Female\"\\]")
    expect_error(le(mx, age_groups = "five", method = "CD-Male"),
                 "unexpected combination of 'age_groups' \\[\"five\"\\] and 'method' \\[\"CD-Male\"\\]")
    expect_error(le(mx, age_groups = "five", method = "HMD-Female"),
                 "unexpected combination of 'age_groups' \\[\"five\"\\] and 'method' \\[\"HMD-Female\"\\]")
    expect_error(le(mx, age_groups = "five", method = "HMD-Male"),
                 "unexpected combination of 'age_groups' \\[\"five\"\\] and 'method' \\[\"HMD-Male\"\\]")
})

test_that("'le' throws correct error message when 'mx' has 0 rows", {
    mx <- matrix(numeric(), ncol = 1)
    expect_error(le(mx = mx, age_groups = "lt", method = "mid"),
                 "'mx' has 0 rows")
})

test_that("'le' throws correct error message when 'age_groups' does not have length 1", {
    mx <- matrix(c(0.1, 0.05), nrow = 1)
    expect_error(le(mx = mx, age_groups = character(), method = "mid"),
                 "'age_groups' has length 0")
    expect_error(le(mx = mx, age_groups = c("lt", "single"), method = "mid"),
                 "'age_groups' has length 2")
})

test_that("'le' throws correct error message when 'age_groups' has invalid value", {
    mx <- matrix(c(0.1, 0.05), nrow = 1)
    expect_error(le(mx = mx, age_groups = "wrong", method = "mid"),
                 "unexpected value for 'age_groups' : \"wrong\"")
    expect_error(le(mx = mx, age_groups = NA_character_, method = "mid"),
                 "unexpected value for 'age_groups' : \"NA\"")
})

test_that("'le' throws correct error message when 'method' does not have length 1", {
    mx <- matrix(c(0.1, 0.05), nrow = 1)
    expect_error(le(mx = mx, age_groups = "lt", method = character()),
                 "'method' has length 0")
    expect_error(le(mx = mx, age_groups = "single", method = c("const", "mid")),
                 "'method' has length 2")
})

test_that("'le' throws correct error message when 'method' has invalid value", {
    mx <- matrix(c(0.1, 0.05), nrow = 1)
    expect_error(le(mx = mx, age_groups = "lt", method = "wrong"),
                 "unexpected value for 'method' : \"wrong\"")
    expect_error(le(mx = mx, age_groups = "lt", method = NA_character_),
                 "unexpected value for 'method' : \"NA\"")
})


## 'le_ax_five' ----------------------------------------------------------

test_that("'le_ax_five' gives correct answer with valid inputs - more than one age group", {
    mx <- cbind(c(0.02, 0.01, 0.5),
                c(0.025, 0.015, 0.4))
    ans_obtained <- le_ax_five(mx)
    qx <- 5 * mx / (1 + 2.5 * mx)
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(2.5 * (lx[-3, ] + lx[-1, ]), lx[3, ] / mx[3, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_five' gives correct answer with valid inputs - one age group", {
    mx <- matrix(c(0.12, 0.05), nc = 2)
    ans_obtained <- le_ax_five(mx)
    ans_expected <- as.numeric(1 / mx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_five' handles mortality rate of 0 correctly - closed age group", {
    mx <- matrix(c(0.12, 0, 0.05), nc = 1)
    ans_obtained <- le_ax_five(mx)
    qx <- 5 * mx / (1 + 2.5 * mx)
    px <- 1 - qx
    lx <- rbind(1, px[1, ], px[1, ])
    Lx <- rbind(2.5 * (lx[1, ] + lx[2, ]), 5 * px[1, ], lx[3, ] / mx[3, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_five' handles mortality rate of 0 correctly - open age group", {
    mx <- matrix(c(0.12, 0.05, 0), nc = 1)
    ans_obtained <- le_ax_five(mx)
    ans_expected <- Inf
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_five' handles mortality rate of Inf correctly - closed age group", {
    mx <- matrix(c(0.12, Inf, 0), nc = 1)
    ans_obtained <- le_ax_five(mx)
    qx <- 5 * mx / (1 + 2.5 * mx)
    px <- 1 - qx
    lx <- rbind(1, px[1, ], 0)
    Lx <- rbind(2.5 * (lx[1, ] + lx[2, ]), 2.5 * lx[2, ], 0)
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_five' handles mortality rate of Inf correctly - open age group", {
    mx <- matrix(c(0.12, 0.05, Inf), nc = 1)
    ans_obtained <- le_ax_five(mx)
    qx <- 5 * mx / (1 + 2.5 * mx)
    px <- 1 - qx
    lx <- rbind(1, px[1, ], px[1, ] * px[2, ])
    Lx <- c(2.5 * (lx[1:2, ] + lx[2:3, ]), 0)
    ans_expected <- sum(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_five' handles mortality rate of NA correctly - closed age group", {
    mx <- matrix(c(0.12, NA, 0), nc = 1)
    ans_obtained <- le_ax_five(mx)
    ans_expected <- NA_real_
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_five' handles mortality rate of NA correctly - open age group", {
    mx <- matrix(c(0.12, 0.05, NA), nc = 1)
    ans_obtained <- le_ax_five(mx)
    ans_expected <- NA_real_
    expect_equal(ans_obtained, ans_expected)
})


## 'le_ax_lt' ------------------------------------------------------------

test_that("'le_ax_lt' gives correct answer - 4+ age groups, ax = mid", {
    mx <- cbind(c(0.011, 0.01, 0.05, 0.25),
                c(0.012, 0.015, 0.06, 0.4))
    ans_obtained <- le_ax_lt(mx, method = "mid")
    qx <- rbind(mx[1, ] / (1 + 0.5 * mx[1, ]),
                4 * mx[2, ] / (1 + 2 * mx[2, ]),
                5 * mx[3, ] / (1 + 2.5 * mx[3, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ], px[1, ] * px[2, ] * px[3, ])
    Lx <- rbind(0.5 * (lx[1, ] + lx[2, ]),
                2 * (lx[2, ] + lx[3, ]),
                2.5 * (lx[3, ] + lx[4, ]),
                lx[4, ] / mx[4, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_lt' gives correct answer - 4+ age groups, ax = CD-Female, m0 >= 0.107", {
    mx <- cbind(c(0.107, 0.01, 0.05, 0.25),
                c(0.11, 0.015, 0.06, 0.4))
    ans_obtained <- le_ax_lt(mx, method = "CD-Female")
    a0 <- 0.35
    a1 <- 1.361
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]),
                5 * mx[3, ] / (1 + 2.5 * mx[3, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ], px[1, ] * px[2, ] * px[3, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                4 * lx[3, ] + a1 * (lx[2, ] - lx[3, ]),
                2.5 * (lx[3, ] + lx[4, ]),
                lx[4, ] / mx[4, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_lt' gives correct answer - 4+ age groups, ax = CD-Female, m0 < 0.107", {
    mx <- cbind(c(0.02, 0.01, 0.05, 0.25),
                c(0.01, 0.015, 0.06, 0.4))
    ans_obtained <- le_ax_lt(mx, method = "CD-Female")
    a0 <- 0.053 + 2.8 * mx[1, ]
    a1 <- 1.522 - 1.518 * mx[1, ]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]),
                5 * mx[3, ] / (1 + 2.5 * mx[3, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ], px[1, ] * px[2, ] * px[3, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                4 * lx[3, ] + a1 * (lx[2, ] - lx[3, ]),
                2.5 * (lx[3, ] + lx[4, ]),
                lx[4, ] / mx[4, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})
 
test_that("'le_ax_lt' gives correct answer - 4+ age groups, ax = CD-Male, m0 >= 0.107", {
    mx <- cbind(c(0.107, 0.01, 0.05, 0.25),
                c(0.11, 0.015, 0.06, 0.4))
    ans_obtained <- le_ax_lt(mx, method = "CD-Male")
    a0 <- 0.33
    a1 <- 1.352
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]),
                5 * mx[3, ] / (1 + 2.5 * mx[3, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ], px[1, ] * px[2, ] * px[3, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                4 * lx[3, ] + a1 * (lx[2, ] - lx[3, ]),
                2.5 * (lx[3, ] + lx[4, ]),
                lx[4, ] / mx[4, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_lt' gives correct answer - 4+ age groups, ax = CD-Male, m0 < 0.107", {
    mx <- cbind(c(0.02, 0.01, 0.05, 0.25),
                c(0.01, 0.015, 0.06, 0.4))
    ans_obtained <- le_ax_lt(mx, method = "CD-Male")
    a0 <- 0.045 + 2.684 * mx[1, ]
    a1 <- 1.651 - 2.816 * mx[1, ]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]),
                5 * mx[3, ] / (1 + 2.5 * mx[3, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ], px[1, ] * px[2, ] * px[3, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                4 * lx[3, ] + a1 * (lx[2, ] - lx[3, ]),
                2.5 * (lx[3, ] + lx[4, ]),
                lx[4, ] / mx[4, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_lt' gives correct answer - 3 age groups, ax = mid", {
    mx <- cbind(c(0.011, 0.01, 0.25),
                c(0.012, 0.015, 0.4))
    ans_obtained <- le_ax_lt(mx, method = "mid")
    qx <- rbind(mx[1, ] / (1 + 0.5 * mx[1, ]),
                4 * mx[2, ] / (1 + 2 * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(0.5 * (lx[1, ] + lx[2, ]),
                2 * (lx[2, ] + lx[3, ]),
                lx[3, ] / mx[3, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_lt' gives correct answer - 3 age groups, ax = CD-Female, m0 >= 0.107", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- le_ax_lt(mx, method = "CD-Female")
    a0 <- 0.35
    a1 <- 1.361
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                4 * lx[3, ] + a1 * (lx[2, ] - lx[3, ]),
                lx[3, ] / mx[3, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_lt' gives correct answer - 3 age groups, ax = CD-Female, m0 < 0.107", {
    mx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- le_ax_lt(mx, method = "CD-Female")
    a0 <- 0.053 + 2.8 * mx[1, ]
    a1 <- 1.522 - 1.518 * mx[1, ]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                4 * lx[3, ] + a1 * (lx[2, ] - lx[3, ]),
                lx[3, ] / mx[3, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_lt' gives correct answer - 3 age groups, ax = CD-Male, m0 >= 0.107", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- le_ax_lt(mx, method = "CD-Male")
    a0 <- 0.33
    a1 <- 1.352
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                4 * lx[3, ] + a1 * (lx[2, ] - lx[3, ]),
                lx[3, ] / mx[3, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_lt' gives correct answer - 3 age groups, ax = CD-Male, m0 < 0.107", {
    mx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- le_ax_lt(mx, method = "CD-Male")
    a0 <- 0.045 + 2.684 * mx[1, ]
    a1 <- 1.651 - 2.816 * mx[1, ]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                4 * lx[3, ] + a1 * (lx[2, ] - lx[3, ]),
                lx[3, ] / mx[3, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_lt' gives correct answer - 2 age groups, ax = mid", {
    mx <- cbind(c(0.011, 0.01),
                c(0.012, 0.015))
    ans_obtained <- le_ax_lt(mx, method = "mid")
    qx <- mx[1, ] / (1 + 0.5 * mx[1, ])
    px <- 1 - qx
    lx <- rbind(c(1, 1), px)
    Lx <- rbind(0.5 * (lx[1, ] + lx[2, ]),
                lx[2, ] / mx[2, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_lt' gives correct answer - 2 age groups, ax = CD-Female, m0 >= 0.107", {
    mx <- cbind(c(0.107, 0.01),
                c(0.11, 0.015))
    ans_obtained <- le_ax_lt(mx, method = "CD-Female")
    a0 <- 0.35
    qx <- mx[1, ] / (1 + (1 - a0) * mx[1, ])
    px <- 1 - qx
    lx <- rbind(c(1, 1), px)
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                lx[2, ] / mx[2, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_lt' gives correct answer - 2 age groups, ax = CD-Female, m0 < 0.107", {
    mx <- cbind(c(0.02, 0.25),
                c(0.01, 0.4))
    ans_obtained <- le_ax_lt(mx, method = "CD-Female")
    a0 <- 0.053 + 2.8 * mx[1, ]
    qx <- mx[1, ] / (1 + (1 - a0) * mx[1, ])
    px <- 1 - qx
    lx <- rbind(c(1, 1), px)
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                lx[2, ] / mx[2, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_lt' gives correct answer - 2 age groups, ax = CD-Male, m0 >= 0.107", {
    mx <- cbind(c(0.107, 0.25),
                c(0.11, 0.4))
    ans_obtained <- le_ax_lt(mx, method = "CD-Male")
    a0 <- 0.33
    qx <- mx[1, ] / (1 + (1 - a0) * mx[1, ])
    px <- 1 - qx
    lx <- rbind(c(1, 1), px)
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                lx[2, ] / mx[2, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_lt' gives correct answer - 2 age groups, ax = CD-Male, m0 < 0.107", {
    mx <- cbind(c(0.02, 0.25),
                c(0.01, 0.4))
    ans_obtained <- le_ax_lt(mx, method = "CD-Male")
    a0 <- 0.045 + 2.684 * mx[1, ]
    qx <- mx[1, ] / (1 + (1 - a0) * mx[1, ])
    px <- 1 - qx
    lx <- rbind(c(1, 1), px)
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                lx[2, ] / mx[2, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_lt' gives correct answer - 1 age group, ax = mid", {
    mx <- matrix(c(0.011, 0.012), nc = 2)
    ans_obtained <- le_ax_lt(mx, method = "mid")
    ans_expected <- colSums(1/mx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_lt' gives correct answer - 1 age group, ax = CD-Female, m0 >= 0.107", {
    mx <- matrix(c(0.107, 0.11), nc = 2)
    ans_obtained <- le_ax_lt(mx, method = "CD-Female")
    ans_expected <- colSums(1/mx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_lt' gives correct answer - 1 age group, ax = CD-Female, m0 < 0.107", {
    mx <- matrix(c(0.02, 0.01), nc = 2)
    ans_obtained <- le_ax_lt(mx, method = "CD-Female")
    ans_expected <- colSums(1/mx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_lt' gives correct answer - 1 age group, ax = CD-Male, m0 >= 0.107", {
    mx <- matrix(c(0.107, 0.11), nc = 2)
    ans_obtained <- le_ax_lt(mx, method = "CD-Male")
    ans_expected <- colSums(1/mx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_lt' gives correct answer - 1 age group, ax = CD-Male, m0 < 0.107", {
    mx <- matrix(c(0.02, 0.01), nc = 2)
    ans_obtained <- le_ax_lt(mx, method = "CD-Male")
    ans_expected <- colSums(1/mx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_lt' handles mortality rate of 0 correctly - closed age group", {
    mx <- matrix(c(0.12, 0, 0.05), nc = 1)
    ans_obtained <- le_ax_lt(mx, method = "mid")
    qx <- mx[1, ] / (1 + 0.5 * mx[1, ])
    px <- 1 - qx
    lx <- c(1, px, px)
    Lx <- c(0.5 * (lx[1] + lx[2]),
            4 * lx[2],
            lx[2] / mx[3])
    ans_expected <- sum(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_lt' handles mortality rate of 0 correctly - open age group", {
    mx <- matrix(c(0.12, 0.05, 0), nc = 1)
    ans_obtained <- le_ax_lt(mx, method = "CD-Male")
    ans_expected <- Inf
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_lt' handles mortality rate of Inf correctly - closed age group", {
    mx <- matrix(c(0.12, Inf, 0), nc = 1)
    ans_obtained <- le_ax_lt(mx, method = "mid")
    qx <- mx[1, ] / (1 + 0.5 * mx[1, ])
    lx = c(1, 1 - qx)
    ans_expected <- mean(lx) + 2 * lx[2]
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_lt' handles mortality rate of Inf correctly - open age group", {
    mx <- matrix(c(0.12, 0.05, Inf), nc = 1)
    ans_obtained <- le_ax_lt(mx, method = "CD-Female")
    a0 <- 0.35
    a1 <- 1.361
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(1, px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                4 * lx[3, ] + a1 * (lx[2, ] - lx[3, ]),
                0)
    ans_expected <- sum(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_lt' handles mortality rate of NA correctly - closed age group", {
    mx <- matrix(c(0.12, NA, 0), nc = 1)
    for (i in 1:3) {
        ans_obtained <- le_ax_lt(mx, method = "mid")
        ans_expected <- NA_real_
        expect_equal(ans_obtained, ans_expected)
    }
})

test_that("'le_ax_lt' handles mortality rate of NA correctly - open age group", {
    mx <- matrix(c(0.12, 0.05, NA), nc = 1)
    for (i in 1:3) {
        ans_obtained <- le_ax_lt(mx, method = "CD-Male")
        ans_expected <- NA_real_
        expect_equal(ans_obtained, ans_expected)
    }
})

test_that("'le_ax_lt' throws correct error when 'mx' is negative", {
    mx <- matrix(c(0.12, 0.05, -0.001), nc = 1)
    for (method in c("mid", "CD-Female", "CD-Male"))
        expect_error(le_ax_lt(mx, method = method),
                     "'mx' has negative value \\[-0.001\\]")
})

test_that("'le_ax_lt' thows error when 'index_method' out of range", {
    mx <- cbind(c(0.02, 0.01, 0.8, 0.25),
                c(0.3, 0.3, 0.3, 0.4))
    expect_error(le_ax_lt(mx, method = "HMD-Female"),
                 "unexpected value for 'method' : \"HMD-Female\"")
})


## 'le_ax_single' --------------------------------------------------------

test_that("'le_ax_single' gives correct answer - 3+ age groups, ax = mid", {
    mx <- cbind(c(0.011, 0.01, 0.25),
                c(0.012, 0.015, 0.4))
    ans_obtained <- le_ax_single(mx, method = "mid")
    qx <- rbind(mx[1, ] / (1 + 0.5 * mx[1, ]),
                mx[2, ] / (1 + 0.5 * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(0.5 * (lx[1, ] + lx[2, ]),
                0.5 * (lx[2, ] + lx[3, ]),
                lx[3, ] / mx[3, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_single' gives correct answer - 3+ age groups, ax = CD-Female, m0 >= 0.107", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- le_ax_single(mx, method = "CD-Female")
    a0 <- 0.35
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

test_that("'le_ax_single' gives correct answer - 3+ age groups, ax = CD-Female, m0 < 0.107", {
    mx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- le_ax_single(mx, method = "CD-Female")
    a0 <- 0.053 + 2.8 * mx[1, ]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                1 * mx[2, ] / (1 + 0.5 * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                0.5 * (lx[2, ] + lx[3, ]),
                lx[3, ] / mx[3, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_single' gives correct answer - 3+ age groups, ax = CD-Male, m0 >= 0.107", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- le_ax_single(mx, method = "CD-Male")
    a0 <- 0.33
    a1 <- 1.352
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                mx[2, ] / (1 + 0.5 * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                0.5 * (lx[2, ] + lx[3, ]),
                lx[3, ] / mx[3, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_single' gives correct answer - 3+ age groups, ax = CD-Male, m0 < 0.107", {
    mx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- le_ax_single(mx, method = "CD-Male")
    a0 <- 0.045 + 2.684 * mx[1, ]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                mx[2, ] / (1 + 0.5 * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                0.5 * (lx[2, ] + lx[3, ]),
                lx[3, ] / mx[3, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_single' gives correct answer - 3+ age groups, ax = HMD-Female, m0 >= 0.06891", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- le_ax_single(mx, method = "HMD-Female")
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

test_that("'le_ax_single' gives correct answer - 3+ age groups, ax = HMD-Female, 0.01724 <= m0 < 0.06891", {
    mx <- cbind(c(0.01724, 0.01, 0.25),
                c(0.02, 0.015, 0.4))
    ans_obtained <- le_ax_single(mx, method = "HMD-Female")
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

test_that("'le_ax_single' gives correct answer - 3+ age groups, ax = HMD-Female, m0 < 0.01724", {
    mx <- cbind(c(0.01723, 0.01, 0.25),
                c(0.001, 0.015, 0.4))
    ans_obtained <- le_ax_single(mx, method = "HMD-Female")
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

test_that("'le_ax_single' gives correct answer - 3+ age groups, ax = HMD-Male, m0 >= 0.08307", {
    mx <- cbind(c(0.08307, 0.01, 0.25),
                c(0.25, 0.015, 0.4))
    ans_obtained <- le_ax_single(mx, method = "HMD-Male")
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

test_that("'le_ax_single' gives correct answer - 3+ age groups, ax = HMD-Male, 0.023 <= m0 < 0.08307", {
    mx <- cbind(c(0.023, 0.01, 0.25),
                c(0.05, 0.015, 0.4))
    ans_obtained <- le_ax_single(mx, method = "HMD-Male")
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

test_that("'le_ax_single' gives correct answer - 3+ age groups, ax = HMD-Male, m0 < 0.0230", {
    mx <- cbind(c(0.02299, 0.01, 0.25),
                c(0.001, 0.015, 0.4))
    ans_obtained <- le_ax_single(mx, method = "HMD-Male")
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

test_that("'le_ax_single' gives correct answer - 2 age groups, ax = mid", {
    mx <- cbind(c(0.011, 0.25),
                c(0.012, 0.4))
    ans_obtained <- le_ax_single(mx, method = "mid")
    qx <- rbind(mx[1, ] / (1 + 0.5 * mx[1, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ])
    Lx <- rbind(0.5 * (lx[1, ] + lx[2, ]),
                lx[2, ] / mx[2, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_single' gives correct answer - 1 age group, ax = mid", {
    mx <- cbind(0.011,
                0.012)
    ans_obtained <- le_ax_single(mx, method = "mid")
    ans_expected <- 1 / mx[1, ]
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_single' handles mortality rate of 0 correctly - first age group", {
    mx <- matrix(c(0, 0.12, 0.05), nc = 1)
    ans_obtained <- le_ax_single(mx, method = "CD-Male")
    qx <- c(0, c(mx[2, ] / (1 + 0.5 * mx[2, ])))
    px <- 1 - qx
    lx <- c(1, px)
    Lx <- c(1, 0.5 * (lx[2] + lx[3]), lx[3]/mx[3])
    ans_expected <- sum(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_single' handles mortality rate of 0 correctly - closed age group", {
    mx <- matrix(c(0.12, 0, 0.05), nc = 1)
    ans_obtained <- le_ax_single(mx, method = "mid")
    qx <- c(mx[1, ] / (1 + 0.5 * mx[1, ]))
    px <- 1 - qx
    lx <- c(1, px, px)
    Lx <- c(0.5 * (lx[1] + lx[2]), lx[2], lx[2]/mx[3])
    ans_expected <- sum(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_single' handles mortality rate of 0 correctly - open age group", {
    mx <- matrix(c(0.12, 0.05, 0), nc = 1)
    ans_obtained <- le_ax_single(mx, method = "HMD-Male")
    ans_expected <- Inf
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_single' handles mortality rate of Inf correctly - first age group", {
    mx <- matrix(c(Inf, 0.12, 0), nc = 1)
    ans_obtained <- le_ax_single(mx, method = "CD-Female")
    ans_expected <- 0.35
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_single' handles mortality rate of Inf correctly - closed age group", {
    mx <- matrix(c(0.12, Inf, 0), nc = 1)
    ans_obtained <- le_ax_single(mx, method = "mid")
    qx <- c(mx[1, ] / (1 + 0.5 * mx[1, ]))
    px <- 1 - qx
    lx <- c(1, px)
    Lx <- c(0.5 * (lx[1] + lx[2]), 0.5 * lx[2])
    ans_expected <- sum(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_single' handles mortality rate of Inf correctly - open age group", {
    mx <- matrix(c(0.12, 0.05, Inf), nc = 1)
    ans_obtained <- le_ax_single(mx, method = "mid")
    qx <- c(mx[, -3] / (1 + 0.5 * mx[, -3]))
    px <- 1 - qx
    lx <- c(1, px[1], px[1] * px[2])
    Lx <- c(0.5 * (lx[-3] + lx[-1]), 0)
    ans_expected <- sum(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_single' handles mortality rate of NA correctly - first age group", {
    mx <- matrix(c(0.12, NA, 0), nc = 1)
    ans_obtained <- le_ax_single(mx, method = "HMD-Male")
    ans_expected <- NA_real_
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_single' handles mortality rate of NA correctly - closed age group", {
    mx <- matrix(c(0.12, NA, 0), nc = 1)
    ans_obtained <- le_ax_single(mx, method = "CD-Male")
    ans_expected <- NA_real_
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_single' handles mortality rate of NA correctly - open age group", {
    mx <- matrix(c(0.12, 0.05, NA), nc = 1)
    ans_obtained <- le_ax_single(mx, method = "CD-Female")
    ans_expected <- NA_real_
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_ax_single' throws correct error when 'mx' is negative", {
    mx <- matrix(c(0.12, 0.05, -0.001), nc = 1)
    expect_error(le_ax_single(mx, method = "mid"),
                 "'mx' has negative value \\[-0.001\\]")
})

test_that("'le_ax_single' thows error when 'method' out of range", {
    mx <- cbind(c(0.02, 0.01, 0.8, 0.25),
                c(0.3, 0.3, 0.3, 0.4))
    expect_error(le_ax_single(mx, method = "wrong"),
                 "unexpected value for 'method' : \"wrong\"")
})


## 'le_const_five' ----------------------------------------------------------

test_that("'le_const_five' gives correct answer with valid inputs - more than one age group", {
    mx <- cbind(c(0.02, 0.01, 0.5),
                c(0.025, 0.015, 0.4))
    ans_obtained <- le_const_five(mx)
    px <- exp(-5 * mx)
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind((lx[-3, ] / mx[-3, ]) - (lx[-1, ] / mx[-3, ]), lx[3, ] / mx[3, ])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_const_five' gives correct answer with valid inputs - one age group", {
    mx <- matrix(c(0.12, 0.05), nc = 2)
    ans_obtained <- le_const_five(mx)
    ans_expected <- as.numeric(1 / mx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_const_five' handles mortality rate of 0 correctly - closed age group", {
    mx <- matrix(c(0.12, 0, 0.05), nc = 1)
    ans_obtained <- le_const_five(mx)
    px <- exp(-5 * mx)
    Lx <- rbind(1/mx[1] - px[1]/mx[1], 5 * px[1], px[1] /mx[3])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_const_five' handles mortality rate of 0 correctly - open age group", {
    mx <- matrix(c(0.12, 0.05, 0), nc = 1)
    ans_obtained <- le_const_five(mx)
    ans_expected <- Inf
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_const_five' handles mortality rate of Inf correctly - closed age group", {
    mx <- matrix(c(0.12, Inf, 0), nc = 1)
    ans_obtained <- le_const_five(mx)
    px <- exp(-5 * mx)
    ans_expected <- 1/mx[1] - px[1]/mx[1]
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_const_five' handles mortality rate of Inf correctly - open age group", {
    mx <- matrix(c(0.12, 0.05, Inf), nc = 1)
    ans_obtained <- le_const_five(mx)
    px <- exp(-5 * mx)
    lx <- rbind(1, px[1, ], px[1, ] * px[2, ])
    Lx <- (lx[-3,] / mx[-3,]) - (lx[-1,] / mx[-3,])
    ans_expected <- sum(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_const_five' handles mortality rate of NA correctly - closed age group", {
    mx <- matrix(c(0.12, NA, 0), nc = 1)
    ans_obtained <- le_const_five(mx)
    ans_expected <- NA_real_
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_const_five' handles mortality rate of NA correctly - open age group", {
    mx <- matrix(c(0.12, 0.05, NA), nc = 1)
    ans_obtained <- le_const_five(mx)
    ans_expected <- NA_real_
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_const_five' throws correct error when 'mx' is negative", {
    mx <- matrix(c(0.12, 0.05, -0.001), nc = 1)
    expect_error(le_const_five(mx),
                 "'mx' has negative value \\[-0.001\\]")
})


## 'le_const_lt' ---------------------------------------------------------

test_that("'le_const_lt' gives correct answer - 4+ age groups, ax = mid", {
    mx <- cbind(c(0.011, 0.01, 0.05, 0.25),
                c(0.012, 0.015, 0.06, 0.4))
    ans_obtained <- le_const_lt(mx)
    px <- rbind(exp(-mx[1, ]), exp(-4 * mx[2, ]), exp(-5 * mx[3, ]))
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ], px[1, ] * px[2, ] * px[3, ])
    Lx <- rbind((lx[-4,] / mx[-4,]) - (lx[-1,] / mx[-4,]), (lx[4, ] / mx[4, ]))
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_const_lt' gives correct answer - 3 age groups, ax = mid", {
    mx <- cbind(c(0.011, 0.01, 0.25),
                c(0.012, 0.015, 0.4))
    ans_obtained <- le_const_lt(mx)
    px <- rbind(exp(-mx[1, ]), exp(-4 * mx[2, ]))
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind((lx[-3,] / mx[-3,]) - (lx[-1,] / mx[-3,]), (lx[3, ] / mx[3, ]))
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_const_lt' agrees with example from MEASURE Evaulation", {
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
    ans_obtained <- le_const_lt(mx)
    ans_expected <- 62.97331
    expect_equal(ans_obtained, ans_expected, tolerance = 0.0001) ## MEASURE calculations involve rounding
})

test_that("'le_const_lt' handles mortality rate of 0 correctly - closed age group", {
    mx <- matrix(c(0.12, 0, 0.05), nc = 1)
    ans_obtained <- le_const_lt(mx)
    px1 <- exp(-mx[1])
    Lx <- rbind(1/mx[1] - px1/mx[1], 4 * px1, px1 / mx[3])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_const_lt' handles mortality rate of 0 correctly - open age group", {
    mx <- matrix(c(0.12, 0.05, 0), nc = 1)
    ans_obtained <- le_const_lt(mx)
    ans_expected <- Inf
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_const_lt' handles mortality rate of Inf correctly - closed age group", {
    mx <- matrix(c(0.12, Inf, 0), nc = 1)
    ans_obtained <- le_const_lt(mx)
    px1 <- exp(-mx[1])
    ans_expected <- 1/mx[1] - px1/mx[1]
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_const_lt' handles mortality rate of Inf correctly - open age group", {
    mx <- matrix(c(0.12, 0.05, Inf), nc = 1)
    ans_obtained <- le_const_lt(mx)
    px1 <- exp(-mx[1])
    px2 <- exp(-4*mx[2])
    lx <- rbind(1, px1, px1 * px2)
    Lx <- (lx[-3,] / mx[-3,]) - (lx[-1,] / mx[-3,])
    ans_expected <- sum(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_const_lt' handles mortality rate of NA correctly - closed age group", {
    mx <- matrix(c(0.12, NA, 0), nc = 1)
    ans_obtained <- le_const_lt(mx)
    ans_expected <- NA_real_
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_const_lt' handles mortality rate of NA correctly - open age group", {
    mx <- matrix(c(0.12, 0.05, NA), nc = 1)
    ans_obtained <- le_const_lt(mx)
    ans_expected <- NA_real_
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_const_lt' throws correct error when 'mx' is negative", {
    mx <- matrix(c(0.12, 0.05, -0.001), nc = 1)
    expect_error(le_const_lt(mx),
                 "'mx' has negative value \\[-0.001\\]")
})


## 'le_const_single' ----------------------------------------------------------

test_that("'le_const_single' gives correct answer with valid inputs - more than one age group", {
    mx <- cbind(c(0.02, 0.01, 0.5),
                c(0.025, 0.015, 0.4))
    ans_obtained <- le_const_single(mx)
    px <- exp(-mx)
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind((lx[-3,] / mx[-3,]) - (lx[-1,] / mx[-3,]), lx[3, ] / mx[3,])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_const_single' gives correct answer with valid inputs - one age group", {
    mx <- matrix(c(0.12, 0.05), nc = 2)
    ans_obtained <- le_const_single(mx)
    ans_expected <- as.numeric(1 / mx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_const_single' handles mortality rate of 0 correctly - closed age group", {
    mx <- matrix(c(0.12, 0, 0.05), nc = 1)
    ans_obtained <- le_const_single(mx)
    px <- exp(-mx)
    Lx <- rbind(1/mx[1] - px[1]/mx[1], px[1], px[1] /mx[3])
    ans_expected <- colSums(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_const_single' handles mortality rate of 0 correctly - open age group", {
    mx <- matrix(c(0.12, 0.05, 0), nc = 1)
    ans_obtained <- le_const_single(mx)
    ans_expected <- Inf
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_const_single' handles mortality rate of Inf correctly - closed age group", {
    mx <- matrix(c(0.12, Inf, 0), nc = 1)
    ans_obtained <- le_const_single(mx)
    px <- exp(-mx)
    ans_expected <- 1/mx[1] - px[1]/mx[1]
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_const_single' handles mortality rate of Inf correctly - open age group", {
    mx <- matrix(c(0.12, 0.05, Inf), nc = 1)
    ans_obtained <- le_const_single(mx)
    px <- exp(-mx)
    lx <- rbind(1, px[1, ], px[1, ] * px[2, ])
    Lx <- (lx[-3,] / mx[-3,]) - (lx[-1,] / mx[-3,])
    ans_expected <- sum(Lx)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_const_single' handles mortality rate of NA correctly - closed age group", {
    mx <- matrix(c(0.12, NA, 0), nc = 1)
    ans_obtained <- le_const_single(mx)
    ans_expected <- NA_real_
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_const_single' handles mortality rate of NA correctly - open age group", {
    mx <- matrix(c(0.12, 0.05, NA), nc = 1)
    ans_obtained <- le_const_single(mx)
    ans_expected <- NA_real_
    expect_equal(ans_obtained, ans_expected)
})

test_that("'le_const_single' throws correct error when 'mx' is negative", {
    mx <- matrix(c(0.12, 0.05, -0.001), nc = 1)
    expect_error(le_const_single(mx),
                 "'mx' has negative value \\[-0.001\\]")
})
