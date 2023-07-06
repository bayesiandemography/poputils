
## 'lifeexp_inner' ------------------------------------------------------------

test_that("lifeexp_inner works with valid inputs - mx has one col, age is single with min 0, method is const", {
    mx <- matrix(c(0.2, 0.1, 0.05, 0.3), nc = 1)
    ans_obtained <- lifeexp_inner(mx = mx,
                                   age = c("0", "1", "2", "3+"),
                                   sex = "Female",
                                   method = "const")
    ans_expected <- .lifeexp(mx, age_groups = "single", method = "const")
    expect_identical(ans_obtained, ans_expected)
})

test_that("lifeexp_inner works with valid inputs - mx has two cols, age is single with min 0, method is const", {
    mx <- matrix(c(0.2, 0.1, 0.05, 0.3,
                   0.25, 0.15, 0.1, 0.35),
                 nrow = 4, ncol = 2)
    ans_obtained <- lifeexp_inner(mx = mx,
                                   age = c("0", "1", "2", "3+"),
                                   sex = "Female",
                                   method = "const")
    ans_expected <- .lifeexp(mx, age_groups = "single", method = "const")
    ans_expected <- rvec::rvec(matrix(ans_expected, nr = 1))
    expect_identical(ans_obtained, ans_expected)
})

test_that("lifeexp_inner works with valid inputs - mx has one col, age is single with min 1, method is const", {
    mx <- matrix(c(0.2, 0.1, 0.05, 0.3), nc = 1)
    ans_obtained <- lifeexp_inner(mx = mx,
                                   age = c("1", "2", "3", "4+"),
                                   sex = "Female",
                                   method = "const")
    ans_expected <- .lifeexp(mx, age_groups = "single", method = "const")
    expect_identical(ans_obtained, ans_expected)
})

test_that("lifeexp_inner works with valid inputs - mx has one col, age is single with min 1, method is mid", {
    mx <- matrix(c(0.2, 0.1, 0.05, 0.3), nc = 1)
    ans_obtained <- lifeexp_inner(mx = mx,
                                   age = c("1", "2", "3", "4+"),
                                   sex = "Female",
                                   method = "mid")
    ans_expected <- .lifeexp(mx, age_groups = "single", method = "mid")
    expect_identical(ans_obtained, ans_expected)
})

test_that("lifeexp_inner works with valid inputs - mx has one col, age is single with min 1, method is HMD", {
    mx <- matrix(c(0.2, 0.1, 0.05, 0.3), nc = 1)
    ans_obtained <- lifeexp_inner(mx = mx,
                                   age = c("1", "2", "3", "4+"),
                                   sex = "Female",
                                   method = "HMD")
    ans_expected <- .lifeexp(mx, age_groups = "single", method = "mid")
    expect_identical(ans_obtained, ans_expected)
})

test_that("lifeexp_inner works with valid inputs - mx has one col, age is single with min 0, method is HMD", {
    mx <- matrix(c(0.2, 0.1, 0.05, 0.3), nc = 1)
    ans_obtained <- lifeexp_inner(mx = mx,
                                   age = c("0", "1", "2", "3+"),
                                   sex = "Female",
                                   method = "HMD")
    ans_expected <- .lifeexp(mx, age_groups = "single", method = "HMD-Female")
    expect_identical(ans_obtained, ans_expected)
})

test_that("lifeexp_inner works with valid inputs - mx has one col, age is single with min 1, method is mid", {
    mx <- matrix(c(0.2, 0.1, 0.05, 0.3), nc = 1)
    ans_obtained <- lifeexp_inner(mx = mx,
                                   age = c("1", "2", "3", "4+"),
                                   sex = "Female",
                                   method = "CD")
    ans_expected <- .lifeexp(mx, age_groups = "single", method = "mid")
    expect_identical(ans_obtained, ans_expected)
})

test_that("lifeexp_inner works with valid inputs - mx has one col, age is lt with min 1, method is mid", {
    mx <- matrix(c(0.2, 0.1, 0.05, 0.3), nc = 1)
    ans_obtained <- lifeexp_inner(mx = mx,
                                   age = c("1-4", "5-9", "10-14", "15+"),
                                   sex = "Female",
                                   method = "mid")
    ans_expected <- .lifeexp(mx, age_groups = "lt", method = "mid")
    expect_identical(ans_obtained, ans_expected)
})

test_that("lifeexp_inner works with valid inputs - mx has one col, age is lt with min 1, method is CD", {
    mx <- matrix(c(0.2, 0.1, 0.05, 0.3), nc = 1)
    ans_obtained <- lifeexp_inner(mx = mx,
                                   age = c("1-4", "5-9", "10-14", "15+"),
                                   sex = "Female",
                                   method = "CD")
    ans_expected <- .lifeexp(rbind(0,mx), age_groups = "lt", method = "CD-Female") - 1
    expect_identical(ans_obtained, ans_expected)
})

test_that("lifeexp_inner works with valid inputs - mx has one col, age is lt with min 5, method is CD", {
    mx <- matrix(c(0.2, 0.1, 0.05, 0.3), nc = 1)
    ans_obtained <- lifeexp_inner(mx = mx,
                                   age = c("5-9", "10-14", "15-19", "20+"),
                                   sex = "Female",
                                   method = "CD")
    ans_expected <- .lifeexp(mx, age_groups = "five", method = "mid")
    expect_identical(ans_obtained, ans_expected)
})

test_that("lifeexp_inner works with valid inputs - mx has one col, age is lt with min 5, method is CD", {
    mx <- matrix(c(0.2, 0.1, 0.05, 0.3), nc = 1)
    ans_obtained <- lifeexp_inner(mx = mx,
                                   age = c("5-9", "10-14", "15-19", "20+"),
                                   sex = "Female",
                                   method = "CD")
    ans_expected <- .lifeexp(mx, age_groups = "five", method = "mid")
    expect_identical(ans_obtained, ans_expected)
})


## '.lifeexp' -----------------------------------------------------------------

test_that(".lifeexp works", {
    mx <- matrix(c(0.2, 0.1, 0.05, 0.3), nc = 1)
    for (age_groups in c("single", "five", "lt")) {
        for (method in c("const", "mid", "CD-Female", "CD-Male", "HMD-Female", "HMD-Male")) {
            invalid_five <- (age_groups == "five") && (method %in% c("CD-Female", "CD-Male", "HMD-Female", "HMD-Male"))
            invalid_single <- (age_groups == "lt") && (method %in% c("HMD-Female", "HMD-Male"))
            if (!invalid_five && !invalid_single) {
                ans <- .lifeexp(mx = mx,
                                age_groups = age_groups,
                                method = method)
                expect_true(ans > 0)
            }
        }
    }
})
                            
