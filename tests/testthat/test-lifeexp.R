
## 'lifeexp_inner' ------------------------------------------------------------

test_that("lifeexp_inner works with valid inputs - mx has one col, age is single with at = 0, method is const", {
    mx <- matrix(c(0.2, 0.1, 0.05, 0.3), nc = 1)
    ans_obtained <- lifeexp_inner(mx = mx,
                                  age = c("0", "1", "2", "3+"),
                                  sex = "Female",
                                  method = "const",
                                  at = 0)
    ans_expected <- .lifeexp(mx, age_group_type = "single", method = "const")
    expect_identical(ans_obtained, ans_expected)
})

test_that("lifeexp_inner works with valid inputs - mx has two cols, age is single with at = 0, method is const", {
    mx <- matrix(c(0.2, 0.1, 0.05, 0.3,
                   0.25, 0.15, 0.1, 0.35),
                 nrow = 4, ncol = 2)
    ans_obtained <- lifeexp_inner(mx = mx,
                                  age = c("0", "1", "2", "3+"),
                                  sex = "Female",
                                  method = "const",
                                  at = 0)
    ans_expected <- .lifeexp(mx, age_group_type = "single", method = "const")
    ans_expected <- rvec::rvec(matrix(ans_expected, nr = 1))
    expect_identical(ans_obtained, ans_expected)
})

test_that("lifeexp_inner works with valid inputs - mx has one col, age is single with min 1, method is const", {
    mx <- matrix(c(0.2, 0.1, 0.05, 0.3), nc = 1)
    ans_obtained <- lifeexp_inner(mx = mx,
                                  age = c("1", "2", "3", "4+"),
                                  sex = "Female",
                                  method = "const",
                                  at = 1)
    ans_expected <- .lifeexp(mx, age_group_type = "single", method = "const")
    expect_identical(ans_obtained, ans_expected)
})

test_that("lifeexp_inner works with valid inputs - mx has one col, age is single with min 1, method is mid", {
    mx <- matrix(c(0.2, 0.1, 0.05, 0.3), nc = 1)
    ans_obtained <- lifeexp_inner(mx = mx,
                                  age = c("1", "2", "3", "4+"),
                                  sex = "Female",
                                  method = "mid",
                                  at = 1)
    ans_expected <- .lifeexp(mx, age_group_type = "single", method = "mid")
    expect_identical(ans_obtained, ans_expected)
})

test_that("lifeexp_inner works with valid inputs - mx has one col, age is single with min 1, method is HMD", {
    mx <- matrix(c(0.5, 0.2, 0.1, 0.05, 0.3), nc = 1)
    ans_obtained <- lifeexp_inner(mx = mx,
                                  age = c("0", "1", "2", "3", "4+"),
                                  sex = "Female",
                                  method = "HMD",
                                  at = 1)
    ans_expected <- .lifeexp(mx, age_group_type = "single", method = "mid")
    expect_identical(ans_obtained, ans_expected)
})

test_that("lifeexp_inner works with valid inputs - mx has one col, age is single with min 0, method is HMD", {
    mx <- matrix(c(0.2, 0.1, 0.05, 0.3), nc = 1)
    ans_obtained <- lifeexp_inner(mx = mx,
                                  age = c("0", "1", "2", "3+"),
                                  sex = "Female",
                                  method = "HMD",
                                  at = 0)
    ans_expected <- .lifeexp(mx, age_group_type = "single", method = "HMD-Female")
    expect_identical(ans_obtained, ans_expected)
})

test_that("lifeexp_inner works with valid inputs - mx has one col, age is single with min 1, method is mid", {
    mx <- matrix(c(0.2, 0.1, 0.05, 0.3), nc = 1)
    ans_obtained <- lifeexp_inner(mx = mx,
                                  age = c("1", "2", "3", "4+"),
                                  sex = "Female",
                                  method = "CD",
                                  at = 1)
    ans_expected <- .lifeexp(mx, age_group_type = "single", method = "mid")
    expect_identical(ans_obtained, ans_expected)
})

test_that("lifeexp_inner works with valid inputs - mx has one col, age is lt with min 1, method is mid", {
    mx <- matrix(c(0.2, 0.1, 0.05, 0.3), nc = 1)
    ans_obtained <- lifeexp_inner(mx = mx,
                                  age = c("1-4", "5-9", "10-14", "15+"),
                                  sex = "Female",
                                  method = "mid",
                                  at = 1)
    ans_expected <- .lifeexp(mx, age_group_type = "lt", method = "mid")
    expect_identical(ans_obtained, ans_expected)
})

test_that("lifeexp_inner works with valid inputs - mx has one col, age is lt with min 1, method is CD", {
    mx <- matrix(c(0.2, 0.1, 0.05, 0.3), nc = 1)
    ans_obtained <- lifeexp_inner(mx = mx,
                                  age = c("1-4", "5-9", "10-14", "15+"),
                                  sex = "Female",
                                  method = "CD",
                                  at = 1)
    ans_expected <- .lifeexp(rbind(0,mx), age_group_type = "lt", method = "CD-Female") - 1
    expect_identical(ans_obtained, ans_expected)
})

test_that("lifeexp_inner works with valid inputs - mx has one col, age is lt with min 5, method is CD", {
    mx <- matrix(c(0.2, 0.1, 0.05, 0.3), nc = 1)
    ans_obtained <- lifeexp_inner(mx = mx,
                                  age = c("5-9", "10-14", "15-19", "20+"),
                                  sex = "Female",
                                  method = "CD",
                                  at = 5)
    ans_expected <- .lifeexp(mx, age_group_type = "five", method = "mid")
    expect_identical(ans_obtained, ans_expected)
})

test_that("lifeexp_inner works with valid inputs - mx has one col, age is lt with min 5, method is CD", {
    mx <- matrix(c(0.3, 0.2, 0.1, 0.05, 0.3), nc = 1)
    ans_obtained <- lifeexp_inner(mx = mx,
                                  age = c("0-4", "5-9", "10-14", "15-19", "20+"),
                                  sex = "Female",
                                  method = "CD",
                                  at = 5)
    ans_expected <- .lifeexp(mx, age_group_type = "five", method = "mid")
    expect_identical(ans_obtained, ans_expected)
})

test_that("'lifeexp_inner' throws correct error with min age > at", {
    mx <- matrix(c(0.2, 0.1, 0.05, 0.3), nc = 1)
    expect_error(lifeexp_inner(mx = mx,
                               age = c("5-9", "10-14", "15-19", "20+"),
                               sex = "Female",
                               method = "CD",
                               at = 0),
                 "`at` less than lower limit of youngest age group.")
})

test_that("'lifeexp_inner' throws correct error with at not a lower limit", {
    mx <- matrix(c(0.2, 0.1, 0.05, 0.3), nc = 1)
    expect_error(lifeexp_inner(mx = mx,
                               age = c("5-9", "10-14", "15-19", "20+"),
                               sex = "Female",
                               method = "CD",
                               at = 6),
                 "`at` is not the lower limit of an age group.")
})

    
## '.lifeexp' -----------------------------------------------------------------

test_that(".lifeexp works", {
    mx <- matrix(c(0.2, 0.1, 0.05, 0.3), nc = 1)
    for (age_group_type in c("single", "five", "lt")) {
        for (method in c("const", "mid", "CD-Female", "CD-Male", "HMD-Female", "HMD-Male")) {
            invalid_five <- (age_group_type == "five") && (method %in% c("CD-Female", "CD-Male", "HMD-Female", "HMD-Male"))
            invalid_single <- (age_group_type == "lt") && (method %in% c("HMD-Female", "HMD-Male"))
            if (!invalid_five && !invalid_single) {
                ans <- .lifeexp(mx = mx,
                                age_group_type = age_group_type,
                                method = method)
                expect_true(ans > 0)
            }
        }
    }
})

