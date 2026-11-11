
## '.stable_composition_one_sex' ----------------------------------------------

test_that("'.stable_composition_one_sex' works with numeric inputs", {
  Lx <- c(4.6, 4.5, 4.5, 4.4, 20)
  age_mid <- c(2.5, 7.5, 12.5, 17.5, 30)
  r <- 0.02
  ans <- .stable_composition_one_sex(Lx = Lx, age_mid = age_mid, r = r)
  expected <- exp(-r * age_mid) * Lx
  expected <- expected / sum(expected)
  expect_identical(class(ans), "numeric")
  expect_equal(ans, expected)
  expect_equal(sum(ans), 1)
})

test_that("'.stable_composition_one_sex' with r = 0 gives Lx / sum(Lx)", {
  Lx <- c(4.6, 4.5, 4.5, 4.4, 20)
  age_mid <- c(2.5, 7.5, 12.5, 17.5, 30)
  ans <- .stable_composition_one_sex(Lx = Lx, age_mid = age_mid, r = 0)
  expect_equal(ans, Lx / sum(Lx))
})

test_that("'.stable_composition_one_sex' does not depend on radix", {
  Lx <- c(4.6, 4.5, 4.5, 4.4, 20)
  age_mid <- c(2.5, 7.5, 12.5, 17.5, 30)
  expect_equal(.stable_composition_one_sex(Lx = Lx, age_mid = age_mid, r = 0.01),
               .stable_composition_one_sex(Lx = 1000 * Lx, age_mid = age_mid, r = 0.01))
})

test_that("'.stable_composition_one_sex' works with rvec r", {
  Lx <- c(4.6, 4.5, 4.5, 4.4, 20)
  age_mid <- c(2.5, 7.5, 12.5, 17.5, 30)
  r <- rvec::rvec(matrix(c(-0.01, 0, 0.03), nrow = 1))
  ans <- .stable_composition_one_sex(Lx = Lx, age_mid = age_mid, r = r)
  expect_true(rvec::is_rvec(ans))
  expect_identical(rvec::n_draw(ans), 3L)
  m <- as.matrix(ans)
  for (i in 1:3)
    expect_equal(m[, i],
                 .stable_composition_one_sex(Lx = Lx,
                                             age_mid = age_mid,
                                             r = c(-0.01, 0, 0.03)[[i]]))
})

test_that("'.stable_composition_one_sex' works with rvec Lx and rvec r", {
  Lx <- rvec::rvec(matrix(c(4.6, 4.5, 4.5, 4.4, 20,
                            4.2, 4.0, 3.9, 3.7, 12),
                          ncol = 2))
  age_mid <- c(2.5, 7.5, 12.5, 17.5, 30)
  r <- rvec::rvec(matrix(c(0.01, 0.03), nrow = 1))
  ans <- .stable_composition_one_sex(Lx = Lx, age_mid = age_mid, r = r)
  m <- as.matrix(ans)
  for (i in 1:2)
    expect_equal(m[, i],
                 .stable_composition_one_sex(Lx = as.matrix(Lx)[, i],
                                             age_mid = age_mid,
                                             r = c(0.01, 0.03)[[i]]))
})

test_that("'.stable_composition_one_sex' recycles rvecs with one draw", {
  Lx <- rvec::rvec(matrix(c(4.6, 4.5, 4.5, 4.4, 20), ncol = 1))
  age_mid <- c(2.5, 7.5, 12.5, 17.5, 30)
  r <- rvec::rvec(matrix(c(0.01, 0.03), nrow = 1))
  ans <- .stable_composition_one_sex(Lx = Lx, age_mid = age_mid, r = r)
  expect_identical(rvec::n_draw(ans), 2L)
})

test_that("'.stable_composition_one_sex' is consistent with '.intrinsic_growth_rate'", {
  ## in a stable population, the crude birth rate is sum(c * mx),
  ## and, by the renewal equation, also 1 / sum(exp(-r * age) * Lx)
  Lx <- c(4.6, 4.5, 4.5, 4.4, 4.3, 4.2, 4.1, 4.0, 3.8, 30)
  age_mid <- seq(2.5, by = 5, length.out = 10)
  mx <- c(0, 0, 0, 0.04, 0.12, 0.13, 0.09, 0.04, 0, 0)
  r <- .intrinsic_growth_rate(mx = mx, Lx = Lx, age_mid = age_mid)
  ans <- .stable_composition_one_sex(Lx = Lx, age_mid = age_mid, r = r)
  birth_rate <- 1 / sum(exp(-r * age_mid) * Lx)
  expect_equal(sum(ans * mx), birth_rate)
})

test_that("'.stable_composition_one_sex' gives expected errors", {
  Lx <- c(4.6, 4.5, 4.5, 4.4, 20)
  age_mid <- c(2.5, 7.5, 12.5, 17.5, 30)
  expect_error(.stable_composition_one_sex(Lx = Lx[-1], age_mid = age_mid, r = 0),
               "`Lx` and `age_mid` have different lengths.")
  expect_error(.stable_composition_one_sex(Lx = c(-1, Lx[-1]), age_mid = age_mid, r = 0),
               "`Lx` has negative value.")
  expect_error(.stable_composition_one_sex(Lx = c(NA, Lx[-1]), age_mid = age_mid, r = 0),
               "`Lx` has NA.")
  expect_error(.stable_composition_one_sex(Lx = 0 * Lx, age_mid = age_mid, r = 0),
               "`Lx` is 0 for every age group.")
  expect_error(.stable_composition_one_sex(Lx = Lx, age_mid = rev(age_mid), r = 0),
               "`age_mid` not strictly increasing.")
  expect_error(.stable_composition_one_sex(Lx = Lx, age_mid = c(0, 0, 1, 2, 3), r = 0),
               "`age_mid` not strictly increasing.")
  expect_error(.stable_composition_one_sex(Lx = numeric(), age_mid = numeric(), r = 0),
               "`age_mid` has length 0.")
  expect_error(.stable_composition_one_sex(Lx = Lx,
                                           age_mid = rvec::rvec(matrix(age_mid, ncol = 1)),
                                           r = 0),
               "`age_mid` is an rvec.")
  expect_error(.stable_composition_one_sex(Lx = Lx, age_mid = age_mid, r = c(0, 0.1)),
               "`r` does not have length 1.")
  expect_error(.stable_composition_one_sex(Lx = Lx, age_mid = age_mid, r = NA_real_),
               "`r` is NA.")
  expect_error(.stable_composition_one_sex(Lx = Lx, age_mid = age_mid, r = "0"),
               "`r` is non-numeric.")
  expect_error(.stable_composition_one_sex(Lx = rvec::rvec(matrix(1, nrow = 5, ncol = 2)),
                                           age_mid = age_mid,
                                           r = rvec::rvec(matrix(0, nrow = 1, ncol = 3))),
               "Rvecs have different numbers of draws.")
})


## '.stable_composition_two_sex' ----------------------------------------------

test_that("'.stable_composition_two_sex' works with numeric inputs", {
  Lx_female <- c(4.6, 4.5, 4.5, 4.4, 20)
  Lx_male <- c(4.5, 4.4, 4.3, 4.2, 16)
  age_mid <- c(2.5, 7.5, 12.5, 17.5, 30)
  r <- 0.02
  ans <- .stable_composition_two_sex(Lx_female = Lx_female,
                                     Lx_male = Lx_male,
                                     age_mid = age_mid,
                                     r = r,
                                     sex_ratio = 104)
  f <- (100 / 204) * exp(-r * age_mid) * Lx_female
  m <- (104 / 204) * exp(-r * age_mid) * Lx_male
  total <- sum(f) + sum(m)
  expect_identical(names(ans), c("female", "male"))
  expect_equal(ans$female, f / total)
  expect_equal(ans$male, m / total)
  expect_equal(sum(ans$female) + sum(ans$male), 1)
})

test_that("'.stable_composition_two_sex' with identical Lx: within-sex shares match one sex", {
  Lx <- c(4.6, 4.5, 4.5, 4.4, 20)
  age_mid <- c(2.5, 7.5, 12.5, 17.5, 30)
  ans <- .stable_composition_two_sex(Lx_female = Lx,
                                     Lx_male = Lx,
                                     age_mid = age_mid,
                                     r = 0.01)
  one <- .stable_composition_one_sex(Lx = Lx, age_mid = age_mid, r = 0.01)
  expect_equal(ans$female / sum(ans$female), one)
  expect_equal(sum(ans$male) / sum(ans$female), 1.05)
})

test_that("'.stable_composition_two_sex' works with rvecs", {
  Lx_female <- rvec::rvec(matrix(c(4.6, 4.5, 4.5, 4.4, 20,
                                   4.2, 4.0, 3.9, 3.7, 12),
                                 ncol = 2))
  Lx_male <- c(4.5, 4.4, 4.3, 4.2, 16)
  age_mid <- c(2.5, 7.5, 12.5, 17.5, 30)
  r <- rvec::rvec(matrix(c(0.01, 0.03), nrow = 1))
  ans <- .stable_composition_two_sex(Lx_female = Lx_female,
                                     Lx_male = Lx_male,
                                     age_mid = age_mid,
                                     r = r)
  expect_true(rvec::is_rvec(ans$female))
  expect_true(rvec::is_rvec(ans$male))
  for (i in 1:2) {
    ans_i <- .stable_composition_two_sex(Lx_female = as.matrix(Lx_female)[, i],
                                         Lx_male = Lx_male,
                                         age_mid = age_mid,
                                         r = c(0.01, 0.03)[[i]])
    expect_equal(as.matrix(ans$female)[, i], ans_i$female)
    expect_equal(as.matrix(ans$male)[, i], ans_i$male)
  }
})

test_that("'.stable_composition_two_sex' gives expected errors", {
  Lx_female <- c(4.6, 4.5, 4.5, 4.4, 20)
  Lx_male <- c(4.5, 4.4, 4.3, 4.2, 16)
  age_mid <- c(2.5, 7.5, 12.5, 17.5, 30)
  expect_error(.stable_composition_two_sex(Lx_female = Lx_female,
                                           Lx_male = Lx_male[-1],
                                           age_mid = age_mid,
                                           r = 0),
               "`Lx_male` and `age_mid` have different lengths.")
  expect_error(.stable_composition_two_sex(Lx_female = 0 * Lx_female,
                                           Lx_male = 0 * Lx_male,
                                           age_mid = age_mid,
                                           r = 0),
               "`Lx_female` and `Lx_male` are 0 for every age group.")
  expect_error(.stable_composition_two_sex(Lx_female = Lx_female,
                                           Lx_male = Lx_male,
                                           age_mid = age_mid,
                                           r = 0,
                                           sex_ratio = 0),
               "`sex_ratio` is non-positive.")
  expect_error(.stable_composition_two_sex(Lx_female = Lx_female,
                                           Lx_male = Lx_male,
                                           age_mid = age_mid,
                                           r = 0,
                                           sex_ratio = c(100, 105)),
               "`sex_ratio` does not have length 1.")
  expect_error(.stable_composition_two_sex(Lx_female = Lx_female,
                                           Lx_male = Lx_male,
                                           age_mid = age_mid,
                                           r = 0,
                                           sex_ratio = rvec::rvec(matrix(105, nrow = 1))),
               "`sex_ratio` is an rvec.")
  expect_error(.stable_composition_two_sex(Lx_female = rvec::rvec(matrix(1, nrow = 5, ncol = 2)),
                                           Lx_male = rvec::rvec(matrix(1, nrow = 5, ncol = 3)),
                                           age_mid = age_mid,
                                           r = 0),
               "Rvecs have different numbers of draws.")
})


## Helpers --------------------------------------------------------------------

test_that("'align_draws' works with no rvecs", {
  ans <- align_draws(list(a = 1:3, b = 0.5))
  expect_false(ans$has_draws)
  expect_identical(ans$vals$a, matrix(c(1, 2, 3), ncol = 1))
  expect_identical(ans$vals$b, matrix(0.5, ncol = 1))
})

test_that("'align_draws' recycles to common number of draws", {
  a <- rvec::rvec(matrix(1:6, nrow = 3))
  b <- rvec::rvec(matrix(7, nrow = 1))
  ans <- align_draws(list(a = a, b = b, c = c(8, 9, 10)))
  expect_true(ans$has_draws)
  expect_identical(dim(ans$vals$a), c(3L, 2L))
  expect_identical(ans$vals$b, matrix(7, nrow = 1, ncol = 2))
  expect_identical(ans$vals$c, matrix(c(8, 9, 10), nrow = 3, ncol = 2))
})

test_that("'align_draws' error lists draws, and names the calling function", {
  Lx <- rvec::rvec(matrix(1, nrow = 3, ncol = 2))
  r <- rvec::rvec(matrix(0, nrow = 1, ncol = 3))
  expect_snapshot(.stable_composition_one_sex(Lx = Lx, age_mid = 1:3, r = r),
                  error = TRUE)
})

test_that("'stable_return' returns vector or rvec", {
  m <- matrix(1:4 / 10, nrow = 2)
  expect_identical(stable_return(m[, 1, drop = FALSE], has_draws = FALSE), c(0.1, 0.2))
  ans <- stable_return(m, has_draws = TRUE)
  expect_true(rvec::is_rvec(ans))
  expect_identical(as.matrix(ans), m)
})
