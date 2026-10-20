## 'collapse_dims' ------------------------------------------------------------

test_that("'collapse_dims' sums counts and takes weighted mean of rates", {
  data <- data.frame(age = rep(c("0-4", "5-9"), each = 2),
                     reg = rep(c("a", "b"), times = 2),
                     deaths = 1:4,
                     popn = c(10, 30, 20, 20),
                     mx = c(0.1, 0.2, 0.3, 0.4))
  ans_obtained <- suppressMessages(
    collapse_dims(data, count = deaths, rate = mx, wt = popn, by = age)
  )
  ans_expected <- tibble::tibble(age = c("0-4", "5-9"),
                                 deaths = c(3L, 7L),
                                 popn = c(40, 40),
                                 mx = c((0.1 * 10 + 0.2 * 30) / 40,
                                        (0.3 * 20 + 0.4 * 20) / 40))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'collapse_dims' gives message listing collapsed dimensions", {
  data <- data.frame(age = rep(c("0-4", "5-9"), each = 2),
                     reg = rep(c("a", "b"), times = 2),
                     deaths = 1:4)
  expect_message(collapse_dims(data, count = deaths, by = age),
                 "Collapsing over `reg`.")
  expect_no_message(collapse_dims(data, count = deaths, by = c(age, reg)))
})

test_that("'collapse_dims' works with negation in 'by', removing measures", {
  data <- data.frame(age = rep(c("0-4", "5-9"), each = 2),
                     reg = rep(c("a", "b"), times = 2),
                     deaths = 1:4,
                     popn = c(10, 30, 20, 20),
                     mx = c(0.1, 0.2, 0.3, 0.4))
  ans_neg <- suppressMessages(
    collapse_dims(data, count = deaths, rate = mx, wt = popn, by = !reg)
  )
  ans_pos <- suppressMessages(
    collapse_dims(data, count = deaths, rate = mx, wt = popn, by = age)
  )
  expect_identical(ans_neg, ans_pos)
})

test_that("'collapse_dims' gives same answer with 'by' and with grouped data", {
  skip_if_not_installed("dplyr")
  data <- data.frame(age = rep(c("0-4", "5-9"), each = 2),
                     reg = rep(c("a", "b"), times = 2),
                     deaths = 1:4,
                     popn = c(10, 30, 20, 20),
                     mx = c(0.1, 0.2, 0.3, 0.4))
  ans_by <- suppressMessages(
    collapse_dims(data, count = deaths, rate = mx, wt = popn, by = age)
  )
  ans_groups <- suppressMessages(
    collapse_dims(dplyr::group_by(data, age), count = deaths, rate = mx, wt = popn)
  )
  expect_identical(ans_by, ans_groups)
  expect_false(inherits(ans_groups, "grouped_df"))
})

test_that("'collapse_dims' throws error when 'by' supplied with grouped data", {
  skip_if_not_installed("dplyr")
  data <- data.frame(age = c("0-4", "5-9"), reg = "a", deaths = 1:2)
  data <- dplyr::group_by(data, age)
  expect_error(collapse_dims(data, count = deaths, by = reg),
               "Can't supply `by` when `data` is a grouped data frame.")
})

test_that("'collapse_dims' throws error when grouping variable is a measure", {
  skip_if_not_installed("dplyr")
  data <- data.frame(age = c("0-4", "5-9"), reg = "a", deaths = 1:2)
  data <- dplyr::group_by(data, deaths)
  expect_error(collapse_dims(data, count = deaths),
               "`groups` and `measures` use the same variable.")
})

test_that("'collapse_dims' collapses everything when 'by' is NULL", {
  data <- data.frame(reg = c("a", "b", "c"),
                     deaths = 1:3,
                     popn = c(1, 2, 1),
                     mx = c(0.1, 0.2, 0.3))
  ans_obtained <- suppressMessages(
    collapse_dims(data, count = deaths, rate = mx, wt = popn)
  )
  ans_expected <- tibble::tibble(deaths = 6L,
                                 popn = 4,
                                 mx = (0.1 + 0.4 + 0.3) / 4)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'collapse_dims' keeps columns in original order", {
  data <- data.frame(mx = c(0.1, 0.2),
                     reg = c("a", "b"),
                     popn = c(1, 1),
                     age = "0-4",
                     deaths = 1:2)
  ans <- suppressMessages(
    suppressWarnings(collapse_dims(data, count = deaths, rate = mx, wt = popn, by = age))
  )
  expect_identical(names(ans), c("mx", "popn", "age", "deaths"))
})

test_that("'collapse_dims' keeps factor keys, and first-appearance order", {
  data <- data.frame(age = factor(c("5-9", "0-4", "5-9", "0-4"),
                                  levels = c("0-4", "5-9", "10+")),
                     reg = rep(c("a", "b"), each = 2),
                     deaths = 1:4)
  ans <- suppressMessages(collapse_dims(data, count = deaths, by = age))
  expect_identical(ans$age, factor(c("5-9", "0-4"), levels = c("0-4", "5-9", "10+")))
  expect_identical(ans$deaths, c(4L, 6L))
})

test_that("'collapse_dims' handles several columns of each type", {
  data <- data.frame(reg = c("a", "b"),
                     deaths = 1:2,
                     births = 3:4,
                     popn = c(1, 3),
                     mx = c(0.1, 0.2),
                     fx = c(0.3, 0.4),
                     share = c(0.5, 0.6),
                     age_death = c(70, 80),
                     ratio = c(1, 2))
  ans <- suppressMessages(collapse_dims(data,
                                        count = c(deaths, births),
                                        rate = c(mx, fx),
                                        prob = share,
                                        mean = age_death,
                                        ratio = ratio,
                                        wt = popn))
  expect_equal(ans$deaths, 3L)
  expect_equal(ans$births, 7L)
  expect_equal(ans$mx, (0.1 + 0.6) / 4)
  expect_equal(ans$fx, (0.3 + 1.2) / 4)
  expect_equal(ans$share, (0.5 + 1.8) / 4)
  expect_equal(ans$age_death, (70 + 240) / 4)
  expect_equal(ans$ratio, (1 + 6) / 4)
})

test_that("'collapse_dims' sums rates over shared dimensions, and keeps common weight", {
  data <- data.frame(age = rep(c("0-4", "5-9"), each = 2),
                     cause = rep(c("x", "y"), times = 2),
                     popn = rep(c(10, 20), each = 2),
                     mx = c(0.1, 0.2, 0.3, 0.4))
  ans_obtained <- suppressMessages(
    collapse_dims(data, rate = mx, wt = popn, wt_shared = cause, by = age)
  )
  ans_expected <- tibble::tibble(age = c("0-4", "5-9"),
                                 popn = c(10, 20),
                                 mx = c(0.3, 0.7))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'collapse_dims' collapses shared dimensions before unshared", {
  data <- expand.grid(cause = c("x", "y"),
                      reg = c("a", "b"),
                      KEEP.OUT.ATTRS = FALSE,
                      stringsAsFactors = FALSE)
  data$popn <- rep(c(10, 30), each = 2)
  data$mx <- c(0.1, 0.2, 0.3, 0.4)
  ans <- suppressMessages(
    collapse_dims(data, rate = mx, wt = popn, wt_shared = cause)
  )
  expect_equal(ans$popn, 40)
  expect_equal(ans$mx, ((0.1 + 0.2) * 10 + (0.3 + 0.4) * 30) / 40)
})

test_that("'collapse_dims' allows 'wt_shared' to include dimensions that are kept", {
  data <- data.frame(age = rep(c("0-4", "5-9"), each = 2),
                     cause = rep(c("x", "y"), times = 2),
                     reg = "a",
                     popn = rep(c(10, 20), each = 2),
                     mx = c(0.1, 0.2, 0.3, 0.4))
  ans <- suppressMessages(
    collapse_dims(data, rate = mx, wt = popn, wt_shared = cause, by = c(age, cause))
  )
  expect_equal(ans$mx, data$mx)
  expect_equal(ans$popn, data$popn)
})

test_that("'collapse_dims' throws error when weight varies across shared dimension", {
  data <- data.frame(age = rep(c("0-4", "5-9"), each = 2),
                     cause = rep(c("x", "y"), times = 2),
                     popn = c(10, 10, 20, 21),
                     mx = c(0.1, 0.2, 0.3, 0.4))
  expect_error(suppressMessages(
    collapse_dims(data, rate = mx, wt = popn, wt_shared = cause, by = age)
  ),
  "`popn` varies across `wt_shared` dimension `cause`.")
})

test_that("'collapse_dims' allows small numerical differences in shared weight", {
  data <- data.frame(cause = c("x", "y"),
                     popn = c(10, 10 * (1 + 1e-12)),
                     mx = c(0.1, 0.2))
  ans <- suppressMessages(collapse_dims(data, rate = mx, wt = popn, wt_shared = cause))
  expect_equal(ans$mx, 0.3)
})

test_that("'collapse_dims' throws error when probabilities summed over shared dimension exceed 1", {
  data <- data.frame(cause = c("x", "y"),
                     lx = c(10, 10),
                     qx = c(0.6, 0.5))
  expect_error(suppressMessages(
    collapse_dims(data, prob = qx, wt = lx, wt_shared = cause)
  ),
  "Sum of `qx` over `wt_shared` dimensions is greater than 1.")
})

test_that("'collapse_dims' checks measures", {
  data <- data.frame(reg = c("a", "b"), popn = c(1, 1), x = c(-0.1, 0.1), p = c(0.5, 1.2))
  expect_error(collapse_dims(data, rate = x, wt = popn),
               "`x` has negative value.")
  expect_error(collapse_dims(data, prob = p, wt = popn),
               "`p` has value greater than 1.")
  expect_error(collapse_dims(data, count = reg),
               "`reg` is non-numeric.")
  expect_no_error(suppressMessages(collapse_dims(data, ratio = x, wt = popn)))
  data$popn[1] <- -1
  expect_error(collapse_dims(data, count = x, wt = popn),
               "`popn` has negative value.")
})

test_that("'collapse_dims' checks arguments", {
  data <- data.frame(reg = c("a", "b"), popn = c(1, 2), mx = c(0.1, 0.2), deaths = 1:2)
  expect_error(collapse_dims(data),
               "No measures supplied.")
  expect_error(collapse_dims(data, rate = mx),
               "`rate` supplied, but no value supplied for `wt`.")
  expect_error(collapse_dims(data, count = deaths, wt_shared = reg),
               "`wt_shared` supplied, but no value supplied for `wt`.")
  expect_error(collapse_dims(data, count = deaths, rate = deaths, wt = popn),
               "`count` and `rate` use the same variable.")
  expect_error(collapse_dims(data, rate = mx, wt = c(popn, deaths)),
               "2 variables specified for `wt`.")
  expect_error(collapse_dims(data, rate = mx, wt = popn, wt_shared = mx),
               "`rate` and `wt_shared` use the same variable.")
  expect_error(collapse_dims(1:3, count = deaths),
               "`data` is not a data frame.")
})

test_that("'collapse_dims' throws error when a dimension is an rvec", {
  data <- tibble::tibble(reg = c("a", "b"),
                         deaths = 1:2,
                         x = rvec::rvec(matrix(1:4, 2)))
  expect_error(collapse_dims(data, count = deaths, by = reg),
               "Dimension `x` is an rvec.")
  expect_error(collapse_dims(data, count = deaths, by = c(reg, x)),
               "Dimension `x` is an rvec.")
})

test_that("'collapse_dims' throws error with duplicated dimensions when weights used", {
  data <- data.frame(reg = c("a", "a"), popn = c(1, 2), mx = c(0.1, 0.2))
  expect_error(collapse_dims(data, rate = mx, wt = popn),
               "`data` has two rows with same value for `reg`.")
  data <- data.frame(reg = c("a", "a"), deaths = 1:2)
  ans <- collapse_dims(data, count = deaths, by = reg)
  expect_identical(ans$deaths, 3L)
})

test_that("'collapse_dims' returns NA when total weight is zero", {
  data <- data.frame(age = rep(c("0-4", "5-9"), each = 2),
                     reg = rep(c("a", "b"), times = 2),
                     popn = c(0, 0, 1, 2),
                     mx = c(0.1, 0.2, 0.3, 0.4))
  ans <- suppressMessages(collapse_dims(data, rate = mx, wt = popn, by = age))
  expect_identical(ans$mx[[1]], NA_real_)
  expect_equal(ans$mx[[2]], 1.1 / 3)
})

test_that("'collapse_dims' keeps value when only one row is merged, even with zero weight", {
  data <- data.frame(cause = c("x", "y"), popn = 0, mx = c(0.1, 0.2))
  ans <- suppressMessages(collapse_dims(data, rate = mx, wt = popn, wt_shared = cause))
  expect_equal(ans$mx, 0.3)
  expect_equal(ans$popn, 0)
})

test_that("'collapse_dims' warns when weight is identical across collapsed dimension", {
  data <- data.frame(age = rep(c("0-4", "5-9"), each = 2),
                     cause = rep(c("x", "y"), times = 2),
                     popn = rep(c(10, 20), each = 2),
                     mx = c(0.1, 0.2, 0.3, 0.4))
  expect_warning(suppressMessages(collapse_dims(data, rate = mx, wt = popn, by = age)),
                 "`popn` is identical across the levels of `cause` being collapsed.")
  data$popn[2] <- 11
  expect_no_warning(suppressMessages(collapse_dims(data, rate = mx, wt = popn, by = age)))
})

test_that("'collapse_dims' works with rvec rates and ordinary weights", {
  mx <- rvec::rvec(matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2))
  data <- tibble::tibble(reg = c("a", "b"), popn = c(1, 3), mx = mx, deaths = 1:2)
  ans <- suppressMessages(collapse_dims(data, count = deaths, rate = mx, wt = popn))
  expect_true(rvec::is_rvec(ans$mx))
  expect_equal(as.matrix(ans$mx), matrix(c((0.1 + 0.6) / 4, (0.3 + 1.2) / 4), nrow = 1))
  expect_identical(ans$popn, 4)
  expect_identical(ans$deaths, 3L)
})

test_that("'collapse_dims' works with ordinary rates and rvec weights", {
  popn <- rvec::rvec(matrix(c(1, 3, 3, 1), nrow = 2))
  data <- tibble::tibble(reg = c("a", "b"), popn = popn, mx = c(0.1, 0.2))
  ans <- suppressMessages(collapse_dims(data, rate = mx, wt = popn))
  expect_true(rvec::is_rvec(ans$mx))
  expect_equal(as.matrix(ans$mx), matrix(c(0.7 / 4, 0.5 / 4), nrow = 1))
  expect_equal(as.matrix(ans$popn), matrix(c(4, 4), nrow = 1))
})

test_that("'collapse_dims' throws error when rvecs have different numbers of draws", {
  data <- tibble::tibble(reg = c("a", "b"),
                         popn = rvec::rvec(matrix(1, nrow = 2, ncol = 3)),
                         mx = rvec::rvec(matrix(0.1, nrow = 2, ncol = 2)))
  expect_error(collapse_dims(data, rate = mx, wt = popn),
               "Measures have different numbers of draws.")
})

test_that("'collapse_dims' with rvecs gives same answer as draw-by-draw calculation", {
  set.seed(0)
  n_draw <- 5
  data <- expand.grid(age = c("0-4", "5-9"),
                      reg = c("a", "b", "c"),
                      cause = c("x", "y"),
                      KEEP.OUT.ATTRS = FALSE,
                      stringsAsFactors = FALSE)
  popn <- matrix(runif(6 * n_draw, 10, 20), nrow = 6)
  popn <- popn[rep(1:6, times = 2), ]
  mx <- matrix(runif(12 * n_draw), nrow = 12)
  data <- tibble::as_tibble(data)
  data$popn <- rvec::rvec(popn)
  data$mx <- rvec::rvec(mx)
  ans <- suppressMessages(collapse_dims(data, rate = mx, wt = popn,
                                        wt_shared = cause, by = age))
  for (i_draw in seq_len(n_draw)) {
    data_i <- data
    data_i$popn <- popn[, i_draw]
    data_i$mx <- mx[, i_draw]
    ans_i <- suppressMessages(collapse_dims(data_i, rate = mx, wt = popn,
                                            wt_shared = cause, by = age))
    expect_equal(as.matrix(ans$mx)[, i_draw], ans_i$mx)
    expect_equal(as.matrix(ans$popn)[, i_draw], ans_i$popn)
  }
})


## 'collapse_levels' ----------------------------------------------------------

test_that("'collapse_levels' works with named list", {
  data <- data.frame(reg = c("a", "b", "c"),
                     deaths = 1:3,
                     popn = c(1, 3, 2),
                     mx = c(0.1, 0.2, 0.3))
  ans_obtained <- collapse_levels(data,
                                  dim = reg,
                                  map = list(ab = c("a", "b")),
                                  count = deaths,
                                  rate = mx,
                                  wt = popn)
  ans_expected <- tibble::tibble(reg = c("ab", "c"),
                                 deaths = c(3L, 3L),
                                 popn = c(4, 2),
                                 mx = c(0.7 / 4, 0.3))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'collapse_levels' keeps other dimensions", {
  data <- expand.grid(reg = c("a", "b", "c"),
                      sex = c("f", "m"),
                      KEEP.OUT.ATTRS = FALSE,
                      stringsAsFactors = FALSE)
  data$deaths <- 1:6
  ans <- collapse_levels(data, dim = reg, map = list(ab = c("a", "b")), count = deaths)
  expect_identical(ans$reg, c("ab", "c", "ab", "c"))
  expect_identical(ans$sex, c("f", "f", "m", "m"))
  expect_identical(ans$deaths, c(3L, 3L, 9L, 6L))
})

test_that("'collapse_levels' throws error when list has level not in data", {
  data <- data.frame(reg = c("a", "b"), deaths = 1:2)
  expect_error(collapse_levels(data, dim = reg, map = list(ab = c("a", "B")), count = deaths),
               "`map` has level not found in `dim`: \"B\".")
})

test_that("'collapse_levels' works with data frame", {
  data <- data.frame(reg = c("a", "b", "c"), deaths = 1:3)
  map <- data.frame(old = c("a", "b", "d"), new = c("ab", "ab", "d"))
  ans <- collapse_levels(data, dim = reg, map = map, count = deaths)
  expect_identical(ans$reg, c("ab", "c"))
  expect_identical(ans$deaths, c(3L, 3L))
})

test_that("'collapse_levels' uses levels of factor returned by data frame map", {
  data <- data.frame(age = c("5", "0", "1", "6"), deaths = 1:4)
  map <- data.frame(x = c("0", "1", "5", "6"),
                    y = factor(c("0-4", "0-4", "5-9", "5-9"),
                               levels = c("0-4", "5-9", "10+")))
  ans <- collapse_levels(data, dim = age, map = map, count = deaths)
  expect_identical(ans$age, factor(c("0-4", "5-9"), levels = c("0-4", "5-9", "10+")))
  expect_identical(ans$deaths, c(5L, 5L))
})

test_that("'collapse_levels' works with function", {
  data <- data.frame(age = c("0", "1-4", "5-9", "10+"),
                     deaths = 1:4)
  ans <- collapse_levels(data,
                         dim = age,
                         map = \(x) set_age_open(x, lower = 5),
                         count = deaths)
  expect_identical(ans$age, c("0", "1-4", "5+"))
  expect_identical(ans$deaths, c(1L, 2L, 7L))
})

test_that("'collapse_levels' orders rows by levels of factor returned by function", {
  data <- data.frame(sex = rep(c("m", "f"), each = 3),
                     age = rep(c("b", "a", "c"), 2),
                     deaths = 1:6)
  map <- function(x) factor(ifelse(x == "c", "b", x), levels = c("a", "b", "z"))
  ans <- collapse_levels(data, dim = age, map = map, count = deaths)
  expect_identical(ans$sex, c("m", "m", "f", "f"))
  expect_identical(ans$age, factor(c("a", "b", "a", "b"), levels = c("a", "b", "z")))
  expect_identical(ans$deaths, c(2L, 4L, 5L, 10L))
})

test_that("'collapse_levels' keeps factor dimension, with merged level in position of first old level", {
  data <- data.frame(reg = factor(c("c", "a", "b"), levels = c("a", "b", "c")),
                     deaths = 1:3)
  ans <- collapse_levels(data, dim = reg, map = list(bc = c("b", "c")), count = deaths)
  expect_identical(levels(ans$reg), c("a", "bc"))
  expect_identical(ans$reg, factor(c("a", "bc"), levels = c("a", "bc")))
  expect_identical(ans$deaths, c(2L, 4L))
})

test_that("'collapse_levels' with shared dimension sums rates", {
  data <- data.frame(cause = c("Cancer", "Heart", "Stroke", "Other"),
                     mx = c(0.002, 0.0015, 0.0005, 0.001),
                     popn = 1000)
  ans <- collapse_levels(data,
                         dim = cause,
                         map = list(Circulatory = c("Heart", "Stroke")),
                         rate = mx,
                         wt = popn,
                         wt_shared = cause)
  expect_identical(ans$cause, c("Cancer", "Circulatory", "Other"))
  expect_equal(ans$mx, c(0.002, 0.002, 0.001))
  expect_equal(ans$popn, c(1000, 1000, 1000))
})

test_that("'collapse_levels' handles origin-destination flows", {
  data <- expand.grid(orig = c("a", "b", "c"),
                      dest = c("a", "b", "c"),
                      KEEP.OUT.ATTRS = FALSE,
                      stringsAsFactors = FALSE)
  popn <- c(a = 10, b = 20, c = 30)
  data$popn <- unname(popn[data$orig])
  data$rate <- seq(0.01, 0.09, by = 0.01)
  data$flows <- data$rate * data$popn
  ans <- collapse_levels(data,
                         dim = c(orig, dest),
                         map = list(ab = c("a", "b")),
                         count = flows,
                         rate = rate,
                         wt = popn,
                         wt_shared = dest)
  expect_identical(nrow(ans), 4L)
  ## rates equal flows divided by origin population
  popn_new <- c(ab = 30, c = 30)
  expect_equal(ans$popn, unname(popn_new[ans$orig]))
  expect_equal(ans$rate, ans$flows / ans$popn)
  expect_equal(sum(ans$flows), sum(data$flows))
})

test_that("'collapse_levels' gives same answer as 'collapse_dims' when all levels merged", {
  data <- expand.grid(age = c("0-4", "5-9"),
                      reg = c("a", "b", "c"),
                      KEEP.OUT.ATTRS = FALSE,
                      stringsAsFactors = FALSE)
  data$deaths <- 1:6
  data$popn <- c(10, 20, 30, 40, 50, 60)
  data$mx <- data$deaths / data$popn
  ans_levels <- collapse_levels(data, dim = reg, map = \(x) rep("all", length(x)),
                                count = deaths, rate = mx, wt = popn)
  ans_dims <- suppressMessages(collapse_dims(data, count = deaths, rate = mx, wt = popn, by = age))
  expect_identical(ans_levels$reg, rep("all", 2))
  ans_levels$reg <- NULL
  expect_equal(ans_levels, ans_dims)
})

test_that("'collapse_levels' gives message when no rows merged", {
  data <- data.frame(reg = c("a", "b"), deaths = 1:2)
  expect_message(ans <- collapse_levels(data, dim = reg, map = list(A = "a"), count = deaths),
                 "No rows were merged.")
  expect_identical(ans$reg, c("A", "b"))
})

test_that("'collapse_levels' throws expected errors", {
  skip_if_not_installed("dplyr")
  data <- data.frame(reg = c("a", "b"), sex = "f", deaths = 1:2)
  expect_error(collapse_levels(data, dim = reg, count = deaths),
               "No value supplied for `map`.")
  expect_error(collapse_levels(data, map = list(ab = c("a", "b")), count = deaths),
               "No value supplied for `dim`.")
  expect_error(collapse_levels(data, dim = deaths, map = list(x = 1), count = deaths),
               "`dim` and `measures` use the same variable.")
  expect_error(collapse_levels(dplyr::group_by(data, reg), dim = reg, map = list(ab = c("a", "b")),
                               count = deaths),
               "`dim` and `groups` use the same variable.")
  expect_error(collapse_levels(data, dim = reg, map = "ab", count = deaths),
               "`map` is not a named list, a data frame, or a function.")
})


## Helpers --------------------------------------------------------------------

test_that("'as_plain_tibble' removes grouping", {
  skip_if_not_installed("dplyr")
  data <- dplyr::group_by(data.frame(a = 1:2, b = 3:4), a)
  ans <- as_plain_tibble(data)
  expect_identical(ans, tibble::tibble(a = 1:2, b = 3:4))
})

test_that("'check_prob_le_one' works", {
  expect_true(check_prob_le_one(c(0, 1, NA), nm_x = "p", is_sum = FALSE))
  expect_error(check_prob_le_one(c(0, 1.1), nm_x = "p", is_sum = FALSE),
               "`p` has value greater than 1.")
  expect_error(check_prob_le_one(rvec::rvec(matrix(c(0.5, 1.5), 1)), nm_x = "p", is_sum = TRUE),
               "Sum of `p` over `wt_shared` dimensions is greater than 1.")
})

test_that("'is_const_by_group' works", {
  m <- matrix(c(1, 1, 2, 3, NA, NA), ncol = 1)
  g <- c(1L, 1L, 2L, 2L, 3L, 3L)
  expect_identical(is_const_by_group(m, g), c(TRUE, FALSE, TRUE))
  m <- matrix(c(1, 1, 1, 2), ncol = 2)
  expect_identical(is_const_by_group(m, c(1L, 1L)), FALSE)
  m <- matrix(c(1, NA), ncol = 1)
  expect_identical(is_const_by_group(m, c(1L, 1L)), FALSE)
})

test_that("'sum_by_group' works", {
  m <- matrix(1:6, ncol = 2)
  expect_identical(sum_by_group(m, g = c(2L, 1L, 2L), n_group = 2L),
                   matrix(c(2L, 4L, 5L, 10L), ncol = 2))
})

test_that("'to_matrix_draws' and 'from_matrix_draws' work", {
  x <- rvec::rvec(matrix(1:4, 2))
  expect_identical(from_matrix_draws(to_matrix_draws(x), is_rvec = TRUE), x)
  expect_identical(from_matrix_draws(to_matrix_draws(1:3), is_rvec = FALSE), 1:3)
})

test_that("'expand_draws' works", {
  m <- matrix(1:2, ncol = 1)
  expect_identical(expand_draws(m, n_draw = 3L), matrix(rep(1:2, 3), ncol = 3))
  m <- matrix(1:4, ncol = 2)
  expect_identical(expand_draws(m, n_draw = 2L), m)
})

test_that("'recode_lookup' works with character, factor, and numeric inputs", {
  expect_identical(recode_lookup(c("a", "b", "c"), old = c("a", "b"), new = c("x", "x")),
                   c("x", "x", "c"))
  expect_identical(recode_lookup(factor(c("a", "b", "c")), old = "b", new = "a"),
                   factor(c("a", "a", "c"), levels = c("a", "c")))
  expect_identical(recode_lookup(c(1, 2), old = "1", new = "one"),
                   c("one", "2"))
  expect_identical(recode_lookup(c("a", "b", "c"), old = c("a", "b"),
                                 new = factor(c("x", "y"), levels = c("y", "x"))),
                   factor(c("x", "y", "c"), levels = c("y", "x", "c")))
})

test_that("'check_map_list' throws expected errors", {
  expect_true(check_map_list(list(x = c("a", "b"), y = "c")))
  expect_error(check_map_list(list()), "`map` has length 0.")
  expect_error(check_map_list(list("a")), "Elements of `map` do not all have names.")
  expect_error(check_map_list(list(x = "a", x = "b")), "`map` has two elements called \"x\".")
  expect_error(check_map_list(list(x = list("a"))), "Element \"x\" of `map` is not a vector.")
  expect_error(check_map_list(list(x = c("a", NA))), "Element \"x\" of `map` has NA.")
  expect_error(check_map_list(list(x = "a", y = "a")),
               "Level \"a\" appears more than once in `map`.")
})

test_that("'check_map_list_in_data' works", {
  expect_true(check_map_list_in_data(list(x = c("a", "b")), vals_dim = c("a", "b", "c")))
  expect_error(check_map_list_in_data(list(x = c("a", "d")), vals_dim = c("a", "b")),
               "`map` has level not found in `dim`: \"d\".")
})

test_that("'check_map_df' throws expected errors", {
  expect_true(check_map_df(data.frame(x = "a", y = "b")))
  expect_error(check_map_df(data.frame(x = "a")),
               "`map` is a data frame but does not have 2 columns.")
  expect_error(check_map_df(data.frame(x = c("a", "a"), y = c("b", "c"))),
               "Old level \"a\" appears more than once in `map`.")
  expect_error(check_map_df(data.frame(x = "a", y = NA)),
               "New levels in `map` have NA.")
})

test_that("'check_map_fun_return' throws expected errors", {
  x <- c("a", "b", "a")
  expect_true(check_map_fun_return(x = x, ans = c("z", "z", "z"), nm_x = "reg"))
  expect_error(check_map_fun_return(x = x, ans = list(1, 2, 3), nm_x = "reg"),
               "Function supplied for `map` did not return a vector.")
  expect_error(check_map_fun_return(x = x, ans = c("z", "z"), nm_x = "reg"),
               "Function supplied for `map` returned vector with wrong length.")
  expect_error(check_map_fun_return(x = x, ans = c("z", NA, "z"), nm_x = "reg"),
               "Function supplied for `map` returned NA for a non-NA level.")
  expect_error(check_map_fun_return(x = x, ans = c("z", "y", "y"), nm_x = "reg"),
               "mapped a level of `reg` to more than one new level.")
})
