## 'lifetab_decrement' --------------------------------------------------------

test_that("'lifetab_decrement' summed over cells reproduces 'lifetab', mx, all methods", {
  data <- make_data_decrement()
  data$sex <- "Female"
  for (infant in c("constant", "linear", "CD", "AK")) {
    for (child in c("constant", "linear", "CD")) {
      for (closed in c("constant", "linear")) {
        tab <- lifetab_decrement(data, mx = mx, decrement = cause, sex = sex,
                                 infant = infant, child = child, closed = closed)
        tot <- suppressMessages(
          collapse_dims(tab, count = c(qx, lx, dx, Lx), by = c(sex, age))
        )
        data_all <- suppressMessages(
          collapse_dims(data, rate = mx, wt = popn, wt_shared = cause, by = c(sex, age))
        )
        lt <- lifetab(data_all, mx = mx, sex = sex,
                      infant = infant, child = child, closed = closed)
        expect_equal(tot$qx, lt$qx)
        expect_equal(tot$lx, lt$lx)
        expect_equal(tot$dx, lt$dx)
        expect_equal(tot$Lx, lt$Lx)
      }
    }
  }
})

test_that("'lifetab_decrement' divides exits in proportion to rates", {
  data <- make_data_decrement()
  tab <- lifetab_decrement(data, mx = mx, decrement = cause)
  dx <- matrix(tab$dx, ncol = 3)
  mx <- matrix(data$mx, ncol = 3)
  expect_equal(dx / rowSums(dx), mx / rowSums(mx))
})

test_that("'lifetab_decrement' gives internally consistent columns", {
  data <- make_data_decrement()
  tab <- lifetab_decrement(data, mx = mx, decrement = cause)
  lx <- matrix(tab$lx, ncol = 3)
  dx <- matrix(tab$dx, ncol = 3)
  Lx <- matrix(tab$Lx, ncol = 3)
  qx <- matrix(tab$qx, ncol = 3)
  n <- nrow(lx)
  ## lx for cell k is the number who eventually exit through k
  expect_equal(lx, apply(dx, 2, function(x) rev(cumsum(rev(x)))))
  expect_equal(lx[-n, ] - lx[-1, ], dx[-n, ])
  ## crude qx uses all-decrement lx as denominator
  expect_equal(qx, dx / rowSums(lx))
  ## open age group
  expect_equal(lx[n, ], dx[n, ])
  expect_equal(sum(qx[n, ]), 1)
  expect_equal(sum(lx[1, ]), 100000)
  ## Lx between n * lx at end of interval and n * lx at start
  nx <- c(1, 4, rep(5, n - 3))
  expect_true(all(Lx[-n, ] >= nx * lx[-1, ]))
  expect_true(all(Lx[-n, ] <= nx * lx[-n, ]))
})

test_that("'lifetab_decrement' Lx matches numerical integration, constant method", {
  ## under constant hazards, persons alive at time t in the interval
  ## who eventually exit through k are
  ## lx_k(end) + share_k * (l(t) - l(end))
  mx <- c(0.02, 0.05)
  data <- data.frame(age = c("0-4", "0-4", "5+", "5+"),
                     cause = c("a", "b", "a", "b"),
                     mx = c(mx, 0.1, 0.3))
  data <- data[c(1, 3, 2, 4), ]
  data$age <- c("0", "1+")[match(data$age, c("0-4", "5+"))]
  tab <- lifetab_decrement(data, mx = mx, decrement = cause, radix = 1)
  m <- sum(mx)
  l_end <- exp(-m)
  lx_end_k <- tab$lx[tab$age == "1+"]
  share <- mx / m
  Lx_k_expected <- vapply(1:2, function(k) {
    f <- function(t) lx_end_k[k] + share[k] * (exp(-m * t) - l_end)
    stats::integrate(f, 0, 1)$value
  }, 0)
  expect_equal(tab$Lx[tab$age == "0"], Lx_k_expected)
})

test_that("'lifetab_decrement' with one cell reproduces 'lifetab'", {
  data <- make_data_decrement()
  data <- data[data$cause == "a", ]
  tab <- lifetab_decrement(data, mx = mx, decrement = cause)
  lt <- lifetab(data, mx = mx)
  expect_equal(tab$qx, lt$qx)
  expect_equal(tab$lx, lt$lx)
  expect_equal(tab$dx, lt$dx)
  expect_equal(tab$Lx, lt$Lx)
})

test_that("'lifetab_decrement' works with qx, and agrees with mx for closed age groups", {
  data <- make_data_decrement()
  tab_mx <- lifetab_decrement(data, mx = mx, decrement = cause)
  data_qx <- data
  data_qx$qx <- tab_mx$qx
  data_qx$mx <- NULL
  for (method in c("constant", "linear")) {
    tab_mx <- lifetab_decrement(data, mx = mx, decrement = cause,
                                infant = method, child = method, closed = method)
    data_qx$qx <- tab_mx$qx
    tab_qx <- lifetab_decrement(data_qx, qx = qx, decrement = cause,
                                infant = method, child = method, closed = method)
    expect_equal(tab_qx$qx, tab_mx$qx)
    expect_equal(tab_qx$lx, tab_mx$lx)
    expect_equal(tab_qx$dx, tab_mx$dx)
    is_open <- tab_mx$age == "85+"
    expect_equal(tab_qx$Lx[!is_open], tab_mx$Lx[!is_open])
  }
})

test_that("'lifetab_decrement' summed over cells reproduces 'lifetab', qx", {
  data <- make_data_decrement()
  tab_mx <- lifetab_decrement(data, mx = mx, decrement = cause)
  data$qx <- tab_mx$qx
  data$mx <- NULL
  tab <- lifetab_decrement(data, qx = qx, decrement = cause)
  tot <- suppressMessages(collapse_dims(tab, count = c(qx, lx, dx, Lx), by = age))
  lt <- lifetab(tot[c("age", "qx")], qx = qx)
  expect_equal(tot$lx, lt$lx)
  expect_equal(tot$dx, lt$dx)
  expect_equal(tot$Lx, lt$Lx)
})

test_that("'lifetab_decrement' is additive: collapsing inputs equals collapsing outputs", {
  data <- make_data_decrement()
  data$cause_broad <- ifelse(data$cause == "a", "A", "BC")
  tab_fine <- lifetab_decrement(data, mx = mx, decrement = cause)
  tab_fine$cause_broad <- ifelse(tab_fine$cause == "a", "A", "BC")
  ans_outputs <- suppressMessages(
    collapse_dims(tab_fine, count = c(qx, lx, dx, Lx), by = c(age, cause_broad))
  )
  data_broad <- suppressMessages(
    collapse_dims(data, rate = mx, wt = popn, wt_shared = cause, by = c(age, cause_broad))
  )
  ans_inputs <- lifetab_decrement(data_broad, mx = mx, decrement = cause_broad)
  ans_inputs$popn <- NULL
  ans_inputs <- ans_inputs[order(ans_inputs$cause_broad), ]
  ans_outputs <- ans_outputs[order(ans_outputs$cause_broad), ]
  expect_equal(ans_inputs$lx, ans_outputs$lx)
  expect_equal(ans_inputs$dx, ans_outputs$dx)
  expect_equal(ans_inputs$Lx, ans_outputs$Lx)
  expect_equal(ans_inputs$qx, ans_outputs$qx)
})

test_that("'lifetab_decrement' works with two decrement dimensions", {
  data <- make_data_decrement()
  data2 <- rbind(transform(data, place = "home", mx = 0.4 * mx),
                 transform(data, place = "hosp", mx = 0.6 * mx))
  tab2 <- lifetab_decrement(data2, mx = mx, decrement = c(cause, place))
  tab1 <- lifetab_decrement(data, mx = mx, decrement = cause)
  expect_identical(nrow(tab2), 2L * nrow(tab1))
  tot <- suppressMessages(
    collapse_dims(tab2, count = c(qx, lx, dx, Lx), by = c(age, cause))
  )
  tot <- tot[order(match(tot$cause, tab1$cause)), ]
  expect_equal(tot$lx, tab1$lx)
  expect_equal(tot$Lx, tab1$Lx)
})

test_that("'lifetab_decrement' gives same answer with 'by' and grouped data", {
  skip_if_not_installed("dplyr")
  data <- make_data_decrement()
  data <- rbind(transform(data, reg = "x"),
                transform(data, reg = "y", mx = 1.1 * mx))
  ans_by <- lifetab_decrement(data, mx = mx, decrement = cause, by = reg)
  ans_groups <- lifetab_decrement(dplyr::group_by(data, reg), mx = mx, decrement = cause)
  expect_equal(ans_by, ans_groups)
  ans_x <- lifetab_decrement(data[data$reg == "x", ], mx = mx, decrement = cause)
  expect_equal(ans_by$Lx[ans_by$reg == "x"], ans_x$Lx)
})

test_that("'lifetab_decrement' allows cells to be absent from a population group", {
  data <- make_data_decrement()
  data <- rbind(transform(data, reg = "x"),
                transform(data, reg = "y"))
  data <- data[!(data$reg == "y" & data$cause == "c"), ]
  ans <- lifetab_decrement(data, mx = mx, decrement = cause, by = reg)
  expect_identical(nrow(ans), nrow(data))
  data_y <- data[data$reg == "y", ]
  ans_y <- lifetab_decrement(data_y, mx = mx, decrement = cause)
  expect_equal(ans$lx[ans$reg == "y"], ans_y$lx)
})

test_that("'lifetab_decrement' orders rows by cell then age, and passes through other columns", {
  data <- make_data_decrement()
  set.seed(0)
  data <- data[sample(nrow(data)), ]
  ans <- lifetab_decrement(data, mx = mx, decrement = cause)
  expect_identical(ans$cause, rep(unique(data$cause), each = 19))
  expect_identical(ans$age[1:3], c("0", "1-4", "5-9"))
  expect_identical(names(ans), c("age", "cause", "popn", "qx", "lx", "dx", "Lx"))
})

test_that("'lifetab_decrement' works with suffix", {
  data <- make_data_decrement()
  ans <- lifetab_decrement(data, mx = mx, decrement = cause, suffix = "dec")
  expect_identical(names(ans), c("age", "cause", "mx", "popn",
                                 "qx.dec", "lx.dec", "dx.dec", "Lx.dec"))
})

test_that("'lifetab_decrement' removes existing life table columns", {
  data <- make_data_decrement()
  data$lx <- 1
  expect_message(ans <- lifetab_decrement(data, mx = mx, decrement = cause),
                 "Removing existing column")
  expect_identical(sum(names(ans) == "lx"), 1L)
})

test_that("'lifetab_decrement' works with rvecs, and matches draw-by-draw calculation", {
  data <- make_data_decrement(n_draw = 3)
  ans <- lifetab_decrement(data, mx = mx, decrement = cause)
  expect_true(rvec::is_rvec(ans$Lx))
  m <- as.matrix(data$mx)
  for (i_draw in 1:3) {
    data_i <- data
    data_i$mx <- m[, i_draw]
    ans_i <- lifetab_decrement(data_i, mx = mx, decrement = cause)
    expect_equal(as.matrix(ans$Lx)[, i_draw], ans_i$Lx)
    expect_equal(as.matrix(ans$qx)[, i_draw], ans_i$qx)
  }
})

test_that("'lifetab_decrement' works with n_core = 2", {
  skip_on_cran()
  data <- make_data_decrement()
  data <- rbind(transform(data, reg = "x"),
                transform(data, reg = "y", mx = 1.1 * mx))
  ans_1 <- lifetab_decrement(data, mx = mx, decrement = cause, by = reg)
  ans_2 <- lifetab_decrement(data, mx = mx, decrement = cause, by = reg, n_core = 2)
  expect_equal(ans_1, ans_2)
})

test_that("'lifetab_decrement' handles rates of zero and Inf", {
  data <- make_data_decrement()
  data$mx[data$cause == "a" & data$age == "5-9"] <- 0
  data$mx[data$cause == "b" & data$age == "1-4"] <- 0
  ans <- lifetab_decrement(data, mx = mx, decrement = cause)
  expect_identical(ans$dx[ans$cause == "a" & ans$age == "5-9"], 0)
  data <- make_data_decrement()
  data$mx[data$cause == "b" & data$age == "80-84"] <- Inf
  ans <- lifetab_decrement(data, mx = mx, decrement = cause)
  dx_80 <- ans$dx[ans$age == "80-84"]
  expect_identical(dx_80[c(1, 3)], c(0, 0))
  expect_identical(sum(ans$lx[ans$age == "85+"]), 0)
  expect_true(all(is.finite(ans$Lx[ans$age != "85+"])))
})

test_that("'lifetab_decrement' gives NaN and warning when total rate in final age group is 0", {
  data <- make_data_decrement()
  data$mx[data$age == "85+"] <- 0
  expect_warning(ans <- lifetab_decrement(data, mx = mx, decrement = cause),
                 "Rate for final age group, summed across decrement cells, is 0.")
  is_open <- ans$age == "85+"
  expect_true(all(is.nan(ans$lx)))
  expect_true(all(is.nan(ans$Lx)))
  expect_true(all(is.nan(ans$dx[is_open])))
  expect_true(all(is.nan(ans$qx[is_open])))
  expect_false(anyNA(ans$dx[!is_open]))
  expect_false(anyNA(ans$qx[!is_open]))
})

test_that("'lifetab_decrement' warning for zero final rate names group and counts draws", {
  data <- make_data_decrement(n_draw = 3)
  data_x <- data
  data_x$reg <- "x"
  data_y <- data
  data_y$reg <- "y"
  data <- vctrs::vec_rbind(data_x, data_y)
  m <- as.matrix(data$mx)
  m[data$reg == "y" & data$age == "85+", 2] <- 0
  data$mx <- rvec::rvec(m)
  expect_warning(ans <- lifetab_decrement(data, mx = mx, decrement = cause, by = reg),
                 "Affects 1 draw")
  expect_warning(lifetab_decrement(data, mx = mx, decrement = cause, by = reg),
                 "`reg`=\"y\"")
  lx <- as.matrix(ans$lx)
  expect_true(all(is.nan(lx[ans$reg == "y", 2])))
  expect_false(anyNA(lx[ans$reg == "y", c(1, 3)]))
  expect_false(anyNA(lx[ans$reg == "x", ]))
})

test_that("'lifetab_decrement' treats zero total rate in closed age group as no exits", {
  data <- make_data_decrement()
  data$mx[data$age == "10-14"] <- 0
  ans <- expect_no_warning(lifetab_decrement(data, mx = mx, decrement = cause))
  expect_identical(ans$dx[ans$age == "10-14"], c(0, 0, 0))
  expect_false(anyNA(ans$lx))
})

test_that("'lifetab_decrement' works with ax", {
  data <- make_data_decrement()
  data$ax <- ifelse(data$age == "0", 0.1, NA)
  ans <- lifetab_decrement(data, mx = mx, decrement = cause, ax = ax)
  data_all <- suppressMessages(
    collapse_dims(data, rate = mx, wt = popn, wt_shared = cause, by = c(age, ax))
  )
  lt <- lifetab(data_all, mx = mx, ax = ax)
  tot <- suppressMessages(collapse_dims(ans, count = Lx, by = age))
  expect_equal(tot$Lx, lt$Lx)
})


## errors ----------------------------------------------------------------------

test_that("'lifetab_decrement' throws expected errors with arguments", {
  skip_if_not_installed("dplyr")
  data <- make_data_decrement()
  expect_error(lifetab_decrement(data, mx = mx),
               "No value supplied for `decrement`.")
  expect_error(lifetab_decrement(data, mx = mx, decrement = c()),
               "No value supplied for `decrement`.")
  expect_error(lifetab_decrement(data, mx = mx, qx = mx, decrement = cause),
               "Values supplied for `mx` and for `qx`.")
  expect_error(lifetab_decrement(data, mx = mx, decrement = cause, by = cause),
               "`cause` is both a decrement dimension and a `by` variable.")
  expect_error(lifetab_decrement(dplyr::group_by(data, cause), mx = mx, decrement = cause),
               "`cause` is both a decrement dimension and a grouping variable.")
  expect_error(lifetab_decrement(data, mx = mx, decrement = c(cause, mx)),
               "`decrement` and `mx` use the same variable.")
  expect_error(lifetab_decrement(data, mx = mx, decrement = age),
               "`decrement` and `age` use the same variable.")
  expect_error(lifetab_decrement(1, mx = mx, decrement = cause),
               "`data` is not a data frame.")
})

test_that("'lifetab_decrement' throws error when cells have different ages", {
  data <- make_data_decrement()
  data <- data[!(data$cause == "b" & data$age == "10-14"), ]
  expect_error(lifetab_decrement(data, mx = mx, decrement = cause),
               "Decrement cells have different age groups.")
  data <- make_data_decrement()
  data <- rbind(data, data[1, ])
  expect_error(lifetab_decrement(data, mx = mx, decrement = cause),
               "Decrement cell has duplicated age groups.")
})

test_that("'lifetab_decrement' throws error when ax varies across cells", {
  data <- make_data_decrement()
  data$ax <- ifelse(data$age == "0", 0.1, NA)
  data$ax[data$age == "0" & data$cause == "b"] <- 0.2
  expect_error(lifetab_decrement(data, mx = mx, decrement = cause, ax = ax),
               "`ax` varies across decrement cells.")
})

test_that("'lifetab_decrement' throws errors with invalid qx", {
  data <- make_data_decrement()
  tab <- lifetab_decrement(data, mx = mx, decrement = cause)
  data$qx <- tab$qx
  data$mx <- NULL
  data_high <- data
  data_high$qx[data_high$age == "5-9"] <- 0.5
  expect_error(lifetab_decrement(data_high, qx = qx, decrement = cause),
               "Sum of `qx` across decrement cells is greater than 1.")
  data_open <- data
  data_open$qx[data_open$age == "85+"] <- 0.2
  expect_error(lifetab_decrement(data_open, qx = qx, decrement = cause),
               "Sum of `qx` across decrement cells in final age group does not equal 1.")
})

test_that("'lifetab_decrement' names population group in error messages", {
  data <- make_data_decrement()
  data <- rbind(transform(data, reg = "x"),
                transform(data, reg = "y"))
  data <- data[!(data$reg == "y" & data$cause == "b" & data$age == "10-14"), ]
  expect_error(lifetab_decrement(data, mx = mx, decrement = cause, by = reg),
               "Problem occurred where `reg`=\"y\"")
})


## helpers ---------------------------------------------------------------------

test_that("'decrement_shares' works", {
  vals <- array(c(1, 0, Inf, 3, 0, Inf), dim = c(3, 1, 2))
  total <- matrix(c(4, 0, Inf), ncol = 1)
  ans <- decrement_shares(vals = vals, total = total)
  expect_equal(ans[, 1, 1], c(0.25, 0, 0.5))
  expect_equal(ans[, 1, 2], c(0.75, 0, 0.5))
})

test_that("'decrement_shares' gives NaN when total in final age group is 0", {
  vals <- array(c(1, 0, 0, 3, 0, 0), dim = c(3, 1, 2))
  total <- matrix(c(4, 0, 0), ncol = 1)
  ans <- decrement_shares(vals = vals, total = total)
  expect_equal(ans[, 1, 1], c(0.25, 0, NaN))
  expect_equal(ans[, 1, 2], c(0.75, 0, NaN))
})

test_that("'warn_decrement_open_zero' works", {
  expect_no_warning(warn_decrement_open_zero(matrix(c(0, 1), ncol = 1), str_key = NULL))
  expect_warning(warn_decrement_open_zero(matrix(c(1, 0), ncol = 1), str_key = NULL),
                 "Rate for final age group")
})

test_that("'check_qx_decrement_total' works", {
  age <- c("0", "1+")
  expect_true(check_qx_decrement_total(matrix(c(0.5, 1), ncol = 1), age = age))
  expect_true(check_qx_decrement_total(matrix(c(NA, NA), ncol = 1), age = age))
  expect_error(check_qx_decrement_total(matrix(c(1.1, 1), ncol = 1), age = age),
               "Sum of `qx` across decrement cells is greater than 1.")
  expect_error(check_qx_decrement_total(matrix(c(0.5, 0.9), ncol = 1), age = age),
               "does not equal 1")
})

test_that("'check_decrement_colnums' works", {
  expect_true(check_decrement_colnums(c(cause = 2L), list(mx = c(mx = 1L), by = c(reg = 3L))))
  expect_error(check_decrement_colnums(integer(), list()),
               "No value supplied for `decrement`.")
})
