## Helpers for creating test data ---------------------------------------------

make_data_decompose <- function(n_draw = NULL) {
  data <- make_data_decrement(n_draw = n_draw)
  data_1 <- data
  data_1$year <- 2000L
  data_2 <- data
  data_2$year <- 2010L
  data_2$mx <- data_2$mx * ifelse(data_2$cause == "a", 0.7, 0.9)
  data_3 <- data
  data_3$year <- 2020L
  data_3$mx <- data_3$mx * ifelse(data_3$cause == "b", 0.6, 0.85)
  vctrs::vec_rbind(data_1, data_2, data_3)
}

make_lifetab_decompose <- function(n_draw = NULL, ...) {
  data <- make_data_decompose(n_draw = n_draw)
  data <- suppressMessages(
    collapse_dims(data, rate = mx, wt = popn, wt_shared = cause, by = c(year, age))
  )
  lifetab(data, mx = mx, by = year, ...)
}

ex_at <- function(tab, year, age) {
  tab$ex[tab$year == year & tab$age == age]
}


## 'decompose_ex' - Arriaga ----------------------------------------------------

test_that("'decompose_ex' Arriaga contributions sum to difference in life expectancy", {
  tab <- make_lifetab_decompose()
  ans <- suppressMessages(
    decompose_ex(tab, compare_along = year, compare_base = 2000, method = "Arriaga")
  )
  expect_identical(names(ans), c("age", "year", "ex_contrib"))
  expect_identical(unique(ans$year), c(2010L, 2020L))
  for (y in c(2010L, 2020L))
    expect_equal(sum(ans$ex_contrib[ans$year == y]),
                 ex_at(tab, y, "0") - ex_at(tab, 2000L, "0"))
})

test_that("'decompose_ex' Arriaga works with 'at'", {
  tab <- make_lifetab_decompose()
  ans <- suppressMessages(
    decompose_ex(tab, compare_along = year, compare_base = 2000, method = "Arriaga", at = 65)
  )
  expect_identical(ans$age[1:2], c("65-69", "70-74"))
  expect_equal(sum(ans$ex_contrib[ans$year == 2010L]),
               ex_at(tab, 2010L, "65-69") - ex_at(tab, 2000L, "65-69"))
})

test_that("'decompose_ex' Arriaga matches formula in Preston et al", {
  tab <- make_lifetab_decompose()
  ans <- suppressMessages(
    decompose_ex(tab, compare_along = year, compare_base = 2000, method = "Arriaga")
  )
  t1 <- tab[tab$year == 2000L, ]
  t2 <- tab[tab$year == 2010L, ]
  n <- nrow(t1)
  T2 <- rev(cumsum(rev(t2$Lx)))
  l0 <- t1$lx[[1]]
  expected <- c((t1$lx[-n] / l0) * (t2$Lx[-n] / t2$lx[-n] - t1$Lx[-n] / t1$lx[-n]) +
                  (T2[-1] / l0) * (t1$lx[-n] / t2$lx[-n] - t1$lx[-1] / t2$lx[-1]),
                (t1$lx[n] / l0) * (T2[n] / t2$lx[n] - t1$Lx[n] / t1$lx[n]))
  expect_equal(ans$ex_contrib[ans$year == 2010L], expected)
})

test_that("'decompose_ex' Arriaga is not symmetric", {
  tab <- make_lifetab_decompose()
  tab <- tab[tab$year != 2020L, ]
  ans_fwd <- suppressMessages(
    decompose_ex(tab, compare_along = year, compare_base = 2000, method = "Arriaga")
  )
  ans_bwd <- suppressMessages(
    decompose_ex(tab, compare_along = year, compare_base = 2010, method = "Arriaga")
  )
  expect_equal(sum(ans_fwd$ex_contrib), -sum(ans_bwd$ex_contrib))
  expect_false(isTRUE(all.equal(ans_fwd$ex_contrib, -ans_bwd$ex_contrib)))
})


## 'decompose_ex' - Horiuchi ---------------------------------------------------

test_that("'decompose_ex' Horiuchi contributions approximately sum to difference, and are symmetric", {
  tab <- make_lifetab_decompose()
  tab <- tab[tab$year != 2020L, ]
  ans_fwd <- suppressMessages(
    decompose_ex(tab, compare_along = year, compare_base = 2000, method = "Horiuchi")
  )
  ans_bwd <- suppressMessages(
    decompose_ex(tab, compare_along = year, compare_base = 2010, method = "Horiuchi")
  )
  diff <- ex_at(tab, 2010L, "0") - ex_at(tab, 2000L, "0")
  expect_equal(sum(ans_fwd$ex_contrib), diff, tolerance = 1e-4)
  expect_equal(ans_fwd$ex_contrib, -ans_bwd$ex_contrib, tolerance = 1e-8)
  ans_arriaga <- suppressMessages(
    decompose_ex(tab, compare_along = year, compare_base = 2000, method = "Arriaga")
  )
  expect_equal(ans_fwd$ex_contrib, ans_arriaga$ex_contrib, tolerance = 0.05)
})

test_that("'decompose_ex' Horiuchi approximately sums to difference with each method", {
  for (methods in list(c("constant", "constant", "constant"),
                       c("linear", "linear", "linear"),
                       c("CD", "CD", "constant"),
                       c("AK", "linear", "linear"))) {
    data <- make_data_decompose()
    data <- suppressMessages(
      collapse_dims(data, rate = mx, wt = popn, wt_shared = cause, by = c(year, age))
    )
    data$sex <- "Female"
    tab <- lifetab(data, mx = mx, by = year, sex = sex,
                   infant = methods[[1]], child = methods[[2]], closed = methods[[3]])
    ans <- suppressMessages(
      decompose_ex(tab, compare_along = year, compare_base = 2000, method = "Horiuchi",
                   by = sex)
    )
    for (y in c(2010L, 2020L))
      expect_equal(sum(ans$ex_contrib[ans$year == y]),
                   ex_at(tab, y, "0") - ex_at(tab, 2000L, "0"),
                   tolerance = 1e-4)
  }
})

test_that("'ex_from_rates' reproduces life expectancy from life tables", {
  for (method in c("constant", "linear")) {
    tab <- make_lifetab_decompose(infant = method, child = method, closed = method)
    tab <- tab[tab$year == 2000L, ]
    mx <- matrix(tab$dx / tab$Lx, ncol = 1)
    nx <- c(1, 4, rep(5, 16), Inf)
    method_row <- c(rep(method, 18), "open")
    ans <- ex_from_rates(mx = mx, a_table = mx, nx = nx, method_row = method_row)
    expect_equal(ans, tab$ex[[1]])
  }
  tab <- make_lifetab_decompose()
  tab <- tab[tab$year == 2000L, ]
  lx_end <- c(tab$lx[-1], 0)
  nx <- c(1, 4, rep(5, 16), Inf)
  ax <- (tab$Lx - c(nx[-19], 0) * lx_end) / tab$dx
  ans <- ex_from_rates(mx = matrix(tab$dx / tab$Lx, ncol = 1),
                       a_table = matrix(ax, ncol = 1),
                       nx = nx,
                       method_row = c(rep("table", 18), "open"))
  expect_equal(ans, tab$ex[[1]])
})


## 'decompose_ex' - general ----------------------------------------------------

test_that("'decompose_ex' compares each level with base", {
  tab <- make_lifetab_decompose()
  ans_all <- suppressMessages(
    decompose_ex(tab, compare_along = year, compare_base = 2000, method = "Arriaga")
  )
  ans_pair <- suppressMessages(
    decompose_ex(tab[tab$year != 2010L, ], compare_along = year,
                 compare_base = 2000, method = "Arriaga")
  )
  expect_equal(ans_all[ans_all$year == 2020L, ], ans_pair)
})

test_that("'decompose_ex' orders levels of factor 'compare_along' by levels", {
  tab <- make_lifetab_decompose()
  tab$year <- factor(tab$year, levels = c(2020, 2010, 2000))
  ans <- suppressMessages(
    decompose_ex(tab, compare_along = year, compare_base = "2000", method = "Arriaga")
  )
  expect_identical(as.character(unique(ans$year)), c("2020", "2010"))
  expect_true(is.factor(ans$year))
})

test_that("'decompose_ex' works with population dimensions", {
  skip_if_not_installed("dplyr")
  tab <- make_lifetab_decompose()
  tab <- vctrs::vec_rbind(transform(tab, reg = "x"), transform(tab, reg = "y"))
  tab$popn <- seq_len(nrow(tab))
  msgs <- capture_messages(
    ans_inferred <- decompose_ex(tab, compare_along = year, compare_base = 2000,
                                 method = "Arriaga")
  )
  expect_match(msgs, "Using population dimension `reg`.", all = FALSE)
  ans_by <- suppressMessages(
    decompose_ex(tab, compare_along = year, compare_base = 2000, method = "Arriaga", by = reg)
  )
  ans_groups <- suppressMessages(
    decompose_ex(dplyr::group_by(tab, reg), compare_along = year, compare_base = 2000,
                 method = "Arriaga")
  )
  expect_equal(ans_inferred, ans_by)
  expect_equal(ans_by, ans_groups)
  expect_identical(names(ans_by), c("age", "year", "reg", "ex_contrib"))
})

test_that("'decompose_ex' works with rvecs, and matches draw-by-draw calculation", {
  tab <- make_lifetab_decompose(n_draw = 2)
  for (method in c("Arriaga", "Horiuchi")) {
    ans <- suppressMessages(
      decompose_ex(tab, compare_along = year, compare_base = 2000, method = method)
    )
    expect_true(rvec::is_rvec(ans$ex_contrib))
    for (i_draw in 1:2) {
      tab_i <- tab
      for (nm in c("qx", "lx", "dx", "Lx", "ex"))
        tab_i[[nm]] <- as.matrix(tab[[nm]])[, i_draw]
      ans_i <- suppressMessages(
        decompose_ex(tab_i, compare_along = year, compare_base = 2000, method = method)
      )
      expect_equal(as.matrix(ans$ex_contrib)[, i_draw], ans_i$ex_contrib)
    }
  }
})

test_that("'decompose_ex' works with suffix", {
  data <- make_data_decompose()
  data <- suppressMessages(
    collapse_dims(data, rate = mx, wt = popn, wt_shared = cause, by = c(year, age))
  )
  tab <- lifetab(data, mx = mx, by = year, suffix = "lt")
  ans <- suppressMessages(
    decompose_ex(tab, compare_along = year, compare_base = 2000, method = "Arriaga",
                 suffix = "lt")
  )
  expect_identical(names(ans), c("age", "year", "ex_contrib.lt"))
})

test_that("'decompose_ex' checks methods for age groups when supplied", {
  tab <- make_lifetab_decompose()
  expect_error(suppressMessages(
    decompose_ex(tab, compare_along = year, compare_base = 2000, method = "Arriaga",
                 closed = "linear")
  ),
  "`closed` is \"linear\", but the life table is not consistent with method \"linear\".")
  expect_no_error(suppressMessages(
    decompose_ex(tab, compare_along = year, compare_base = 2000, method = "Arriaga",
                 closed = "constant")
  ))
})

test_that("'decompose_ex' throws expected errors", {
  tab <- make_lifetab_decompose()
  expect_error(decompose_ex(tab, compare_base = 2000, method = "Arriaga"),
               "No value supplied for `compare_along`.")
  expect_error(decompose_ex(tab, compare_along = year, method = "Arriaga"),
               "No value supplied for `compare_base`.")
  expect_error(decompose_ex(tab, compare_along = year, compare_base = 2000),
               "No value supplied for `method`.")
  expect_error(decompose_ex(tab, compare_along = year, compare_base = 2000, method = "arriaga"),
               "Invalid value for `method`.")
  expect_error(decompose_ex(tab, compare_along = year, compare_base = 1999, method = "Arriaga"),
               "`compare_base` not found in `year`.")
  expect_error(decompose_ex(tab, compare_along = year, compare_base = "2000", method = "Arriaga"),
               "`compare_base` is not a valid value for `year`.")
  expect_error(decompose_ex(tab, compare_along = year, compare_base = c(2000, 2010),
                            method = "Arriaga"),
               "`compare_base` does not have length 1.")
  expect_error(decompose_ex(tab, compare_along = year, compare_base = 2000, method = "Arriaga",
                            at = c(0, 5)),
               "`at` has length 2.")
  expect_error(decompose_ex(tab, compare_along = year, compare_base = 2000, method = "Arriaga",
                            at = 3),
               "Invalid value for `at`.")
  expect_error(decompose_ex(tab, compare_along = year, compare_base = 2000, method = "Arriaga",
                            by = year),
               "`compare_along` and `by` use the same variable.")
  expect_error(suppressMessages(decompose_ex(tab[tab$year == 2000L, ], compare_along = year,
                                             compare_base = 2000, method = "Arriaga")),
               "`year` has only one value.")
  expect_error(decompose_ex(tab[-4], compare_along = year, compare_base = 2000,
                            method = "Arriaga"),
               "`data` does not have life table column `qx`.")
})

test_that("'decompose_ex' throws error when age groups differ", {
  tab <- make_lifetab_decompose()
  tab <- tab[!(tab$year == 2010L & tab$age == "85+"), ]
  expect_error(suppressMessages(
    decompose_ex(tab, compare_along = year, compare_base = 2000, method = "Arriaga")
  ),
  "Problem with life table where `year` is \"2010\".")
})


## 'decompose_ex_decrement' ----------------------------------------------------

test_that("'decompose_ex_decrement' summed over cells equals 'decompose_ex'", {
  data <- make_data_decompose()
  tab <- lifetab_decrement(data, mx = mx, decrement = cause, by = year)
  tab_all <- suppressMessages(
    collapse_dims(tab, count = c(qx, lx, dx, Lx), by = !cause)
  )
  for (method in c("Arriaga", "Horiuchi")) {
    ans <- suppressMessages(
      decompose_ex_decrement(tab, decrement = cause, compare_along = year,
                             compare_base = 2000, method = method)
    )
    expect_identical(names(ans), c("age", "cause", "year", "ex_contrib"))
    ans_all <- suppressMessages(
      decompose_ex(tab_all, compare_along = year, compare_base = 2000, method = method)
    )
    ans_sum <- suppressMessages(collapse_dims(ans, count = ex_contrib, by = !cause))
    ans_sum <- ans_sum[order(ans_sum$year, match(ans_sum$age, ans_all$age)), ]
    tol <- if (method == "Arriaga") 1e-8 else 1e-3
    expect_equal(ans_sum$ex_contrib, ans_all$ex_contrib, tolerance = tol)
  }
})

test_that("'decompose_ex_decrement' Arriaga divides contributions in proportion to differences in rates", {
  data <- make_data_decompose()
  data <- data[data$year != 2020L, ]
  tab <- lifetab_decrement(data, mx = mx, decrement = cause, by = year)
  ans <- suppressMessages(
    decompose_ex_decrement(tab, decrement = cause, compare_along = year,
                           compare_base = 2000, method = "Arriaga")
  )
  contrib <- matrix(ans$ex_contrib, ncol = 3)
  diff_mx <- matrix(data$mx[data$year == 2010L] - data$mx[data$year == 2000L], ncol = 3)
  expect_equal(contrib / rowSums(contrib), diff_mx / rowSums(diff_mx))
})

test_that("'decompose_ex_decrement' Arriaga: collapsing decrements before and after gives same answer", {
  data <- make_data_decompose()
  data$cause_broad <- ifelse(data$cause == "a", "A", "BC")
  tab_fine <- lifetab_decrement(data[names(data) != "cause_broad"], mx = mx,
                                decrement = cause, by = year)
  ans_fine <- suppressMessages(
    decompose_ex_decrement(tab_fine, decrement = cause, compare_along = year,
                           compare_base = 2000, method = "Arriaga")
  )
  ans_fine$cause_broad <- ifelse(ans_fine$cause == "a", "A", "BC")
  ans_after <- suppressMessages(
    collapse_dims(ans_fine, count = ex_contrib, by = !cause)
  )
  data_broad <- suppressMessages(
    collapse_dims(data, rate = mx, wt = popn, wt_shared = cause,
                  by = c(year, age, cause_broad))
  )
  tab_broad <- lifetab_decrement(data_broad, mx = mx, decrement = cause_broad, by = year)
  ans_before <- suppressMessages(
    decompose_ex_decrement(tab_broad, decrement = cause_broad, compare_along = year,
                           compare_base = 2000, method = "Arriaga")
  )
  ans_after <- ans_after[order(ans_after$year, ans_after$cause_broad), ]
  ans_before <- ans_before[order(ans_before$year, ans_before$cause_broad), ]
  expect_equal(ans_after$ex_contrib, ans_before$ex_contrib)
})

test_that("'decompose_ex_decrement' Arriaga handles equal all-decrement rates", {
  data <- make_data_decompose()
  data <- data[data$year != 2020L, ]
  is_2010 <- data$year == 2010L
  ## age 5-9: identical rates; age 10-14: same total, different causes
  data$mx[is_2010 & data$age == "5-9"] <- data$mx[!is_2010 & data$age == "5-9"]
  mx_10 <- data$mx[!is_2010 & data$age == "10-14"]
  data$mx[is_2010 & data$age == "10-14"] <- mx_10 * c(1.5, 0.75, 1) * sum(mx_10) /
    sum(mx_10 * c(1.5, 0.75, 1))
  tab <- lifetab_decrement(data, mx = mx, decrement = cause, by = year)
  expect_warning(ans <- suppressMessages(
    decompose_ex_decrement(tab, decrement = cause, compare_along = year,
                           compare_base = 2000, method = "Arriaga")
  ),
  NA)
  expect_equal(ans$ex_contrib[ans$age == "5-9"], c(0, 0, 0))
  base <- list(age = c("0", "1+"),
               lx = matrix(c(1, 0.5)), Lx = matrix(c(0.75, 1)), dx = matrix(c(0.5, 0.5)),
               dx_k = array(c(0.25, 0.25, 0.25, 0.25), dim = c(2, 1, 2)))
  comp <- base
  comp$dx_k <- array(c(0.4, 0.25, 0.1, 0.25), dim = c(2, 1, 2))
  comp$lx <- matrix(c(1, 0.5))
  ans <- decompose_arriaga(base = base, comp = comp, i_at = 1L)
  expect_identical(ans$n_nan, 1L)
  expect_true(all(is.nan(ans$contrib[1, 1, ])))
  expect_identical(ans$contrib[2, 1, ], c(0, 0))
})

test_that("'decompose_ex_decrement' gives warning when contributions cannot be divided", {
  tab <- data.frame(year = rep(c(2000L, 2010L), each = 4),
                    cause = rep(rep(c("a", "b"), each = 2), times = 2),
                    age = rep(c("0", "1+"), times = 4),
                    lx = c(0.5, 0.25, 0.5, 0.25, 0.7, 0.25, 0.3, 0.25),
                    dx = c(0.25, 0.25, 0.25, 0.25, 0.45, 0.25, 0.05, 0.25),
                    Lx = c(0.375, 0.5, 0.375, 0.5, 0.375, 0.5, 0.375, 0.5))
  tab$qx <- tab$dx / stats::ave(tab$lx, tab$year, tab$age, FUN = sum)
  expect_warning(suppressMessages(
    decompose_ex_decrement(tab, decrement = cause, compare_along = year,
                           compare_base = 2000, method = "Arriaga")
  ),
  "All-decrement rates are equal in base and comparison life tables")
})

test_that("'decompose_ex_decrement' treats missing cells as having no exits, with message", {
  data <- make_data_decompose()
  data <- data[data$year != 2020L, ]
  data_zero <- data
  data_zero$mx[data_zero$year == 2010L & data_zero$cause == "c"] <- 0
  tab_zero <- lifetab_decrement(data_zero, mx = mx, decrement = cause, by = year)
  ans_zero <- suppressMessages(
    decompose_ex_decrement(tab_zero, decrement = cause, compare_along = year,
                           compare_base = 2000, method = "Arriaga")
  )
  tab_missing <- tab_zero[!(tab_zero$year == 2010L & tab_zero$cause == "c"), ]
  msgs <- capture_messages(
    ans_missing <- decompose_ex_decrement(tab_missing, decrement = cause, compare_along = year,
                                          compare_base = 2000, method = "Arriaga")
  )
  expect_match(msgs, "Decrement cell \"c\" does not appear in every life table", all = FALSE)
  expect_equal(ans_missing, ans_zero)
})

test_that("'decompose_ex_decrement' works with rvecs, and matches draw-by-draw calculation", {
  data <- make_data_decompose(n_draw = 2)
  data <- data[data$year != 2020L, ]
  tab <- lifetab_decrement(data, mx = mx, decrement = cause, by = year)
  for (method in c("Arriaga", "Horiuchi")) {
    ans <- suppressMessages(
      decompose_ex_decrement(tab, decrement = cause, compare_along = year,
                             compare_base = 2000, method = method)
    )
    for (i_draw in 1:2) {
      tab_i <- tab
      for (nm in c("qx", "lx", "dx", "Lx"))
        tab_i[[nm]] <- as.matrix(tab[[nm]])[, i_draw]
      ans_i <- suppressMessages(
        decompose_ex_decrement(tab_i, decrement = cause, compare_along = year,
                               compare_base = 2000, method = method)
      )
      expect_equal(as.matrix(ans$ex_contrib)[, i_draw], ans_i$ex_contrib)
    }
  }
})

test_that("'decompose_ex_decrement' works with n_core = 2", {
  skip_on_cran()
  data <- make_data_decompose()
  data <- vctrs::vec_rbind(transform(data, reg = "x"), transform(data, reg = "y"))
  tab <- lifetab_decrement(data, mx = mx, decrement = cause, by = c(reg, year))
  ans_1 <- suppressMessages(
    decompose_ex_decrement(tab, decrement = cause, compare_along = year, compare_base = 2000,
                           method = "Horiuchi", by = reg)
  )
  ans_2 <- suppressMessages(
    decompose_ex_decrement(tab, decrement = cause, compare_along = year, compare_base = 2000,
                           method = "Horiuchi", by = reg, n_core = 2)
  )
  expect_equal(ans_1, ans_2)
})

test_that("'decompose_ex_decrement' throws expected errors", {
  data <- make_data_decompose()
  tab <- lifetab_decrement(data, mx = mx, decrement = cause, by = year)
  expect_error(decompose_ex_decrement(tab, compare_along = year, compare_base = 2000,
                                      method = "Arriaga"),
               "No value supplied for `decrement`.")
  expect_error(decompose_ex_decrement(tab, decrement = cause, compare_along = cause,
                                      compare_base = "a", method = "Arriaga"),
               "`decrement` and `compare_along` use the same variable.")
})


## Helpers ---------------------------------------------------------------------

test_that("'check_compare_base' works", {
  expect_identical(check_compare_base(2000, 2000:2001, "year"), 2000L)
  expect_identical(check_compare_base("f", factor(c("f", "m")), "sex"),
                   factor("f", levels = c("f", "m")))
  expect_error(check_compare_base(NA, 2000:2001, "year"),
               "`compare_base` is NA.")
  expect_error(check_compare_base(2000.5, 2000:2001, "year"),
               "`compare_base` is not a valid value for `year`.")
})

test_that("'rev_cumsum_cols' works", {
  m <- matrix(1:6, nrow = 3)
  expect_identical(rev_cumsum_cols(m), matrix(c(6L, 5L, 3L, 15L, 11L, 6L), nrow = 3))
  expect_identical(rev_cumsum_cols(matrix(1:2, nrow = 1)), matrix(1:2, nrow = 1))
})

test_that("'expand_draws_array' works", {
  x <- array(1:4, dim = c(2, 1, 2))
  ans <- expand_draws_array(x, n_draw = 3L)
  expect_identical(dim(ans), c(2L, 3L, 2L))
  expect_identical(ans[, 3, 2], 3:4)
  expect_identical(expand_draws_array(x, n_draw = 1L), x)
})
