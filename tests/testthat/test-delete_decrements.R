## 'delete_decrements' --------------------------------------------------------

test_that("'delete_decrements' with constant method equals 'lifetab' with reduced rates", {
  data <- make_data_decrement()
  data <- rbind(transform(data, reg = "x"),
                transform(data, reg = "y", mx = 1.2 * mx))
  tab <- lifetab_decrement(data, mx = mx, decrement = cause, by = reg)
  ans <- suppressMessages(delete_decrements(tab, decrement = cause, by = reg))
  expect_identical(names(ans), c("reg", "cause_deleted", "age", "qx", "lx", "dx", "Lx", "ex"))
  expect_identical(nrow(ans), nrow(tab))
  for (reg in c("x", "y")) {
    for (cause in c("a", "b", "c")) {
      data_k <- data[data$reg == reg & data$cause != cause, ]
      data_k <- suppressMessages(
        collapse_dims(data_k, rate = mx, wt = popn, wt_shared = cause, by = age)
      )
      expected <- lifetab(data_k, mx = mx)
      obtained <- ans[ans$reg == reg & ans$cause_deleted == cause, ]
      expect_equal(obtained$qx, expected$qx)
      expect_equal(obtained$lx, expected$lx)
      expect_equal(obtained$dx, expected$dx)
      expect_equal(obtained$Lx, expected$Lx)
      expect_equal(obtained$ex, expected$ex)
    }
  }
})

test_that("'delete_decrements' with linear method matches numerical integration", {
  data <- make_data_decrement()
  tab <- lifetab_decrement(data, mx = mx, decrement = cause,
                           infant = "linear", child = "linear", closed = "linear")
  ans <- suppressMessages(delete_decrements(tab, decrement = cause))
  ans_explicit <- suppressMessages(
    delete_decrements(tab, decrement = cause,
                      infant = "linear", child = "linear", closed = "linear")
  )
  expect_equal(ans, ans_explicit)
  ## under the linear method, all-decrement survival within an
  ## interval is 1 - q t / n, and survival with decrement k deleted
  ## is (1 - q t / n)^R
  tab_a <- tab[tab$cause == "a", ]
  lx <- tapply(tab$lx, tab$age, sum)[tab_a$age]
  dx <- tapply(tab$dx, tab$age, sum)[tab_a$age]
  ans_a <- ans[ans$cause_deleted == "a", ]
  nx <- c(1, 4, rep(5, 16))
  for (i in c(1, 2, 10, 18)) {
    q <- dx[[i]] / lx[[i]]
    R <- 1 - tab_a$dx[[i]] / dx[[i]]
    f <- function(t) (1 - q * t / nx[[i]])^R
    expected <- ans_a$lx[[i]] * stats::integrate(f, 0, nx[[i]])$value
    expect_equal(ans_a$Lx[[i]], expected, tolerance = 1e-6)
  }
})

test_that("'delete_decrements' reproduces original table when deleting a cell with no exits", {
  data <- make_data_decrement()
  data_z <- data[data$cause == "a", ]
  data_z$cause <- "z"
  data_z$mx <- 0
  data <- rbind(data, data_z)
  data$sex <- "Female"
  for (methods in list(c("constant", "constant", "constant"),
                       c("linear", "linear", "linear"),
                       c("CD", "CD", "constant"),
                       c("AK", "linear", "linear"))) {
    tab <- lifetab_decrement(data, mx = mx, decrement = cause, sex = sex,
                             infant = methods[[1]], child = methods[[2]], closed = methods[[3]])
    ans <- suppressMessages(delete_decrements(tab, decrement = cause, by = sex))
    ans_z <- ans[ans$cause_deleted == "z", ]
    data_all <- suppressMessages(
      collapse_dims(data, rate = mx, wt = popn, wt_shared = cause, by = c(sex, age))
    )
    lt <- lifetab(data_all, mx = mx, sex = sex,
                  infant = methods[[1]], child = methods[[2]], closed = methods[[3]])
    expect_equal(ans_z$lx, lt$lx)
    expect_equal(ans_z$Lx, lt$Lx)
    expect_equal(ans_z$ex, lt$ex)
  }
})

test_that("'delete_decrements' uses Preston formula when method cannot be identified", {
  data <- make_data_decrement()
  data$sex <- "Male"
  tab <- lifetab_decrement(data, mx = mx, decrement = cause, sex = sex,
                           infant = "CD", child = "CD")
  ans <- suppressMessages(delete_decrements(tab, decrement = cause, by = sex))
  ans_explicit <- suppressMessages(
    delete_decrements(tab, decrement = cause, by = sex,
                      infant = "CD", child = "CD", closed = "constant")
  )
  expect_equal(ans, ans_explicit)
  ## infant age group
  tab_0 <- tab[tab$age == "0", ]
  l <- sum(tab_0$lx)
  d <- sum(tab_0$dx)
  L <- sum(tab_0$Lx)
  R <- 1 - tab_0$dx[[1]] / d
  expected <- l * (1 - R * (1 - L / l))
  expect_equal(ans$Lx[ans$cause_deleted == "a" & ans$age == "0"], expected)
})

test_that("'delete_decrements' throws error when supplied method is inconsistent with table", {
  data <- make_data_decrement()
  tab <- lifetab_decrement(data, mx = mx, decrement = cause)
  expect_error(suppressMessages(delete_decrements(tab, decrement = cause, closed = "linear")),
               "`closed` is \"linear\", but the life table is not consistent with method \"linear\".")
  expect_no_error(suppressMessages(delete_decrements(tab, decrement = cause, closed = "constant")))
})

test_that("'delete_decrements' gives Inf when all exits in final age group are deleted", {
  data <- make_data_decrement()
  data$mx[data$cause %in% c("b", "c") & data$age == "85+"] <- 0
  tab <- lifetab_decrement(data, mx = mx, decrement = cause)
  ans <- suppressMessages(delete_decrements(tab, decrement = cause))
  ans_a <- ans[ans$cause_deleted == "a", ]
  expect_identical(ans_a$Lx[[19]], Inf)
  expect_identical(ans_a$ex[[1]], Inf)
  ans_b <- ans[ans$cause_deleted == "b", ]
  expect_true(all(is.finite(ans_b$ex)))
})

test_that("'delete_decrements' infers population dimensions and drops other columns", {
  data <- make_data_decrement()
  data <- rbind(transform(data, reg = "x"),
                transform(data, reg = "y", mx = 1.2 * mx))
  tab <- lifetab_decrement(data, mx = mx, decrement = cause, by = reg)
  msgs <- capture_messages(ans_inferred <- delete_decrements(tab, decrement = cause))
  expect_match(msgs, "Using population dimension `reg`.", all = FALSE)
  expect_match(msgs, "Dropping column `popn`, which is not a population dimension.", all = FALSE)
  msgs <- capture_messages(ans_by <- delete_decrements(tab, decrement = cause, by = reg))
  expect_match(msgs, "Dropping column `popn`, which is not a `by` variable.")
  expect_no_match(msgs, "Using population")
  expect_equal(ans_inferred, ans_by)
  tab$popn <- NULL
  expect_no_message(delete_decrements(tab, decrement = cause, by = reg))
  tab_x <- tab[tab$reg == "x", ]
  tab_x$reg <- NULL
  expect_message(delete_decrements(tab_x, decrement = cause),
                 "Treating `data` as a single population.")
})

test_that("'delete_decrements' gives same answer with 'by' and grouped data", {
  skip_if_not_installed("dplyr")
  data <- make_data_decrement()
  data <- rbind(transform(data, reg = "x"),
                transform(data, reg = "y", mx = 1.2 * mx))
  tab <- lifetab_decrement(data, mx = mx, decrement = cause, by = reg)
  ans_by <- suppressMessages(delete_decrements(tab, decrement = cause, by = reg))
  ans_groups <- suppressMessages(delete_decrements(dplyr::group_by(tab, reg), decrement = cause))
  expect_equal(ans_by, ans_groups)
  expect_message(delete_decrements(dplyr::group_by(tab, reg), decrement = cause),
                 "which is not a grouping variable")
})

test_that("'delete_decrements' throws error when cells are missing", {
  data <- make_data_decrement()
  tab <- lifetab_decrement(data, mx = mx, decrement = cause)
  tab <- tab[tab$cause != "c", ]
  expect_error(suppressMessages(delete_decrements(tab, decrement = cause)),
               "Life table columns are not consistent with a complete set of decrement cells.")
})

test_that("'delete_decrements' throws error when variable describing cells is treated as population dimension", {
  data <- make_data_decrement()
  tab <- lifetab_decrement(data, mx = mx, decrement = cause)
  tab$cause_label <- toupper(tab$cause)
  expect_error(suppressMessages(delete_decrements(tab, decrement = cause)),
               "Problem occurred where `cause_label`=\"A\"")
  expect_no_error(suppressMessages(delete_decrements(tab, decrement = c(cause, cause_label))))
})

test_that("'delete_decrements' works with two decrement dimensions", {
  data <- make_data_decrement()
  data2 <- rbind(transform(data, place = "home", mx = 0.4 * mx),
                 transform(data, place = "hosp", mx = 0.6 * mx))
  tab <- lifetab_decrement(data2, mx = mx, decrement = c(cause, place))
  ans <- suppressMessages(delete_decrements(tab, decrement = c(cause, place)))
  expect_identical(names(ans)[1:3], c("cause_deleted", "place_deleted", "age"))
  expect_identical(nrow(ans), nrow(tab))
})

test_that("'delete_decrements' works with rvecs, and matches draw-by-draw calculation", {
  data <- make_data_decrement(n_draw = 3)
  tab <- lifetab_decrement(data, mx = mx, decrement = cause)
  ans <- suppressMessages(delete_decrements(tab, decrement = cause))
  expect_true(rvec::is_rvec(ans$ex))
  m <- as.matrix(data$mx)
  for (i_draw in 1:3) {
    data_i <- data
    data_i$mx <- m[, i_draw]
    tab_i <- lifetab_decrement(data_i, mx = mx, decrement = cause)
    ans_i <- suppressMessages(delete_decrements(tab_i, decrement = cause))
    expect_equal(as.matrix(ans$Lx)[, i_draw], ans_i$Lx)
    expect_equal(as.matrix(ans$ex)[, i_draw], ans_i$ex)
  }
})

test_that("'delete_decrements' works with suffix", {
  data <- make_data_decrement()
  tab <- lifetab_decrement(data, mx = mx, decrement = cause, suffix = "dec")
  ans <- suppressMessages(delete_decrements(tab, decrement = cause, suffix = "dec"))
  expect_identical(names(ans), c("cause_deleted", "age",
                                 "qx.dec", "lx.dec", "dx.dec", "Lx.dec", "ex.dec"))
})

test_that("'delete_decrements' throws expected errors", {
  data <- make_data_decrement()
  tab <- lifetab_decrement(data, mx = mx, decrement = cause)
  expect_error(delete_decrements(tab),
               "No value supplied for `decrement`.")
  expect_error(delete_decrements(tab[-4], decrement = cause),
               "`data` does not have life table column `qx`.")
  expect_error(delete_decrements(tab, decrement = cause, by = cause),
               "`cause` is both a decrement dimension and a `by` variable.")
  expect_error(delete_decrements(tab, decrement = cause, closed = "CD"),
               "Invalid value for `closed`.")
  expect_error(delete_decrements(1, decrement = cause),
               "`data` is not a data frame.")
})


## helpers ---------------------------------------------------------------------

test_that("'check_deleted_method' works", {
  expect_null(check_deleted_method(NULL, nm = "closed", choices = c("constant", "linear")))
  expect_identical(check_deleted_method("linear", nm = "closed", choices = c("constant", "linear")),
                   "linear")
  expect_error(check_deleted_method(c("linear", "constant"), nm = "closed",
                                    choices = c("constant", "linear")),
               "Invalid value for `closed`.")
})

test_that("'is_pop_dim' works", {
  age <- rep(c("0", "1-4", "5+"), times = 2)
  expect_true(is_pop_dim(rep(c("f", "m"), each = 3), age = age))
  expect_false(is_pop_dim(1:6, age = age))
  expect_true(is_pop_dim(rep("x", 6), age = age))
  expect_false(is_pop_dim(c("a", "a", "b", "b", "b", "b"), age = age))
  expect_false(is_pop_dim(rvec::rvec(matrix(1:12, 6)), age = age))
})

test_that("'ax_const_prop' agrees with constant-method ax used by 'lifetab'", {
  m <- c(1e-3, 0.01, 0.5, 5)
  expected <- 1 / m - exp(-m) / (1 - exp(-m))
  expect_equal(ax_const_prop(m), expected)
  ## naive formula loses precision for small values, so compare with series
  expect_equal(ax_const_prop(1e-10), 0.5 - 1e-10 / 12)
  expect_identical(ax_const_prop(Inf), 0)
  ## agrees with ax implied by 'lifetab'
  data <- data.frame(age = c("0-4", "5+"), mx = c(0.2, 0.3))
  data$age <- c("0", "1+")
  lt <- lifetab(data, mx = mx, radix = 1)
  ax <- (lt$Lx[1] - lt$lx[2]) / lt$dx[1]
  expect_equal(ax_const_prop(0.2), ax)
})

test_that("'deleted_Lx_closed' works", {
  ## R = 1 gives original Lx under each method
  q <- 0.2
  nx <- 5
  x <- -log(1 - q)
  L_const <- nx * (1 - exp(-x)) / x
  expect_equal(deleted_Lx_closed(l = 1, q = q, R = 1, nx = nx, L_over_l = NA, method = "constant"),
               L_const)
  expect_equal(deleted_Lx_closed(l = 1, q = q, R = 1, nx = nx, L_over_l = NA, method = "linear"),
               nx * (1 - q / 2))
  expect_equal(deleted_Lx_closed(l = 1, q = q, R = 1, nx = nx, L_over_l = 4.3, method = "table"),
               4.3)
  ## R = 0 means no one leaves
  for (method in c("constant", "linear", "table"))
    expect_equal(deleted_Lx_closed(l = 2, q = q, R = 0, nx = nx, L_over_l = 4.3, method = method),
                 10)
  ## q = 1 with constant method
  expect_equal(deleted_Lx_closed(l = 1, q = 1, R = 0.5, nx = nx, L_over_l = 0, method = "constant"),
               0)
})
