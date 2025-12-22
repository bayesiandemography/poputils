
## 'as_all_of_if_vector' ------------------------------------------------------

test_that("as_all_of_if_vector wraps an external character vector symbol", {
  df <- data.frame(country = 1, time = 2, age = 3)
  cols <- c("country", "time")
  q  <- rlang::quo(cols)
  q2 <- as_all_of_if_vector(q)
  expect_true(rlang::is_call(rlang::get_expr(q2), "all_of"))
  sel <- tidyselect::eval_select(q2, data = df)
  expect_identical(sel, c(country = 1L, time = 2L))
})

test_that("as_all_of_if_vector wraps a literal character scalar", {
  df <- data.frame(age = 1, country = 2)
  q2 <- as_all_of_if_vector(rlang::quo("age"))
  expect_true(rlang::is_call(rlang::get_expr(q2), "all_of"))
  sel <- tidyselect::eval_select(q2, data = df)
  expect_identical(sel, c(age = 1L))
})

test_that("as_all_of_if_vector wraps an integer vector (positional selection)", {
  df <- data.frame(a = 1, b = 2, c = 3)
  q2 <- as_all_of_if_vector(rlang::quo(c(1L, 3L)))
  expect_true(rlang::is_call(rlang::get_expr(q2), "all_of"))
  sel <- tidyselect::eval_select(q2, data = df)
  expect_identical(sel, c(a = 1L, c = 3L))
})

test_that("as_all_of_if_vector leaves tidyselect expressions unchanged", {
  q <- rlang::quo(tidyselect::starts_with("c"))
  q2 <- as_all_of_if_vector(q)
  expect_identical(rlang::get_expr(q2), rlang::get_expr(q))
})

test_that("as_all_of_if_vector leaves non character/integer values unchanged", {
  q_num <- rlang::quo(1.5)          # double
  q_lst <- rlang::quo(list("x"))    # list
  q_nil <- rlang::quo(NULL)
  expect_identical(rlang::get_expr(as_all_of_if_vector(q_num)), rlang::get_expr(q_num))
  expect_identical(rlang::get_expr(as_all_of_if_vector(q_lst)), rlang::get_expr(q_lst))
  expect_identical(rlang::get_expr(as_all_of_if_vector(q_nil)), rlang::get_expr(q_nil))
})


## 'groups_colnums' -----------------------------------------------------------

test_that("'groups_colnums' works with grouped data frame", {
    data <- data.frame(a = -1, b = 99, c = "x")
    data <- dplyr::group_by(data, c, a)
    ans_obtained <- groups_colnums(data)
    ans_expected <- c(c = 3L, a = 1L)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'groups_colnums' works with non-grouped data frame", {
    data <- data.frame(a = -1, b = 99, c = "x")
    ans_obtained <- groups_colnums(data)
    ans_expected <- integer()
    names(ans_expected) <- character()
    expect_identical(ans_obtained, ans_expected)
})

test_that("'groups_colnums' throws appopriate error with non data frame", {
    expect_error(groups_colnums("a"),
                 "`data` is not a data frame.")
})

    
## 'make_str_key' -------------------------------------------------------------

test_that("'make_str_key' works with valid input", {
    row <- data.frame(a = 1, b = "x")
    row$c <- list(1:3)
    row$d <- rvec::rvec(matrix(1:3, 1))
    ans <- make_str_key(row)
    expect_true(is.character(ans))
    expect_identical(length(ans), 1L)
})
    
    
## 'matrix_to_list_of_cols' ---------------------------------------------------

test_that("'matrix_to_list_of_cols' works with nrow > 0, ncol > 0", {
    m <- matrix(1:12, nr = 4, nc = 3)
    colnames(m) <- c("a", "b", "c")
    ans_obtained <- matrix_to_list_of_cols(m)
    ans_expected <- list(a = 1:4, b = 5:8, c = 9:12)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'matrix_to_list_of_cols' works with nrow = 0, ncol > 0", {
    m <- matrix(NA, nr = 0, nc = 3)
    ans_obtained <- matrix_to_list_of_cols(m)
    ans_expected <- list(logical(), logical(), logical())
    expect_identical(ans_obtained, ans_expected)
})

test_that("'matrix_to_list_of_cols' works with nrow > 0, ncol = 0", {
    m <- matrix(NA, nr = 3, nc = 0)
    ans_obtained <- matrix_to_list_of_cols(m)
    ans_expected <- list()
    expect_identical(ans_obtained, ans_expected)
})

test_that("'matrix_to_list_of_cols' works with nrow = 0, ncol = 0", {
    m <- matrix(1, nr = 0, nc = 0)
    ans_obtained <- matrix_to_list_of_cols(m)
    ans_expected <- list()
    expect_identical(ans_obtained, ans_expected)
})

test_that("'matrix_to_list_of_cols' raises expected error message when m is not a matrix", {
    expect_error(matrix_to_list_of_cols("a"),
                 "`m` is not a matrix.")
})


## 'matrix_to_list_of_rows' ---------------------------------------------------

test_that("'matrix_to_list_of_rows' works with nrow > 0, ncol > 0", {
    m <- matrix(1:12, nr = 4, nc = 3, byrow = TRUE)
    rownames(m) <- c("a", "b", "c", "d")
    ans_obtained <- matrix_to_list_of_rows(m)
    ans_expected <- list(a = 1:3, b = 4:6, c = 7:9, d = 10:12)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'matrix_to_list_of_rows' works with nrow = 0, ncol > 0", {
    m <- matrix(NA, nr = 0, nc = 3)
    ans_obtained <- matrix_to_list_of_rows(m)
    ans_expected <- list()
    expect_identical(ans_obtained, ans_expected)
})

test_that("'matrix_to_list_of_rows' works with nrow > 0, ncol = 0", {
    m <- matrix(NA, nr = 3, nc = 0)
    ans_obtained <- matrix_to_list_of_rows(m)
    ans_expected <- list(logical(), logical(), logical())
    expect_identical(ans_obtained, ans_expected)
})

test_that("'matrix_to_list_of_rows' works with nrow = 0, ncol = 0", {
    m <- matrix(1, nr = 0, nc = 0)
    ans_obtained <- matrix_to_list_of_rows(m)
    ans_expected <- list()
    expect_identical(ans_obtained, ans_expected)
})

test_that("'matrix_to_list_of_rows' raises expected error message when m is not a matrix", {
    expect_error(matrix_to_list_of_rows("a"),
                 "`m` is not a matrix.")
})
