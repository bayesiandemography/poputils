
## 'ex_to_lifetab_brass' ------------------------------------------------------

test_that("'ex_to_lifetab_brass' works with valid inputs", {
    data <- data.frame(ex = c(70, 80),
                       beta = c(1, 1.1),
                       sex = c("Female", "Male"))
    age <- age_labels(type = "lt")
    lx_standard <- 100000 * exp(-0.5 * seq_along(age))
    ax <- rep(NA_real_, times = length(age))
    ans <- ex_to_lifetab_brass(data = data,
                               lx_standard = lx_standard,
                               age = age,
                               ax = ax,
                               infant = "CD",
                               child = "CD")
    expect_true(is.data.frame(ans))
    expect_setequal(names(ans), c("beta", "sex", "age", "qx", "lx", "Lx", "dx", "ex"))
    expect_equal(ans$ex[c(1, nrow(ans)/2 + 1)], c(70, 80), tolerance = 0.001)
})

test_that("'ex_to_lifetab_brass' works with valid inputs - ex is rvec, beta is rvec", {
    data <- data.frame(ex = rvec::rvec(list(c(70, 75), c(71, 76))),
                       beta = rvec::rvec(list(c(1, 1.1), c(1.2, 1.06))),
                       sex = c("Female", "Male"))
    age <- age_labels(type = "lt")
    lx_standard <- 100000 * exp(-0.5 * seq_along(age))
    ax <- rep(NA_real_, times = length(age))
    ans <- ex_to_lifetab_brass(data = data,
                               lx_standard = lx_standard,
                               age = age,
                               ax = ax,
                               infant = "CD",
                               child = "CD")
    expect_true(is.data.frame(ans))
    expect_setequal(names(ans), c("beta", "sex", "age", "qx", "lx", "Lx", "dx", "ex"))
    expect_true(rvec::is_rvec(ans$qx))
})

test_that("'ex_to_lifetab_brass' works with valid inputs - beta is rvec", {
    data <- data.frame(ex = c(70, 75),
                       beta = rvec::rvec(list(c(1, 1.1), c(1.2, 1.06))),
                       sex = c("Female", "Male"))
    age <- age_labels(type = "lt")
    lx_standard <- 100000 * exp(-0.5 * seq_along(age))
    ax <- rep(NA_real_, times = length(age))
    ans <- ex_to_lifetab_brass(data = data,
                               lx_standard = lx_standard,
                               age = age,
                               ax = ax,
                               infant = "CD",
                               child = "CD")
    expect_true(is.data.frame(ans))
    expect_setequal(names(ans), c("beta", "sex", "age", "qx", "lx", "Lx", "dx", "ex"))
    expect_true(rvec::is_rvec(ans$qx))
})




## 'ex_to_lifetab_brass_inner' ------------------------------------------------------

test_that("'ex_to_lifetab_brass_inner' works with valid inputs - not rvec", {
    ex <- c(70, 80)
    beta <- c(1, 1.1)
    n_draw <- NULL
    age <- age_labels(type = "lt")
    sex <- c("Female", "Male")
    lx_standard <- 100000 * exp(-0.5 * seq_along(age))
    ax <- rep(NA_real_, times = length(age))
    methods <- c(infant = "CD", child = "CD", closed = "linear", open = "constant")
    radix <- 1000
    suffix <- "brass"
    ans <- ex_to_lifetab_brass_inner(ex = ex,
                                     beta = beta,
                                     n_draw = n_draw,
                                     lx_standard = lx_standard,
                                     age = age,
                                     sex = sex,
                                     ax = ax,
                                     methods = methods,
                                     radix = radix,
                                     suffix = suffix)
    expect_true(is.data.frame(ans))
    expect_setequal(names(ans), paste(c("qx", "lx", "Lx", "dx", "ex"), "brass", sep = "."))
    expect_equal(ans$ex.brass[c(1, nrow(ans)/2 + 1)], c(70, 80), tolerance = 0.001)
})

test_that("'ex_to_lifetab_brass_inner' works with valid inputs - is rvec", {
    ex <- c(70, 80, 71, 81)
    beta <- c(1, 1.1, 1, 1.1)
    n_draw <- 2L
    age <- age_labels(type = "lt")
    sex <- c("Female", "Male")
    lx_standard <- 100000 * exp(-0.3 * seq_along(age))
    ax <- rep(NA_real_, times = length(age))
    methods <- c(infant = "CD", child = "CD", closed = "linear", open = "constant")
    radix <- 1000
    suffix <- "brass"
    ans <- ex_to_lifetab_brass_inner(ex = ex,
                                     beta = beta,
                                     n_draw = n_draw,
                                     lx_standard = lx_standard,
                                     age = age,
                                     sex = sex,
                                     ax = ax,
                                     methods = methods,
                                     radix = radix,
                                     suffix = suffix)
    expect_true(is.data.frame(ans))
    expect_setequal(names(ans), paste(c("qx", "lx", "Lx", "dx", "ex"), "brass", sep = "."))
    expect_true(rvec::is_rvec(ans$ex.brass))
})


## 'make_ex_beta_n_draw' ------------------------------------------------------

test_that("'make_ex_beta_n_draw' works with ex non-rvec, beta missing", {
    data <- data.frame(ex = 80:81, sex = c("M", "F"))
    ans_obtained <- make_ex_beta_n_draw(data)
    ans_expected <- list(ex = c(80, 81),
                         beta = c(1, 1),
                         n_draw = NULL)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_ex_beta_n_draw' works with ex rvec, beta missing", {
    data <- data.frame(sex = c("M", "F"))
    data$ex <- rvec::rvec(list(80:81, 90:91))
    ans_obtained <- make_ex_beta_n_draw(data)
    ans_expected <- list(ex = c(80, 90, 81, 91),
                         beta = c(1, 1, 1, 1),
                         n_draw = 2L)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_ex_beta_n_draw' works with ex non-rvec, beta non-rvec", {
    data <- data.frame(ex = 80:81, beta = c(1, 0.1))
    ans_obtained <- make_ex_beta_n_draw(data)
    ans_expected <- list(ex = c(80, 81),
                         beta = c(1, 0.1),
                         n_draw = NULL)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_ex_beta_n_draw' works with ex rvec, beta non-rvec", {
    data <- data.frame(beta = c(1, 0.1))
    data$ex <- rvec::rvec(list(80:81, 90:91))
    ans_obtained <- make_ex_beta_n_draw(data)
    ans_expected <- list(ex = c(80, 90, 81, 91),
                         beta = c(1, 0.1, 1, 0.1),
                         n_draw = 2L)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_ex_beta_n_draw' works with ex non-rvec, beta rvec", {
    data <- data.frame(ex = 80:81)
    data$beta <- rvec::rvec(list(c(1, 2), c(0.1, 0.2)))
    ans_obtained <- make_ex_beta_n_draw(data)
    ans_expected <- list(ex = c(80, 81, 80, 81),
                         beta = c(1, 0.1, 2, 0.2),
                         n_draw = 2L)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_ex_beta_n_draw' works with ex rvec, beta rvec", {
    data <- data.frame(sex = c("F", "M"))
    data$ex <- rvec::rvec(list(80:81, 90:91))
    data$beta <- rvec::rvec(list(c(1, 2), c(0.1, 0.2)))
    ans_obtained <- make_ex_beta_n_draw(data)
    ans_expected <- list(ex = c(80, 90, 81, 91),
                         beta = c(1, 0.1, 2, 0.2),
                         n_draw = 2L)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_ex_beta_n_draw' works with ex rvec 1 draw, beta rvec", {
    data <- data.frame(sex = c("F", "M"))
    data$ex <- rvec::rvec(list(80, 90))
    data$beta <- rvec::rvec(list(c(1, 2), c(0.1, 0.2)))
    ans_obtained <- make_ex_beta_n_draw(data)
    ans_expected <- list(ex = c(80, 90, 80, 90),
                         beta = c(1, 0.1, 2, 0.2),
                         n_draw = 2L)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_ex_beta_n_draw' works with ex rvec, beta rvec 1 draw", {
    data <- data.frame(sex = c("F", "M"))
    data$ex <- rvec::rvec(list(80:81, 90:91))
    data$beta <- rvec::rvec(list(1, 0.1))
    ans_obtained <- make_ex_beta_n_draw(data)
    ans_expected <- list(ex = c(80, 90, 81, 91),
                         beta = c(1, 0.1, 1, 0.1),
                         n_draw = 2L)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_ex_beta_n_draw' works throws correct error when ex, beta have different (non-1) draws", {
    data <- data.frame(sex = c("F", "M"))
    data$ex <- rvec::rvec(list(80:82, 90:92))
    data$beta <- rvec::rvec(list(c(1, 0.1), c(2, 0.2)))
    expect_error(make_ex_beta_n_draw(data),
                 "`ex` and `beta` have different numbers of draws.")
})


## 'make_sex_ex_to_lifetab' ---------------------------------------------------

test_that("'make_sex_ex_to_lifetab' works with sex supplied, sex needed", {
    data <- data.frame(ex = 80:81, sex = c("M", "F"))
    methods <- c(infant = "CD",
                 child = "CD",
                 closed = "constant",
                 open = "constant")
    ans_obtained <- make_sex_ex_to_lifetab(data = data, methods = methods)
    ans_expected <- c("Male", "Female")
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_sex_ex_to_lifetab' works with sex supplied, sex not needed", {
    data <- data.frame(ex = 80:81, sex = c("M", "F"))
    methods <- c(infant = "constant",
                 child = "constant",
                 closed = "constant",
                 open = "constant")
    ans_obtained <- make_sex_ex_to_lifetab(data = data, methods = methods)
    ans_expected <- NULL
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_sex_ex_to_lifetab' throws correct error when sex not supplied, sex needed", {
    data <- data.frame(ex = 80:81)
    methods <- c(infant = "CD",
                 child = "CD",
                 closed = "constant",
                 open = "constant")
    expect_error(make_sex_ex_to_lifetab(data = data, methods = methods),
                 "`data` does not have a variable called \"sex\"")
})

test_that("'make_sex_ex_to_lifetab' works with sex not supplied, sex not needed", {
    data <- data.frame(ex = 80:81)
    methods <- c(infant = "constant",
                 child = "constant",
                 closed = "constant",
                 open = "constant")
    ans_obtained <- make_sex_ex_to_lifetab(data = data, methods = methods)
    ans_expected <- NULL
    expect_identical(ans_obtained, ans_expected)
})


