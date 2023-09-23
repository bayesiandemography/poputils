
## 'check_at_most_one_colnum' -------------------------------------------------

test_that("'check_at_most_one_colnum' returns TRUE with valid inputs", {
    z <- integer()
    names(z) <- character()
    expect_true(check_at_most_one_colnum(list(x = c(a = 1L), y = c(b = 2L), z = z)))
    expect_true(check_at_most_one_colnum(list()))
})

test_that("'check_at_most_one_colnum' raises correct error with length-2 vector", {
    expect_error(check_at_most_one_colnum(list(x = c(a = 1L), y = c(b = 2:3))),
                 "2 variables specified for `y`.")
})


## 'check_flag' ---------------------------------------------------------------

test_that("'check_flag' returns TRUE with valid inputs", {
    x <- TRUE
    expect_true(check_flag(x))
    x <- FALSE
    expect_true(check_flag(x))
})

test_that("'check_flag' throws expected error non-length-1", {
    y <- logical()
    expect_error(check_flag(y),
                 "`y` does not have length 1")
    z <- c(TRUE, TRUE)
    expect_error(check_flag(z),
                 "`z` does not have length 1")
})

test_that("'check_flag' throws expected error non-logical", {
    x <- "hello"
    expect_error(check_flag(x),
                 "`x` does not have class <logical>")
})

test_that("'check_flag' throws expected error NA", {
    x <- NA
    expect_error(check_flag(x),
                 "`x` is NA")
})


## 'check_infant_child_age_compatible' ----------------------------------------

test_that("'check_infant_child_age_compatible' returns TRUE when value supplied for 'infant' appropriately", {
    expect_true(check_infant_child_age_compatible(infant_supplied = TRUE,
                                                  child_supplied = FALSE,
                                                  age = c("0", "1", "2", "3+"),
                                                  methods = c(infant = "CD",
                                                              child = "constant",
                                                              closed = "constant",
                                                              open = "constant")))
})

test_that("'check_infant_child_age_compatible' returns TRUE when value supplied for 'child' appropriately", {
    expect_true(check_infant_child_age_compatible(infant_supplied = FALSE,
                                                  child_supplied = TRUE,
                                                  age = c("0", "1-4", "5-9", "10+"),
                                                  methods = c(infant = "constant",
                                                              child = "CD",
                                                              closed = "constant",
                                                              open = "constant")))
})

test_that("'check_infant_child_age_compatible' returns TRUE when no value supplied for 'infant' or 'child'", {
    expect_true(check_infant_child_age_compatible(infant_supplied = FALSE,
                                                  child_supplied = FALSE,
                                                  age = c("0-4", "5-9", "10-14", "15+"),
                                                  methods = c(infant = "constant",
                                                              child = "constant",
                                                              closed = "constant",
                                                              open = "constant")))    
})

test_that("'check_infant_child_age_compatible' when value supplied for 'infant' inappropriately", {
    expect_error(check_infant_child_age_compatible(infant_supplied = TRUE,
                                                   child_supplied = FALSE,
                                                   age = c("0-4", "5-9", "10-14", "15+"),
                                                   methods = c(infant = "CD",
                                                               child = "constant",
                                                               closed = "constant",
                                                               open = "constant")),
    "Value supplied for `infant`, but `age` does not include age group \"0\"")
})

test_that("'check_infant_child_age_compatible' when value supplied for 'child' inappropriately", {
    expect_error(check_infant_child_age_compatible(infant_supplied = FALSE,
                                                   child_supplied = TRUE,
                                                   age = c("0-4", "5-9", "10-14", "15+"),
                                                   methods = c(infant = "CD",
                                                               child = "constant",
                                                               closed = "constant",
                                                               open = "constant")),
    "Value supplied for `child`, but `age` does not include age group \"1-4\"")
})    


## 'check_lifeexp_sex' --------------------------------------------------------

test_that("'check_lifeexp_sex' returns TRUE with valid inputs", {
    sex <- "Female"
    expect_true(check_lifeexp_sex(sex))
    sex <- c("Male", "Male", "Male")
    expect_true(check_lifeexp_sex(sex))
})

test_that("'check_lifeexp_sex' throws correct error with non-character", {
    sex <- c(TRUE, FALSE)
    expect_error(check_lifeexp_sex(sex),
                 "`sex` is not a character vector.")
})

test_that("'check_lifeexp_sex' throws correct error with invalid value", {
    sex <- c("F", "F")
    expect_error(check_lifeexp_sex(sex),
                 "`sex` has invalid value.")
})

test_that("'check_lifeexp_sex' throws correct error when values vary", {
    sex <- c("Female", "Male")
    expect_error(check_lifeexp_sex(sex),
                 "Values for `sex` not all the same.")
})


## 'check_method_compatible_with_age' -----------------------------------------

test_that("'check_method_compatible_with_age' returns TRUE with valid inputs", {
    


## 'check_mx' -----------------------------------------------------------------

test_that("'check_mx_rvec' returns TRUE with valid rvec inputs", {
    x <- rvec::rvec_dbl()
    expect_true(check_mx(x))
    x <- rvec::rvec_dbl(matrix(1:6))
    expect_true(check_mx(x))
    x <- rvec::rvec_int(matrix(1:6))
    expect_true(check_mx(x))
})

test_that("'check_mx_vec' returns TRUE with valid vector inputs", {
    x <- 1:3
    expect_true(check_mx(x))
    x <- c(0.2, 0.1, NA)
    expect_true(check_mx(x))
    x <- double()
    expect_true(check_mx(x))
})

test_that("'check_mx' throws correct error with non-numeric", {
    x <- rvec::rvec_lgl()
    expect_error(check_mx(x),
                 "`mx` is non-numeric")
    x <- NULL
    expect_error(check_mx(x),
                 "`mx` is non-numeric")
    x <- c(TRUE, FALSE)
    expect_error(check_mx(x),
                 "`mx` is non-numeric")
})

test_that("'check_mx' throws correct error with negative value", {
    x <- rvec::rvec_dbl(matrix(c(1, 0, NA, -0.1), nrow = 1))
    expect_error(check_mx(x),
                 "`mx` has negative value.")
    expect_error(check_mx(c(1, -1, 0, -1, 1)),
                 "`mx` has negative values.")
})


## 'check_no_overlap_colnums' -------------------------------------------------

test_that("'check_no_overlap_colnums' returns TRUE with valid inputs - 3 elements", {
    x <- list(x = c(a = 1L, b = 2L), y = c(c = 3L), z = integer())
    expect_true(check_no_overlap_colnums(x))
})

test_that("'check_no_overlap_colnums' returns TRUE with valid inputs - 1 element", {
    x <- list(x = c(a = 1L, b = 2L))
    expect_true(check_no_overlap_colnums(x))
})

test_that("'check_no_overlap_colnums' returns TRUE with valid inputs - 0 elements", {
    x <- list()
    expect_true(check_no_overlap_colnums(x))
})

test_that("'check_no_overlap_colnums' throws correct error with overlap", {
    x <- list(x = c(a = 1L, b = 2L), y = c(b = 2L, a = 1L))
    expect_error(check_no_overlap_colnums(x),
                 "`x` and `y` use the same variables.")
})


## 'check_no_overlap_colnums_pair' --------------------------------------------

test_that("'check_no_overlap_colnums_pair' returns TRUE with valid inputs - both nonempty", {
    pair <- list(x = c(a = 1L, b = 2L), y = c(c = 3L))
    expect_true(check_no_overlap_colnums_pair(pair = pair))
})

test_that("'check_no_overlap_colnums_pair' returns TRUE with valid inputs - one empty", {
    pair <- list(x = c(a = 1L, b = 2L), y = integer())
    expect_true(check_no_overlap_colnums_pair(pair = pair))
})

test_that("'check_no_overlap_colnums_pair' throws correct error with one overlap", {
    pair <- list(x = c(a = 1L, b = 2L), y = c(c = 3L, a = 1L))
    expect_error(check_no_overlap_colnums_pair(pair = pair),
                 "`x` and `y` use the same variable.")
})

test_that("'check_no_overlap_colnums_pair' throws correct error with two overlap", {
    pair <- list(x = c(a = 1L, b = 2L), y = c(b = 2L, a = 1L))
    expect_error(check_no_overlap_colnums_pair(pair = pair),
                 "`x` and `y` use the same variables.")
})


## 'check_number' --------------------------------------------------------------

test_that("'check_number' returns TRUE with valid inputs", {
    expect_true(check_number(1L, x_arg = "x", is_positive = TRUE, is_nonneg = TRUE, is_whole = TRUE))
    expect_true(check_number(0.001, x_arg = "x", is_positive = TRUE,
                             is_nonneg = TRUE, is_whole = FALSE))
    expect_true(check_number(0, x_arg = "x", is_positive = FALSE, is_nonneg = TRUE, is_whole = TRUE))
    expect_true(check_number(-1, x_arg = "x", is_positive = FALSE, is_nonneg = FALSE, is_whole = TRUE))
})

test_that("'check_number' returns correct error with non-numeric", {
    expect_error(check_number("1", x_arg = "x", is_positive = TRUE, is_nonneg = TRUE, is_whole = TRUE),
                 "`x` is non-numeric.")
})

test_that("'check_number' returns correct error with wrong length", {
    expect_error(check_number(1:2, x_arg = "x", is_positive = TRUE, is_nonneg = TRUE, is_whole = TRUE),
                 "`x` does not have length 1.")
})

test_that("'check_number' returns correct error with NA", {
    expect_error(check_number(NA_real_, x_arg = "x", is_positive = TRUE,
                              is_nonneg = TRUE, is_whole = TRUE),
                 "`x` is NA.")
})

test_that("'check_number' returns correct error with Inf", {
    expect_error(check_number(Inf, x_arg = "x", is_positive = TRUE, is_nonneg = TRUE, is_whole = TRUE),
                 "`x` is infinite.")
})

test_that("'check_number' returns correct error with is_positive", {
    expect_error(check_number(0, x_arg = "x", is_positive = TRUE, is_nonneg = TRUE, is_whole = TRUE),
                 "`x` is non-positive.")
    expect_error(check_number(-1, x_arg = "x", is_positive = TRUE, is_nonneg = TRUE, is_whole = TRUE),
                 "`x` is non-positive.")
})

test_that("'check_number' returns correct error with is_nonneg", {
    expect_error(check_number(-1, x_arg = "x", is_positive = FALSE, is_nonneg = TRUE, is_whole = TRUE),
                 "`x` is negative.")
})

test_that("'check_number' returns correct error with is_whole", {
    expect_error(check_number(0.5, x_arg = "x", is_positive = FALSE,
                              is_nonneg = TRUE, is_whole = TRUE),
                 "`x` is not a whole number.")
})


## 'check_sex_not_needed' -----------------------------------------------------

test_that("'check_sex_not_needed' returns TRUE when methods don't require sex variable", {
    methods <- c(infant = "constant",
                 child = "linear",
                 closed = "linear",
                 open = "constant")
    expect_true(check_sex_not_needed(methods))
})

test_that("'check_sex_not_needed' returns correct error when methods do require sex variable", {
    methods <- c(infant = "constant",
                 child = "CD",
                 closed = "linear",
                 open = "constant")
    expect_error(check_sex_not_needed(methods),
                 "`child` is \"CD\" but no value supplied for `sex`")
})

    
## 'check_string' -------------------------------------------------------------

test_that("'check_string' returns TRUE with valid input", {
    expect_true(check_string(x = "z", x_arg = "x"))
    expect_true(check_string(x = "helloworld", x_arg = "x"))
})

test_that("'check_string' returns error with non-character", {
    expect_error(check_string(x = 1, x_arg = "y"),
                 "`y` is non-character.")
})

test_that("'check_string' returns error with length 2", {
    expect_error(check_string(x = c("a", "b"), x_arg = "y"),
                 "`y` does not have length 1.")
})

test_that("'check_string' returns error with NA", {
    expect_error(check_string(x = NA_character_, x_arg = "y"),
                 "`y` is NA.")
})

test_that("'check_string' returns error with nchar = 0", {
    expect_error(check_string(x = "", x_arg = "y"),
                 "`y` is blank.")
})

test_that("'check_string' returns error with blanks", {
    expect_error(check_string(x = "hello world", x_arg = "y"),
                 "`y` contains blanks.")
})







## 'check_valid_colnum_list' --------------------------------------------------

test_that("'check_valid_colnum_list' returns TRUE with valid inputs - 3 elements", {
    z <- integer()
    names(z) <- character()
    x <- list(x = c(a = 1L, b = 2L), y = c(c = 3L), z = z)
    expect_true(check_valid_colnum_list(x))
})

test_that("'check_valid_colnum_list' returns TRUE with valid inputs - 0 elements", {
    x <- list()
    expect_true(check_valid_colnum_list(x))
})

test_that("'check_valid_colnum_list' throws expected error when non-list", {
    expect_error(check_valid_colnum_list(NULL),
                 "Internal error: `x` is not a list.")
})

test_that("'check_valid_colnum_list' throws expected error when x does not have names", {
    expect_error(check_valid_colnum_list(list(c(a = 1L))),
                 "Internal error: `x` does not have names.")
})

test_that("'check_valid_colnum_list' throws expected error when x has duplicated names", {
    expect_error(check_valid_colnum_list(list(x = c(a = 1L), x = c(b = 2L))),
                 "Internal error: names for `x` have duplicates.")
})

test_that("'check_valid_colnum_list' throws expected error when x not all integer", {
    expect_error(check_valid_colnum_list(list(x = c(a = 1L), y = c(b = 2))),
                 "Internal error: elements of `x` are not all integer vectors.")
})

test_that("'check_valid_colnum_list' throws expected error when x not named", {
    expect_error(check_valid_colnum_list(list(x = c(a = 1L), y = 2L)),
                 "Internal error: elements of `x` are not all named.")
})



    


    




