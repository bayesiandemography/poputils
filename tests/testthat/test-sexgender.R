
test_that("'reformat_sex' works with valid inputs - factor, length > 0", {
    x <- c("fem", "MA", "  boys  ", NA, "F")
    ans_obtained <- reformat_sex(x)
    ans_expected <- factor(c("Female", "Male", "Male", NA, "Female"),
                           levels = c("Female", "Male", NA),
                           exclude = character())
    expect_identical(ans_obtained, ans_expected)
    x <- NA
    ans_obtained <- reformat_sex(x)
    ans_expected <- factor(NA,
                           levels = c("Female", "Male", NA),
                           exclude = character())
    expect_identical(ans_obtained, ans_expected)
})

test_that("'reformat_sex' works with valid inputs - character, length > 0", {
    x <- c("fem", "MA", "  boys  ", NA, "F")
    ans_obtained <- reformat_sex(x, factor = FALSE)
    ans_expected <- c("Female", "Male", "Male", NA, "Female")
    expect_identical(ans_obtained, ans_expected)
})

test_that("'reformat_sex' works with valid inputs - factor, length == 0", {
    ans_obtained <- reformat_sex(character())
    ans_expected <- factor(character(), levels = c("Female", "Male"))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'reformat_sex' works with valid inputs - character, length == 0", {
    ans_obtained <- reformat_sex(character(), factor = FALSE)
    ans_expected <- character()
    expect_identical(ans_obtained, ans_expected)
})

test_that("'reformat_sex' works with vector except", {
    x <- c("fem", "MA", "  boys  ", NA, "F", "X", "Y")
    ans_obtained <- reformat_sex(x, factor = FALSE, except = c("X", "Y"))
    ans_expected <- c("Female", "Male", "Male", NA, "Female", "X", "Y")
    expect_identical(ans_obtained, ans_expected)
})

test_that("'reformat_sex' works with factor except", {
    x <- c("fem", "MA", "  boys  ", NA, "F", "X", "Y")
    ans_obtained <- reformat_sex(x, factor = FALSE, except = factor(c("X", "Y")))
    ans_expected <- c("Female", "Male", "Male", NA, "Female", "X", "Y")
    expect_identical(ans_obtained, ans_expected)
})

test_that("'reformat_sex' works when x is factor", {
    x <- factor(c("F", "M", NA))
    ans_obtained <- reformat_sex(x)
    ans_expected <- factor(c("Female", "Male", NA), exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'reformat_sex' works when x is factor, with except", {
    x <- factor(c("F", "M", NA, "D"))
    ans_obtained <- reformat_sex(x, except = "D")
    ans_expected <- factor(c("Female", "Male", NA, "D"),
                           levels = c("Female", "Male", "D", NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'reformat_sex' accepts singular woman, girl, man, and boy", {
    x <- c("woman", "girl", "man", "boy")
    expect_identical(reformat_sex(x, factor = FALSE),
                     c("Female", "Female", "Male", "Male"))
})

test_that("'reformat_sex' throws correct error with invalid label", {
    expect_error(reformat_sex("wrong"),
                 "Can't parse label \"wrong\".")
})

test_that("'reformat_sex' throws correct error with invalid except", {
    expect_error(reformat_sex("M", except = lm),
                 "`except` is not a vector or a factor.")
})






    


## 'sex_is_missing', 'sex_is_total' -------------------------------------------

test_that("'sex_is_missing' recognises missing labels", {
  x <- c("Missing", "Not stated", "NOT_STATED", "unknown", "Unk", "N/A",
         "Don't know", "Unspecified", "Sex not stated", "Gender: unknown", NA)
  expect_true(all(sex_is_missing(x)))
})

test_that("'sex_is_missing' returns FALSE for other labels", {
  x <- c("Female", "M", "Total", "Both sexes", "Other", "X", "", "sex", "Nat")
  expect_false(any(sex_is_missing(x)))
})

test_that("'sex_is_total' recognises total labels", {
  x <- c("Total", "TOTAL", "All", "Overall", "Both", "Both sexes",
         "both_sexes", "All sexes", "Persons", "People", "All persons",
         "Total, both sexes", "Total (persons)", "Total - all sexes",
         "Sex: total")
  expect_true(all(sex_is_total(x)))
})

test_that("'sex_is_total' returns FALSE for other labels, including NA", {
  x <- c("Female", "M", "Not stated", NA, "Other", "Totals?x", "", "Person x")
  expect_false(any(sex_is_total(x)))
})

test_that("'sex_is_missing' and 'sex_is_total' never overlap with each other or with reformat_sex", {
  x <- c("Female", "Male", "F", "m", "women", "boys", "Missing", "Not stated",
         NA, "Total", "Both sexes", "Persons", "Other")
  is_missing <- sex_is_missing(x)
  is_total <- sex_is_total(x)
  expect_false(any(is_missing & is_total))
  is_sex <- !is.na(x) & !is_missing & !is_total & x != "Other"
  expect_silent(reformat_sex(x[is_sex]))
})

test_that("'sex_is_missing' and 'sex_is_total' work with factors and zero-length input", {
  x <- factor(c("Female", "Not stated", "Total", NA), exclude = NULL)
  expect_identical(sex_is_missing(x), c(FALSE, TRUE, FALSE, TRUE))
  expect_identical(sex_is_total(x), c(FALSE, FALSE, TRUE, FALSE))
  expect_identical(sex_is_missing(character()), logical())
  expect_identical(sex_is_total(character()), logical())
  expect_identical(sex_is_missing(NA), TRUE)
  expect_identical(sex_is_total(NA), FALSE)
})

test_that("'sex_is_missing' and 'sex_is_total' throw expected errors", {
  expect_error(sex_is_missing(data.frame(x = 1)),
               "`x` is not a vector or a factor.")
  expect_error(sex_is_total(list("Total")),
               "`x` is a list.")
})

test_that("'clean_sex_labels' works", {
  expect_identical(clean_sex_labels(c("Sex not stated", "GENDER", "Both-Sexes", NA)),
                   c("notstated", "gender", "bothsexes", NA))
})
