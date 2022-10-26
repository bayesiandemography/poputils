
## 'age_single' ---------------------------------------------------------------

test_that("'age_single' gives correct answer with all defaults", {
    expect_identical(age_single(),
                     c(0:99, "100+"))
})

test_that("'age_single' gives correct answer with non-default 'open'", {
    expect_identical(age_single(open = FALSE),
                     as.character(0:99))
})

test_that("'age_single' gives correct answer with non-default 'min'", {
    expect_identical(age_single(min = 20),
                     as.character(20:99))
})

test_that("'age_single' gives correct answer with non-default 'min' and 'open'", {
    expect_identical(age_single(min = 20, open = TRUE),
                     c(20:99, "100+"))
})

test_that("'age_single' gives correct answer with single open age group", {
    expect_identical(age_single(max = 0),
                     "0+")
    expect_identical(age_single(min = 100, open = TRUE),
                     "100+")
})

test_that("'age_single' gives correct answer with single closed age group", {
    expect_identical(age_single(max = 1, open = FALSE),
                     "0")
    expect_identical(age_single(min = 100, max = 101),
                     "100")
})

test_that("'age_single' gives correct answer with non-default 'min', 'max', and 'open'", {
    expect_identical(age_single(min = 20, max = 30, open = TRUE),
                     c(20:29, "30+"))
})

test_that("'age_single' gives correct answer with negative 'min', 'max'", {
    expect_identical(age_single(min = -20, max = -10),
                     as.character((-20):(-11)))
})

test_that("'age_single' throws correct error when 'max' smaller than 'min'", {
    expect_error(age_single(min = 20, max = 10),
                 "'max' \\[10\\] is less than 'min' \\[20\\]")
})

test_that("'age_single' throws correct error when 'max' equals 'min' and 'open' is FALSE", {
    expect_error(age_single(min = 20, max = 20, open = FALSE),
                 "'max' \\[20\\] equals 'min' \\[20\\] but 'open' is FALSE")
})


## 'age_five' ---------------------------------------------------------------

test_that("'age_five' gives correct answer with all defaults", {
    expect_identical(age_five(),
                     c(paste(seq(0, 95, 5), seq(4, 99, 5), sep = "-"), "100+"))
})

test_that("'age_five' gives correct answer with non-default 'open'", {
    expect_identical(age_five(open = FALSE),
                     paste(seq(0, 95, 5), seq(4, 99, 5), sep = "-"))
})

test_that("'age_five' gives correct answer with non-default 'min'", {
    expect_identical(age_five(min = 20),
                     paste(seq(20, 95, 5), seq(24, 99, 5), sep = "-"))
})

test_that("'age_five' gives correct answer with non-default 'min' and 'open'", {
    expect_identical(age_five(min = 20, open = TRUE),
                     c(paste(seq(20, 95, 5), seq(24, 99, 5), sep = "-"), "100+"))
})

test_that("'age_five' gives correct answer with single open age group", {
    expect_identical(age_five(max = 0),
                     "0+")
    expect_identical(age_five(min = 100, open = TRUE),
                     "100+")
})

test_that("'age_five' gives correct answer with single closed age group", {
    expect_identical(age_five(max = 5, open = FALSE),
                     "0-4")
    expect_identical(age_five(min = 100, max = 105),
                     "100-104")
})

test_that("'age_five' gives correct answer with negative 'min', 'max'", {
    expect_identical(age_five(min = -20, max = -10),
                     c("-20--16", "-15--11"))
})

test_that("'age_five' gives correct answer with non-default 'min', 'max', and 'open'", {
    expect_identical(age_five(min = 20, max = 30, open = TRUE),
                     c("20-24", "25-29", "30+"))
})

test_that("'age_five' throws correct error when 'max' smaller than 'min'", {
    expect_error(age_five(min = 20, max = 10),
                 "'max' \\[10\\] is less than 'min' \\[20\\]")
})

test_that("'age_five' throws correct error when 'max' equals 'min' and 'open' is FALSE", {
    expect_error(age_five(min = 20, max = 20, open = FALSE),
                 "'max' \\[20\\] equals 'min' \\[20\\] but 'open' is FALSE")
})

test_that("'age_five' throws correct error when 'max' minus 'min' not divisible by 5", {
    expect_error(age_five(min = 20, max = 29),
                 "difference between 'max' \\[29\\] and 'min' \\[20\\] is not divisible by 5")
})


## 'age_lt' ---------------------------------------------------------------

test_that("'age_lt' gives correct answer with all defaults", {
    expect_identical(age_lt(),
                     c("0", "1-4", paste(seq(5, 95, 5), seq(9, 99, 5), sep = "-"), "100+"))
})

test_that("'age_lt' gives correct answer with 'max' equals 0", {
    expect_identical(age_lt(max = 0),
                     "0+")
})

test_that("'age_lt' gives correct answer with 'max' equals 1", {
    expect_identical(age_lt(max = 1),
                     c("0", "1+"))
})

test_that("'age_lt' gives correct answer with 'max' equals 5", {
    expect_identical(age_lt(max = 5),
                     c("0", "1-4", "5+"))
})

test_that("'age_lt' gives correct answer with 'max' equals 10", {
    expect_identical(age_lt(max = 10),
                     c("0", "1-4", "5-9", "10+"))
})

test_that("'age_lt' throws correct error when 'max' not a life table value", {
    expect_error(age_lt(max = 2),
                 "'max' \\[2\\] must be one of 0, 1, 5, 10, ...")
})

test_that("'age_lt' throws correct error when 'max' not divisible by 5", {
    expect_error(age_lt(max = 29),
                 "'max' \\[29\\] is not divisible by 5")
})





