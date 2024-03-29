

## 'find_label_female' -----------------------------------------------------------------

test_that("'find_label_female' returns female with valid input", {
    expect_identical(find_label_female(c("female", "x", "male", NA)),
                     "female")
    expect_identical(find_label_female(c(22, "F", "M", "agex", NA)),
                     "F")
    expect_identical(find_label_female("WOMEN"),
                     "WOMEN")
})

test_that("'find_label_female' returns NULL with no valid values", {
    expect_identical(find_label_female(c("ff", "x", "mm", NA)),
                     NULL)
})

test_that("'find_label_female' returns NULL with two or more valid values", {
    expect_identical(find_label_female(c("f", "fem")),
                     NULL)
})


## 'find_label_male' -----------------------------------------------------------------

test_that("'find_label_male' returns male with valid input", {
    expect_identical(find_label_male(c("female", "x", "male", NA)),
                     "male")
    expect_identical(find_label_male(c(22, "F", "M", "agex", NA)),
                     "M")
    expect_identical(find_label_male("MEN"),
                     "MEN")
})

test_that("'find_label_male' returns NULL with no valid values", {
    expect_identical(find_label_male(c("ff", "x", "mm", NA)),
                     NULL)
})

test_that("'find_label_male' returns NULL with two or more valid values", {
    expect_identical(find_label_male(c("m", "male")),
                     NULL)
})


## 'find_var_age' -----------------------------------------------------------------

test_that("'find_var_age' returns age with valid input", {
    expect_identical(find_var_age(c("age", "x", "agex", NA)),
                     "age")
    expect_identical(find_var_age(c(22, "Age.Years", "x", "agex", NA)),
                     "Age.Years")
    expect_identical(find_var_age("age_group"),
                     "age_group")
})

test_that("'find_var_age' returns NULL with no valid values", {
    expect_identical(find_var_age(c("sage", "x", "agex", NA)),
                     NULL)
    expect_identical(find_var_age(c(22, "Age.Days", "x", "agex", NA)),
                     NULL)
})

test_that("'find_var_age' returns NULL with two or more valid values", {
    expect_identical(find_var_age(c("age", "x", "ages", "age.group", NA)),
                     NULL)
})


## 'find_var_sexgender' ----------------------------------------------------------------

test_that("'find_var_sexgender' returns sexgender with valid input", {
    expect_identical(find_var_sexgender(c("sexgender", "x", "sexgenderx", NA)),
                     "sexgender")
    expect_identical(find_var_sexgender(c(22, "gender", "x", "sexgenderx", NA)),
                     "gender")
    expect_identical(find_var_sexgender("Sexes"),
                     "Sexes")
})

test_that("'find_var_sexgender' returns NULL with no valid values", {
    expect_identical(find_var_sexgender(c("ssexgender", "x", "xxx", NA)),
                     NULL)
    expect_identical(find_var_sexgender(c(22, "Time", "x", "sexgenderx", NA)),
                     NULL)
})

test_that("'find_var_sexgender' returns NULL with two or more valid values", {
    expect_identical(find_var_sexgender(c("sexgender", "x", "sexes", "month__year", NA)),
                     NULL)
})


## 'find_var_time' ----------------------------------------------------------------

test_that("'find_var_time' returns time with valid input", {
    expect_identical(find_var_time(c("time", "x", "timex", NA)),
                     "time")
    expect_identical(find_var_time(c(22, "quarters", "x", "timex", NA)),
                     "quarters")
    expect_identical(find_var_time("month_year"),
                     "month_year")
})

test_that("'find_var_time' returns NULL with no valid values", {
    expect_identical(find_var_time(c("stime", "x", "timex", NA)),
                     NULL)
    expect_identical(find_var_time(c(22, "Time.Days", "x", "timex", NA)),
                     NULL)
})

test_that("'find_var_time' returns NULL with two or more valid values", {
    expect_identical(find_var_time(c("time", "x", "times", "month__year", NA)),
                     NULL)
})


## 'find_inner' ---------------------------------------------------------------

test_that("'find_inner' returns element of 'nms' with valid input", {
    expect_identical(find_inner(c("time", "x", "blaa", NA),
                                p_valid = "time"),
                     "time")
})

test_that("'find_inner' returns NULL with no valid values", {
    expect_identical(find_inner(c("timey", "x", "timex", NA),
                                p_valid = "time"),
                     NULL)
})

test_that("'find_inner' returns NULL with two valid values", {
    expect_identical(find_inner(c("time", "x", "time", NA),
                                p_valid = "time"),
                     NULL)
})


