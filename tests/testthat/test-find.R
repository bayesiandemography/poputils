

## 'find_label_female' -----------------------------------------------------------------

test_that("'find_label_female' returns female with valid input", {
    expect_identical(find_label_female(c("female", "x", "male", NA)),
                     "female")
    expect_identical(find_label_female(c(22, "F", "M", "agex", NA)),
                     "F")
    expect_identical(find_label_female("WOMEN"),
                     "WOMEN")
    expect_identical(find_label_female("woman"),
                     "woman")
})

test_that("'find_label_female' returns NULL with no valid values", {
    expect_identical(find_label_female(c("ff", "x", "mm", NA)),
                     NULL)
    expect_identical(find_label_female("girlsclub"),
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
    expect_identical(find_label_male("man"),
                     "man")
})

test_that("'find_label_male' returns NULL with no valid values", {
    expect_identical(find_label_male(c("ff", "x", "mm", NA)),
                     NULL)
    expect_identical(find_label_male("boysroom"),
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



## 'find_var_popn' ------------------------------------------------------------

test_that("'find_var_popn' returns population variable with valid input", {
    expect_identical(find_var_popn(c("popn", "x", "popnx", NA)),
                     "popn")
    expect_identical(find_var_popn(c(22, "Population", "age", NA)),
                     "Population")
    expect_identical(find_var_popn("COUNT"),
                     "COUNT")
    expect_identical(find_var_popn("pop."),
                     "pop.")
    expect_identical(find_var_popn("Values"),
                     "Values")
    expect_identical(find_var_popn(c("age", "sex", "Persons")),
                     "Persons")
    expect_identical(find_var_popn(c("Age", "person")),
                     "person")
    expect_identical(find_var_popn(c("age", "sex", "n")),
                     "n")
})

test_that("'find_var_popn' returns NULL with no valid values", {
    expect_identical(find_var_popn(c("people", "nn", "popular", NA)),
                     NULL)
    expect_identical(find_var_popn(character()),
                     NULL)
})

test_that("'find_var_popn' returns NULL with two or more valid values", {
    expect_identical(find_var_popn(c("popn", "count")),
                     NULL)
    expect_identical(find_var_popn(c("n", "Persons")),
                     NULL)
})


## 'error' argument of 'find_var_*' -------------------------------------------

test_that("'find_var_*' with 'error = TRUE' return the match when there is one", {
    expect_identical(find_var_age(c("Sex", "AgeGroup"), error = TRUE), "AgeGroup")
    expect_identical(find_var_popn(c("Sex", "count"), error = TRUE), "count")
    expect_identical(find_var_sexgender(c("Sex", "Age"), error = TRUE), "Sex")
    expect_identical(find_var_time(c("year", "Age"), error = TRUE), "year")
})

test_that("'find_var_*' with 'error = TRUE' raise error when no match", {
    expect_error(find_var_age(c("Sex", "Year"), error = TRUE),
                 "Can't find an age variable")
    expect_error(find_var_popn(c("age", "sex", "Total"), error = TRUE),
                 "Can't find a population variable")
    expect_error(find_var_sexgender(c("Age", "Year"), error = TRUE),
                 "Can't find a sex or gender variable")
    expect_error(find_var_time(c("Age", "Sex"), error = TRUE),
                 "Can't find a time variable")
})

test_that("'find_var_*' with 'error = TRUE' raise error when several matches", {
    expect_error(find_var_age(c("age", "ages"), error = TRUE),
                 "Found more than one age variable")
    expect_error(find_var_popn(c("count", "popn"), error = TRUE),
                 "Found more than one population variable")
    expect_error(find_var_sexgender(c("sex", "gender"), error = TRUE),
                 "Found more than one sex or gender variable")
    expect_error(find_var_time(c("time", "year"), error = TRUE),
                 "Found more than one time variable")
})

test_that("'find_var_*' error names the calling function", {
    f <- function(nms) find_var_popn(nms, error = TRUE)
    err <- expect_error(f(c("age", "sex")))
    expect_identical(err$call, quote(f(c("age", "sex"))))
})

test_that("'find_var_*' raise error when 'error' is not a flag", {
    expect_error(find_var_age("age", error = NA),
                 "`error` is")
    expect_error(find_var_age("age", error = "yes"),
                 "`error` does not have class")
})


## 'find_var_inner' -----------------------------------------------------------

test_that("'find_var_inner' returns element, NULL, or error", {
    expect_identical(find_var_inner(c("time", "x"), valid = "time",
                                    desc = "time", article = "a",
                                    error = FALSE, call = NULL),
                     "time")
    expect_identical(find_var_inner(c("y", "x"), valid = "time",
                                    desc = "time", article = "a",
                                    error = FALSE, call = NULL),
                     NULL)
    expect_error(find_var_inner(c("y", "x"), valid = "time",
                                desc = "time", article = "a",
                                error = TRUE, call = NULL),
                 "Recognised names")
    expect_error(find_var_inner(c("time", "Time"), valid = "time",
                                desc = "time", article = "a",
                                error = TRUE, call = NULL),
                 "Keep one, or rename the others")
})


## 'find_matches' -------------------------------------------------------------

test_that("'find_matches' cleans names before matching", {
    expect_identical(find_matches(c("Age.Group", "x", NA, "age_group"),
                                  valid = "agegroup"),
                     c(1L, 4L))
    expect_identical(find_matches(character(), valid = "agegroup"),
                     integer())
})

## 'find_inner' ---------------------------------------------------------------

test_that("'find_inner' returns element of 'nms' with valid input", {
    expect_identical(find_inner(c("time", "x", "blaa", NA),
                                valid = "time"),
                     "time")
})

test_that("'find_inner' returns NULL with no valid values", {
    expect_identical(find_inner(c("timey", "x", "timex", NA),
                                valid = "time"),
                     NULL)
})

test_that("'find_inner' returns NULL with two valid values", {
    expect_identical(find_inner(c("time", "x", "time", NA),
                                valid = "time"),
                     NULL)
})




test_that("'find_var_*' errors have classes for no match and several matches", {
  expect_error(find_var_time(c("Age", "Sex"), error = TRUE),
               class = "poputils_error_find_var_none")
  expect_error(find_var_time(c("Age", "Sex"), error = TRUE),
               class = "poputils_error_find_var")
  expect_error(find_var_time(c("time", "year"), error = TRUE),
               class = "poputils_error_find_var_multiple")
  expect_error(find_var_time(c("time", "year"), error = TRUE),
               class = "poputils_error_find_var")
  expect_error(find_var_age(c("Sex"), error = TRUE),
               class = "poputils_error_find_var_none")
  expect_error(find_var_popn(c("n", "count"), error = TRUE),
               class = "poputils_error_find_var_multiple")
})

test_that("'find_var_*' 'none' class can be caught for an optional variable", {
  f <- function(nms)
    tryCatch(find_var_time(nms, error = TRUE),
             poputils_error_find_var_none = function(e) NULL)
  expect_null(f(c("Age", "Sex")))
  expect_identical(f(c("Age", "Year")), "Year")
  expect_error(f(c("time", "year")),
               class = "poputils_error_find_var_multiple")
})
