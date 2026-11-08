
## Functions to infer variable types or labels from vectors of names

## Find labels ----------------------------------------------------------------

## HAS_TESTS
#' Identify Sex or Gender Labels Referring to Females
#'
#' Given labels for sex or gender, try to infer
#' which (if any) refer to females.
#' If no elements look like a label for females,
#' or if two or more elements do,
#' then return `NULL`.
#'
#' @param nms A character vector
#'
#' @returns An element of `nms` or `NULL`.
#'
#' @seealso [find_label_male()], [find_var_sexgender()]
#' 
#' @examples
#' find_label_female(c("Female", "Male")) ## one valid
#' find_label_female(c("0-4", "5-9"))     ## none valid
#' find_label_female(c("F", "Fem"))       ## two valid
#' @export
find_label_female <- function(nms) {
    valid <- c("female", "f", "fem", "women", "woman", "girl",
               "females", "girls")
    find_inner(nms = nms, valid = valid)
}


## HAS_TESTS
#' Identify Sex or Gender Labels Referring to Males
#'
#' Given labels for sex or gender, try to infer
#' which (if any) refer to males.
#' If no elements look like a label for males,
#' or if two or more elements do,
#' then return `NULL`.
#'
#' @param nms A character vector
#'
#' @returns An element of `nms` or `NULL`.
#'
#' @seealso [find_label_female()], [find_var_sexgender()]
#' 
#' @examples
#' find_label_male(c("Female", "Male")) ## one valid
#' find_label_male(c("0-4", "5-9"))     ## none valid
#' find_label_male(c("male", "m"))      ## two valid
#' @export
find_label_male <- function(nms) {
    valid <- c("male", "m", "men", "man", "boy",
               "males", "boys")
    find_inner(nms = nms, valid = valid)
}


## Find variables -------------------------------------------------------------

## HAS_TESTS
#' Identify an Age Variable
#'
#' Find the element of `nms` that looks like an age variable.
#' If no elements look like an age variable, or if
#' two or more elements do,
#' then return `NULL`, or, if `error` is `TRUE`,
#' raise an error.
#'
#' Names are compared after converting to lower case
#' and removing everything except letters,
#' so that, for instance, `"Age.Group"` matches `"agegroup"`.
#' Recognised names are
#' `"age"`, `"agegroup"`, `"agegp"`, `"ageyear"`, `"ageinterval"`,
#' and their plurals.
#'
#' @param nms A character vector
#' @param error Whether to raise an error, rather than
#' return `NULL`, when no element, or more than one
#' element, of `nms` is recognised. Default is `FALSE`.
#' @param call Execution environment of the function
#' to be named in error messages. Only needed when
#' `error` is `TRUE` and `find_var_*()` is called
#' from inside another function.
#' Default is [rlang::caller_env()].
#'
#' @returns An element of `nms`, or `NULL`.
#'
#' @seealso [find_var_popn()], [find_var_sexgender()], [find_var_time()]
#' 
#' @examples
#' find_var_age(c("Sex", "Year", "AgeGroup", NA)) ## one valid
#' find_var_age(c("Sex", "Year"))                 ## none valid
#' find_var_age(c("age", "age.years"))            ## two valid
#' try(find_var_age(c("Sex", "Year"), error = TRUE))
#' @export
find_var_age <- function(nms, error = FALSE, call = rlang::caller_env()) {
    valid <- c("age", "agegroup", "agegp", "ageyear", "ageinterval",
               "ages", "agegroups", "agegps", "ageyears", "ageintervals")
    find_var_inner(nms = nms,
                   valid = valid,
                   desc = "age",
                   article = "an",
                   error = error,
                   call = call)
}


## HAS_TESTS
#' Identify a Population Variable
#'
#' Find the element of `nms` that looks like
#' a population (or population count) variable.
#' If no elements look like a population variable,
#' or if two or more elements do,
#' then return `NULL`, or, if `error` is `TRUE`,
#' raise an error.
#'
#' Names are compared after converting to lower case
#' and removing everything except letters.
#' Recognised names are
#' `"popn"`, `"pop"`, `"population"`, `"count"`, `"value"`,
#' `"person"`, their plurals, and `"n"`
#' (the default name used by `dplyr::count()`).
#'
#' @inheritParams find_var_age
#'
#' @returns An element of `nms`, or `NULL`.
#'
#' @seealso [find_var_age()], [find_var_sexgender()], [find_var_time()]
#' 
#' @examples
#' find_var_popn(c("Sex", "Age", "Population")) ## one valid
#' find_var_popn(c("Sex", "Age"))               ## none valid
#' find_var_popn(c("popn", "count"))            ## two valid
#' try(find_var_popn(c("Sex", "Age"), error = TRUE))
#' @export
find_var_popn <- function(nms, error = FALSE, call = rlang::caller_env()) {
    valid <- c("popn", "pop", "population", "count", "value", "person", "n",
               "pops", "populations", "counts", "values", "persons")
    find_var_inner(nms = nms,
                   valid = valid,
                   desc = "population",
                   article = "a",
                   error = error,
                   call = call)
}


## HAS_TESTS
#' Identify a Sex or Gender Variable
#'
#' Find the element of `nms` that looks like
#' a sex or gender variable.
#' If no elements look like a sex or gender variable,
#' or if two or more elements do,
#' then return `NULL`, or, if `error` is `TRUE`,
#' raise an error.
#'
#' Names are compared after converting to lower case
#' and removing everything except letters.
#' Recognised names are
#' `"sex"`, `"gender"`, `"sexgender"`,
#' and the plurals of `"sex"` and `"gender"`.
#'
#' @inheritParams find_var_age
#'
#' @returns An element of `nms`, or `NULL`.
#'
#' @seealso [find_var_age()], [find_var_popn()], [find_var_time()],
#' [find_label_female()], [find_label_male()]
#' 
#' @examples
#' find_var_sexgender(c("Sex", "Year", "AgeGroup", NA)) ## one valid
#' find_var_sexgender(c("Age", "Region"))               ## none valid
#' find_var_sexgender(c("sexgender", "sexes"))          ## two valid
#' try(find_var_sexgender(c("Age", "Region"), error = TRUE))
#' @export
find_var_sexgender <- function(nms, error = FALSE, call = rlang::caller_env()) {
    valid <- c("sex", "gender", "sexgender",
               "sexes", "genders")
    find_var_inner(nms = nms,
                   valid = valid,
                   desc = "sex or gender",
                   article = "a",
                   error = error,
                   call = call)
}


## HAS_TESTS
#' Identify a Time Variable
#'
#' Find the element of `nms` that looks like a time variable.
#' If no elements look like a time variable, or if
#' two or more elements do,
#' then return `NULL`, or, if `error` is `TRUE`,
#' raise an error.
#'
#' Names are compared after converting to lower case
#' and removing everything except letters.
#' Recognised names are
#' `"time"`, `"period"`, `"year"`, `"month"`, `"quarter"`,
#' their plurals,
#' and `"yearmonth"`, `"monthyear"`, `"yearquarter"`, `"quarteryear"`.
#'
#' @inheritParams find_var_age
#'
#' @returns An element of `nms`, or `NULL`.
#'
#' @seealso [find_var_age()], [find_var_popn()], [find_var_sexgender()]
#' 
#' @examples
#' find_var_time(c("Sex", "Year", "AgeGroup", NA)) ## one valid
#' find_var_time(c("Sex", "Region"))               ## none valid
#' find_var_time(c("time", "year"))                ## two valid
#' try(find_var_time(c("Sex", "Region"), error = TRUE))
#' @export
find_var_time <- function(nms, error = FALSE, call = rlang::caller_env()) {
    valid <- c("time", "period", "year", "month", "quarter",
               "times", "periods", "years", "months", "quarters",
               "yearmonth", "monthyear", "yearquarter", "quarteryear")
    find_var_inner(nms = nms,
                   valid = valid,
                   desc = "time",
                   article = "a",
                   error = error,
                   call = call)
}



## Helper functions -----------------------------------------------------------

## HAS_TESTS
#' Helper function for find_label_* functions
#'
#' @param nms A character vector
#' @param valid Recognised names, in lower case,
#' letters only
#'
#' @returns An element of 'nms' or NULL.
#'
#' @noRd
find_inner <- function(nms, valid) {
    i <- find_matches(nms = nms, valid = valid)
    if (identical(length(i), 1L))
        nms[[i]]
    else
        NULL
}


## HAS_TESTS
#' Helper function for find_var_* functions
#'
#' Like 'find_inner()', but optionally raises an
#' error when there is not exactly one match.
#'
#' @param nms A character vector
#' @param valid Recognised names, in lower case,
#' letters only
#' @param desc Description of the variable,
#' used in error messages, eg "age"
#' @param article "a" or "an", to go before 'desc'
#' @param error Whether to raise an error
#' @param call Environment named in error messages
#'
#' @returns An element of 'nms' or NULL.
#'
#' @noRd
find_var_inner <- function(nms, valid, desc, article, error, call) {
    check_flag(error)
    i <- find_matches(nms = nms, valid = valid)
    n <- length(i)
    if (identical(n, 1L))
        return(nms[[i]])
    if (!error)
        return(NULL)
    if (identical(n, 0L))
        cli::cli_abort(c("Can't find {article} {desc} variable.",
                         i = "Variables: {.var {nms}}.",
                         i = "Recognised names (case and punctuation ignored): {.val {valid}}.",
                         i = "Rename a variable, e.g. with {.fn dplyr::rename}."),
                       call = call)
    cli::cli_abort(c("Found more than one {desc} variable: {.var {nms[i]}}.",
                     i = "Keep one, or rename the others."),
                   call = call)
}


## HAS_TESTS
#' Find positions of elements of 'nms' that
#' match 'valid' after cleaning
#'
#' Cleaning converts to lower case and removes
#' everything except letters.
#'
#' @param nms A character vector
#' @param valid Recognised names, in lower case,
#' letters only
#'
#' @returns An integer vector
#'
#' @noRd
find_matches <- function(nms, valid) {
    nms_cleaned <- tolower(nms)
    nms_cleaned <- gsub("[^a-z]", "", nms_cleaned)
    which(nms_cleaned %in% valid)
}
