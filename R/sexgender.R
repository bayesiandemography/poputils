
#' Reformat a Binary Sex Variable
#'
#' Reformat a binary sex variable so
#' that it consists entirely of
#' values `"Female"`, `"Male"`,
#' and possibly `NA` and any values
#' included in `except`.
#'
#' When parsing labels, `reformat_sex()`
#' ignores case: `"FEMALE"` and `"fEmAlE"`
#' are equivalent.
#'
#' White space is removed from the beginning
#' and end of labels.
#'
#' `reformat_sex()` does not try to interpret
#' numeric codes (eg `1`, `2`).
#'
#' @param x A vector.
#' @param except Values to exclude when reformatting.
#' @param factor Whether the return value
#' should be a factor.
#' 
#' @return If `factor` is `TRUE`,
#' then `reformat_sex()` returns a factor;
#' otherwise it returns a character vector.
#'
#' @seealso [age_labels()], [reformat_age()],
#' [sex_is_missing()], [sex_is_total()]
#' 
#' @examples
#' reformat_sex(c("F", "female", NA, "MALES"))
#'
#' ## values supplied for 'except'
#' reformat_sex(c("Fem", "Other", "Male", "M"),
#'              except = c("Other", "Diverse"))
#'
#' ## return an ordinary character vector
#' reformat_sex(c("F", "female", NA, "MALES"),
#'              factor = FALSE)
#' @export
reformat_sex <- function(x, except = NULL, factor = TRUE) {
  synonyms_female <- c("females", "female", "fem", "fe", "f",
                        "women", "woman", "girls", "girl")
  synonyms_male <- c("males", "male", "ma", "m", "men", "man", "boys", "boy")
  has_except <- !is.null(except)
  if (has_except && !is.vector(except) && !is.factor(except))
    cli::cli_abort(c("{.arg except} is not a vector or a factor.",
                     i = "{.arg except} has class {.cls {class(except)}}."))
  except <- unique(except)
  check_flag(factor)
  is_na <- is.na(x)
  x_new <- gsub("\\s", "", x)
  x_new <- tolower(x_new)
  is_female <- x_new %in% synonyms_female
  is_male <- x_new %in% synonyms_male
  is_valid <- is_na | is_female | is_male
  if (has_except) {
    is_except <- x %in% except
    is_valid <- is_valid | is_except
  }
  i_invalid <- match(FALSE, is_valid, nomatch = 0L)
  if (i_invalid > 0L)
    cli::cli_abort("Can't parse label {.val {x[[i_invalid]]}}.")
  x <- as.character(x)
  x[is_female] <- "Female"
  x[is_male] <- "Male"
  x[is_na] <- NA
  if (factor) {
    levels <- c("Female", "Male")
    if (has_except)
      levels <- c(levels, except)
    if (any(is_na))
      levels <- c(levels, NA)
    factor(x,
           levels = levels,
           exclude = character())
  }
  else
    x
}    
    
    


## HAS_TESTS
#' Identify Labels for Sex Not Stated, or for Totals
#'
#' `sex_is_missing()` identifies labels meaning
#' that sex is not stated, such as `NA`, `"Missing"`,
#' `"Not stated"`, or `"Unknown"`.
#' `sex_is_total()` identifies labels for
#' totals over sexes, such as `"Total"`,
#' `"Both sexes"`, or `"Persons"`.
#'
#' Labels are compared after converting to lower case
#' and removing everything except letters, so that,
#' for instance, `"Not stated"`, `"NOT STATED"`, and
#' `"not_stated"` are equivalent. A prefix of `"sex"`
#' or `"gender"` is also ignored, so that, for instance,
#' `"Sex not stated"` is recognised.
#'
#' Labels recognised by `sex_is_missing()`, apart from `NA`,
#' are those for missing values in
#' package [agetime](https://bayesiandemography.github.io/agetime/),
#' such as `"Missing"`, `"Not stated"`, `"Unknown"`,
#' `"Unspecified"`, `"Don't know"`, and `"N/A"`.
#'
#' Labels recognised by `sex_is_total()` are `"Total"`,
#' `"All"`, `"Overall"`, `"Both"`, `"Both sexes"`,
#' `"All sexes"`, `"Persons"`, `"People"`, `"All persons"`,
#' and `"All people"`, plus combinations such as
#' `"Total, both sexes"` and `"Total (persons)"`.
#'
#' Unlike [reformat_sex()], neither function raises an error
#' for labels it does not recognise: it returns `FALSE`.
#' `NA` is a missing value, not a total.
#'
#' @param x A vector, or a factor.
#'
#' @returns A logical vector with the same length as `x`.
#'
#' @seealso [reformat_sex()] Reformat labels for females and males
#'
#' @examples
#' x <- c("Female", "Male", "Not stated", NA, "Both sexes", "Other")
#' sex_is_missing(x)
#' sex_is_total(x)
#'
#' ## typical use: separate out problem labels
#' ## before calling reformat_sex()
#' is_ok <- !sex_is_missing(x) & !sex_is_total(x)
#' x[is_ok]
#' @export
sex_is_missing <- function(x) {
  check_sex_labels_arg(x)
  x <- as.character(x)
  tokens <- c("unknown", "unk", "unkna", "unspecified",
              "notknown", "dontknow", "missing", "notstated",
              "na", "nk")
  cleaned <- clean_sex_labels(x)
  is.na(x) | (cleaned %in% tokens)
}


#' @rdname sex_is_missing
#' @export
sex_is_total <- function(x) {
  check_sex_labels_arg(x)
  x <- as.character(x)
  tokens <- c("total", "all", "overall", "both", "bothsexes",
              "allsexes", "persons", "people", "allpersons", "allpeople")
  cleaned <- clean_sex_labels(x)
  rest <- sub("^total", "", cleaned)
  is_total <- (cleaned %in% tokens) | (startsWith(cleaned, "total") & rest %in% tokens)
  is_total & !is.na(x)
}


## HAS_TESTS
#' Check argument 'x' of sex_is_missing() and sex_is_total()
#'
#' @param x Argument to check
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_sex_labels_arg <- function(x) {
  if (!is.vector(x) && !is.factor(x))
    cli::cli_abort(c("{.arg x} is not a vector or a factor.",
                     i = "{.arg x} has class {.cls {class(x)}}."),
                   call = rlang::caller_env())
  if (is.list(x))
    cli::cli_abort("{.arg x} is a list.",
                   call = rlang::caller_env())
  invisible(TRUE)
}


## HAS_TESTS
#' Clean sex labels before comparing with tokens
#'
#' Convert to lower case, keep only letters, and
#' remove a leading "sex" or "gender".
#'
#' @param x A character vector
#'
#' @returns A character vector
#'
#' @noRd
clean_sex_labels <- function(x) {
  ans <- tolower(x)
  ans <- gsub("[^a-z]", "", ans)
  ans <- sub("^(sex|gender)(?=.)", "", ans, perl = TRUE)
  ans
}
