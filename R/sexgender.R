

#' Reformat a binary sex variable
#'
#' Reformat a binary sex variable so
#' that it consists entirely of
#' values `"Female"`, `"Male"`,
#' and possibly `NA`.
#'
#' @param x A vector.
#' @param factor Whether the return value
#' should be a factor.
#'
#' When parsing labels, `reformat_sex()`
#' ignores case: `FEMALE` and `fEmAlE` are treated the same.
#'
#' White space is removed from the beginning
#' and end of labels.
#'
#' `reformat_sex()` does not try to interpreting
#' numeric codes (eg `1`, `2`).
#'
#' @return If `factor` is `TRUE`,
#' then `reformat_age()` returns a factor
#' with levels `"Female"`, `"Male"`.
#' otherwise it returns a character vector.
#'
#' @seealso [age_labels()], [reformat_sex()]
#' 
#' @examples
#' reformat_sex(c("F", "female", NA, "MALES"))
#'
#' ## return an ordinary character vector
#' reformat_sex(c("F", "female", NA, "MALES"),
#'              factor = FALSE)
#' @export
reformat_sex <- function(x, factor = TRUE) {
    synonyms_female <- c("females", "female", "fem", "fe", "f", "women", "girls")
    synonyms_male <- c("males", "male", "ma", "m", "men", "boys")
    check_flag(factor)
    is_na <- is.na(x)
    x_new <- gsub("\\s", "", x)
    x_new <- tolower(x_new)
    is_female <- x_new %in% synonyms_female
    is_male <- x_new %in% synonyms_male
    is_valid <- is_na | is_female | is_male
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L)
        cli::cli_abort("Can't parse label {.val {x[[i_invalid]]}}.")
    x_new[is_female] <- "Female"
    x_new[is_male] <- "Male"
    x_new[is_na] <- NA
    if (factor) {
        levels <- c("Female", "Male")
        if (any(is_na))
            levels <- c(levels, NA)
        factor(x_new,
               levels = levels,
               exclude = character())
    }
    else
        x_new
}    
    
    
