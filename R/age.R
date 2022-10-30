
## HAS_TESTS
#' Create age labels
#'
#' Create labels for age groups. Three types of
#' labels are possible, depending on the value
#' of \code{type}:
#' \description{
#'   \item{"five"} Five-year age groups, eg
#'      \code{"0-4"} or \code{"55-59"}, and possibly
#'      an open age group, eg \code{"100+"}.
#'   \item{"lt"} Life table age groups, eg
#'      \code{"0"}, {"1-4"}, \code{"5-9"},
#'      \code{"55-59"}, or \code{"80+"}.
#'   \item{"single"} One-year age groups, eg
#'      \code{"0"} or \code{"55"}, and possibly
#'      an open age group, eg \code{"90+"}.
#' }
#' 
#' The first age group starts at exact age \code{min}.
#' If \code{open} is \code{TRUE}, then the final
#' age group starts at exact age \code{max}, and
#' if \code{open} is \code{FALSE}, then the
#' final age group ends at eact age \code{max}.
#'
#' \code{open} defaults to \code{TRUE} when
#' \code{min} equals zero, and to \code{FALSE}
#' otherwise.
#'
#' @param min Minimum age. Defaults to 0.
#' @param max Maximum age for closed age groups.
#' Defaults to 100.
#' @param open Whether the last age group is
#' "open", ie has no upper limit.
#'
#' @return A character vector.
#'
#' @seealso \code{\link{clean_age_labels}}
#'
#' @examples
#' age_labels(type = "five")
#' age_labels(type = "lt", max = 80)
#' age_labels(type = "single", min = 15, max = 40)
#' @export
age_labels <- function(type, min = 0, max = 100, open = NULL) {
    type <- match.arg(type, choices = c("five", "lt", "single"))
    min <- checkmate::assert_int(min, coerce = TRUE)
    max <- checkmate::assert_int(max, coerce = TRUE)
    checkmate::assert_flag(open, null.ok = TRUE)
    if (max < min)
        stop(gettextf("'%s' [%d] is less than '%s' [%d] ",
                      "max", max, "min", min),
             call. = FALSE)
    if (is.null(open))
        open <- identical(min, 0L)
    if ((max == min) && !open)
        stop(gettextf("'%s' [%d] equals '%s' [%d] but '%s' is %s",
                      "max", max, "min", min, "open", "FALSE"),
             call. = FALSE)
    if (identical(type, "five"))
        age_labels_five(min = min, max = max, open = open)
    else if (identical(type, "lt"))
        age_labels_lt(min = min, max = max, open = open)
    else if (identical(type, "single"))
        age_labels_single(min = min, max = max, open = open)
    else
        stop(gettextf("invalid 'type' : \"%s\"", type),
             call. = FALSE)
}

## HAS_TESTS
#' Create one-year age labels
#'
#' Create labels for one-year age groups.
#'
#' @inheritParams age_labels
#'
#' @return A character vector of length
#' \code{max - min + open}.
#'
#' @noRd
age_labels_single <- function(min, max, open) {
    if (open)
        label_open <- paste0(max, "+")
    if (max == min)
        ans <- label_open
    else {
        ans <- seq.int(from = min, to = max - 1L)
        ans <- as.character(ans)
        if (open)
            ans <- c(ans, label_open)
    }
    ans
}


## HAS_TESTS
#' Create 5-year age labels
#'
#' Create labels for 5-year age groups.
#'
#' @inheritParams age_labels_single
#'
#' @return A character vector of length
#' \code{(max - min)/5 + open}.
#'
#' @noRd
age_labels_five <- function(min, max, open) {
    if ((min %% 5L) != 0L)
        stop(gettextf("'%s' [%d] not divisible by 5",
                      "min", min),
             call. = FALSE)
    if ((max %% 5L) != 0L) {
        stop(gettextf("'%s' [%d] not divisible by 5",
                      "max", max),
             call. = FALSE)
    }
    if (open)
        label_open <- paste0(max, "+")
    if (max == min)
        ans <- label_open
    else {
        lower <- seq.int(from = min, to = max - 5L, by = 5L)
        upper <- lower + 4L
        ans <- paste(lower, upper, sep = "-")
        if (open)
            ans <- c(ans, label_open)
    }
    ans
}


## HAS_TESTS
#' Create life table age labels
#'
#' Create labels for 'abridged' life table
#' age groups, ie \code{"0", "1-4", "5-9",
#' "10-14", \dots, "<max>+"}.
#'
#' @inheritParams age_labels_single
#'
#' @return A character vector.
#'
#' @noRd
age_labels_lt <- function(min, max, open) {
    if (open)
        label_open <- paste0(max, "+")
    msg <- gettextf("age group derived from '%s' [%d] and '%s' [%d] not a valid life table age group",
                    "min", min, "max", max)
    if (min < 0L)
        stop(gettextf("'%s' equals %d : negative values not allowed in life table age groups",
                      "min", min),
             call. = FALSE)
    else if (min == 0L) {
        if (max == 0L) {
            ans <- label_open
        }
        else if (max == 1L) {
            ans <- "0"
            if (open)
                ans <- c(ans, label_open)
        }
        else if (max %in% 2:4) {
            stop(msg, call. = FALSE)
        }
        else if (max == 5L) {
            ans <- c("0", "1-4")
            if (open)
                ans <- c(ans, label_open)
        }
        else {
            ans <- c("0", "1-4",
                     age_labels_five(min = 5L, max = max, open = open))
        }
    }
    else if (min == 1L) {
        if (max == 1L) {
            ans <- label_open
        }
        else if (max %in% 2:4) {
            stop(msg, call. = FALSE)
        }
        else if (max == 5L) {
            ans <- "1-4"
            if (open)
                ans <- c(ans, label_open)
        }
        else {
            ans <- c("1-4",
                     age_labels_five(min = 5L, max = max, open = open))
        }
    }
    else if (min %in% 2:4) {
        stop(gettextf("'%s' [%d] not valid for a life table age group",
                      "min", min),
             call. = FALSE)
    }
    else { ## min >= 5
        ans <- age_labels_five(min = min, max = max, open = open)
    }
    ans
}
            



#' Reformat age group labels
#'
#' Convert age group labels to a standard format.
#' \describe{
#'   \item{Step 1} Tidy and translate text,
#'     eg convert \code{"20 to 24 years"} to
#'     \code{"20-24"}, convert \code{"infant"} to
#'     \code{"0"}, or convert \code{"100 or more"} to
#'     \code{"100+"}.
#'   \item{Step 2} Check whether the resulting
#'     labels could have been produced by
#'     \code{\link{age_labels_five}}, \code{\link{age_labels_lt}},
#'     \code{\link{age_labels_single}}. If not, throw an error.
#'   \item{Step 3} If \code{factor} is \code{TRUE}
#'     (the default), then return a factor where the levels
#'     include all intermediate age groups. Otherwise
#'     return a character vector.
#' }
#'
#' \code{clean_age} allows for two special cases:
#'   \describe{
#'     \item{5-year} Labels consisting entirely of
#'       multiples of 5, with a maximum of at least
#'       50, are assumed to denote 5-year age groups.
#'    \item{Life table} Labels consisting entirely
#'       of 0, 1, and multiples of 5, with a maximum
#'       of at least 50, are assumed to denote
#'       life table age groups.
#' }
#'
#' @param x A vector.
#'
#' @return If \code{factor} is \code{TRUE} a factor;
#' otherwise a character vector.
#'
#' @seealso \code{\link{age_labels_five}}, \code{link{age_labels_lt}},
#' and \code{\link{single}} construct five-year, life table,
#' and single-year age group labels.
#' 
#' @examples
#' ## five-year, factor
#' clean_age_groups(c("65 and over",
#'                    "10--14",
#'                    "20-24 years"))
#'
#' ## five-year, non-factor
#' clean_age_groups(c("65 and over",
#'                    "10--14",
#'                    "20-24 years"),
#'                  factor = FALSE)
#'
#' ## life table
#' clean_age_groups(c("0",
#'                    "30-34",
#'                    "10--14",
#'                    "1-4 years"))
#' 
#' ## single
#' clean_age_groups(c("60",
#'                    "90plus"))
#' @export
clean_age <- function(x, factor = TRUE) {
    checkmate::assert_vector(x)
    checkmate::assert_flag(factor)
    ## constants
    p_single <- "^([0-9]+)$"
    p_low_up <- "^([0-9]+)-([0-9]+)$"
    p_open <- "^([0-9]+)\\+$"
    ## treat 'x' with no non-missing values as valid
    if (all(is.na(x)))
        return(TRUE)
    ## for efficiency, work with unique values
    levels_old <- unique(x)
    n_level <- length(levels_old)
    ## attempt to transform to standard format
    ## using only string operations
    levels_new <- reformat_age_labels(levels_old)
    ## classify levels
    is_na <- is.na(levels_new)
    is_single <- grepl(p_single, levels_new)
    is_low_up <- grepl(p_low_up, levels_new)
    is_open <- grepl(p_open, levels_new)
    is_level_valid <- is_na | is_single | is_low_up | is_open
    i_level_invalid <- match(FALSE, is_level_valid, nomatch = 0L)
    if (i_level_invalid > 0L)
        stop(gettextf("can't parse label \"%s\"",
                      levels_old[[i_level_invalid]]),
             call. = FALSE)
    ## characterise bounds
    lower <- rep(NA, times = n_level)
    lower[is_single] <- as.integer(sub(p_single, "\\1", levels_new[is_single]))
    lower[is_low_up] <- as.integer(sub(p_low_up, "\\1", levels_new[is_low_up]))
    lower[is_open] <- as.integer(sub(p_open, "\\1", levels_new[is_open]))
    upper <- rep(NA, times = n_level)
    upper[is_single] <- lower[is_single] + 1L
    upper[is_low_up] <- as.integer(sub(p_low_up, "\\2", levels_new[is_low_up])) + 1L
    has_na <- any(is_na)
    has_open <- any(is_open)
    min <- min(lower, na.rm = TRUE)
    max <- if (has_open) max(lower, na.rm = TRUE) else max(upper, na.rm = TRUE)
    ## check open interval
    if (has_open) {
        if (any(lower[is_open] != max)) {
            levels_open <- paste(sprintf("\"%s\"", levels_old[is_open]), sep = ", ")
            stop(gettextf("open age groups with different lower limits : %s",
                          levels_open),
                 call. = FALSE)
        }
        is_above_max <- !is_na & (upper > max)
        i_above_max <- match(TRUE, is_above_max, nomatch = 0L)
        if (i_above_max > 0L) {
            stop(gettextf("age groups \"%s\" and \"%s\" overlap",
                          levels_old[i_open][[1L]],
                          levels_old[[i_above_open]]),
                 call. = FALSE)
        }
    }
    examples_invalid <- character(3L)
    ## check 5-year age groups
    is_lower_mult_five <- lower %% 5L == 0L
    is_low_up_mult_five <- is_low_up & is_lower_mult_five & (upper - lower == 5L)
    is_open_mult_five <- is_open & is_lower_mult_five
    is_valid_five <- is_na | is_low_up_mult_five | is_open_mult_five
    i_invalid_five <- match(FALSE, is_valid_five, nomatch = 0L)
    is_all_valid_five <- i_invalid_five == 0L
    if (!is_all_valid_five) {
        examples_invalid[[1L]] <- levels_old[[i_invalid_five]]
        ## check life table age groups
        is_single_zero <- is_single & (lower == 0L)
        is_low_up_one_five <- is_low_up & (lower == 1L) & (upper == 5L)
        is_low_up_mult_five_above_five <- is_low_up_mult_five & (lower >= 5L)
        is_valid_lt <- (is_na
            | is_single_zero
            | is_low_up_one_five
            | is_low_up_mult_five_above_five
            | is_open_mult_five)
        i_invalid_lt <- match(FALSE, is_valid_lt, nomatch = 0L)
        is_all_valid_lt <- i_invalid_lt == 0L
        if (!is_all_valid_lt) {
            examples_invalid[[2L]] <- levels_old[[i_invalid_lt]]
            ## check single age groups
            is_valid_single <- (is_na
                | is_single
                | is_open)
            i_invalid_single <- match(FALSE, is_valid_single, nomatch = 0L)
            is_all_valid_single <- i_invalid_single == 0L
            if (!is_all_valid_lt)
                examples_invalid[[3L]] <- levels_old[[i_invalid_single]]
        }
    }
    if (is_all_valid_five)
        agelab <- age_labels_five(min = min, max = max, open = has_open)
    else if (is_all_valid_lt)
        agelab <- age_labels_lt(max = max, open = has_open)
    else if (is_all_valid_single)
        agelab <- age_labels_single(min = min, max = max, open = has_open)
    else {
        stop(gettextf(paste("unable to parse '%s' as age group labels :",
                            "label \"%s\" incompatible with 5-year age groups,",
                            "label \"%s\" incompatible with life table age groups,",
                            "label \"%s\" incompatible with 1-year age groups"),
                      "x",
                      examples_invalid[[1L]],
                      examples_invalid[[2L]],
                      examples_invalid[[3L]]),
             call. = FALSE)
    }
    if (has_na)
        agelab <- c(agelab, NA)
    i_lev_to_lab <- match(levels_new, agelab)
    i_x_to_lev <- match(x, levels_old)
    i <- i_lev_to_lab[i_x_to_lev]
    ans <- agelab[i]
    if (factor)
        ans <- factor(ans, levels = agelab, exclude = character())
    ans
}




## HAS_TESTS
#' Attempt to treat vector as lower limits
#' of 5-year age groups
#'
#' If \code{x} consists of integer-like values
#' where the unique, sorted
#' values form a series \code{0, 5, 10, ..., A},
#' where \code{A >= 50}, construct labels
#' "0-4", "5-9", "10-14", ..., "A+". 
#' Otherwise return \code{NULL}.
#'
#' \code{x} is allowed to contain \code{NA}s,
#' which are propagated through to the result.
#'
#' @param x A vector.
#'
#' @return A character vector with the same
#' length as \code{x}, or \code{NULL}.
#'
#' @noRd
clean_age_five <- function(x) {
    not_na <- !is.na(x)
    if (sum(not_na) < 11L)
        return(NULL)
    x_int <- suppressWarnings(as.integer(x))
    is_integerish <- all(!is.na(x_int[not_na]) & (x_int[not_na] == x[not_na]))
    if (!is_integerish)
        return(NULL)
    breaks_obtained <- unique(x_int)
    breaks_obtained <- sort.int(breaks_obtained, na.last = TRUE)
    max <- max(breaks_obtained, na.rm = TRUE)
    breaks_expected <- seq.int(from = 0L, to = max, by = 5L)
    has_na <- anyNA(breaks_obtained)
    if (has_na)
        breaks_expected <- c(breaks_expected, NA)
    if (!identical(breaks_obtained, breaks_expected))
        return(NULL)
    labels <- age_labels_five(min = 0L, max = max, open = TRUE)
    if (has_na)
        labels <- c(labels, NA)
    i <- match(x_int, breaks_expected)
    labels[i]
}


## HAS_TESTS
#' Attempt to treat vector as lower limits
#' of life table age groups
#'
#' If \code{x} consists of integer-like values
#' where the unique, sorted
#' values form a series \code{0, 1, 5, 10, ..., A},
#' where \code{A >= 50}, construct labels
#' "0-4", "5-9", "10-14", ..., "A+". 
#' Otherwise return \code{NULL}.
#'
#' \code{x} is allowed to contain \code{NA}s,
#' which are propagated through to the result.
#'
#' @param x A vector.
#'
#' @return A factor with the same
#' length as \code{x}, or \code{NULL}.
#'
#' @noRd
clean_age_lt <- function(x) {
    not_na <- !is.na(x)
    if (sum(not_na) < 12L)
        return(NULL)
    x_int <- suppressWarnings(as.integer(x))
    is_integerish <- all(!is.na(x_int[not_na]) & (x_int[not_na] == x[not_na]))
    if (!is_integerish)
        return(NULL)
    breaks_obtained <- unique(x_int)
    breaks_obtained <- sort.int(breaks_obtained, na.last = TRUE)
    max <- max(breaks_obtained, na.rm = TRUE)
    breaks_expected <- c(0L, 1L, seq.int(from = 5L, to = max, by = 5L))
    has_na <- anyNA(breaks_obtained)
    if (has_na)
        breaks_expected <- c(breaks_expected, NA)
    if (!identical(breaks_obtained, breaks_expected))
        return(NULL)
    labels <- age_labels_lt(max = max)
    if (has_na)
        labels <- c(labels, NA)
    i <- match(x_int, breaks_expected)
    labels[i]
}


## HAS_TESTS
#' Attempt to convert vector into
#' standard age labels
#'
#' Apply a series of text manipulations
#' transformations in an attempt to convert
#' \code{x} into age group labels in the
#' style of \code{\link{age_labels_five}},
#' \code{\link{age_labels_lt}}, or
#' \code{\link{age_labels_single}}.
#'
#' \code{NA}s are permitted, and propagate
#' through to the results.
#' 
#' @param x A vector of possible labels.
#'
#' @return A character vector.
#'
#' @noRd
reformat_age_labels <- function(x) {
    year <- "year|years|yr|yrs"
    infant <- "^infants$|^infant$|^in 1st$|^less than 1$|^under 1$|^less than one$"
    plus <- "and over|plus|and above|and older|or more"
    num <- c("zero", "one", "two", "three", "four",
             "five", "six", "seven", "eight", "nine")
    ## test whether 'x' consists of lower limits
    ## of 5-year or abridged life table age groups
    ans <- clean_age_five(x)
    if (!is.null(ans))
        return(ans)
    ans <- clean_age_lt(x)
    if (!is.null(ans))
        return(ans)
    ## put everything into lower case
    x <- tolower(x)
    ## trim leading zeros from any numbers
    x <- gsub("(?<![0-9])0+(?=[0-9])", "", x, perl = TRUE)
    ## remove "year" labels
    x <- sub(year, "", x)
    ## translate synonyms for age group "0"
    x <- sub(infant, "0", x)
    ## translate synonyms for "+"
    x <- sub(plus, "+", x)
    ## remove spaces
    x <- gsub(" ", "", x)
    ## tranlsate synonyms for "-"
    x <- sub("^([0-9]+)to([0-9]+)$", "\\1-\\2", x)
    x <- sub("^([0-9]+)[[:punct:]]+([0-9]+)$", "\\1-\\2", x)
    ## translate numbers
    for (i in seq_along(num))
        x <- gsub(num[[i]], i - 1L, x)
    ## return result
    x
}
