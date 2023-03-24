
## HAS_TESTS
#' Create age labels
#'
#' @description
#' Create labels for age groups. The labels depend
#' on the `type` argument:
#'
#'   - `"five"`. Five-year age groups, eg
#'      `"0-4"` or `"55-59"`, and possibly
#'      an open age group, eg `"100+"`.
#'   - `"lt"`. Life table age groups, eg
#'      `"0"`, {"1-4"}, `"5-9"`,
#'      `"55-59"`, or `"80+"`.
#'   - `"single"`. One-year age groups, eg
#'      `"0"` or `"55"`, and possibly
#'      an open age group, eg `"90+"`.
#'
#' @details
#' The first age group starts at the age
#' specified by `min`. If `open` is `TRUE`,
#' then the final age group starts at the age
#' specified by `max`. Otherwise, the
#' final age group ends at the age specified
#' by `max`.
#'
#' `open` defaults to `TRUE` when
#' `min` equals zero, and to `FALSE`
#' otherwise.
#'
#' @param type Type of age group labels: `"five"`,
#' `"lt"`, or `"single"`.
#' @param min Minimum age. Defaults to 0.
#' @param max Maximum age for closed age groups.
#' Defaults to 100.
#' @param open Whether the last age group is
#' "open", ie has no upper limit.
#'
#' @returns A character vector.
#'
#' @seealso [clean_age()]
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
#' `max - min + open`.
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
#' `(max - min)/5 + open`.
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
#' age groups, ie `"0", "1-4", "5-9",
#' "10-14", \dots, "<max>+"`.
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


## HAS_TESTS
#' Derive upper and lower limits for age groups
#'
#' Derive upper and lower limits for 1-year,
#' 5-year and life-table age groups. The age
#' groups are processed by function `clean_age()`
#' first, so do not need to conform to standard
#' formats. The upper limit for open groups
#' is `Inf`.
#'
#' @param x A vector of age group labels.
#' Can be a factor.
#'
#' @return A named list with two double
#' vectors. The names are `"lower"` and `"upper"`.
#'
#' @seealso Called by [age_lower()]
#' and [age_upper()].
#'
#' @noRd
age_limits <- function(x) {
    ## reformat 'x', including checking for validity
    x <- clean_age(x, factor = FALSE)
    ## handle all-NA case (which includes zero-length)
    n_x <- length(x)
    if (all(is.na(x))) {
        lower <- rep(NA_real_, times = n_x)
        upper <- rep(NA_real_, times = n_x)
        return(list(lower = lower, upper = upper))
    }
    ## constants
    p_single <- "^([0-9]+)$"
    p_low_up <- "^([0-9]+)-([0-9]+)$"
    p_open <- "^([0-9]+)\\+$"
    p_any <- "^([0-9]+).*$"
    ## work with levels, rather than full vector
    levels <- unique(x)
    n_levels <- length(levels)
    ## classify levels
    is_na <- is.na(levels)
    is_single <- grepl(p_single, levels)
    is_low_up <- grepl(p_low_up, levels)
    is_open <- grepl(p_open, levels)
    ## derive lower
    lower <- rep(NA_real_, times = n_levels)
    lower[!is_na] <- as.numeric(sub(p_any, "\\1", levels[!is_na]))
    ## derive upper
    upper <- rep(NA_real_, times = n_levels)
    upper[is_single] <- lower[is_single] + 1
    upper[is_low_up] <- as.numeric(sub(p_low_up, "\\2", levels[is_low_up])) + 1
    upper[is_open] <- Inf
    ## expand to original vector
    i <- match(x, levels)
    lower <- lower[i]
    upper <- upper[i]
    ## return
    list(lower = lower, upper = upper)
}

## HAS_TESTS
#' Lower limits, midpoints, and upper limits of age groups
#'
#' @description
#' Given a vector `x` of age group labels, return
#' a numeric vector.
#' - `age_lower[()` returns the lower limits of each age group,
#' - `age_mid()` returns the midpoints, and
#' - `age_upper()` returns the upper limits.
#'
#' Vector `x` must describe 1-year, 5-year or life-table
#' age groups: see [age_labels()] for examples. `x` can
#' format these age groups in any way understood by
#' [clean_age()].
#'
#' @details
#' These functions can make age groups easier to work
#' with. Lower and upper limits can be used for
#' selecting on age. Replacing age group with midpoints
#' can improve graphs. 
#'
#' @param x A vector of age group labels.
#'
#' @return A numeric vector, the same length as `x`.
#'
#' @seealso [clean_age()] [age_labels()]
#'
#' @examples
#' x <- c("15-19", "5-9", "50+")
#' age_lower(x)
#' age_mid(x)
#' age_upper(x)
#'
#' ## non-standard formats are OK
#' age_lower(c("infants", "100 and over"))
#'
#' df <- data.frame(age = c("1-4", "10-14", "5-9", "0"),
#'                  rate = c(0.023, 0.015, 0.007, 0.068))
#' df
#' subset(df, age_lower(age) >= 5)
#' plot(rate ~ age_mid(age), data = df)
#' @export
age_lower <- function(x) {
    age_limits(x)$lower
}

#' @export
#' @rdname age_lower
age_mid <- function(x) {
    limits <- age_limits(x)
    lower <- limits$lower
    upper <- limits$upper
    ans <- 0.5 * (lower + upper)
    is_open <- is.infinite(upper)
    if (any(is_open)) {
        if (all(is_open))
            stop(gettextf("unclear whether '%s' consists of 1-year, 5-year, or life-table age groups",
                          "x"),
                 call. = FALSE)
        is_zero <- (lower == 0) & (upper == 1)
        if (all(is_open | is_zero))
            stop(gettextf("unclear whether '%s' consists of 1-year or life-table age groups",
                          "x"),
                 call. = FALSE)
        is_nonzero_single <- !is_zero & (upper - lower == 1)
        increment_open <- if (any(is_nonzero_single)) 0.5 else 2.5
        ans[is_open] <- lower[is_open] + increment_open
    }
    ans
}

#' @export
#' @rdname age_lower
age_upper <- function(x) {
    age_limits(x)$upper
}


#' Clean age group labels
#'
#' @description
#' Convert age group labels to one of three formats:
#'
#' - Single-year age groups, eg
#'      `"0"`, `"1"`, ..., `"99"`, `"100+"`.
#' - Five-year age groups, eg
#'      `"0-4"`, `"5-9"`, ..., `"95-99"`, `"100+"`.
#' - Life table age groups, eg
#'      `"0"`, `"1-4", `"5-9"`, ..., `"95-99"`, `"100+"`.
#'
#' By default `clean_age()` returns a factor
#' that includes all intermediate age groups. 
#' See below for examples.
#'
#' @details
#' `clean_age()` applies the following algorithm:
#' 
#'   1. Tidy and translate text,
#'     eg convert `"20 to 24 years"` to
#'     `"20-24"`, convert `"infant"` to
#'     `"0"`, or convert `"100 or more"` to
#'     `"100+"`.
#'   1. Check whether the resulting
#'     labels could have been produced by
#'     [age_labels()]. If not, throw an error.
#'   1. If `factor` is `TRUE`
#'     (the default), then return a factor. The levels of
#'     this factor include all intermediate age groups.
#'     Otherwise return a character vector.
#'
#' When `x` consists entirely of numbers, `clean_age()`
#' also checks for two special cases:
#' 
#' - If every element of `x` is a multiple of 5,
#'   and if `max(x) >= 50`, then `x` is assumed to
#'   describe 5-year age groups
#' - If every element of `x` is 0, 1, or a multiple
#'   of 5, with `max(x) >= 50`, then `x` is assumed
#'   to describe life table age groups.
#' 
#' @param x A vector.
#' @param factor Whether the return value
#' should be a factor.
#'
#' @return If `factor` is `TRUE`,
#' then `clean_age()` returns a factor;
#' otherwise it returns a character vector.
#'
#' @seealso [age_labels()]
#' 
#' @examples
#' clean_age(c("80 to 84", "90 or more", "85 to 89"))
#'
#' ## factor contains intermediate level missing from 'x'
#' clean_age(c("80 to 84", "90 or more"))
#'
#' ## non-factor
#' clean_age(c("80 to 84", "90 or more"),
#'           factor = FALSE)
#' 
#' ## single
#' clean_age(c("80", "90plus"))
#'
#' ## life table
#' clean_age(c("0",
#'             "30-34",
#'             "10--14",
#'             "1-4 years"))
#' @export
clean_age <- function(x, factor = TRUE) {
    checkmate::assert_vector(x)
    checkmate::assert_flag(factor)
    ## constants
    p_single <- "^([0-9]+)$"
    p_low_up <- "^([0-9]+)-([0-9]+)$"
    p_open <- "^([0-9]+)\\+$"
    ## handle all-NA cases (which includes zero-length)
    if (all(is.na(x)) || (is.factor(x) && all(is.na(levels(x))))) {
        if (factor)
            ans <- factor(x, exclude = character())
        else
            ans <- as.character(x)
        return(ans)
    }
    ## for efficiency, work with unique values
    levels_old <- unique(x)
    n_level <- length(levels_old)
    ## attempt to transform to standard format
    ## using only string operations
    levels_new <- translate_age_labels(levels_old)
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
    ## note that 'upper' is NA when 'is_open' is TRUE, so subsequent
    ## code needs to take precautions in calculations involving 'upper'
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
        is_above_max <- upper > max
        is_above_max[is_open] <- FALSE
        i_above_max <- match(TRUE, is_above_max, nomatch = 0L)
        if (i_above_max > 0L) {
            stop(gettextf("age groups \"%s\" and \"%s\" overlap",
                          levels_old[is_open][[1L]],
                          levels_old[[i_above_max]]),
                 call. = FALSE)
        }
    }
    examples_invalid <- character(3L)
    ## check 5-year age groups
    is_lower_mult_five <- lower %% 5L == 0L
    is_diff_five <- upper - lower == 5L
    is_diff_five[is_open] <- FALSE
    is_low_up_mult_five <- is_low_up & is_lower_mult_five & is_diff_five
    is_open_mult_five <- is_open & is_lower_mult_five
    is_valid_five <- is_na | is_low_up_mult_five | is_open_mult_five
    i_invalid_five <- match(FALSE, is_valid_five, nomatch = 0L)
    is_all_valid_five <- i_invalid_five == 0L
    if (!is_all_valid_five) {
        examples_invalid[[1L]] <- levels_old[[i_invalid_five]]
        ## check life table age groups
        is_single_zero <- is_single & (lower == 0L)
        is_low_up_one_five <- is_low_up & (lower == 1L) & (upper == 5L)
        is_low_up_one_five[is_open] <- FALSE
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
            if (!is_all_valid_single)
                examples_invalid[[3L]] <- levels_old[[i_invalid_single]]
        }
    }
    if (is_all_valid_five)
        type <- "five"
    else if (is_all_valid_lt)
        type <- "lt"
    else if (is_all_valid_single)
        type <- "single"
    else {
        stop(gettextf(paste("unable to parse '%s' as age group labels :",
                            "\n - label \"%s\" incompatible with 5-year age groups,",
                            "\n - label \"%s\" incompatible with life table age groups,",
                            "\n - label \"%s\" incompatible with 1-year age groups"),
                      "x",
                      examples_invalid[[1L]],
                      examples_invalid[[2L]],
                      examples_invalid[[3L]]),
             call. = FALSE)
    }
    levels_complete <- age_labels(type = type,
                                  min = min,
                                  max = max,
                                  open = has_open)
    if (has_na)
        levels_complete <- c(levels_complete, NA)
    i_lev_to_lab <- match(levels_new, levels_complete)
    i_x_to_lev <- match(x, levels_old)
    i <- i_lev_to_lab[i_x_to_lev]
    ans <- levels_complete[i]
    if (factor)
        ans <- factor(ans,
                      levels = levels_complete,
                      exclude = character())
    ans
}


## HAS_TESTS
#' Attempt to treat vector as lower limits
#' of 5-year age groups
#'
#' If `x` consists of integer-like values
#' where the unique, sorted
#' values form a series `0, 5, 10, ..., A`,
#' where `A >= 50`, construct labels
#' "0-4", "5-9", "10-14", ..., "A+". 
#' Otherwise return `NULL`.
#'
#' `x` is allowed to contain `NA`s,
#' which are propagated through to the result.
#'
#' @param x A vector.
#'
#' @return A character vector with the same
#' length as `x`, or `NULL`.
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
#' If `x` consists of integer-like values
#' where the unique, sorted
#' values form a series `0, 1, 5, 10, ..., A`,
#' where `A >= 50`, construct labels
#' "0-4", "5-9", "10-14", ..., "A+". 
#' Otherwise return `NULL`.
#'
#' `x` is allowed to contain `NA`s,
#' which are propagated through to the result.
#'
#' @param x A vector.
#'
#' @return A factor with the same
#' length as `x`, or `NULL`.
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
    min <- min(breaks_obtained, na.rm = TRUE)
    max <- max(breaks_obtained, na.rm = TRUE)
    breaks_valid <- c(0L, 1L, seq.int(from = 5L, to = max, by = 5L))
    has_na <- anyNA(breaks_obtained)
    if (has_na)
        breaks_valid <- c(breaks_valid, NA)
    if (!all(breaks_obtained %in% breaks_valid))
        return(NULL)
    labels <- age_labels_lt(min = min, max = max, open = TRUE)
    if (has_na)
        labels <- c(labels, NA)
    i <- match(x_int, breaks_obtained)
    labels[i]
}


#' Collapse age groups
#'
#' @description
#' Convert age group labels to a less detailed classification.
#' The three classifications recognized by `collapse_age()`
#' are `"single"`, `"five"`, and `"lt"`, as defined on
#' [age_labels()]. The following conversions are permitted:
#'
#' `"single"` ---> `"lt"`
#' `"single"` ---> `"five"`
#' `"lt"` ---> `"five"`
#' 
#' @param x A vector of age labels
#' @param type_to Type of age classification
#' to convert to: `"five"` or `"lt"`.
#' Defaults to `"five"`.
#'
#' @returns If `x` is a factor, then `collapse_age()`
#' returns a factor; otherwise it returns a
#' character vector.
#'
#' @seealso
#' - [age_labels()] to create age group labels
#' - [clean_age()] to convert existing age group labels
#' to a standard format
#' - [set_age_open()] to set the lower limit
#' of the open age group
#'
#' @examples
#' x <- c("0", "5", "3", "12")
#' collapse_age(x)
#' collapse_age(x, type_to = "lt")
#' @export
collapse_age <- function(x, type_to = c("five", "lt")) {
    ## extract values
    type_to <- match.arg(type_to)
    limits_old <- age_limits(x)
    lower_old <- limits_old$lower
    upper_old <- limits_old$upper
    open <- any(is.infinite(upper_old))
    ## deal with degenerate cases
    n_non_na <- sum(!is.na(x))
    if (n_non_na == 0L)
        return(x)
    if ((n_non_na == 1L) && open) {
        age_open <- lower_old[is.infinite(upper_old)][[1L]]
        if ((type_to == "five") && (age_open %% 5L != 0L))
            stop(gettextf(paste("cannot convert to %s age groups :",
                                "open age group starts at %d"),
                          "5-year",
                          age_open))
        if ((type_to == "lt") && (age_open %% 5L != 0L) && (age_open != 1L))
            stop(gettextf(paste("cannot convert to %s age groups :",
                                "open age group starts at %d"),
                          "life table",
                          age_open))
        return(x)
    }
    ## determine type of 'x'
    diff_old <- upper_old - lower_old
    diff_old <- diff_old[!is.na(diff_old)]
    if (open)
        diff_old <- diff_old[is.finite(diff_old)]
    if (all(diff_old == 5L))
        type_from <- "five"
    else if (all(diff_old == 1L))
        type_from <- "single"
    else
        type_from <- "lt"
    ## deal with trivial or impossible conversions
    if (type_from == type_to)
        return(x)
    if (type_from == "five")
        stop(gettextf("cannot convert %s age groups to %s age groups",
                      "5-year",
                      "life table"),
             call. = FALSE)
    ## make new lower bounds
    lower_min_old <- min(lower_old, na.rm = TRUE)
    lower_max_old <- max(lower_old, na.rm = TRUE)
    if (type_to == "five") {
        lower_new <- seq.int(from = lower_min_old,
                             to = lower_max_old,
                             by = 5L)
    }
    else {
        if (lower_min_old == 0L) {
            lower_new <- 0L
            if (lower_max_old >= 1L)
                lower_new <- c(lower_new,
                               1L)
            if (lower_max_old >= 5L)
                lower_new <- c(lower_new,
                               seq.int(from = 5L,
                                       to = lower_max_old,
                                       by = 5L))
        }
        else if (lower_min_old == 1L) {
            lower_new <- 1L
            if (lower_max_old >= 5L)
                lower_new <- c(lower_new,
                               seq.int(from = 5L,
                                       to = lower_max_old,
                                       by = 5L))
        }
        else {
            lower_new <- seq.int(from = lower_min_old,
                                 to = lower_max_old,
                                 by = 5L)
        }
    }
    ## make new labels
    labels_new <- age_labels(type = type_to,
                             min = min(lower_new),
                             max = max(lower_new),
                             open = open)
    ## assign elements of 'x' to new labels
    vec <- c(lower_new, Inf)
    i <- findInterval(lower_old, vec)
    ## construct result and return
    ans <- labels_new[i]
    if (is.factor(x)) {
        if (NA %in% levels(x))
            ans <- factor(ans,
                          levels = c(labels_new, NA),
                          exclude = character())
        else
            ans <- factor(ans,
                          levels = labels_new)
    }
    ans
}


#' Set the open age group
#'
#' Set the lower limit of the open age group.
#' Given a vector of age group labels,
#' recode all age groups with a lower limit
#' greater than or equal to `<lower>` to `<lower>+`.
#'
#' `set_age_open()` requires that `x` and
#' the return value have a
#' a five-year, single-year, or life table format,
#' as described in [age_labels()].
#'
#' @param x A vector of age labels.
#' @param lower An integer. The lower limit
#' for the open age group.
#'
#' @returns A modified version of `x`.
#'
#' @seealso
#' - `set_age_open()` uses [age_lower()] to identify
#' lower limits
#' - [age_labels()] for creating age labels from scratch
#'
#' @examples
#' x <- c("100+", "80-84", "95-99", "20-24")
#' set_age_open(x, 90)
#' set_age_open(x, 25)
#' @export
set_age_open <- function(x, lower) {
    checkmate::assert_vector(x)
    lower <- checkmate::assert_int(x = lower,
                                   lower = 0,
                                   coerce = TRUE)
    if (is.factor(x)) {
        levels_old <- levels(x)
        levels_new <- set_age_open(x = levels_old, lower = lower)
        x <- levels_new[match(x, levels_old)]
        factor(x, levels = unique(levels_new), exclude = character())
    }
    else {
        x <- as.character(x)
        limits <- age_limits(x)
        lower_old <- limits$lower
        upper_old <- limits$upper
        if (all(is.na(lower_old)) && all(is.na(upper_old)))
            return(x)
        is_open_old <- is.infinite(limits$upper)
        if (any(is_open_old)) {
            lower_open_old <- lower_old[is_open_old][[1L]]
            if (lower > lower_open_old)
                stop(gettextf(paste("'%s' [%d] is greater than current lower limit",
                                    "for open age group [%d]"),
                              "lower",
                              lower,
                              lower_open_old),
                     call. = FALSE)
        }
        x[lower_old >= lower] <- paste0(lower, "+")
        val <- tryCatch(clean_age(unique(x)),
                        error = function(e) e)
        if (inherits(val, "error"))
            stop(gettext("new age groups are invalid"),
                 call. = FALSE)
        x
    }
}


## HAS_TESTS
#' Try to translate age labels to
#' standard format
#'
#' Apply a series of text manipulations
#' transformations in an attempt to convert
#' `x` into age group labels in the
#' style of [age_labels_five()],
#' [age_labels_lt()], or
#' [age_labels_single()].
#'
#' `NA`s are permitted, and propagate
#' through to the results.
#' 
#' @param x A vector of possible labels.
#'
#' @return A character vector.
#'
#' @noRd
translate_age_labels <- function(x) {
    year <- "year|years|yr|yrs"
    infant <- "^infants$|^infant$|^in1st$|^lessthan1$|^under1$|^lessthanone$|^0-0|^0_0$"
    plus <- "andover$|plus$|andabove$|andolder$|ormore$|-$|_$|--$|__$"
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
    ## remove spaces
    x <- gsub(" ", "", x)
    ## translate synonyms for age group "0"
    x <- sub(infant, "0", x)
    ## translate synonyms for "+"
    x <- sub(plus, "+", x)
    ## translate synonyms for "-"
    x <- sub("^([0-9]+)to([0-9]+)$", "\\1-\\2", x)
    x <- sub("^([0-9]+)[[:punct:]]+([0-9]+)$", "\\1-\\2", x)
    ## translate numbers
    for (i in seq_along(num))
        x <- gsub(num[[i]], i - 1L, x)
    ## return result
    x
}
