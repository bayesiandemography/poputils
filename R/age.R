
## HAS_TESTS
#' Create one-year age labels
#'
#' Create labels for one-year age groups.
#'
#' \code{min} and \code{max} refer to
#' exact ages. When \code{open} is \code{TRUE},
#' the age groups are defined as
#' \code{[min, min+1), [min+1, min+2), \dots,
#' [max-1, max), [max, Inf)}.
#' When \code{open} is \code{FALSE},
#' the age groups are defined as
#' \code{[min, min+1), [min+1, min+2), \dots,
#' [max-1, max)}.
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
#' @return A character vector of length
#' \code{max - min + open}.
#'
#' @seealso \code{\link{age_five}} for 5-year
#' age groups and \code{\link{age_lt}} for
#' life table age groups.
#'
#' @examples
#' age_single()
#' age_single(max = 5)
#' age_single(max = 5, open = FALSE)
#' age_single(min = 15, max = 40)
#' @export
age_single <- function(min = 0, max = 100, open = NULL) {
    min <- checkmate::assert_int(min, coerce = TRUE)
    max <- checkmate::assert_int(max, coerce = TRUE)
    checkmate::assert_flag(open, null.ok = TRUE)
    if (is.null(open))
        open <- min == 0L
    if (open)
        label_open <- paste0(max, "+")
    diff <- max - min
    if (diff < 0L) {
        stop(gettextf("'%s' [%d] is less than '%s' [%d] ",
                      "max", max, "min", min),
             call. = FALSE)
    }
    else if (diff == 0L) {
        if (open) {
            ans <- label_open
        }
        else {
            stop(gettextf("'%s' [%d] equals '%s' [%d] but '%s' is %s",
                          "max", max, "min", min, "open", "FALSE"),
                 call. = FALSE)
        }
    }
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
#' \code{min} and \code{max} refer to
#' exact ages. When \code{open} is \code{TRUE},
#' the age groups are defined as
#' \code{[min, min+5), [min+5, min+10), \dots,
#' [max-5, max), [max, Inf)}.
#' When \code{open} is \code{FALSE},
#' the age groups are defined as
#' \code{[min, min+5), [min+5, min+10), \dots,
#' [max-5, max)}.
#'
#' \code{open} defaults to \code{TRUE} when
#' \code{min} equals zero, and to \code{FALSE}
#' otherwise.
#'
#' @inheritParams age_single
#'
#' @return A character vector of length
#' \code{(max - min)/5 + open}.
#'
#' @seealso \code{\link{age_single}} for 1-year
#' age groups and \code{\link{age_lt}} for
#' life table age groups.
#'
#' @examples
#' age_five()
#' age_five(max = 25)
#' age_five(max = 25, open = FALSE)
#' age_five(min = 15, max = 40)
#' @export
age_five <- function(min = 0, max = 100, open = NULL) {
    min <- checkmate::assert_int(min, coerce = TRUE)
    max <- checkmate::assert_int(max, coerce = TRUE)
    checkmate::assert_flag(open, null.ok = TRUE)
    if (is.null(open))
        open <- min == 0
    if (open)
        label_open <- paste0(max, "+")
    diff <- max - min
    if (diff < 0L) {
        stop(gettextf("'%s' [%d] is less than '%s' [%d] ",
                      "max", max, "min", min),
             call. = FALSE)
    }
    else if (diff == 0L) {
        if (open) {
            ans <- label_open
        }
        else {
            stop(gettextf("'%s' [%d] equals '%s' [%d] but '%s' is %s",
                          "max", max, "min", min, "open", "FALSE"),
                 call. = FALSE)
        }
    }
    else {
        if ((diff %% 5L) != 0L) {
            stop(gettextf("difference between '%s' [%d] and '%s' [%d] is not divisible by %d",
                          "max", max, "min", min, 5L),
                 call. = FALSE)
        }
        lower <- seq.int(from = min, to = max - 5L, by = 5L)
        upper <- seq.int(from = min + 4L, to = max - 1L, by = 5L)
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
#' @inheritParams age_single
#'
#' @return A character vector.
#'
#' @seealso \code{\link{age_single}} for 1-year
#' age groups and \code{\link{age_five}} for
#' 5-year age groups.
#'
#' @examples
#' age_lt()
#' age_lt(max = 25)
#' @export
age_lt <- function(max = 100) {
    max <- checkmate::assert_int(max, lower = 0L, coerce = TRUE)
    label_open <- paste0(max, "+")
    if (max == 0L)
        ans <- label_open
    else if (max == 1L)
        ans <- c("0", "1+")
    else if (max %in% 2:4)
        stop(gettextf("'%s' [%d] must be one of 0, 1, 5, 10, ...",
                      "max", max))
    else {
        if ((max %% 5L) != 0L) {
            stop(gettextf("'%s' [%d] is not divisible by %d",
                          "max", max, 5L),
                 call. = FALSE)
        }
        head <- c("0", "1-4")
        tail <- age_five(min = 5, max = max, open = TRUE)
        ans <- c(head, tail)
    }
    ans
}
    
                       
