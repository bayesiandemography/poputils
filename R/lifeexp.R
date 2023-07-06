
#' NO_TESTS
#' Calculate life expectancy from mortality rates
#'
#' @param mx A vector of mortality rates
#' (including an [rvec][rvec::rvec()].)
#' @param age A vector of age labels,
#' the same length as `mx`.
#' @param sex Biological sex. Needed
#' only if method is `"CD"` or `"HMD"`.
#' @param method Method used to calculate
#' rates. See Details.
#'
#' @section mx:
#'
#' Non-negative. Can include `Inf`.
#' Can include `NA` (in which case
#' life expectancy is always `NA`.
#'
#' @section age:
#'
#' Must be one single-year, five-year, or life-table
#' age groups (`"single"`, `"five"`, or `"lt"`).
#' See [age_groups()] for details.
#'
#' Last age group must be open.
#'
#' Does not have to start at age 0.
#'
#' @section sex:
#'
#' A single string, either `"Female"` or `"Male"`.
#'
#' A character, with `"Female"` or `"Male"` repeated.
#' (This is useful  when `age` and `sex` are variables in
#'   a data frame.)
#'
#' The `sex` argument is only needed in two cases:
#' 
#' - `method` is `"CD"` and the youngest
#'   age group is less then 5 years old.
#' - `method is `"HMD"` and the youngest
#'    age group is `"0"`.
#' 
#' If mortality experts develop versions of the
#' Coale-Demeny (CD) and Human Mortality Database
#' estimatse for sexes or genders other than `"Female"`,
#' and `"Male"`, we will add them to the package.
#' In the meantime, the `"const"` and `"mid"`
#' methods can be used for any sex or gender.
#'   
#' @section method:
#'
#' There are four choices for the `method` argument.
#' Each method makes different assumptions about
#' the shape of mortality rates, the survival curve,
#' or the average of deaths within each age interval,
#' at least at the youngest ages:
#' 
#' - `"const"`. Mortality rates are constant
#'   within each age group; equivalently, the
#'   life table function `lx` is an exponential
#'   curve within each age interval.
#' - `"mid"`. On average, people die half way
#'   through each age group; equivalently, the
#'   life table function `lx` is a straight
#'   line within each age group.
#' - `"CD"`. As for `"mid"`, except that the average age at
#'   which infants die (and, if `age_groups` is
#'   `"lt"`, the average age at which
#'   children aged 1-4 die), is determined by
#'   formulas developed by Coale and Demeny (1983),
#'   and reported in Preston et al (2001).
#' - `"HMD"`. The  approach used in
#'   the Human Mortality Database.
#'   As for `"mid"`, except that the average
#'   age at which infants die is determined by
#'   formulas developed by Andreev and Kingkade (2015),
#'   and reported in Wilmoth et al (2019).
#' 
#' Some methods cannot be applied to some
#' types of age groups:
#' 
#' |           | `"single"` | `"five"` | `"lt"` |
#' |:----------|------------|----------|--------|
#' | `"const"` | Yes        | Yes      | Yes    |
#' | `"mid"`   | Yes        | Yes      | Yes    |
#' | `"CD"`    | Yes        | No       | Yes    |
#' | `"HMD"`   | Yes        | No       | No     |
#'
#' 
#' @returns
#' - A scalar, if `mx` is an ordinary vector
#' - An [rvec][rvec::rvec()], if `mx` is an rvec.
#'
#' @examples
#' mx <- c(0.1, 0.05, 0.01, 0.2)
#' age <- c("0", "1-4", "5-9", "10+")
#' lifeexp(mx = mx,
#'         age = age)
#' lifeexp(mx = mx,
#'         age = age,
#'         sex = "Female",
#'         method = "CD")
#'
#' ## more involved example using
#' ## tidyverse functions
#' library(dplyr)
#' west_mx %>%
#'   group_by(sex, level) %>%
#'   summarise(ex0 = lifeexp(mx = mx,
#'                           age = age,
#'                           sex = sex,
#'                           method = "CD"))
#'
#' ## rvecs
#' library(rvec)
#' mx_rv <- rvec(list(c(0.1,  0.12),
#'                    c(0.05, 0.06),
#'                    c(0.01, 0.012),
#'                    c(0.2,  0.24)))
#' mx_rv
#' lifeexp(mx = mx_rv,
#'         age = age)
#' @export
lifeexp <- function(mx,
                    age,
                    sex,
                    method = c("const",
                               "mid",
                               "CD",
                               "HMD")) {
    UseMethod("lifeexp")
}

#' @export
lifeexp.default <- function(mx,
                            age,
                            sex = NULL,
                            method = c("const",
                                       "mid",
                                       "CD",
                                       "HMD")) {
    check_mx_vec(mx)
    mx <- matrix(mx, ncol = 1L)
    method <- match.arg(method)
    lifeexp_inner(mx = mx,
                  age = age,
                  sex = sex,
                  method = method)
}

#' @export
lifeexp.rvec <- function(mx,
                         age,
                         sex = NULL,
                         method = c("const",
                                    "mid",
                                    "CD",
                                    "HMD")) {
    check_mx_rvec(mx)
    mx <- as.matrix(mx)
    method <- match.arg(method)
    lifeexp_inner(mx = mx,
                  age = age,
                  sex = sex,
                  method = method)
}


## HAS_TESTS
#' Calculate life expectancy
#'
#' Wrapper around function .lifeexp
#' which passes to C++ functions.
#' `lifeexp_helper()` prepares data
#' and reformats result.
#'
#' The underlying C++ functions assume
#' the minimum age is always 0.
#' The "CD" and "HMD" methods apply special
#' sex-specific treatments
#' to age groups "0" and (in case of "CD") "1-4",
#' before switching to sex-neutral "mid" method.
#' We take this behavior into account when
#' calculating life expectancy at ages
#' greater than 0.
#'
#' @param mx Matrix of mortality rates
#' @param age Vector of ages
#' @param sex Scalar or vector giving sex
#' (If vector, all elements should be identical.)
#' @param method String describing method
#' for calculating life expectancy.
#'
#' @returns A scalar or an rvec
#'
#' @noRd
lifeexp_inner <- function(mx, age, sex, method) {
    mx <- matrix(as.double(mx),
                 nrow = nrow(mx),
                 ncol = ncol(mx))
    check_age(x = age,
              complete = TRUE,
              unique = TRUE,
              zero = FALSE,
              open = TRUE)
    age_groups <- age_groups(age)
    min_age <- min(age_lower(age), na.rm = TRUE)
    if (min_age == 0L) {
        if (method %in% c("CD", "HMD")) {
            sex <- reformat_sex(sex, factor = FALSE)
            check_lifeexp_sex(sex)
            method <- paste(method, sex[[1L]], sep = "-")
        }
        ans <- .lifeexp(mx = mx,
                        age_groups = age_groups,
                        method = method)
    }
    else if (min_age == 1L) {
        if ((method == "CD") && (age_groups == "lt")) {
            mx_augmented <- rbind(0, mx) ## no mortality in extra year
            sex <- reformat_sex(sex, factor = FALSE)
            check_lifeexp_sex(sex)
            method <- paste(method, sex[[1L]], sep = "-")
            ans <- .lifeexp(mx = mx_augmented,
                            age_groups = age_groups,
                            method = method)
            ans <- ans - 1 ## subtract extra year
        }
        else {
            if (method %in% c("CD", "HMD"))
                method <- "mid"
            ans <- .lifeexp(mx = mx,
                            age_groups = age_groups,
                            method = method)
        }
    }
    else { ## min_age > 1L
        if (method %in% c("CD", "HMD"))
            method <- "mid"
        ans <- .lifeexp(mx = mx,
                        age_groups = age_groups,
                        method = method)
    }
    is_ans_rvec <- length(ans) > 1L
    if (is_ans_rvec) {
        ans <- matrix(ans, nrow = 1L)
        ans <- rvec::rvec_dbl(ans)
    }
    ans
}


## HAS_TESTS
#' Low-level function for calculating life expectancy
#' from mortality rates
#'
#' Calculate life expectancy from mortality rates.
#' This is an internal functions, for use by
#' developers, and not normally be called directly
#' by end users. 
#'
#' Mortality rates `mx` are held in a matrix,
#' with one set of age-specific rates per column.
#'
#' There are three choices for argument `age_groups`:
#' - `"lt"`. Life table age groups `"0", "1-4", "5-9", \dots, "A+"`.
#' - `"single"`. `"0", "1", "2", \dots, "A+"`.
#' - `"five"`. `"0-4", "5-9", \dots, "A+"`.
#' 
#' In all three cases, the last interval is always
#' assumed to be open, meaning that
#' it includes everyone over a certain age.
#'
#'
#'
#' @param mx Mortality rates. A matrix of non-negative values.
#' Must have at least one column.
#' @param age_groups Type of age groups used. Choices are
#' `"lt"`, `"single"`, and `"five"`. See Details.
#' @param method Method for converting calculating
#' life expectancy. See Details for options.
#'
#' @returns A numeric vector with length `ncol(mx)`.
#'
#' @seealso
#' - [lifeexp()] is a much more user-friendly
#'   function for calculating life expectancy.
#'
#' @examples
#' mx <- matrix(c(0.010, 0.002, 0.070, 0.200,
#'                0.011, 0.003, 0.072, 0.210),
#'              ncol = 2)
#'
#' .lifeexp(mx, age_groups = "lt", method = "mid")
#' .lifeexp(mx, age_groups = "lt", method = "CD-Female")
#' .lifeexp(mx, age_groups = "single", method = "CD-Female")
#' .lifeexp(mx, age_groups = "single", method = "const")
#' @keywords internal
#' @export
.lifeexp <- function(mx, age_groups, method) {
    le(mx, age_groups, method)
}

