
#' NO_TESTS
#' Calculate life expectancies or life tables.
#'
#' @param data Data frame with mortality data.
#' @param mx <[`tidyselect`][tidyselect::language]>
#' Mortality rates. Possibly an [rvec][rvec::rvec()].
#' @param qx <[`tidyselect`][tidyselect::language]>
#' Probabilities of dying. Possibly an [rvec][rvec::rvec()].
#' @param age <[`tidyselect`][tidyselect::language]>
#' Age group labels.
#' @param sex <[`tidyselect`][tidyselect::language]>
#' Biological sex. Needed only if method is
#' `"CD"` or `"HMD"`.
#' @param by <[`tidyselect`][tidyselect::language]>
#' Separate life expectancies, or life tables, are
#' calculated for each combination the `by` variables.
#' @param method Method used to calculate
#' rates. See Details.
#' @param at Age at which life expectancy
#' is calculated. Default is `0` (ie birth.)
#' @param prefix Optional prefix added to new
#' columns in result.
#'
#' @section mx:
#'
#' Non-negative. Can include `Inf`.
#' Can include `NA` (in which case
#' life expectancy is always `NA`.)
#'
#' @section age:
#'
#' Must be one single-year, five-year, or life-table
#' age groups (`"single"`, `"five"`, or `"lt"`).
#' See [age_groups()] for details.
#'
#' The final age group must be open, ie
#' have no upper limit.
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
#' @seealso
#' - `lifeexp()` uses [find_var_age()] to
#'   identify the age column if no value for
#'   `age` is supplied.
#'
#' @examples
#' mx <- c(0.1, 0.05, 0.01, 0.2)
#' age <- c("0", "1-4", "5-9", "10+")
#' lifeexp(mx, age = age)
#' lifeexp(mx = mx,
#'         age = age,
#'         sex = "Female",
#'         method = "CD")
#' lifeexp(mx, age = age, at = 5)
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
lifetab <- function(data,
                    mx,
                    qx,
                    age = NULL,
                    sex = NULL,
                    by = NULL,
                    method = c("const",
                               "mid",
                               "CD",
                               "HMD"),
                    a0 = NULL,
                    at = 0,
                    prefix = NULL) {
    mx_quo <- rlang::enquo(mx)
    qx_quo <- rlang::enquo(qx)
    age_quo <- rlang::enquo(age)
    sex_quo <- rlang::enquo(sex)
    by_quo <- rlang::enquo(by)
    method <- match.arg(method)
    life(data = data,
         mx_quo = mx_quo,
         qx_quo = qx_you,
         age_quo = age_quo,
         sex_quo = sex_quo,
         by_quo = by_quo,
         method = method,
         a0 = a0,
         at = at,
         prefix = prefix,
         is_table = TRUE)
}
    
    stop("not written yet")
}


#' @export
#' @rdname lifetab
lifeexp <- function(data,
                    mx,
                    qx,
                    age = NULL,
                    sex = NULL,
                    by = NULL,
                    method = c("const",
                               "mid",
                               "CD",
                               "HMD"),
                    a0 = NULL,
                    at = 0,
                    prefix = NULL) {
    mx_quo <- rlang::enquo(mx)
    qx_quo <- rlang::enquo(qx)
    age_quo <- rlang::enquo(age)
    sex_quo <- rlang::enquo(sex)
    by_quo <- rlang::enquo(by)
    method <- match.arg(method)
    life(data = data,
         mx_quo = mx_quo,
         qx_quo = qx_you,
         age_quo = age_quo,
         sex_quo = sex_quo,
         by_quo = by_quo,
         method = method,
         a0 = a0,
         at = at,
         prefix = prefix,
         is_table = FALSE)
}
    

life_inner <- function(data, 
                       mx_quo,
                       qx_quo,
                       age_quo,
                       sex_quo,
                       by_quo,
                       method,
                       a0,
                       at,
                       prefix,
                       is_table) {
    if (!is.data.frame(data))
        cli::cli_abort(c("{.arg data} is not a data frame.",
                         i = "{.arg data} has class {.cls {class(data)}}."))
    mx_colnum <- tidyselect::eval_select(mx_quo, data = data)
    qx_colnum <- tidyselect::eval_select(qx_quo, data = data)
    age_colnum <- tidyselect::eval_select(age_quo, data = data)
    by_colnums <- tidyselect::eval_select(by_quo, data = data)
    groups_colnums <- get_groups_colnums(data)
    has_by <- length(by_colnums) > 0L
    has_groups <- length(groups_colnums) > 0L
    if (!has_by && !has_groups) {
        ans <- life_inner_one(data = data,
                              mx_colnum = mx_colnum,
                              qx_colnum = qx_colnum,
                              age_colnum = age_colnum,
                              method = method,
                              a0 = a0,
                              is_table = is_table)
    }
    else {
        if (has_by && has_groups)
            cli::cli_abort("Can't supply {.arg by} when {.arg data} is a grouped data
  frame.")
        else if (has_by && !has_groups)
            by <- data[by_colnums]
        else 
            by <- data[groups_colnums]
        data <- vctrs::vec_split(data, by = by)
        for (i in new(data)) {
            return_val <- tryCatch(life_inner_one(data = data$val[[i]],
                                                  mx_colnum = mx_colnum,
                                                  mx_colnum = mx_colnum,
                                                  qx_colnum = qx_colnum,
                                                  age_colnum = age_colnum,
                                                  method = method,
                                                  a0 = a0,
                                                  is_table = is_table),
                                   error = function(e) e)
            if (inherits(return_val, "error")) {
                str_key <- make_str_key(data$key[i, , drop = FALSE])
                cli::cli_abort(c("Problem with inputs for {str_key}",
                                 i = return_val$message))
            }
            data$val[[i]] <- return_val
        }
        ans <- vctrs::vec_rbind(data$val)
        
        
                                  
                                      
            data$val[[i]]                                         
        data$res <- .mapply(life_inner_one,
                            dots = list(data = val$data,
                            
                            dots = list(key = key,
                                        val = val)
    
    
    
    
        
        || (!bhas_by && has_groups)) {
        by <- 


    }
    else {
        
    if (has_by) {
        
        by <- 

        
    age <- lifetab_prepare_age(data = data,
                               age_colnum = age_colnum)
    check_a0(a0)
    a0 <- as.double(a0)
    mx <- lifetab_prepare_mx(data = data,
                             mx_colnum = mx_colnum,
                             qx_colnum = qx_colnum,
                             age = age,
                             method = method,
                             a0 = a0)
    if (is_table) {
        lx <- mx_to_lx(mx = mx,
                       age_group_type = age_group_type,
                       sex = sex,
                       a0 = a0)
        Lx <- mx_to_Lx(mx = mx,
                       age_group_type = age_group_type,
                       sex = sex,
                       a0 = a0)
        qx <- lx_to_qx(lx)
        dx <- lx_to_dx(lx)
        ex <- Lx_to_ex(Lx)
        tab <- tibble(qx = qx,
                         lx = lx,
                         dx = dx,
                         Lx = Lx,
                      ex = ex)
        if (!is.null(prefix))
            names(tab) <- paste(prefix, names(tab), sep = ".")
        ans <- rbind(data, tab)
    }
    else {
        ex <- mx_to_ex(mx = mx,
                       age_group_type = age_group_type,
                       sex = sex,
                       a0 = a0)
        
        
        
        
                     
}



lifetab_prepare_mx <- function(mx_colnum, qx_colnum, age, method, a0) {
    n_mx <- length(mx_colnum)
    n_qx <- length(qx_colnum)
    has_mx <- n_mx > 0L
    has_qx <- n_qx > 0L
    if (!has_mx && !has_qx)
        cli::cli_abort("No value supplied for {.arg mx} or for {.arg qx}.")
    else if (has_mx && !has_qx) {
        if (n_mx > 1L)
            cli::cli_abort("Attempt to select more than one {.arg mx} variable.")
        mx <- data[[mx_colnum]]
        if (!is.numeric(mx))
            cli::cli_abort(c("{.arg mx} is non-numeric.",
                             i = "{.arg mx} has class {.cls {class(mx)}}."))
        if (inherits(mx, "rvec"))
            ans <- as.matrix(mx)
        else if (is.atomic(mx))
            ans <- matrix(mx, nrow = length(mx), ncol = 1L)
        else
            cli::cli_abort(c("{.arg mx} is not a vector.",
                             i = "{.arg mx} has class {.cls {class(mx)}}."))
    }
    else if (!has_mx && has_qx) {
        if (n_qx > 1L)
            cli::cli_abort("Attempt to select more than one {.arg qx} variable.")
        qx <- data[[qx_colnum]]
        if (!is.numeric(qx))
            cli::cli_abort(c("{.arg qx} is non-numeric.",
                             i = "{.arg qx} has class {.cls {class(qx)}}."))
        if (inherits(qx, "rvec"))
            qx <- as.matrix(qx)
        else if (is.atomic(qx))
            qx <- matrix(qx, nrow = length(qx), ncol = 1L)
        else
            cli::cli_abort(c("{.arg qx} is not a vector.",
                             i = "{.arg qx} has class {.cls {class(qx)}}."))
        ans <- qx_to_mx(qx = qx,
                        age = age,
                        method = method,
                        a0 = a0)
    }
    else
        cli::cli_abort(c("Value supplied for {.arg mx} and for {.arg qx}.",
                         i = "Must supply value for {.arg mx} or for {.arg qx}, but not both."))
    ans
}
                         

mx_to_ex <- function(mx, age_group_type, method, sex, a0) {
    if (method == "CD")
        ans <- mx_to_ex_cd(mx, age_group_type, a0)
    else if (method == "const")
        ans <- mx_to_ex_const(mx, age_group_type, sex, a0)
    else if (method == "HMD")
        ans <- mx_to_ex_hmd(mx, age_group_type, sex, a0)
    else if (method == "mid")
        ans <- mx_to_ex_hmd(mx, age_group_type, a0)
    else
        cli::cli_abort(c("Internal error: Invalid value for {.arg method}",
                         i = "{.arg method} is {.val {method}}."))
    if (identical(ncol(ans), 1L))
        ans <- rvec_dbl(ans)
    ans
}

mx_to_lx <- function(mx, age_group_type, method, sex, a0) {
    if (method == "CD")
        ans <- mx_to_lx_cd(mx, age_group_type, a0)
    else if (method == "const")
        ans <- mx_to_lx_const(mx, age_group_type, sex, a0)
    else if (method == "HMD")
        ans <- mx_to_lx_hmd(mx, age_group_type, sex, a0)
    else if (method == "mid")
        ans <- mx_to_lx_hmd(mx, age_group_type, a0)
    else
        cli::cli_abort(c("Internal error: Invalid value for {.arg method}",
                         i = "{.arg method} is {.val {method}}."))
    if (identical(ncol(ans), 1L))
        ans <- rvec_dbl(ans)
    ans
}


mx_to_Lx <- function(mx, age_group_type, method, sex, a0) {
    if (method == "CD")
        ans <- mx_to_Lx_cd(mx, age_group_type, a0)
    else if (method == "const")
        ans <- mx_to_Lx_const(mx, age_group_type, sex, a0)
    else if (method == "HMD")
        ans <- mx_to_Lx_hmd(mx, age_group_type, sex, a0)
    else if (method == "mid")
        ans <- mx_to_Lx_hmd(mx, age_group_type, a0)
    else
        cli::cli_abort(c("Internal error: Invalid value for {.arg method}",
                         i = "{.arg method} is {.val {method}}."))
    if (identical(ncol(ans), 1L))
        ans <- rvec_dbl(ans)
    ans
}


qx_to_mx <- function(qx, age_group_type, method, sex, a0) {
    if (method == "CD")
        ans <- qx_to_mx_cd(qx, age_group_type, a0)
    else if (method == "const")
        ans <- qx_to_mx_const(qx, age_group_type, sex, a0)
    else if (method == "HMD")
        ans <- qx_to_mx_hmd(qx, age_group_type, sex, a0)
    else if (method == "mid")
        ans <- qx_to_mx_hmd(qx, age_group_type, a0)
    else
        cli::cli_abort(c("Internal error: Invalid value for {.arg method}",
                         i = "{.arg method} is {.val {method}}."))
    if (identical(ncol(ans), 1L))
        ans <- rvec_dbl(ans)
    ans
}

        
        


## ## HAS_TESTS
## #' Calculate life expectancy
## #'
## #' Wrapper around function .lifeexp
## #' which passes to C++ functions.
## #' `lifeexp_helper()` prepares data
## #' and reformats result.
## #'
## #' The underlying C++ functions assume
## #' the minimum age is always 0.
## #' The "CD" and "HMD" methods apply special
## #' sex-specific treatments
## #' to age groups "0" and (in case of "CD") "1-4",
## #' before switching to sex-neutral "mid" method.
## #' We take this behavior into account when
## #' calculating life expectancy at ages
## #' greater than 0.
## #'
## #' @param mx Matrix of mortality rates
## #' @param age Vector of ages
## #' @param sex Scalar or vector giving sex
## #' (If vector, all elements should be identical.)
## #' @param method String describing method
## #' for calculating life expectancy.
## #' @param at Age at which life expectancy
## #' is calculated. Default is 0 (ie birth.)
## #'
## #' @returns A scalar or an rvec
## #'
## #' @noRd
## lifeexp_inner <- function(mx, age, sex, method, at) {
##     mx <- matrix(as.double(mx),
##                  nrow = nrow(mx),
##                  ncol = ncol(mx))
##     check_age(x = age,
##               complete = TRUE,
##               unique = TRUE,
##               zero = FALSE,
##               open = TRUE)
##     age_groups <- age_groups(age)
##     min_age <- min(age_lower(age), na.rm = TRUE)
##     check_number(x = at,
##                  x_arg = "at",
##                  is_positive = FALSE,
##                  is_nonneg = TRUE,
##                  is_whole = TRUE)
##     if (min_age > at) {
##         youngest <- age[age_lower(age) == min_age][[1L]]
##         cli::cli_abort(c("{.arg at} less than lower limit of youngest age group.",
##                          i = "{.arg at} is {.val {at}}",
##                          i = "Youngest age group is {.val {youngest}}"))
##     }
##     if (!(at %in% age_lower(age))) {
##         cli::cli_abort(c("{.arg at} is not the lower limit of an age group.",
##                          i = "{.arg at} is {.val {at}}.",
##                          i = "Age groups: {.val {unique(age)}}."))
##     }
##     if (at == 0L) {
##         if (method %in% c("CD", "HMD")) {
##             sex <- reformat_sex(sex, factor = FALSE)
##             check_lifeexp_sex(sex)
##             method <- paste(method, sex[[1L]], sep = "-")
##         }
##         ans <- .lifeexp(mx = mx,
##                         age_groups = age_groups,
##                         method = method)
##     }
##     else if (at == 1L) {
##         if ((method == "CD") && (age_groups == "lt")) {
##             mx_augmented <- rbind(0, mx) ## no mortality in extra year
##             sex <- reformat_sex(sex, factor = FALSE)
##             check_lifeexp_sex(sex)
##             method <- paste(method, sex[[1L]], sep = "-")
##             ans <- .lifeexp(mx = mx_augmented,
##                             age_groups = age_groups,
##                             method = method)
##             ans <- ans - 1 ## subtract extra year
##         }
##         else {
##             if (method %in% c("CD", "HMD"))
##                 method <- "mid"
##             ans <- .lifeexp(mx = mx,
##                             age_groups = age_groups,
##                             method = method)
##         }
##     }
##     else { ## at > 1L
##         if (method %in% c("CD", "HMD"))
##             method <- "mid"
##         ans <- .lifeexp(mx = mx,
##                         age_groups = age_groups,
##                         method = method)
##     }
##     is_ans_rvec <- length(ans) > 1L
##     if (is_ans_rvec) {
##         ans <- matrix(ans, nrow = 1L)
##         ans <- rvec::rvec_dbl(ans)
##     }
##     ans
## }


## ## HAS_TESTS
## #' Low-level function for calculating life expectancy
## #' from mortality rates
## #'
## #' Calculate life expectancy from mortality rates.
## #' This is an internal functions, for use by
## #' developers, and not normally be called directly
## #' by end users. 
## #'
## #' Mortality rates `mx` are held in a matrix,
## #' with one set of age-specific rates per column.
## #'
## #' There are three choices for argument `age_groups`:
## #' - `"lt"`. Life table age groups `"0", "1-4", "5-9", \dots, "A+"`.
## #' - `"single"`. `"0", "1", "2", \dots, "A+"`.
## #' - `"five"`. `"0-4", "5-9", \dots, "A+"`.
## #' 
## #' In all three cases, the last interval is always
## #' assumed to be open, meaning that
## #' it includes everyone over a certain age.
## #'
## #'
## #'
## #' @param mx Mortality rates. A matrix of non-negative values.
## #' Must have at least one column.
## #' @param age_groups Type of age groups used. Choices are
## #' `"lt"`, `"single"`, and `"five"`. See Details.
## #' @param method Method for converting calculating
## #' life expectancy. See Details for options.
## #'
## #' @returns A numeric vector with length `ncol(mx)`.
## #'
## #' @seealso
## #' - [lifeexp()] is a much more user-friendly
## #'   function for calculating life expectancy.
## #'
## #' @examples
## #' mx <- matrix(c(0.010, 0.002, 0.070, 0.200,
## #'                0.011, 0.003, 0.072, 0.210),
## #'              ncol = 2)
## #'
## #' .lifeexp(mx, age_groups = "lt", method = "mid")
## #' .lifeexp(mx, age_groups = "lt", method = "CD-Female")
## #' .lifeexp(mx, age_groups = "single", method = "CD-Female")
## #' .lifeexp(mx, age_groups = "single", method = "const")
## #' @keywords internal
## #' @export
## .lifeexp <- function(mx, age_groups, method) {
##     le(mx, age_groups, method)
## }

