
#' NO_TESTS
#' Calculate life tables or life expectancies
#'
#' Calculate full life tables, or just life expectancies.
#'
#' @section age:
#'
#' The `age` variable must contain labels that functions
#' such as [reformat_age()] or [age_groups()]
#' can interpret, ie that can be interpreted
#' as single, five-year or life-table age groups. The final
#' age group must be "open", with no upper limit. There also
#' must not be any gaps between the lowest and highest
#' age groups.
#'
#' @section sex:
#'
#' A vector containing, for each set of ages, a single
#' string, which can be interpreted by [reformat_sex()],
#' as either `"Female"` or `"Male"`.
#'
#' The `sex` argument is only need in two cases:
#' 
#' - `method` is `"CD"` and the youngest
#'   age group is less then 5 years old.
#' - `method is `"HMD"` and the youngest
#'    age group is `"0"`.
#' 
#' If Coale-Demeny (CD) and Human Mortality Database
#' formulas are produced for sexes or genders other than `"Female"`,
#' and `"Male"`, we will add these formulas to the package.
#' In the meantime, `"const"` and `"mid"`
#' methods can be used for any sex or gender.
#'
#' @section ax:
#'
#' The average age at which 
#'
#' @section by:
#'
#' Life tables or life expectancies are calculated
#' separately for each combination of the `by` variables.
#' If a `sex` argument is specified, then that variable
#' is automatically included in the `by` variables.
#'   
#' @section method:
#'
#' There are four choices for the `method` argument.
#' Each method makes different assumptions about
#' the way that mortality rates vary within age group,
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
#' @section ax:
#'
#' Average number of years lived within age group
#' by people who die within the age group: sometimes
#' referred to as a 'separation factor'. If a non-`NA`
#' value is supplied for an age group, then
#' calculations based on the identity
#'
#' \deqn{m_x = d_x / (n_x l_x + a_x d_x)}
#' 
#' (where \eqn{m_x} is the mortality rate,
#'  \eqn{d_x} is deaths, \eqn{n_x} is the width of the
#' age group, and \eqn{l_x} is the probability
#' of surviving to age \eqn{x})
#' are used for that age group, regardless of the
#' value for `"method"`. Can be used to fine-tune
#' methods (by suppling only a few non-`NA` values),
#' or to create life tables or life expectancies based
#' entirely on `ax` (by suppliying non-`NA` values for
#' all age groups.) See below for examples.
#'
#' @param data Data frame with mortality data.
#' @param mx <[`tidyselect`][tidyselect::language]>
#' Mortality rates. Possibly an [rvec][rvec::rvec()].
#' @param age <[`tidyselect`][tidyselect::language]>
#' Age group labels.
#' @param sex <[`tidyselect`][tidyselect::language]>
#' Biological sex. Needed only if method is
#' `"CD"` or `"HMD"`.
#' @param ax <[`tidyselect`][tidyselect::language]>
#' Average age at death within age group.
#' Optional. See Details. 
#' @param by <[`tidyselect`][tidyselect::language]>
#' Separate life expectancies, or life tables, are
#' calculated for each combination the `by` variables.
#' @param method Method used to calculate
#' rates. See Details.
#' @param at Age at which life tables start (`lifetab()`),
#' or at which life expectancy is calculate (`lifeexp()`).
#' Default is `0`.
#' @param prefix Optional prefix added to new
#' columns in result.
#'
#' @returns A [tibble][tibble::tibble()].
#'
#' @references
#' - Preston S H, Heuveline P, and Guillot M. 2001.
#' *Demography: Measuring and Modeling Population Processes*
#' Oxford: Blackwell.
#' - Human Mortality Database [Methods Protocol](https://www.mortality.org/File/GetDocument/Public/Docs/MethodsProtocolV6.pdf).
#' - IUSSP [Tools for Demographic Estimation](https://demographicestimation.iussp.org).
#'
#' @export
lifetab <- function(data,
                    mx,
                    age = NULL,
                    sex = NULL,
                    ax = NULL,
                    by = NULL,
                    method = c("const",
                               "mid",
                               "CD",
                               "HMD"),
                    at = 0,
                    prefix = NULL) {
    mx_quo <- rlang::enquo(mx)
    age_quo <- rlang::enquo(age)
    sex_quo <- rlang::enquo(sex)
    ax_quo <- rlang::enquo(ax)
    by_quo <- rlang::enquo(by)
    method <- match.arg(method)
    life_inner(data = data,
               mx_quo = mx_quo,
               age_quo = age_quo,
               sex_quo = sex_quo,
               ax_quo = ax_quo,
               by_quo = by_quo,
               method = method,
               at = at,
               prefix = prefix,
               is_table = TRUE)
}


#' @export
#' @rdname lifetab
lifeexp <- function(data,
                    mx,
                    age = NULL,
                    sex = NULL,
                    ax = NULL,
                    by = NULL,
                    method = c("const",
                               "mid",
                               "CD",
                               "HMD"),
                    at = 0,
                    prefix = NULL) {
    mx_quo <- rlang::enquo(mx)
    age_quo <- rlang::enquo(age)
    sex_quo <- rlang::enquo(sex)
    ax_quo <- rlang::enquo(ax)
    by_quo <- rlang::enquo(by)
    method <- match.arg(method)
    life_inner(data = data,
               mx_quo = mx_quo,
               age_quo = age_quo,
               sex_quo = sex_quo,
               ax_quo = ax_quo,
               by_quo = by_quo,
               method = method,
               at = at,
               prefix = prefix,
               is_table = FALSE)
}



#' Check structure of inputs, reformat, and pass to
#' function doing detailed checkings and calculation
#'
#' @noRd
life_inner <- function(data, 
                       mx_quo,
                       age_quo,
                       sex_quo,
                       ax_quo,
                       by_quo,
                       method,
                       at,
                       prefix,
                       is_table) {
    if (!is.data.frame(data))
        cli::cli_abort(c("{.arg data} is not a data frame.",
                         i = "{.arg data} has class {.cls {class(data)}}."))
    mx_colnum <- tidyselect::eval_select(mx_quo, data = data)
    age_colnum <- tidyselect::eval_select(age_quo, data = data)
    sex_colnum <- tidyselect::eval_select(sex_quo, data = data)
    ax_colnum <- tidyselect::eval_select(ax_quo, data = data)
    by_colnums <- tidyselect::eval_select(by_quo, data = data)
    groups_colnums <- groups_colnums(data)
    check_colnums_lifetab(mx_colnum = mx_colnum,
                          age_colnum = age_colnum,
                          sex_colnum = sex_colnum,
                          ax_colnum = ax_colnum,
                          by_colnums = by_colnums,
                          groups_colnums = groups_colnums)
    is_sex_supplied <- length(sex_colnum) > 0L
    is_by_supplied <- length(by_colnums) > 0L
    is_groups_supplied <- length(groups_colnums) > 0L
    if (!is_sex_supplied && !is_by_supplied && !is_groups_supplied) {
        ans <- life_inner_one(data = data,
                              mx_colnum = mx_colnum,
                              age_colnum = age_colnum,
                              sex_colnum = sex_colnum,
                              ax_colnum = ax_colnum,
                              method = method,
                              at = at,
                              prefix = prefix,
                              is_table = is_table)
    }
    else {
        if (is_by_supplied)
            by_colnums <- unique(c(by_colnums, sex_colnum))
        else
            by_colnums <- unique(c(groups_colnums, sex_colnum))
        data <- vctrs::vec_split(data[-by_colnums], by = by[by_colnums])
        for (i in new(data)) {
            return_val <- tryCatch(life_inner_one(data = data$val[[i]],
                                                  mx_colnum = mx_colnum,
                                                  age_colnum = age_colnum,
                                                  sex_colnum = sex_colnum,
                                                  ax_colnum = ax_colnum,
                                                  method = method,
                                                  at = at,
                                                  prefix = prefix,
                                                  is_table = is_table),
                                   error = function(e) e)
            if (inherits(return_val, "error")) {
                str_key <- make_str_key(data$key[i, , drop = FALSE])
                cli::cli_abort(c("Problem calculating life table functions for case where {str_key}.",
                                 i = return_val$message))
            }
            data$val[[i]] <- return_val
        }
        nrow_val <- vapply(data$val, nrow, 0L)
        ans_by <- vctrs::vec_rep_each(data$by, nrow_val)
        ans_val <- do.call(vctrs::vec_rbind, data$val)
        ans <- vctrs::vec_cbind(ans_by, ans_val)
    }
    ans
}


#' Check that no columns of 'data' are used more than once,
#' and that required arguments are supplied
#'
#' @noRd        
check_colnums_lifetab <- function(mx_colnum,
                                  age_colnum,
                                  sex_colnum,
                                  ax_colnum,
                                  by_colnums,
                                  groups_colnums) {
    has_mx <- length(mx_colnum) > 0L
    has_age <- length(age_colnum) > 0L
    has_sex <- length(sex_colnum) > 0L
    has_by <- length(by_colnums) > 0L
    has_groups <- length(groups_colnums) > 0L
    if (!has_mx)
        cli::cli_abort("No value supplied for {.arg mx}.")
    if (!has_age)
        cli::cli_abort("No value supplied for {.arg age}.")
    if (has_by && has_groups)
        cli::cli_abort("Can't supply {.arg by} when {.arg data} is a grouped data
  frame.")
    at_most_one <- list(mx = mx_colnum,
                        age = age_colnum,
                        sex = sex_colnum,
                        ax = ax_colnum)
    check_at_most_one_colnum(at_most_one)
    no_overlap <- list(mx = mx_colnum,
                       age = age_colnum,
                       sex = sex_colnum,
                       ax = ax_colnum)
    check_no_overlap_colnums(no_overlap)
    
}



check_at_most_one_colnum <- function(x) {
    check_valid_colnum_list(x)
    nms <- names(x)
    for (i in seq_along(x)) {
        n_col <- length(x[[i]])
        if (n_col > 1L) {
            nm_arg <- nms[[i]]
            nms_cols <- names(x[[i]])
            cli::cli_abort(c("{.arg {nm_arg}} should be a single variable.",
                             i = "{n_col} variables selected: {.val {nms_cols}}."))
        }
    }
    invisible(TRUE)
}





#' Check inputs, and do life table calculations,
#' for a single population (ie a single set
#' of age groups)
#'
#' @noRd
life_inner_one <- function(data,
                           mx_colnum,
                           age_colnum,
                           sex_colnum,
                           ax_colnum,
                           method,
                           at = at,
                           prefix,
                           is_table) {
    data <- subset_and_order_by_age(data = data,
                                    age_colnum = age_colnum,
                                    at = age)
    age <- data[[age_colnum]]
    check_age_compatible_with_method(age = age, method = method)
    method_uses_sex <- method_uses_sex(method)
    if (method_uses_sex) {
        is_sex_supplied <- length(sex_colnum) > 0L
        if (!is_sex_supplied)
            cli::cli_abort("{.arg method} is {.val {method}} but value for {.arg sex} not supplied.")
        sex <- data[[sex_colnum]]
        sex <- reformat_sex(sex, factor = FALSE)
    }
    else
        sex <- NULL
    age_group_type <- age_group_type(age)
    has_ax <- length(ax_colnum) > 0L
    if (has_ax) {
        ax <- data[[ax_colnum]]
        check_ax(ax = ax, age_group_type = age_group_type)
    }
    else
        ax <- rep(NA, times = length(age))
    mx <- data[[mx_colnum]]
    check_mx(mx)
    mx <- as.matrix(mx)
    if (is_table) {
        lifetab <- mx_to_lifetab(mx = mx,
                                 age_group_type = age_group_type,
                                 sex = sex,
                                 ax = ax,
                                 method = method,
                                 prefix = prefix)
        ans <- rbind(data, lifetab)
    }
    else {
        ans <- mx_to_ex(mx = mx,
                        age_group_type = age_group_type,
                        sex = sex,
                        ax = ax,
                        method = method)
    }
    ans
}        


#' @noRd
subset_and_order_by_age <- function(data,
                                    age_colnum,
                                    at = at) {
    check_number(x = at,
                 x_arg = "at",
                 is_positive = TRUE,
                 is_nonneg = TRUE,
                 is_whole = TRUE)
    age_all <- data[[age_colnum]]
    age_lower_all <- age_lower(age_all)
    age_min <- age_lower_all[[1L]]
    if (age_min > at) {
        youngest <- age_all[age_lower_all == age_min][[1L]]
        cli::cli_abort(c("{.arg at} less than lower limit of youngest age group.",
                         i = "{.arg at} is {.val {at}}",
                         i = "Youngest age group is {.val {youngest}}"))
    }
    if (!(at %in% age_lower_all))
        cli::cli_abort(c("{.arg at} is not the lower limit of an age group.",
                         i = "{.arg at} is {.val {at}}.",
                         i = "Age groups: {.val {unique(age)}}."))
    is_ge_at <- age_lower_all >= at
    ans <- data[is_ge_at, , drop = FALSE]
    age_keep <- ans[[age_colnum]]
    check_age(x = age_keep,
              complete = TRUE,
              unique = TRUE,
              zero = FALSE,
              open = TRUE)
    age_lower_keep <- age_lower_all[is_ge_at]
    ord <- order(age_lower_keep)
    ans <- ans[ord, ]
    ans
}


