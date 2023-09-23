
#' NO_TESTS
#' Calculate life tables or life expectancies
#'
#' Transform estimates of mortality rates
#' into life table quantities. Function
#' `lifetab()` returns the full life table.
#' Function `lifeexp()` life expectancy at birth.
#'
#' @section Definitions of life table quantities:
#'
#' - `qx` Probability of surviving from the start
#' to the end of age group 'x'.
#' - `lx` Number of people surviving until
#' the start of age group `x`.
#' - `dx` Number of deaths in age group `x`
#' - `Lx` Expected number of person years lived in
#' age group `x`
#' - `ex` Life expectancy, calculated at the start
#' of age group `x`.
#'
#' @section Calculation methods:
#'
#' Demographers use a variety of methods for
#' calculating life table quantities
#' from mortality rates. Each method makes
#' different assumptions about
#' the way that mortality rates vary within
#' age intervals:
#' 
#' - `"constant"` Mortality rates are constant
#'   within each interval.
#' - `"linear"`. Life table quantity `lx`
#'   is a straight line within each interval.
#'   Equivalent to assuming that deaths are
#'   distributed uniformly across and interval.
#' - `"CD"`. Used only with age groups "0"
#'   and "1-4" (which are specified by the
#'   `infant` and `child` arguments.)
#'   Mortality rates decline over the  age interval,
#'   with the slope depending on the absolute
#'   level of infant mortality. The formulas were
#'   developed by Coale and Demeny (1983),
#'   and used in Preston et al's (2001) classic
#'   demographic text.
#' - `"HMD"`. Used only with age group "0"
#'   (which is specified by the `infant` argument.)
#'   Mortality rates decline over the age interval,
#'   with the slope depending on the absolute
#'   level of infant mortality. The formulas
#'   were formulas developed by Andreev and Kingkade (2015),
#'   and are used in the Human Mortality Database
#'   [methods protocol](https://www.mortality.org/File/GetDocument/Public/Docs/MethodsProtocolV6.pdf).
#'
#' For a detailed description of the methods,
#' see the vignette GIVE LINK.
#'
#' @section ax:
#'
#' `ax` is the average number of years
#' lived within an age group by people who
#' die in that age group. Demographers sometimes
#' call it the 'separation factor'. If a non-`NA`
#' value is supplied for an age group, then
#' calculations based on the formula
#'
#' \deqn{m_x = d_x / (n_x l_x + a_x d_x)}
#' 
#' (where \eqn{m_x} is the mortality rate,
#'  \eqn{d_x} is deaths, \eqn{n_x} is the width of the
#' age group, and \eqn{l_x} is the probability
#' of surviving to age \eqn{x})
#' are used for that age group, over-riding any procedure
#' set via the `infant`, `child`, `closed` or `open`
#' arguments. See below for examples.
#'
#' @section Using rvecs to represent uncertainty:
#'
#' An [rvec][rvec:rvec()] is a 'random vector',
#' holding multiple draws from a distribution.
#' Using an rvec for the `mx` argument to
#' `lifetab()` or `lifeexp()` is a way of representing
#' uncertainty about underlying mortality conditions.
#' The uncertainty captured by `mx` is propogated
#' through to the life table values, which will
#' also be rvecs.
#'
#' @param data Data frame with mortality data.
#' @param mx <[`tidyselect`][tidyselect::language]>
#' Mortality rates. Possibly an [rvec][rvec::rvec()].
#' @param age <[`tidyselect`][tidyselect::language]>
#' Age group labels. The labels must be
#' interpretable by functions
#' such as [reformat_age()] and [age_group_type()].
#' The first age group must start at age 0, and the
#' last age group must be "open", with no upper
#' limit.
#' @param sex <[`tidyselect`][tidyselect::language]>
#' Biological sex, with labels that can be
#' interprted by [reformat_sex()]. Needed only when
#' `infant` is `"CD"` or `"HMD"`, or `child` is
#' `"CD"`.
#' @param ax <[`tidyselect`][tidyselect::language]>
#' Average age at death within age group.
#' Optional. See Details. 
#' @param by <[`tidyselect`][tidyselect::language]>
#' Separate life tables, or life expectancies, 
#' calculated for each combination the `by` variables.
#' Any `sex` argument automatically included
#' in the `by` variables.
#' @param infant Method used to calculate
#' life table values in age group `"0"`.
#' Ignored if `age` does not include age group `"0"`.
#' Default is `"constant"`.
#' @param child Method used to calculate
#' life table values in age group `"1-4"`.
#' Ignored if `age` does not include age group `"0"`.
#' Default is `"constant"`.
#' @param closed Method used to calculate
#' life table values in closed age intervals
#' other than `"0"` and `"1-4"` (ie intervals
#' such as "10-14" or "12"). Default is `"constant"`.
#' @param open Method used to calculate
#' life table values in the final, open age group
#' (eg `"80+"` or `"110+"`).
#' Currently the only option is `"constant".
#' @param radix Initial population for the
#' `lx` column. Default is `100000`.
#' @param prefix Optional prefix added to new
#' columns in result.
#'
#' @returns A [tibble][tibble::tibble()].
#'
#' @references
#' - Preston SH, Heuveline P, and Guillot M. 2001.
#' *Demography: Measuring and Modeling Population Processes*
#' Oxford: Blackwell.
#' - Coale AJ, Demeny P,  and Vaughn B. 1983.
#' **Regional model life tables and stable populations**
#' New York: Academic Press.
#' - Andreev, E.M. and Kingkade, W.W., 2015.
#' Average age at death in infancy and infant mortality level:
#' Reconsidering the Coale-Demeny formulas at current
#' levels of low mortality. **Demographic Research**,
#' 33, pp.363-390.
#' - Human Mortality Database [Methods Protocol](https://www.mortality.org/File/GetDocument/Public/Docs/MethodsProtocolV6.pdf).
#' - [Tools for Demographic Estimation](https://demographicestimation.iussp.org).
#'
#' @export
lifetab <- function(data,
                    mx,
                    age = NULL,
                    sex = NULL,
                    ax = NULL,
                    by = NULL,
                    infant = c("constant", "linear", "CD", "HMD"),
                    child = c("constant", "linear", "CD"),
                    closed = c("constant", "linear"),
                    open = "constant",
                    radix = 100000,
                    prefix = NULL) {
    mx_quo <- rlang::enquo(mx)
    age_quo <- rlang::enquo(age)
    sex_quo <- rlang::enquo(sex)
    ax_quo <- rlang::enquo(ax)
    by_quo <- rlang::enquo(by)
    infant_supplied <- hasArg(infant)
    child_supplied <- hasArg(child)
    infant <- match.arg(infant)
    child <- match.arg(child)
    closed <- match.arg(closed)
    open <- match.arg(open)
    methods <- c(infant = infant,
                 child = child,
                 closed = closed,
                 open = open)
    life_inner(data = data,
               mx_quo = mx_quo,
               age_quo = age_quo,
               sex_quo = sex_quo,
               ax_quo = ax_quo,
               by_quo = by_quo,
               infant_supplied = infant_supplied,
               child_supplied = child_supplied,
               methods = methods,
               radix = radix,
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
                    infant = c("constant", "linear", "CD", "HMD"),
                    child = c("constant", "linear", "CD"),
                    closed = c("constant", "linear"),
                    open = "constant",
                    prefix = NULL) {
    mx_quo <- rlang::enquo(mx)
    age_quo <- rlang::enquo(age)
    sex_quo <- rlang::enquo(sex)
    ax_quo <- rlang::enquo(ax)
    by_quo <- rlang::enquo(by)
    infant_supplied <- hasArg(infant)
    child_supplied <- hasArg(child)
    methods <- c(infant = infant,
                 child = child,
                 closed = closed,
                 open = copen)
    life_inner(data = data,
               mx_quo = mx_quo,
               age_quo = age_quo,
               sex_quo = sex_quo,
               ax_quo = ax_quo,
               by_quo = by_quo,
               infant_supplied = infant_supplied,
               child_supplied = child_supplied,
               methods = methods,
               radix = NULL,
               prefix = prefix,
               is_table = TRUE)
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
                       methods,
                       radix,
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
                              infant_supplied = infant_supplied,
                              child_supplied = child_supplied,
                              methods = methods,
                              radix = radix,
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
                                                  infant_supplied = infant_supplied,
                                                  child_supplied = child_supplied,
                                                  methods = methods,
                                                  radix = radix,
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
                           infant_supplied,
                           child_supplied,
                           methods,
                           radix,
                           prefix,
                           is_table) {
    age_unord <- data[[age_colnum]]
    check_age(x = age_unord,
              complete = TRUE,
              unique = TRUE,
              zero = TRUE,
              open = TRUE)
    ord <- order(age_lower(age_unord))
    ans <- ans[ord, ]
    age <- data[[age_column]]
    check_child_infant_age_compatible(infant_supplied = infant_supplied,
                                      child_supplied = child_supplied,
                                      age = age,
                                      methods = methods)
    has_sex <- length(sex_colnum) > 0L
    if (has_sex) {
        sex <- data[[sex_colnum]]
        sex <- reformat_sex(sex, factor = FALSE)
    }
    else {
        check_sex_not_needed(methods)
        sex <- NULL
    }
    has_ax <- length(ax_colnum) > 0L
    if (has_ax) {
        ax <- data[[ax_colnum]]
        check_ax(ax = ax, age = age)
    }
    else
        ax <- rep(NA, times = length(age))
    mx <- data[[mx_colnum]]
    check_mx(mx)
    mx <- as.matrix(mx)
    age_group_categ <- age_group_categ(age)
    check_number(x = radix,
                 x_arg = "radix",
                 is_positive = TRUE,
                 is_nonnegative = TRUE,
                 is_whole = FALSE)
    check_string(x = prefix,
                 x_arg = "prefix")
    if (is_table) {
        ans <- mx_to_lifetab(mx = mx,
                             age_group_categ = age_group_categ,
                             sex = sex,
                             ax = ax,
                             methods = methods,
                             radix = radix,
                             prefix = prefix)
        has_draws <- ncol(ans[[1L]]) > 1L
        if (has_draws)
            ans <- lapply(ans, rvec_dbl)
    }
    else {
        ans <- mx_to_ex(mx = mx,
                        age_group_categ = age_group_categ,
                        sex = sex,
                        ax = ax,
                        methods = methods)
        has_draws <- ncol(ans) > 1L
        if (has_draws)
            ans <- rvec_dbl(ans)
    }
    ans <- vctrs::vec_rbind(data, ans)
    ans
}


