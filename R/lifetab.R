
#' HAS_TESTS
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
#' of age group 'x' to the end.
#' - `lx` Number of people alive at
#' the start of age group `x`.
#' - `dx` Number of deaths in age group `x`
#' - `Lx` Expected number of person years lived in
#' age group `x`.
#' - `ex` Life expectancy, calculated at the start
#' of age group `x`.
#' 
#' The input to the calculations, mortality rates `mx`,
#' should be defined as deaths per person-year lived.
#' (Mortality rates are sometimes defined as deaths 
#' per 1000, or per 100,000, person-years lived.)
#'
#' @section Calculation methods:
#'
#' `lifetab()` and `lifeexp()` implement a number
#' of different methods for 
#' calculating life table quantities
#' from mortality rates. Each method makes
#' different assumptions about
#' the way that mortality rates vary within
#' age intervals:
#' 
#' - `"constant"` Mortality rates are constant
#'   within each interval.
#' - `"linear"`. Life table quantity `lx`
#'   is a straight line within each interval:
#'   equivalently, deaths are distributed uniformly
#'   within each interval.
#' - `"CD"`. Used only with age groups "0"
#'   and "1-4". Mortality rates decline
#'   over the  age interval,
#'   with the slope depending on the absolute
#'   level of infant mortality. The formulas were
#'   developed by Coale and Demeny (1983),
#'   and used in Preston et al (2001).
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
#' see the vignette for **poputils**.
#'
#' @section ax:
#'
#' `ax` is the average number of years
#' lived in an age interval by people who
#' die in that interval. Demographers sometimes
#' refer to `ax` as a 'separation factor'. If a non-`NA`
#' value of `ax` is supplied for an age group,
#' then the results for that age group are based
#' on the formula
#'
#' \deqn{m_x = d_x / (n_x l_x + a_x d_x)},
#'
#' (where `n_x` is the width of the age interval)
#' over-riding any methods specified via the `infant`, `child`,
#' `closed` and `open` arguments.
#'
#' @section Using rvecs to represent uncertainty:
#'
#' An [rvec][rvec::rvec()] is a 'random vector',
#' holding multiple draws from a distribution.
#' Using an rvec for the `mx` argument to
#' `lifetab()` or `lifeexp()` is a way of representing
#' uncertainty. This uncertainty is propogated
#' through to the life table values, which will
#' also be rvecs.
#'
#' @param data Data frame with mortality data.
#' @param mx <[`tidyselect`][tidyselect::language]>
#' Mortality rates, expressed as deaths per 
#' person-year lived. Possibly an [rvec][rvec::rvec()].
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
#' @examples
#' library(dplyr)
#'
#' ## life table for females based on 'level 1'
#' ## mortality rates "West" model life table
#' west_mx %>%
#'     filter(sex == "Female",
#'            level == 1) %>%
#'     lifetab()
#'
#' ## change method for infant and children from
#' ## default ("constant") to "CD"
#' west_mx %>%
#'     filter(sex == "Female",
#'            level == 1) %>%
#'     lifetab(sex = sex,
#'             infant = "CD",
#'             child = "CD")
#'
#' ## calculate life expectancies
#' ## for all levels, using the 'by'
#' ## argument to distinguish levels
#' west_mx %>%
#'     lifeexp(sex = sex,
#'             infant = "CD",
#'             child = "CD",
#'             by = level)
#'
#' ## obtain the same result using
#' ## 'group_by'
#' west_mx %>%
#'   group_by(level) %>%
#'   lifeexp(sex = sex,
#'           infant = "CD",
#'           child = "CD")
#' @export
lifetab <- function(data,
                    mx = mx,
                    age = age,
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
               methods = methods,
               radix = radix,
               prefix = prefix,
               is_table = TRUE)
}


#' @export
#' @rdname lifetab
lifeexp <- function(data,
                    mx = mx,
                    age = age,
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
               methods = methods,
               radix = 1,
               prefix = prefix,
               is_table = FALSE)
}


## HAS_TESTS - via 'lifetab' and 'lifeexp', which is more convenient because of quosures
#' Check structure of inputs, reformat, and pass to
#' function doing detailed checkings and calculation
#'
#' @param data Data frame with mortality data.
#' @param mx_quo Quosure identifying 'mx'
#' @param age_quo Quosure identifying 'age'
#' @param sex_quo Quosure identifying 'sex'
#' @param ax_quo Quosure identifying 'ax'
#' @param by_quo Quosure identifying 'by'
#' @param methods Named character vector specifying
#' calculation methods
#' @param radix Initial population for 'lx'
#' @param prefix Prefix added to new columns
#'
#' @returns A tibble
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
    check_life_colnums(mx_colnum = mx_colnum,
                       age_colnum = age_colnum,
                       sex_colnum = sex_colnum,
                       ax_colnum = ax_colnum,
                       by_colnums = by_colnums,
                       groups_colnums = groups_colnums)
    is_sex_supplied <- length(sex_colnum) > 0L
    is_by_supplied <- length(by_colnums) > 0L
    is_groups_supplied <- length(groups_colnums) > 0L
    if (is_by_supplied)
        by_colnums <- unique(c(by_colnums, sex_colnum))
    else
        by_colnums <- unique(c(groups_colnums, sex_colnum))
    has_by <- length(by_colnums) > 0L
    if (has_by) {
        inputs <- vctrs::vec_split(x = data,
                                   by = data[by_colnums])
        for (i in seq_len(nrow(inputs))) {
            return_val <- tryCatch(life_inner_one(data = inputs$val[[i]],
                                                  mx_colnum = mx_colnum,
                                                  age_colnum = age_colnum,
                                                  sex_colnum = sex_colnum,
                                                  ax_colnum = ax_colnum,
                                                  methods = methods,
                                                  radix = radix,
                                                  prefix = prefix,
                                                  is_table = is_table),
                                   error = function(cnd) {
                                       str_key <- make_str_key(inputs$key[i, , drop = FALSE])
                                       msg1 <- "Problem calculating life table functions."
                                       msg2 <- paste("Problem occurred with where", str_key)
                                       cli::cli_abort(c(msg1, i = msg2), parent = cnd)
                                   })
            inputs$val[[i]] <- return_val
        }
        ans <- do.call(vctrs::vec_rbind, inputs$val)
        if (!is_table)
            ans <- vctrs::vec_cbind(inputs$key, ans)
    }
    else {
        ans <- life_inner_one(data = data,
                              mx_colnum = mx_colnum,
                              age_colnum = age_colnum,
                              sex_colnum = sex_colnum,
                              ax_colnum = ax_colnum,
                              methods = methods,
                              radix = radix,
                              prefix = prefix,
                              is_table = is_table)
    }
    ans
}


## HAS_TESTS
#' Check inputs, and do life table calculations,
#' for a single population (ie a single set
#' of age groups)
#'
#' @param data Data frame with mortality data.
#' @param mx_colnum Named integer vector identifying 'mx'
#' @param age_colnum Named integer vector identifying 'age'
#' @param sex_colnum Named integer vector identifying 'sex'
#' @param ax_colnum Named integer vector identifying 'ax'
#' @param by_colnums Named integer vector identifying 'by'
#' @param methods Named character vector specifying
#' calculation methods
#' @param radix Initial population for 'lx'
#' @param prefix Prefix added to new columns
#' @param is_table Whether to return full life table
#' or just life expectancy at birth
#'
#' @returns A tibble with
#' - nrow(data) rows and (ncol(data) + 5) columns, if is_table is TRUE
#' - 1 row and 1 column, if is_table is FALSE
#'
#' @noRd
life_inner_one <- function(data,
                           mx_colnum,
                           age_colnum,
                           sex_colnum,
                           ax_colnum,
                           methods,
                           radix,
                           prefix,
                           is_table) {
    n <- nrow(data)
    age_unord <- data[[age_colnum]]
    check_age(x = age_unord,
              complete = TRUE,
              unique = TRUE,
              zero = TRUE,
              open = TRUE)
    ord <- order(age_lower(age_unord))
    data <- data[ord, ]
    age <- data[[age_colnum]]
    has_sex <- length(sex_colnum) > 0L
    if (has_sex) {
        sex <- data[[sex_colnum]]
        sex <- reformat_sex(sex, factor = FALSE)
    }
    else {
        check_sex_not_needed(methods)
        sex <- rep(NA_character_, times = n)
    }
    has_ax <- length(ax_colnum) > 0L
    if (has_ax) {
        ax <- data[[ax_colnum]]
        check_ax(ax = ax, age = age)
        ax <- as.double(ax)
    }
    else
        ax <- rep(NA_real_, times = n)
    mx <- data[[mx_colnum]]
    check_mx(mx)
    mx <- as.matrix(mx)
    age_group_categ <- age_group_categ(age)
    check_number(x = radix,
                 x_arg = "radix",
                 check_positive = TRUE,
                 check_nonneg = TRUE,
                 check_whole = FALSE)
    if (!is.null(prefix)) {
        check_string(x = prefix,
                     x_arg = "prefix")
    }
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
            ans <- lapply(ans, rvec::rvec_dbl)
        else
            ans <- lapply(ans, as.double)
        ans <- tibble::as_tibble(ans)
        ans <- vctrs::vec_cbind(data, ans)
    }
    else {
        ans <- mx_to_ex(mx = mx,
                        age_group_categ = age_group_categ,
                        sex = sex,
                        ax = ax,
                        methods = methods)
        has_draws <- ncol(ans) > 1L
        if (has_draws)
            ans <- rvec::rvec_dbl(ans)
        else
            ans <- as.numeric(ans)
        ans <- tibble::tibble(ex = ans)
        if (!is.null(prefix))
            names(ans) <- paste(prefix, names(ans), sep = ".")
    }
    ans
}


## HAS_TESTS
#' Calculate life table from values for 'mx'
#'
#' Assume all inputs have been checked and are valid.
#'
#' @param mx Numeric matrix with mortality rates
#' @param age_group_categ Character vector with
#' age group categories ("0", "1-4", "single", "five", "open")
#' @param sex Character vector with "Female", "Male", or NA_character_
#' repeated nrow(mx) times
#' @param ax Numeric vector
#' @param methods Named character vector
#' @param radix Positive number
#' @param prefix String or NULL
#'
#' @returns A named list
#'
#' @noRd
mx_to_lifetab <- function(mx,
                          age_group_categ,
                          sex,
                          ax,
                          methods,
                          radix,
                          prefix) {
    lx <- mx_to_lx(mx = mx,
                   age_group_categ = age_group_categ,
                   sex = sex,
                   ax = ax,
                   methods = methods)
    Lx <- mx_to_Lx(mx = mx,
                   age_group_categ = age_group_categ,
                   sex = sex,
                   ax = ax,
                   methods = methods)
    dx <- lx_to_dx(lx)
    qx <- lx_to_qx(lx)
    ex <- Lx_to_ex(Lx)
    lx <- radix * lx
    dx <- radix * dx
    Lx <- radix * Lx
    ans <- list(qx = qx,
                lx = lx,
                dx = dx,
                Lx = Lx,
                ex = ex)
    if (!is.null(prefix))
        names(ans) <- paste(prefix, names(ans), sep = ".")
    ans
}
               
