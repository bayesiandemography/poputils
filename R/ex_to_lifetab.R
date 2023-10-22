
#' Derive Life Tables that Match Life Expectancies,
#' using a Brass Logit Model
#'
#' Turn life expectances at birth into full life tables,
#' using the Brass logit model. The method is simple
#' and is designed for simulations or for
#' settings with little or no data on age-specific
#' mortality rates. In settings where data
#' on age-specific mortality is available,
#' other methods might be more appropriate.
#'
#' @section Method:
#'
#' The method implemented by `ex_to_lifetab_brass()` is
#' based on the observation that, if populations A and B
#' are demographically similar, then, in many cases,
#'
#' \deqn{\text{logit}(l_x^{\text{B}}) \approx \alpha + \beta \text{logit}(l_x^{\text{A}})}
#'
#' where \eqn{l_x} is the "survivorship probability" quantity
#' from a life table. When populations are
#' similar, \eqn{beta} is often close to 1.
#'
#' Given (i) target life expectency,
#' (ii) a set of \eqn{l_x^{\text{A}}}),
#' (referred to as a "standard"), and
#' (iii) a value for \eqn{\beta},
#' `ex_to_lifetab_brass()` finds
#' a value for \eqn{\alpha} that yields a set of
#' \eqn{l_x^{\text{B}}}) with the required life expectancy.
#'
#' @section `data` argument:
#'
#' The argument `data` is a data frame specifying
#' life expectancies for each population being modelled,
#' plus, possibly, inputs to the calculations,
#' and background characteristics.
#'
#' - A variable called `"ex"`, with life expectancy at birth
#'   must be included in `data`.
#' - A variable called `"beta"` with values
#'   for `beta` can be included in `data`.
#'   This variable can be an [rvec][rvec::rvec()].
#'   If no `"beta"` variable is included in `data`,
#'   then `ex_to_lifetab_brass()` assumes that
#'   \eqn{beta \equiv 1}.
#' - A variable called `"sex"`. If the `infant`
#'   argument to `ex_to_lifetab_brass()` is is `"CD"` or `"AK"`,
#'   or if the `child` argument is `"CD"`,
#'   `data` must include a `"sex" variable, and the
#'   labels for this variable must be interpretable
#'   by function [format_sex()]. Otherwise,
#'   the `"sex"` variable  is optional,
#'   and there is no restriction on labels.
#' - Other variables used to distinguish between
#'   life expectancies, such as time or region,
#'   can also be included.
#'
#' @param data A data frame containing a variable called
#' `"ex"`, and possibly others. See Details.
#' @param lx_standard A vector of `lx` values.
#' Internally these are standardized so that the first
#' value equals 1. Values must be non-increasing.
#' Cannot be an rvec.
#' @param age A vector of age labels for
#' `lx_standard`. `ex_to_lifetab_brass() parses these
#' labels using [reformat_age()].
#' @param ax Average age at death within each age group.
#' See [lifetab()] for a definition. A numeric vector,
#' the same length as `lx_standard`. Optional.
#' @param infant,child,closed,open Methods used to
#' calculate life expectancy. See [lifetab()] for details.
#' @param radix Initial population for the
#' `lx` column in the derived life table(s).
#' Default is `100000`.
#' @param suffix Optional suffix added to life table columns.
#'
#' @returns
#' A data frame containing a full life table for each
#' row in `data`.
#'
#' @seealso
#' - [logit()], [invlogit()] Logit function
#' - [lifeexp()] Calculate life expectancy from detailed inputs
#'
#' @references
#' Brass W, Coale AJ. 1968. “Methods of analysis and estimation,”
#' in Brass, W,  Coale AJ, Demeny P, Heisel DF, et al. (eds).
#' The Demography of Tropical Africa. Princeton NJ:
#' Princeton University Press, pp. 88–139.
#' 
#' Moultrie TA, Timæus IM. 2013. Introduction to Model Life Tables.
#' In Moultrie T, Dorrington R, Hill A, Hill K, Timæus I, Zaba B.
#' (eds). Tools for Demographic Estimation.
#' Paris: International Union for the Scientific Study of Population.
#' [online version](https://demographicestimation.iussp.org/content/using-models-derive-life-tables-incomplete-data).
#'
#' @export
ex_to_lifetab_brass <- function(data,
                                lx_standard,
                                age,
                                ax = NULL,
                                infant = c("constant", "linear", "CD", "AK"),
                                child = c("constant", "linear", "CD"),
                                closed = c("constant", "linear"),
                                open = "constant",
                                radix = 100000,
                                suffix = NULL) {
    check_data_ex_to_lifetab_brass(data)
    check_lx_standard(lx_standard)
    infant <- match.arg(infant)
    child <- match.arg(child)
    closed <- match.arg(closed)
    open <- match.arg(open)
    methods <- c(infant = infant,
                 child = child,
                 closed = closed,
                 open = open)
    check_number(x = radix,
                 x_arg = "radix",
                 check_na = TRUE,
                 check_positive = TRUE,
                 check_nonneg = TRUE,
                 check_whole = FALSE)
    if (!is.null(suffix))
        check_string(suffix, x_arg = "suffix")
    check_age(x = age,
              complete = TRUE,
              unique = TRUE,
              zero = TRUE,
              open = TRUE)
    check_equal_length(x = age,
                       y = lx_standard,
                       nm_x = "age",
                       nm_y = "lx_standard")
    n_age <- length(age)
    has_ax <- !is.null(ax)
    if (has_ax) {
        check_ax(ax = ax, age = age)
        ax <- as.double(ax)
    }
    else
        ax <- rep(NA_real_, times = n_age)
    ord <- order(age_lower(age))
    age <- age[ord]
    lx_standard <- lx_standard[ord]
    ax <- ax[ord]
    l <- make_ex_beta_n_draw(data)
    ex <- l[["ex"]]
    beta <- l[["beta"]]
    n_draw <- l[["n_draw"]]
    n_val <- length(ex)
    sex <- make_sex_ex_to_lifetab(data = data,
                                  methods = methods)
    ans_val <- ex_to_lifetab_brass_inner(ex = ex,
                                         beta = beta,
                                         n_draw = n_draw,
                                         lx_standard = lx_standard,
                                         age = age,
                                         sex = sex,
                                         ax = ax,
                                         methods = methods,
                                         radix = radix,
                                         suffix = suffix)
    ans_by <- data[-match("ex", names(data))]
    ans_by <- vctrs::vec_rep_each(ans_by, times = n_age)
    age <- rep(age, times = nrow(data))
    ans <- vctrs::vec_cbind(ans_by, age = age, ans_val)
    ans
}



## Helper functions -----------------------------------------------------------


#' Calculate life tables, given processed inputs
#'
#' @param ex Numeric vector of life expectancies at birth
#' @param beta Beta parameter in Brass logit model
#' @param n_draw Number of draws from original rvecs for 'ex'
#' and 'beta'; NULL if neither was an rvec
#' @param lx_standard Standard lx for Brass logit model
#' @param age Labels for age groups, same length as 'lx_standard'
#' @param sex Labels for sex, length equal to ex / n_draw,
#' or ex (if n_draw is NULL)
#' @param ax Average years lived in interval by people
#' who die in interval. Numeric vector.
#' @param methods Named character vectors with methods
#' for calculating life table
#' @param radix Radix for life table to be created
#' @param suffix Suffix added to columns of life table,
#' or NULL.
#'
#' @returns A data frame
#'
#' @noRd
ex_to_lifetab_brass_inner <- function(ex,
                                      beta,
                                      n_draw,
                                      lx_standard,
                                      age,
                                      sex,
                                      ax,
                                      methods,
                                      radix,
                                      suffix) {
    lx_standard <- lx_standard / lx_standard[[1L]]
    logit_lx_standard <- logit(lx_standard)
    age_group_categ <- age_group_categ(age)
    sex_rep <- if (is.null(n_draw)) sex else rep(sex, each = n_draw)
    ## closure capturing 'logit_lx_standard',
    ## 'age_group_categ', 'ax', and 'methods'
    alpha_to_ex <- function(alpha, beta_i, sex_i) {
        logit_lx <- alpha + beta_i * logit_lx_standard
        lx <- invlogit_inner(logit_lx)
        qx <- lx_to_qx(lx)
        qx_to_ex(qx = qx,
                 age_group_categ = age_group_categ,
                 sex = sex_i,
                 ax = ax,
                 methods = methods)
    }
    n_val <- length(ex)
    lx_ans <- vector(mode = "list", length = n_val)
    for (i in seq_len(n_val)) {
        ex_i <- ex[[i]]
        beta_i <- beta[[i]]
        sex_i <- sex_rep[[i]]
        ## closure capturing 'beta_i', 'sex_i'
        abs_error <- function(alpha) {
            ex_derived <- alpha_to_ex(alpha = alpha,
                                      beta = beta_i,
                                      sex = sex_i)
            abs(ex_derived - ex_i)
        }
        val_optim <- stats::optimize(f = abs_error, interval = c(0, 10))
        alpha_min_i <- val_optim$minimum
        logit_lx_i <- alpha_min_i + beta_i * logit_lx_standard
        lx_i <- invlogit(logit_lx_i)
        lx_ans[[i]] <- lx_i
    }
    has_draws <- !is.null(n_draw)
    if (has_draws) {
        n_by <- n_val %/% n_draw
        n_age <- length(age)
        lx_ans <- array(unlist(lx_ans), dim = c(n_age, n_by, n_draw))
        lx_ans <- apply(lx_ans, 2L, matrix, nrow = n_age, ncol = n_draw, simplify = FALSE)
    }
    ans <- vector(mode = "list", length = length(lx_ans))
    for (i in seq_along(ans)) {
        lx <- lx_ans[[i]]
        qx <- lx_to_qx(lx)
        lifetab <- qx_to_lifetab(qx = qx,
                                 age_group_categ = age_group_categ,
                                 sex = sex[[i]],
                                 ax = ax,
                                 methods = methods,
                                 radix = radix,
                                 suffix = suffix)
        if (has_draws)
            lifetab <- lapply(lifetab, rvec::rvec_dbl)
        else
            lifetab <- lapply(lifetab, as.double)
        lifetab <- tibble::as_tibble(lifetab)
        ans[[i]] <- lifetab
    }
    ans <- vctrs::vec_rbind(!!!ans)
    ans
}

## HAS_TESTS
#' Use 'data' to prepare 'ex', 'beta', and 'n_draw' arguments
#'
#' If on of 'ex' and 'beta' is an rvec, and the other is not
#' then the non-rvec vector is replicated
#'
#' If neither 'ex' nor 'beta' is an rvec, than 'n_draw' is NULL.
#'
#' @param data A data frame with an 'ex' variable
#' and possibly a 'beta' variable, both of which
#' can be rvecs
#'
#' @returns A named list with 'ex', 'beta', and 'n_draw'.
#'
#' @noRd
make_ex_beta_n_draw <- function(data) {
    nms <- names(data)
    nrow <- nrow(data)
    ex <- data[["ex"]]
    has_beta <- "beta" %in% nms
    if (has_beta)
        beta <- data[["beta"]]
    else
        beta <- rep(1, times = nrow)
    is_rv_ex <- rvec::is_rvec(ex)
    is_rv_beta <- rvec::is_rvec(beta)
    if (is_rv_ex && is_rv_beta) {
        n_draw_ex <- rvec::n_draw(ex)
        n_draw_beta <- rvec::n_draw(beta)
        if (n_draw_ex != n_draw_beta) {
            if (n_draw_ex == 1L) {
                ex <- as.numeric(ex)
                ex <- rep(ex, times = n_draw_beta)
                beta <- as.numeric(beta)
                n_draw <- n_draw_beta
            }
            else if (n_draw_beta == 1L) {
                ex <- as.numeric(ex)
                beta <- as.numeric(beta)
                beta <- rep(beta, times = n_draw_ex)
                n_draw <- n_draw_ex
            }
            else {
                cli::cli_abort(c("{.arg ex} and {.arg beta} have different numbers of draws.",
                                 i = "{.arg ex} has {n_draw_ex} draws.",
                                 i = "{.arg beta} has {n_draw_beta} draws."))
            }
        }
        else {
            ex <- as.numeric(ex)
            beta <- as.numeric(beta)
            n_draw <- n_draw_ex
        }
    }
    else if (!is_rv_ex && is_rv_beta) {
        n_draw <- rvec::n_draw(beta)
        ex <- rep(as.numeric(ex), times = n_draw)
        beta <- as.numeric(beta)
    }
    else if (is_rv_ex && !is_rv_beta) {
        n_draw <- rvec::n_draw(ex)
        ex <- as.numeric(ex)
        beta <- rep(as.numeric(beta), times = n_draw)
    }
    else {
        ex <- as.numeric(ex)
        beta <- as.numeric(beta)
        n_draw <- NULL
    }
    list(ex = ex,
         beta = beta,
         n_draw = n_draw)
}


## HAS_TESTS
#' Make sex variable for ex_to_lifetab function
#'
#' @param data A data frame, possibly containing
#' a variable called "sex"
#' @param A named character vector with
#' methods for calculating
#' life table quantities
#'
#' @returns A vector with nrow(data) elements,
#' or NULL.
#'
#' @noRd
make_sex_ex_to_lifetab <- function(data, methods) {
    has_sex <- "sex" %in% names(data)
    methods_need_sex <- get_methods_need_sex()
    is_needs_sex <- methods %in% methods_need_sex
    i_needs_sex <- match(TRUE, is_needs_sex, nomatch = 0L)
    if (i_needs_sex > 0L) {
        if (has_sex) {
            ans <- data[["sex"]]
            ans <- reformat_sex(ans, factor = FALSE)
        }
        else {
            nm_arg <- names(methods)[[i_needs_sex]]
            arg <- methods[[i_needs_sex]]
            val <- "sex"
            cli::cli_abort(c("{.arg data} does not have a variable called {.val {val}}.",
                             i = paste("If {.arg {nm_arg}} is {.val {arg}}, then {.arg data} must",
                                       "have a variable called {.val {val}}.")))
        }
    }
    else
        ans <- NULL
    ans
}

