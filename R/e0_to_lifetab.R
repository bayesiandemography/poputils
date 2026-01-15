
#' Derive Life Tables that Match Life Expectancies,
#' using a Brass Logit Model
#'
#' Turn life expectancies at birth into full life tables,
#' using the Brass logit model. The method is simple
#' and is designed for simulations or for
#' settings with little or no data on age-specific
#' mortality rates. In settings where data
#' on age-specific mortality is available,
#' other methods might be more appropriate.
#'
#' @section Method:
#'
#' The method implemented by `e0_to_lifetab_logit()` is
#' based on the observation that, if populations A and B
#' are demographically similar, then, in many cases,
#'
#' \deqn{\text{logit}(q_x^{\text{B}}) \approx \alpha + \beta \text{logit}(q_x^{\text{A}})}
#'
#' where \eqn{q_x} is the life table probability of dying
#' between birth and age \eqn{x}. By definition, \eqn{q_x = 1 - l_x},
#' where \eqn{l_x} is the standard life table function,
#' with radix (\eqn{l_0}) equal to 1.
#'
#' Given (i) target life expectancy,
#' (ii) a set of \eqn{l_x^{\text{A}}}),
#' (referred to as a "standard"), and
#' (iii) a value for \eqn{\beta},
#' `e0_to_lifetab_logit()` finds
#' a value for \eqn{\alpha} that yields a set of
#' \eqn{q_x^{\text{B}}}) with the required life expectancy.
#' If populations A and B
#' are similar, then \eqn{beta} is likely to close to 1.
#'
#' @section The `target` argument:
#'
#' `target` is a data frame specifying
#' life expectancies for each population being modelled,
#' and, optionally, inputs to the calculations, and
#' 'by' variables.
#'
#' `target` contains the following variables:
#'
#' - A variable called `"e0"` giving life expectancy at birth.
#' - Optionally, a variable called `"beta"` with values
#'   for \eqn{\beta}.
#'   Can be an ordinary numeric vector
#'   or an [rvec][rvec::rvec()].
#'   If `target` does not include a `"beta"` variable,
#'   then `e0_to_lifetab_logit()` sets \eqn{\beta} to 1.
#' - A variable called `"sex"`. The `"sex"` variable
#'   must be supplied if the `infant`
#'   argument to `e0_to_lifetab_logit()` is `"CD"` or `"AK"`,
#'   or if the `child` argument is `"CD"`.
#' - Optionally, 'by' variables. Typical examples
#'   are time, region, and model variant.
#'
#' @section The `standard` argument:
#'
#' `standard` is a data frame specifying
#' the \eqn{l_x} to be used with each life expectancy
#' in `target`, and, optionally, values for the average age
#' person-years lived by people who die in each group,
#' \eqn{_na_x}. Values in `standard` are age-specific.
#'
#' `standard` contains the following variables:
#'
#' - A variable called `"age"`, with labels that
#'   can be parsed by [reformat_age()].
#' - A variable called `"lx"`.  Cannot be an rvec.
#' - Additional variables used to match rows in `standard`
#'   to rows in `target`.
#'
#' @param target A data frame containing a variable called
#' `"e0"`, and possibly other varibles. See Details.
#' @param standard A data frame containing variables
#' called `age` and `lx`, and possibly others.
#' See Details.
#' @param infant,child,closed,open Methods used to
#' calculate life expectancy. See [lifetab()] for details.
#' @param radix Initial population for the
#' `lx` column in the derived life table(s).
#' Default is `100000`.
#' @param suffix Optional suffix added to life table columns.
#'
#' @returns A [tibble][tibble::tibble()].

#' @seealso
#' - [tfr_to_asfr_scale] Fertility equivalent of
#'   `e0_to_lifetab_logit()`
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
#' @examples
#' ## create new life tables based on level-1
#' ## 'West' model life tables, but with lower
#' ## life expectancy
#'
#' library(dplyr, warn.conflicts = FALSE)
#' 
#' target <- data.frame(sex = c("Female", "Male"), 
#'                      e0 = c(17.5, 15.6))
#' 
#' standard <- west_lifetab |>
#'     filter(level == 1) |>
#'     select(sex, age, lx)
#'     
#' e0_to_lifetab_logit(target = target,
#'                     standard = standard,
#'                     infant = "CD",
#'                     child = "CD")
#'
#' ## target is an rvec
#' library(rvec, warn.conflicts = FALSE)
#' target_rvec <- data.frame(sex = c("Female", "Male"), 
#'                           e0 = rnorm_rvec(n = 2,
#'                                           mean = c(17.5, 15.6),
#'                                           n_draw = 1000))
#' e0_to_lifetab_logit(target = target_rvec,
#'                     standard = standard)
#' @export
e0_to_lifetab_logit <- function(target,
                                standard,
                                infant = c("constant", "linear", "CD", "AK"),
                                child = c("constant", "linear", "CD"),
                                closed = c("constant", "linear"),
                                open = "constant",
                                radix = 100000,
                                suffix = NULL) {
  ## check inputs
  check_target_e0_to_lifetab(target)
  check_standard_e0_to_lifetab(standard)
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
  combined <- combine_target_standard_e0_to_lifetab_logit(target = target,
                                      standard = standard)
  key <- combined$key
  n_val <- nrow(combined)
  ans_val <- vector(mode = "list", length = n_val)
  for (i in seq_len(n_val)) {
    val_i <- combined$val[[i]]
    sex_i <- key$sex[[i]] ## possibly NULL
    by_i <- key[i, , drop = FALSE]
    str_key <- make_str_key(by_i)
    return_val <- tryCatch(e0_to_lifetab_logit_one(val = val_i,
                                                   sex = sex_i,
                                                   methods = methods,
                                                   radix = radix,
                                                   suffix = suffix),
                           error = function(e) e)
    if (inherits(return_val, "error")) {
      cli::cli_abort(c(paste0("Problem with calculations for ", str_key, "."),
                       i = return_val$message,
                       return_val$body))
    }
    ans_val[[i]] <- return_val
  }
  sizes_vals <- vapply(ans_val, nrow, 0L)
  ans_val <- vctrs::vec_rbind(!!!ans_val)
  ans_by <- vctrs::vec_rep_each(key, times = sizes_vals)
  ans <- vctrs::vec_cbind(ans_by, ans_val)
  ans <- tibble::tibble(ans)
  ans
}


## Helper functions -----------------------------------------------------------

## HAS_TEST
#' Merge 'target' and 'standard' data frames
#'
#' Includes
#' - checking for gaps in 'standard',
#' - adding 'beta' and 'ax' columns if necessary
#' - calling 'vec_split' at the end
#'
#' @param target,standard Data frames
#'
#' @returns A data frame with columns 'key' and 'value'
#'
#' @noRd
combine_target_standard_e0_to_lifetab_logit <- function(target, standard) {
  nms_by <- intersect(names(target), names(standard))
  ans <- merge(x = target,
               y = standard,
               by = nms_by,
               all.x = TRUE)
  age <- ans[["age"]]
  i_na <- match(TRUE, is.na(age), nomatch = 0L)
  if (i_na > 0L) {
    str_key <- make_str_key(ans[i_na, nms_by, drop = FALSE])
    cli::cli_abort(paste("{.arg standard} does not have values for case where", str_key))
  }
  nms_all <- names(ans)
  nms_val <- c("e0", "beta", "age", "lx", "ax")
  nms_key <- setdiff(nms_all, nms_val)
  has_beta <- "beta" %in% nms_all
  if (has_beta)
    nms_key <- c(nms_key, "beta")
  else
    ans$beta <- 1
  has_ax <- "ax" %in% nms_all
  if (!has_ax)
    ans$ax <- NA_real_
  vctrs::vec_split(ans[nms_val], ans[nms_key])
}


#' Calculate life tables, given processed inputs
#'
#' @param val Data frame with following columns:
#' - e0 Numeric vector of life expectancies at birth
#' - beta Beta parameter in Brass logit model
#' - lx Standard lx for Brass logit model
#' - age Labels for age groups
#' - ax Average years lived in interval by people
#'      who die in interval. Numeric vector.
#' @param sex String or NULL.
#' @param methods Named character vectors with methods
#' for calculating life table
#' @param radix Radix for life table to be created
#' @param suffix Suffix added to columns of life table,
#' or NULL.
#'
#' @returns A data frame
#'
#' @noRd
e0_to_lifetab_logit_one <- function(val,
                                    sex,
                                    methods,
                                    radix,
                                    suffix) {
  ord <- order(age_lower(val$age))
  val <- val[ord, , drop = FALSE]
  l <- make_e0_beta_n_draw(e0 = val$e0[[1L]],
                           beta = val$beta[[1L]])
  e0 <- l$e0
  beta <- l$beta
  n_draw <- l$n_draw
  sex <- make_sex_e0_to_lifetab(sex = sex,
                                methods = methods,
                                nm_data = "standard")
  sex_rep <- if (is.null(n_draw)) sex else rep(sex, each = n_draw)
  lx_standard <- val$lx
  lx_standard <- lx_standard / lx_standard[[1L]]
  xq0_standard <- 1 - lx_standard ## not ordinary nqx
  logit_xq0_standard <- logit(xq0_standard)
  age <- val$age
  age_group_categ <- age_group_categ(age)
  ax <- val$ax
  ## closure capturing 'logit_xq0_standard',
  ## 'age_group_categ', 'ax', and 'methods'
  alpha_to_e0 <- function(alpha, beta_i, sex_i) {
    logit_xq0 <- alpha + beta_i * logit_xq0_standard
    xq0 <- invlogit_inner(logit_xq0)
    lx <- 1 - xq0
    qx <- lx_to_qx(lx) ## ordinary nqx
    qx_to_ex(qx = qx,
             age_group_categ = age_group_categ,
             sex = sex_i,
             ax = ax,
             methods = methods)
  }
  n_val <- length(e0)
  lx_ans <- vector(mode = "list", length = n_val)
  for (i in seq_len(n_val)) {
    e0_i <- e0[[i]]
    beta_i <- beta[[i]]
    sex_i <- sex_rep[[i]]
    ## closure capturing 'beta_i', 'sex_i'
    abs_error <- function(alpha) {
      e0_derived <- alpha_to_e0(alpha = alpha,
                                beta = beta_i,
                                sex = sex_i)
      abs(e0_derived - e0_i)
    }
    val_optim <- stats::optimize(f = abs_error, interval = c(-10, 10))
    alpha_min_i <- val_optim$minimum
    logit_xq0_i <- alpha_min_i + beta_i * logit_xq0_standard
    xq0_i <- invlogit(logit_xq0_i)
    lx_i <- 1 - xq0_i
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
                             sex = sex_rep[[i]],
                             ax = ax,
                             methods = methods,
                             radix = radix,
                             suffix = suffix)
    if (has_draws)
      lifetab <- lapply(lifetab, rvec::rvec_dbl)
    else
      lifetab <- lapply(lifetab, as.double)
    lifetab <- tibble::as_tibble(lifetab)
    lifetab <- tibble::tibble(age = age, lifetab)
    ans[[i]] <- lifetab
  }
  ans <- vctrs::vec_rbind(!!!ans)
  ans
}


## HAS_TESTS
#' Prepare 'e0', 'beta', and 'n_draw' arguments
#'
#' If one of 'e0' and 'beta' is an rvec, and the other is not
#' then the non-rvec vector is replicated
#'
#' If neither 'e0' nor 'beta' is an rvec, than 'n_draw' is NULL.
#'
#' @param e0 Vector or rvec
#' @param beta Vector or rvec
#'
#' @returns A named list with 'e0', 'beta', and 'n_draw'.
#'
#' @noRd
make_e0_beta_n_draw <- function(e0, beta) {
    is_rv_e0 <- rvec::is_rvec(e0)
    is_rv_beta <- rvec::is_rvec(beta)
    if (is_rv_e0 && is_rv_beta) {
        n_draw_e0 <- rvec::n_draw(e0)
        n_draw_beta <- rvec::n_draw(beta)
        if (n_draw_e0 != n_draw_beta) {
            if (n_draw_e0 == 1L) {
                e0 <- as.numeric(e0)
                e0 <- rep(e0, times = n_draw_beta)
                beta <- as.numeric(beta)
                n_draw <- n_draw_beta
            }
            else if (n_draw_beta == 1L) {
                e0 <- as.numeric(e0)
                beta <- as.numeric(beta)
                beta <- rep(beta, times = n_draw_e0)
                n_draw <- n_draw_e0
            }
            else {
                cli::cli_abort(c("{.arg e0} and {.arg beta} have different numbers of draws.",
                                 i = "{.arg e0} has {n_draw_e0} draws.",
                                 i = "{.arg beta} has {n_draw_beta} draws."))
            }
        }
        else {
            e0 <- as.numeric(e0)
            beta <- as.numeric(beta)
            n_draw <- n_draw_e0
        }
    }
    else if (!is_rv_e0 && is_rv_beta) {
        n_draw <- rvec::n_draw(beta)
        e0 <- rep(as.numeric(e0), times = n_draw)
        beta <- as.numeric(beta)
    }
    else if (is_rv_e0 && !is_rv_beta) {
        n_draw <- rvec::n_draw(e0)
        e0 <- as.numeric(e0)
        beta <- rep(as.numeric(beta), times = n_draw)
    }
    else {
        e0 <- as.numeric(e0)
        beta <- as.numeric(beta)
        n_draw <- NULL
    }
    list(e0 = e0,
         beta = beta,
         n_draw = n_draw)
}


## HAS_TESTS
#' Make sex variable for e0_to_lifetab function
#'
#' @param sex A character vector or NULL
#' @param A named character vector with
#' methods for calculating
#' life table quantities
#' @param nm_data Name of the input data to be
#' used in error messsages
#'
#' @returns A vector or NULL.
#'
#' @noRd
make_sex_e0_to_lifetab <- function(sex, methods, nm_data) {
    has_sex <- !is.null(sex)
    methods_need_sex <- get_methods_need_sex()
    is_needs_sex <- methods %in% methods_need_sex
    i_needs_sex <- match(TRUE, is_needs_sex, nomatch = 0L)
    if (i_needs_sex > 0L) {
        if (has_sex) {
            ans <- reformat_sex(sex, factor = FALSE)
        }
        else {
            nm_arg <- names(methods)[[i_needs_sex]]
            arg <- methods[[i_needs_sex]]
            val <- "sex"
            cli::cli_abort(c("{.arg {nm_data}} does not have a variable called {.val {val}}.",
                             i = paste("If {.arg {nm_arg}} is {.val {arg}}, then ",
                                       "{.arg {nm_data}} must have a variable called {.val {val}}.")))
        }
    }
    else
        ans <- NA_character_
    ans
}

