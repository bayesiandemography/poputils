## HAS_TESTS
#' Calculate Multiple-Decrement Life Tables
#'
#' Calculate a life table in which people leave
#' the population through several decrements,
#' such as different causes of death.
#' The inputs are decrement-specific mortality rates (`mx`)
#' or decrement-specific probabilities (`qx`), though not both.
#'
#' @section Population and decrement dimensions:
#'
#' A multiple-decrement life table has two kinds
#' of dimension.
#'
#' - **Population dimensions**, such as sex or region,
#'   divide the population into groups. Each group has its
#'   own life table. Population dimensions are
#'   specified using `by` or [grouping][dplyr::group_by()],
#'   plus `sex`.
#' - **Decrement dimensions**, such as cause of death or
#'   place of death, classify the ways that people leave
#'   the population. Decrement dimensions are specified
#'   using `decrement`. There can be more than one. Each
#'   combination of decrement dimensions is a 'cell'.
#'
#' The population at risk is the same for every decrement cell.
#' Exposure is therefore divided up across population dimensions,
#' but repeated across decrement dimensions. In a data frame of
#' cause-specific death rates, for instance, the population for
#' each age group appears once for each cause.
#'
#' Columns that are not inputs, `age`, `sex`, `ax`, `by` variables,
#' or `decrement` variables, are passed through to the result.
#'
#' @section Life table quantities:
#'
#' The result contains the following columns for each
#' decrement cell `k`.
#'
#' - `qx` The probability that a person alive at the start of age
#'   group `x` leaves the population through decrement `k` during the
#'   age group. This is the 'crude' probability: its denominator
#'   is the number of people alive at the start of the age group,
#'   from all decrements combined.
#' - `lx` The number of people alive at the start of age group `x`
#'   who will eventually leave through decrement `k`.
#' - `dx` The number of people leaving through decrement `k` in
#'   age group `x`.
#' - `Lx` The number of person-years lived in age group `x` by people
#'   who will eventually leave through decrement `k`.
#'
#' All four columns are additive: summing them across decrement
#' cells gives the corresponding values for the ordinary,
#' all-decrement life table, as calculated by [lifetab()].
#' Life expectancy is not included, since it is not a property
#' of a single decrement cell.
#'
#' @section Calculation methods:
#'
#' `lifetab_decrement()` assumes that, within each age group,
#' the decrement-specific rates have the same shape over
#' the age group, so that the rates for different decrements are
#' proportional to each other ('proportional hazards').
#' The all-decrement life table is calculated from the
#' sum of the decrement-specific rates or probabilities,
#' using the methods specified by `infant`, `child`, `closed`,
#' and `open`, and the average time lived in each age group by
#' people who leave in that age group (`ax`) is the same for
#' every decrement. Exits in each age group are then
#' divided among decrements in proportion to the
#' decrement-specific rates or probabilities.
#'
#' See [lifetab()] for descriptions of the methods.
#'
#' @section Inputs:
#'
#' Mortality rates `mx` are rates for leaving through
#' decrement `k`, using all person-years lived
#' as the denominator. They are summed across decrements to
#' give the all-decrement rate.
#'
#' Probabilities `qx` are crude probabilities, as defined above.
#' They are summed across decrements to give the all-decrement
#' probability, and the sum must not exceed 1. In the final, open
#' age group, everyone eventually leaves, so the values of
#' `qx` are the shares of exits from each decrement,
#' and must sum to 1. As with [lifetab()], when the inputs
#' are probabilities, conditions in the final age group
#' are inferred from the second-to-final age group.
#'
#' If the rate for the final age group, summed across decrements,
#' is 0, then no one ever leaves the final age group, and the
#' decrement through which people eventually leave is undefined.
#' `lifetab_decrement()` gives a warning, and sets
#' `lx` and `Lx` for all age groups, and `qx` and `dx` for the final
#' age group, to `NaN`.
#'
#' Within each population group, every decrement cell that is
#' present must have the same age groups. A cell can be absent
#' from a population group altogether, which is equivalent to that
#' decrement having rates of zero.
#'
#' @section Combining decrements:
#'
#' To calculate a life table with broader decrement categories,
#' such as broader groups of causes, or with fewer decrement
#' dimensions, combine the rates before calculating the
#' life table, using [collapse_levels()] or [collapse_dims()].
#' Rates are summed across decrements, so the decrement dimensions
#' are supplied to `wt_shared`. For instance,
#'
#' ```
#' data |>
#'   collapse_dims(rate = mx,
#'                 wt = popn,
#'                 wt_shared = c(cause, place),
#'                 by = !place) |>
#'   lifetab_decrement(mx = mx, decrement = cause)
#' ```
#'
#' Since the life table columns are additive, the same
#' result can be obtained by summing the columns of a more
#' detailed life table.
#'
#' @section Other quantities:
#'
#' Several other quantities can be calculated from the
#' output of `lifetab_decrement()`.
#' Assume the output is a data frame `tab`, with population
#' dimension `sex` and decrement dimension `cause`.
#'
#' - **The all-decrement life table**:
#'   `collapse_dims(tab, count = c(qx, lx, dx, Lx), by = !cause)`.
#' - **Probability that a person alive at age `x` eventually
#'   leaves through decrement `k`**:
#'   `mutate(tab, prob_eventual = lx / sum(lx), .by = c(sex, age))`.
#' - **Expected remaining time for people alive at age `x` who
#'   eventually leave through decrement `k`**:
#'   `mutate(tab, ex_eventual = rev(cumsum(rev(Lx))) / lx, .by = c(sex, cause))`.
#'   This relies on rows being sorted by age within each cell,
#'   which is how `lifetab_decrement()` returns them.
#'
#' @section Using rvecs to represent uncertainty:
#'
#' An [rvec][rvec::rvec()] is a 'random vector',
#' holding multiple draws from a distribution.
#' Using an rvec for `mx` or `qx` is a way of
#' representing uncertainty. This uncertainty is propagated
#' through to the life table values, which will
#' also be rvecs.
#'
#' @inheritParams lifetab
#' @param mx <[`tidyselect`][tidyselect::language]>
#' Decrement-specific rates, expressed as exits per
#' person-year lived. Possibly an [rvec][rvec::rvec()].
#' @param qx <[`tidyselect`][tidyselect::language]>
#' Crude decrement-specific probabilities.
#' An alternative to `mx`. Possibly an [rvec][rvec::rvec()].
#' @param decrement <[`tidyselect`][tidyselect::language]>
#' Decrement dimensions, such as cause of death.
#' One or more variables.
#' @param ax <[`tidyselect`][tidyselect::language]>
#' Average time lived in an age group by people who leave
#' during the age group. Optional. Must be the same for
#' every decrement cell. See [lifetab()].
#' @param by <[`tidyselect`][tidyselect::language]>
#' Population dimensions. Separate life tables are
#' calculated for each combination of the `by` variables.
#' If a `sex` variable was specified, then that
#' variable is automatically included among the `by`
#' variables. If `data` is a
#' [grouped](https://dplyr.tidyverse.org/reference/group_data.html)
#' data frame, then the grouping variables
#' are used instead of `by`, and supplying
#' `by` as well is an error.
#'
#' @returns A [tibble][tibble::tibble()], with rows
#' grouped by population group, then by decrement cell,
#' in order of first appearance, and sorted by age within
#' each cell.
#'
#' @seealso
#' - [lifetab()] Calculate an ordinary life table
#' - [collapse_dims()], [collapse_levels()] Combine decrements
#'   before calculating a life table
#'
#' @references
#' - Preston SH, Heuveline P, and Guillot M. 2001.
#' *Demography: Measuring and Modeling Population Processes*
#' Oxford: Blackwell. Chapter 4.
#' - Chiang CL. 1968. *Introduction to Stochastic
#' Processes in Biostatistics*. New York: Wiley.
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' ## illustrative cause-specific death rates,
#' ## with circulatory disease accounting for
#' ## a larger share of deaths at older ages
#' library(tidyr)
#' mort_cause <- nzl_mort |>
#'   filter(year == 2022) |>
#'   select(gender, age, mx) |>
#'   mutate(lower = age_lower(age),
#'          Cancer = 0.25,
#'          Circulatory = 0.7 * lower / (lower + 60),
#'          Other = 1 - Cancer - Circulatory) |>
#'   pivot_longer(c(Cancer, Circulatory, Other),
#'                names_to = "cause",
#'                values_to = "share") |>
#'   mutate(mx = mx * share) |>
#'   select(gender, age, cause, mx)
#'
#' ## multiple-decrement life table
#' tab <- mort_cause |>
#'   lifetab_decrement(mx = mx,
#'                     decrement = cause,
#'                     by = gender)
#' tab
#'
#' ## summing over causes gives the
#' ## ordinary life table
#' tab |>
#'   collapse_dims(count = c(qx, lx, dx, Lx),
#'                 by = !cause)
#'
#' ## probability that a newborn eventually
#' ## dies from each cause
#' tab |>
#'   filter(age == "Infant") |>
#'   mutate(prob_eventual = lx / sum(lx),
#'          .by = gender)
#' @export
lifetab_decrement <- function(data,
                              mx = NULL,
                              qx = NULL,
                              decrement,
                              age = age,
                              sex = NULL,
                              ax = NULL,
                              by = NULL,
                              infant = c("constant", "linear", "CD", "AK"),
                              child = c("constant", "linear", "CD"),
                              closed = c("constant", "linear"),
                              open = "constant",
                              radix = 100000,
                              suffix = NULL,
                              n_core = 1) {
  if (!is.data.frame(data))
    cli::cli_abort(c("{.arg data} is not a data frame.",
                     i = "{.arg data} has class {.cls {class(data)}}."))
  if (missing(decrement))
    cli::cli_abort("No value supplied for {.arg decrement}.")
  infant <- match.arg(infant)
  child <- match.arg(child)
  closed <- match.arg(closed)
  open <- match.arg(open)
  methods <- c(infant = infant,
               child = child,
               closed = closed,
               open = open)
  mx_colnum <- tidyselect::eval_select(rlang::enquo(mx), data = data)
  qx_colnum <- tidyselect::eval_select(rlang::enquo(qx), data = data)
  decrement_colnums <- tidyselect::eval_select(rlang::enquo(decrement), data = data)
  age_colnum <- tidyselect::eval_select(rlang::enquo(age), data = data)
  sex_colnum <- tidyselect::eval_select(rlang::enquo(sex), data = data)
  ax_colnum <- tidyselect::eval_select(rlang::enquo(ax), data = data)
  by_colnums <- tidyselect::eval_select(rlang::enquo(by), data = data)
  groups_colnums <- groups_colnums(data)
  check_life_colnums(mx_colnum = mx_colnum,
                     qx_colnum = qx_colnum,
                     age_colnum = age_colnum,
                     sex_colnum = sex_colnum,
                     ax_colnum = ax_colnum,
                     by_colnums = by_colnums,
                     groups_colnums = groups_colnums)
  check_decrement_colnums(decrement_colnums = decrement_colnums,
                          others = list(mx = mx_colnum,
                                        qx = qx_colnum,
                                        age = age_colnum,
                                        sex = sex_colnum,
                                        ax = ax_colnum,
                                        by = by_colnums,
                                        groups = groups_colnums))
  for (i in decrement_colnums) {
    if (rvec::is_rvec(data[[i]]))
      cli::cli_abort("Decrement variable {.var {names(data)[[i]]}} is an rvec.")
  }
  check_number(x = radix,
               x_arg = "radix",
               check_na = TRUE,
               check_positive = TRUE,
               check_nonneg = TRUE,
               check_whole = FALSE)
  if (!is.null(suffix))
    check_string(x = suffix, x_arg = "suffix")
  check_n(n = n_core,
          nm_n = "n_core",
          min = 1L,
          max = NULL,
          divisible_by = NULL)
  protected <- unique(c(mx_colnum,
                        qx_colnum,
                        decrement_colnums,
                        age_colnum,
                        sex_colnum,
                        ax_colnum,
                        by_colnums,
                        groups_colnums))
  removed_cols <- remove_existing_lifetab_cols(data = data,
                                               is_table = TRUE,
                                               suffix = suffix,
                                               protected = protected,
                                               has_ax_input = length(ax_colnum) > 0L)
  data <- removed_cols$data
  removed <- removed_cols$removed
  mx_colnum <- shift_colnums(mx_colnum, removed)
  qx_colnum <- shift_colnums(qx_colnum, removed)
  decrement_colnums <- shift_colnums(decrement_colnums, removed)
  age_colnum <- shift_colnums(age_colnum, removed)
  sex_colnum <- shift_colnums(sex_colnum, removed)
  ax_colnum <- shift_colnums(ax_colnum, removed)
  by_colnums <- shift_colnums(by_colnums, removed)
  groups_colnums <- shift_colnums(groups_colnums, removed)
  if (length(by_colnums) > 0L)
    by_colnums <- unique(c(by_colnums, sex_colnum))
  else
    by_colnums <- unique(c(groups_colnums, sex_colnum))
  data <- tibble::tibble(data)
  decrement_one <- function(val, str_key = NULL) {
    lifetab_decrement_one(data = val,
                          mx_colnum = mx_colnum,
                          qx_colnum = qx_colnum,
                          decrement_colnums = decrement_colnums,
                          age_colnum = age_colnum,
                          sex_colnum = sex_colnum,
                          ax_colnum = ax_colnum,
                          methods = methods,
                          radix = radix,
                          suffix = suffix,
                          str_key = str_key)
  }
  has_by <- length(by_colnums) > 0L
  if (has_by) {
    inputs <- vctrs::vec_split(x = data, by = data[by_colnums])
    vals <- inputs$val
    key <- inputs$key
    keys <- lapply(seq_len(nrow(key)), function(i) key[i, , drop = FALSE])
    decrement_by <- function(val, key) {
      tryCatch(decrement_one(val, str_key = make_str_key(key)),
               error = function(cnd) {
                 str_key <- make_str_key(key)
                 msg1 <- "Problem calculating multiple-decrement life table."
                 msg2 <- paste("Problem occurred where", str_key)
                 cli::cli_abort(c(msg1, i = msg2), parent = cnd)
               })
    }
    if (n_core > 1L) {
      cl <- parallel::makeCluster(n_core)
      on.exit(parallel::stopCluster(cl), add = TRUE)
      ans <- parallel::clusterMap(cl = cl, fun = decrement_by, val = vals, key = keys)
    }
    else
      ans <- .mapply(FUN = decrement_by,
                     dots = list(val = vals, key = keys),
                     MoreArgs = list())
    ans <- vctrs::vec_rbind(!!!ans)
  }
  else
    ans <- decrement_one(data)
  ans
}


## HAS_TESTS
#' Check the 'decrement' Argument Against Other Arguments
#'
#' @param decrement_colnums Named integer vector identifying 'decrement'
#' @param others Named list of colnum vectors for other arguments
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_decrement_colnums <- function(decrement_colnums, others) {
  if (length(decrement_colnums) == 0L)
    cli::cli_abort("No value supplied for {.arg decrement}.")
  for (nm in names(others)) {
    pair <- list(decrement = decrement_colnums)
    pair[[nm]] <- others[[nm]]
    if (nm %in% c("by", "groups")) {
      overlap <- intersect(decrement_colnums, others[[nm]])
      if (length(overlap) > 0L) {
        nms_overlap <- names(decrement_colnums)[decrement_colnums %in% overlap]
        msg <- if (nm == "by")
                 "{.var {nms_overlap}} is both a decrement dimension and a {.arg by} variable."
               else
                 "{.var {nms_overlap}} is both a decrement dimension and a grouping variable."
        cli::cli_abort(c(msg,
                         i = paste("Decrement dimensions classify exits, and",
                                   "{.arg by} and grouping variables classify the population."),
                         i = "A variable can only be one of these."))
      }
    }
    else
      check_no_overlap_colnums(pair)
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Calculate a Multiple-Decrement Life Table
#' for a Single Population Group
#'
#' @param data A tibble
#' @param mx_colnum,qx_colnum,age_colnum,sex_colnum,ax_colnum
#' Named integer vectors identifying columns
#' @param decrement_colnums Named integer vector identifying
#' decrement dimensions
#' @param methods Named character vector specifying
#' calculation methods
#' @param radix Initial population for 'lx'
#' @param suffix Suffix added to new columns
#' @param str_key String identifying the population group,
#' for use in warnings, or NULL
#'
#' @returns A tibble
#'
#' @noRd
lifetab_decrement_one <- function(data,
                                  mx_colnum,
                                  qx_colnum,
                                  decrement_colnums,
                                  age_colnum,
                                  sex_colnum,
                                  ax_colnum,
                                  methods,
                                  radix,
                                  suffix,
                                  str_key = NULL) {
  nms_data <- names(data)
  cell <- vctrs::vec_group_id(data[decrement_colnums])
  n_cell <- attr(cell, "n")
  cell <- as.integer(cell)
  age <- data[[age_colnum]]
  check_age(x = age,
            complete = FALSE,
            unique = FALSE,
            zero = TRUE,
            open = TRUE)
  ## order rows by cell, then by age within cell
  ord <- order(cell, age_lower(age))
  data <- vctrs::vec_slice(data, ord)
  cell <- cell[ord]
  age <- data[[age_colnum]]
  is_cell_1 <- cell == 1L
  age_ref <- age[is_cell_1]
  check_decrement_rectangle(age = age,
                            cell = cell,
                            age_ref = age_ref,
                            cells = data[decrement_colnums])
  check_age(x = age_ref,
            complete = TRUE,
            unique = TRUE,
            zero = TRUE,
            open = TRUE)
  n_age <- length(age_ref)
  age_group_categ <- age_group_categ(age_ref)
  has_sex <- length(sex_colnum) > 0L
  if (has_sex) {
    sex <- reformat_sex(data[[sex_colnum]][is_cell_1], factor = FALSE)
  }
  else {
    check_sex_not_needed(methods)
    sex <- rep(NA_character_, times = n_age)
  }
  has_ax <- length(ax_colnum) > 0L
  if (has_ax) {
    ax_all <- data[[ax_colnum]]
    check_ax(ax = ax_all, age = age)
    ax_all <- matrix(as.double(ax_all), nrow = n_age)
    is_same <- apply(ax_all, 1L, function(x) length(unique(x)) == 1L)
    if (!all(is_same)) {
      age_bad <- as.character(age_ref[!is_same][[1L]])
      cli::cli_abort(c("{.arg ax} varies across decrement cells.",
                       i = "Values for age group {.val {age_bad}} differ.",
                       i = "{.arg ax} must be the same for every decrement."))
    }
    ax <- ax_all[, 1L]
  }
  else
    ax <- rep(NA_real_, times = n_age)
  has_mx <- length(mx_colnum) > 0L
  if (has_mx) {
    vals <- data[[mx_colnum]]
    check_mx(vals)
  }
  else {
    vals <- data[[qx_colnum]]
    check_qx(qx = vals, nm_qx = "qx")
  }
  vals <- as.matrix(vals)
  n_draw <- ncol(vals)
  ## array with dimensions age, draw, cell
  vals <- array(vals, dim = c(n_age, n_cell, n_draw))
  vals <- aperm(vals, perm = c(1L, 3L, 2L))
  total <- apply(vals, c(1L, 2L), sum)
  total <- matrix(total, nrow = n_age, ncol = n_draw)
  if (has_mx) {
    lx <- mx_to_lx(mx = total,
                   age_group_categ = age_group_categ,
                   sex = sex,
                   ax = ax,
                   methods = methods)
    Lx <- mx_to_Lx(mx = total,
                   age_group_categ = age_group_categ,
                   sex = sex,
                   ax = ax,
                   methods = methods)
  }
  else {
    check_qx_has_previous_age(age_ref)
    check_qx_decrement_total(total = total, age = age_ref)
    total[n_age, ] <- 1
    lx <- qx_to_lx(total)
    Lx <- qx_to_Lx(qx = total,
                   age_group_categ = age_group_categ,
                   sex = sex,
                   ax = ax,
                   methods = methods)
  }
  dx <- lx_to_dx(lx)
  share <- decrement_shares(vals = vals, total = total)
  if (has_mx)
    warn_decrement_open_zero(total = total, str_key = str_key)
  nx <- age_upper(age_ref) - age_lower(age_ref)
  nx[n_age] <- 0
  lx_end <- rbind(lx[-1L, , drop = FALSE], 0)
  Lx_exit <- Lx - nx * lx_end
  ans_qx <- array(NA_real_, dim = dim(vals))
  ans_lx <- ans_qx
  ans_dx <- ans_qx
  ans_Lx <- ans_qx
  for (k in seq_len(n_cell)) {
    share_k <- matrix(share[, , k], nrow = n_age, ncol = n_draw)
    dx_k <- dx * share_k
    lx_k <- apply(dx_k, 2L, function(x) rev(cumsum(rev(x))))
    lx_k <- matrix(lx_k, nrow = n_age, ncol = n_draw)
    lx_k_end <- rbind(lx_k[-1L, , drop = FALSE], 0)
    Lx_k <- nx * lx_k_end + Lx_exit * share_k
    ans_qx[, , k] <- dx_k / lx
    ans_lx[, , k] <- radix * lx_k
    ans_dx[, , k] <- radix * dx_k
    ans_Lx[, , k] <- radix * Lx_k
  }
  ans <- list(qx = ans_qx,
              lx = ans_lx,
              dx = ans_dx,
              Lx = ans_Lx)
  to_col <- function(x) {
    x <- aperm(x, perm = c(1L, 3L, 2L))
    x <- matrix(x, nrow = n_age * n_cell, ncol = n_draw)
    if (n_draw > 1L)
      rvec::rvec_dbl(x)
    else
      as.double(x)
  }
  ans <- lapply(ans, to_col)
  if (!is.null(suffix))
    names(ans) <- paste(names(ans), suffix, sep = ".")
  if (is.null(suffix)) {
    i_input <- if (has_mx) mx_colnum else qx_colnum
    data <- data[-i_input]
  }
  ans <- tibble::as_tibble(ans)
  vctrs::vec_cbind(data, ans)
}


## HAS_TESTS
#' Check That Every Decrement Cell Has the Same Age Groups
#'
#' Assumes rows are sorted by cell, then age.
#'
#' @param age Age labels for all rows
#' @param cell Integer cell IDs for all rows
#' @param age_ref Age labels for the first cell
#' @param cells Data frame with decrement dimensions
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_decrement_rectangle <- function(age, cell, age_ref, cells) {
  lower_ref <- age_lower(age_ref)
  upper_ref <- age_upper(age_ref)
  lower <- age_lower(age)
  upper <- age_upper(age)
  n_cell <- max(cell)
  for (k in seq_len(n_cell)) {
    is_k <- cell == k
    if (anyDuplicated(lower[is_k])) {
      i_k <- match(k, cell)
      str_key <- make_str_key(vctrs::vec_slice(cells, i_k))
      cli::cli_abort(c("Decrement cell has duplicated age groups.",
                       i = "Cell: {str_key}."))
    }
    is_same <- (sum(is_k) == length(lower_ref)) &&
      all(lower[is_k] == lower_ref) &&
      all(upper[is_k] == upper_ref)
    if (!is_same) {
      i_k <- match(k, cell)
      str_key <- make_str_key(vctrs::vec_slice(cells, i_k))
      str_key_ref <- make_str_key(vctrs::vec_slice(cells, 1L))
      cli::cli_abort(c("Decrement cells have different age groups.",
                       i = "Age groups for {str_key_ref}: {.val {as.character(age_ref)}}.",
                       i = "Age groups for {str_key}: {.val {as.character(age[is_k])}}.",
                       i = "Every decrement cell must have the same age groups."))
    }
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check Sums of Decrement-Specific Probabilities
#'
#' Sums must not exceed 1 in closed age groups,
#' and must equal 1 in the open age group.
#'
#' @param total Matrix of sums, with one row per age group
#' @param age Age labels
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_qx_decrement_total <- function(total, age) {
  tol <- sqrt(.Machine$double.eps)
  n_age <- nrow(total)
  closed <- total[-n_age, , drop = FALSE]
  is_high <- !is.na(closed) & closed > 1 + tol
  if (any(is_high)) {
    i_age <- which(rowSums(is_high) > 0L)[[1L]]
    age_bad <- as.character(age[[i_age]])
    cli::cli_abort(c("Sum of {.arg qx} across decrement cells is greater than 1.",
                     i = "Age group: {.val {age_bad}}."))
  }
  open <- total[n_age, ]
  is_not_one <- !is.na(open) & abs(open - 1) > tol
  if (any(is_not_one)) {
    age_open <- as.character(age[[n_age]])
    cli::cli_abort(c("Sum of {.arg qx} across decrement cells in final age group does not equal 1.",
                     i = "Age group: {.val {age_open}}.",
                     i = paste("In the final age group, {.arg qx} gives the share",
                               "of exits from each decrement cell, so the values",
                               "must sum to 1.")))
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Calculate the Share of Exits Due to Each Decrement Cell
#'
#' Within each age group and draw, the share for a cell is its
#' rate (or probability) divided by the total. Where the total is 0
#' in a closed age group, there are no exits, and shares are set to 0.
#' Where the total is 0 in the final age group, no one ever leaves,
#' so shares are undefined, and are set to NaN. Where the total is
#' infinite, exits are divided equally among cells with infinite rates.
#'
#' @param vals Array with dimensions age, draw, cell
#' @param total Matrix with dimensions age, draw
#'
#' @returns Array with same dimensions as 'vals'
#'
#' @noRd
decrement_shares <- function(vals, total) {
  n_cell <- dim(vals)[[3L]]
  ans <- vals / as.vector(total)
  is_zero <- !is.na(total) & total == 0
  n_age <- nrow(total)
  is_zero_open <- is_zero & (row(total) == n_age)
  is_zero <- is_zero & !is_zero_open
  is_inf <- !is.na(total) & is.infinite(total)
  if (any(is_zero) || any(is_zero_open) || any(is_inf)) {
    n_inf <- apply(is.infinite(vals), c(1L, 2L), sum)
    for (k in seq_len(n_cell)) {
      ans_k <- ans[, , k]
      vals_k <- vals[, , k]
      ans_k[is_zero] <- 0
      ans_k[is_zero_open] <- NaN
      ans_k[is_inf] <- as.double(is.infinite(vals_k[is_inf])) / n_inf[is_inf]
      ans[, , k] <- ans_k
    }
  }
  ans
}


## HAS_TESTS
#' Warn If the Total Rate in the Final Age Group is Zero
#'
#' If no one leaves the final age group, the decrement through
#' which people eventually leave is undefined.
#'
#' @param total Matrix of total rates, with one row per age group
#' @param str_key String identifying the population group, or NULL
#'
#' @returns TRUE, invisibly
#'
#' @noRd
warn_decrement_open_zero <- function(total, str_key) {
  open <- total[nrow(total), ]
  n_zero <- sum(!is.na(open) & open == 0)
  if (n_zero > 0L) {
    msg <- c("Rate for final age group, summed across decrement cells, is 0.",
             i = paste("No one leaves the final age group, so the decrement",
                       "through which people eventually leave is undefined."),
             i = paste("Setting {.var lx} and {.var Lx} for all age groups, and",
                       "{.var qx} and {.var dx} for the final age group, to {.val {NaN}}."))
    if (ncol(total) > 1L)
      msg <- c(msg, i = "Affects {n_zero} draw{?s}.")
    if (!is.null(str_key))
      msg <- c(msg, i = paste("Problem occurred where", str_key))
    cli::cli_warn(msg)
  }
  invisible(TRUE)
}
