## HAS_TESTS
#' Collapse Dimensions or Levels of a Data Frame
#'
#' Aggregate counts, rates, probabilities, means,
#' and ratios, using the rule that is appropriate
#' for each type of measure.
#' Function `collapse_dims()` removes whole
#' dimensions, such as region or sex.
#' Function `collapse_levels()` merges levels
#' within dimensions, such as regions within
#' a region variable, or age groups within an
#' age variable.
#'
#' @section Measures:
#'
#' Measures are the columns that are aggregated.
#' All other columns are treated as dimensions.
#'
#' - `count` Counts, such as deaths or population.
#'   Counts are summed.
#' - `rate`, `prob`, `mean`, `ratio` Quantities
#'   formed by dividing an additive numerator by
#'   the weight `wt`. Examples are mortality rates
#'   (deaths divided by person-years), proportions
#'   (persons with a characteristic divided by persons),
#'   mean age at death (total age at death divided by deaths),
#'   and sex ratios (males divided by females).
#'   The four types use the same calculations.
#'   They differ only in the checks applied:
#'   rates must be non-negative, and
#'   probabilities must lie between 0 and 1.
#'   If in doubt, use `ratio`.
#' - `wt` Weights. Required whenever `rate`, `prob`,
#'   `mean`, or `ratio` is supplied. All `rate`, `prob`,
#'   `mean`, and `ratio` columns in a call
#'   share the same weight. Quantities with
#'   different denominators need separate calls.
#'
#' Quantities that are not an additive numerator
#' divided by `wt`, such as medians, life expectancies,
#' or total fertility rates, cannot be aggregated
#' with these functions.
#'
#' @section Shared and partitioned weights:
#'
#' The rule for aggregating a rate, probability, mean, or ratio
#' depends on how the weight relates to the dimension being
#' collapsed.
#'
#' - **Partitioned weights.** Different rows refer to different
#'   people, as with population or exposure across regions or
#'   age groups. The result is the weighted mean, and the
#'   weights are summed.
#' - **Shared weights.** The rows share a single denominator,
#'   as with exposure across causes of death, destinations of
#'   migrants, or the sex of children born. The values are summed,
#'   and the weight is the common value, which must be the same
#'   in every row. Dimensions with shared weights are specified
#'   using `wt_shared`.
#'
#' Whether a dimension is shared depends on the denominator,
#' not on the dimension. For instance, origin is a shared
#' dimension for in-migration rates that use the population of
#' the destination as the denominator, but not for out-migration
#' rates that use the population of the origin.
#'
#' When both kinds of dimension are being collapsed, the
#' shared dimensions are collapsed first.
#'
#' The weighted-mean rule assumes that weights are
#' partitioned across the rows being merged. This is
#' not true of life table probabilities `qx` across
#' age groups, since the people at the start of one age
#' group are the survivors from the previous age group.
#' To aggregate life tables over age, collapse the
#' mortality rates (`mx`) and then construct a new
#' life table using [lifetab()].
#'
#' @section Keys and row order:
#'
#' In `collapse_dims()`, the dimensions that are kept
#' are the `by` variables or, if `data` is a
#' [grouped](https://dplyr.tidyverse.org/reference/group_data.html)
#' data frame, the grouping variables. Measures are
#' removed from `by` automatically, so `by = !region`
#' keeps every dimension except `region`. All other
#' dimensions are collapsed, and a message lists them.
#'
#' In `collapse_levels()`, every column that is not a
#' measure is kept.
#'
#' Rows appear in the order in which their combination
#' of dimensions first appears in `data`. In `collapse_levels()`,
#' if a recoded `dim` variable is a factor, then rows are
#' ordered by its levels, within the remaining dimensions.
#'
#' The result is an ungrouped [tibble][tibble::tibble()].
#'
#' @section Specifying the map:
#'
#' Argument `map` in `collapse_levels()` can be
#'
#' - **A named list.** Each element gives the old levels that
#'   are merged into the new level given by its name, as in
#'   `list(North = c("Northland", "Auckland"))`.
#'   Every old level must appear in `data`.
#' - **A data frame.** A lookup table with two columns:
#'   old levels first and new levels second. Each old
#'   level appears at most once. The lookup table can
#'   include levels that do not appear in `data`.
#' - **A function.** A function, such as
#'   [combine_age()], that takes a vector of levels and
#'   returns a vector of new levels with the same length,
#'   for instance `\(x) combine_age(x, to = "lt")`.
#'   The function is applied once to each `dim` variable.
#'   Each old level must map to a single new level.
#'
#' With a list or a data frame, levels that
#' are not mentioned are left unchanged.
#' If a `dim` variable is a factor, or a function or
#' lookup table returns a factor, then the result is
#' a factor, and keeps any unused levels.
#'
#' If `dim` specifies more than one variable, such as
#' origin and destination, the same map is applied to each.
#' When merged regions are both origins and destinations,
#' flows between them become flows within the merged region.
#'
#' @section Using rvecs to represent uncertainty:
#'
#' An [rvec][rvec::rvec()] is a 'random vector',
#' holding multiple draws from a distribution.
#' Measures, including `wt`, can be rvecs.
#' Calculations are done separately for each draw.
#' A rate, probability, mean, or ratio is an rvec
#' in the result if it, or `wt`, is an rvec.
#' Dimensions cannot be rvecs.
#'
#' @section Checks:
#'
#' - When `wt_shared` is supplied, `wt` must be
#'   constant across the shared dimensions.
#' - After values have been summed over shared dimensions,
#'   probabilities must not exceed 1.
#' - When `rate`, `prob`, `mean`, `ratio`, or `wt` is supplied,
#'   `data` must not have two rows with the same dimensions.
#' - If `wt` is identical across the levels of a collapsed
#'   dimension in every case, `wt` may be a shared weight,
#'   and the functions give a warning.
#' - `collapse_levels()` gives a message if no rows are merged.
#'
#' If a weighted mean has total weight of zero,
#' the result is `NA`.
#'
#' @param data A data frame.
#' @param dim <[`tidyselect`][tidyselect::language]>
#' Dimensions whose levels are merged
#' (`collapse_levels()` only).
#' @param map How old levels are mapped to new
#' levels (`collapse_levels()` only).
#' A named list, a data frame, or a function.
#' See below for details.
#' @param count <[`tidyselect`][tidyselect::language]>
#' Counts. Possibly [rvec][rvec::rvec()]s.
#' @param rate <[`tidyselect`][tidyselect::language]>
#' Rates. Possibly rvecs.
#' @param prob <[`tidyselect`][tidyselect::language]>
#' Probabilities or proportions. Possibly rvecs.
#' @param mean <[`tidyselect`][tidyselect::language]>
#' Means. Possibly rvecs.
#' @param ratio <[`tidyselect`][tidyselect::language]>
#' Other ratios. Possibly rvecs.
#' @param wt <[`tidyselect`][tidyselect::language]>
#' Weights used with `rate`, `prob`, `mean`,
#' and `ratio`. A single variable.
#' Possibly an rvec.
#' @param wt_shared <[`tidyselect`][tidyselect::language]>
#' Dimensions across which `wt` is shared.
#' @param by <[`tidyselect`][tidyselect::language]>
#' Dimensions to keep (`collapse_dims()` only).
#'
#' @returns A [tibble][tibble::tibble()].
#'
#' @seealso
#' - [combine_age()], [set_age_open()] Functions that can
#'   be used with `map`
#' - [lifetab()] Calculate life tables from collapsed rates
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' ## collapse over gender
#' nzl_mort |>
#'   collapse_dims(count = deaths,
#'                 rate = mx,
#'                 wt = popn,
#'                 by = !gender)
#'
#' ## the same result, using 'group_by'
#' nzl_mort |>
#'   group_by(year, age) |>
#'   collapse_dims(count = deaths,
#'                 rate = mx,
#'                 wt = popn)
#'
#' ## merge age groups, then calculate
#' ## life expectancy
#' nzl_mort |>
#'   collapse_levels(dim = age,
#'                   map = \(x) set_age_open(x, lower = 80),
#'                   count = deaths,
#'                   rate = mx,
#'                   wt = popn) |>
#'   lifeexp(mx = mx, by = c(year, gender))
#'
#' ## shared weights: death rates by cause share
#' ## a common exposure
#' deaths_cause <- tibble(
#'   cause = c("Cancer", "Heart", "Stroke", "Other"),
#'   mx = c(0.002, 0.0015, 0.0005, 0.001),
#'   popn = 1000
#' )
#' deaths_cause |>
#'   collapse_levels(dim = cause,
#'                   map = list(Circulatory = c("Heart", "Stroke")),
#'                   rate = mx,
#'                   wt = popn,
#'                   wt_shared = cause)
#' @export
collapse_dims <- function(data,
                          count = NULL,
                          rate = NULL,
                          prob = NULL,
                          mean = NULL,
                          ratio = NULL,
                          wt = NULL,
                          wt_shared = NULL,
                          by = NULL) {
  check_collapse_data(data)
  measures <- collapse_measures(data = data,
                                count_quo = rlang::enquo(count),
                                rate_quo = rlang::enquo(rate),
                                prob_quo = rlang::enquo(prob),
                                mean_quo = rlang::enquo(mean),
                                ratio_quo = rlang::enquo(ratio),
                                wt_quo = rlang::enquo(wt),
                                wt_shared_quo = rlang::enquo(wt_shared))
  by_colnums <- tidyselect::eval_select(rlang::enquo(by), data = data)
  groups_colnums <- groups_colnums(data)
  has_by <- length(by_colnums) > 0L
  has_groups <- length(groups_colnums) > 0L
  if (has_by && has_groups)
    cli::cli_abort("Can't supply {.arg by} when {.arg data} is a grouped data frame.")
  if (has_groups) {
    check_no_overlap_colnums(list(groups = groups_colnums,
                                  measures = measures$all))
    keys_colnums <- groups_colnums
  }
  else
    keys_colnums <- by_colnums[!(by_colnums %in% measures$all)]
  is_measure <- seq_along(data) %in% measures$all
  dims_colnums <- which(!is_measure)
  collapsed_colnums <- setdiff(dims_colnums, keys_colnums)
  check_collapse_dims(data = data,
                      dims_colnums = dims_colnums,
                      measures = measures)
  nms_data <- names(data)
  if (length(collapsed_colnums) > 0L) {
    nms_collapsed <- nms_data[collapsed_colnums]
    cli::cli_alert_info("Collapsing over {.var {nms_collapsed}}.")
  }
  data <- as_plain_tibble(data)
  keys_out <- data[keys_colnums]
  unshared_colnums <- setdiff(collapsed_colnums, measures$wt_shared)
  keys_stage1 <- data[c(keys_colnums, unshared_colnums)]
  vals <- collapse_inner(data = data,
                         keys_out = keys_out,
                         keys_stage1 = keys_stage1,
                         unshared = data[unshared_colnums],
                         nms_unshared = nms_data[unshared_colnums],
                         measures = measures)
  ans <- c(vals$keys, vals$measures)
  nms_ans <- nms_data[sort(c(keys_colnums, measures$all))]
  ans <- tibble::as_tibble(ans[nms_ans])
  ans
}


## HAS_TESTS
#' @rdname collapse_dims
#' @export
collapse_levels <- function(data,
                            dim,
                            map,
                            count = NULL,
                            rate = NULL,
                            prob = NULL,
                            mean = NULL,
                            ratio = NULL,
                            wt = NULL,
                            wt_shared = NULL) {
  check_collapse_data(data)
  measures <- collapse_measures(data = data,
                                count_quo = rlang::enquo(count),
                                rate_quo = rlang::enquo(rate),
                                prob_quo = rlang::enquo(prob),
                                mean_quo = rlang::enquo(mean),
                                ratio_quo = rlang::enquo(ratio),
                                wt_quo = rlang::enquo(wt),
                                wt_shared_quo = rlang::enquo(wt_shared))
  dim_colnums <- tidyselect::eval_select(rlang::enquo(dim), data = data)
  if (length(dim_colnums) == 0L)
    cli::cli_abort("No value supplied for {.arg dim}.")
  check_no_overlap_colnums(list(dim = dim_colnums,
                                measures = measures$all))
  groups_colnums <- groups_colnums(data)
  check_no_overlap_colnums(list(dim = dim_colnums,
                                groups = groups_colnums))
  if (missing(map))
    cli::cli_abort("No value supplied for {.arg map}.")
  is_measure <- seq_along(data) %in% measures$all
  dims_colnums <- which(!is_measure)
  check_collapse_dims(data = data,
                      dims_colnums = dims_colnums,
                      measures = measures)
  data <- as_plain_tibble(data)
  recode <- make_collapse_recode(map)
  nms_data <- names(data)
  if (is.list(map) && !is.data.frame(map)) {
    vals_dim <- unlist(lapply(data[dim_colnums], as.character), use.names = FALSE)
    check_map_list_in_data(map = map, vals_dim = vals_dim)
  }
  keys_out <- data[dims_colnums]
  for (i in dim_colnums) {
    nm <- nms_data[[i]]
    keys_out[[nm]] <- recode(x = data[[i]], nm_x = nm)
  }
  unshared_colnums <- setdiff(dim_colnums, measures$wt_shared)
  unshared <- data[unshared_colnums]
  names(unshared) <- sprintf(".unshared_%d", seq_along(unshared))
  keys_stage1 <- vctrs::vec_cbind(keys_out, unshared, .name_repair = "minimal")
  vals <- collapse_inner(data = data,
                         keys_out = keys_out,
                         keys_stage1 = keys_stage1,
                         unshared = unshared,
                         nms_unshared = nms_data[unshared_colnums],
                         measures = measures)
  ans <- c(vals$keys, vals$measures)
  ans <- tibble::as_tibble(ans[nms_data])
  n_ans <- nrow(ans)
  if (n_ans == nrow(data))
    cli::cli_alert_info("No rows were merged.")
  nms_dim <- nms_data[dim_colnums]
  is_fac_dim <- vapply(ans[nms_dim], is.factor, TRUE)
  if (any(is_fac_dim)) {
    nms_other <- setdiff(nms_data[dims_colnums], nms_dim)
    rank_other <- vctrs::vec_group_id(ans[nms_other])
    ranks_dim <- lapply(ans[nms_dim],
                        function(x) if (is.factor(x)) as.integer(x) else vctrs::vec_group_id(x))
    ord <- do.call(order, c(list(rank_other), unname(ranks_dim)))
    ans <- vctrs::vec_slice(ans, ord)
  }
  ans
}


## Helpers --------------------------------------------------------------------

## HAS_TESTS
#' Convert a Data Frame to an Ungrouped Tibble
#'
#' Removes grouping and other attributes.
#'
#' @param data A data frame
#'
#' @returns A tibble
#'
#' @noRd
as_plain_tibble <- function(data) {
  ans <- vctrs::new_data_frame(as.list(data), n = nrow(data))
  tibble::as_tibble(ans)
}


## HAS_TESTS
#' Check That 'data' is a Data Frame
#'
#' @param data Object passed as 'data'
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_collapse_data <- function(data) {
  if (!is.data.frame(data))
    cli::cli_abort(c("{.arg data} is not a data frame.",
                     i = "{.arg data} has class {.cls {class(data)}}."))
  invisible(TRUE)
}


## HAS_TESTS
#' Identify and Check Measure Columns
#'
#' @param data A data frame
#' @param count_quo,rate_quo,prob_quo,mean_quo,ratio_quo,wt_quo,wt_shared_quo
#' Quosures for the measure arguments
#'
#' @returns A named list of named integer vectors
#' ('count', 'rate', 'prob', 'mean', 'ratio', 'wt', 'wt_shared'),
#' plus 'all', the positions of all measure columns
#'
#' @noRd
collapse_measures <- function(data,
                              count_quo,
                              rate_quo,
                              prob_quo,
                              mean_quo,
                              ratio_quo,
                              wt_quo,
                              wt_shared_quo) {
  ans <- list(count = tidyselect::eval_select(count_quo, data = data),
              rate = tidyselect::eval_select(rate_quo, data = data),
              prob = tidyselect::eval_select(prob_quo, data = data),
              mean = tidyselect::eval_select(mean_quo, data = data),
              ratio = tidyselect::eval_select(ratio_quo, data = data),
              wt = tidyselect::eval_select(wt_quo, data = data))
  check_at_most_one_colnum(ans["wt"])
  check_no_overlap_colnums(ans)
  wt_shared <- tidyselect::eval_select(wt_shared_quo, data = data)
  all <- sort(unlist(unname(ans)))
  if (length(all) == 0L)
    cli::cli_abort(paste("No measures supplied. Need at least one of",
                         "{.arg count}, {.arg rate}, {.arg prob},",
                         "{.arg mean}, {.arg ratio}, or {.arg wt}."))
  check_no_overlap_colnums(c(ans, list(wt_shared = wt_shared)))
  has_wt <- length(ans$wt) > 0L
  for (nm in c("rate", "prob", "mean", "ratio")) {
    if (length(ans[[nm]]) > 0L && !has_wt)
      cli::cli_abort("{.arg {nm}} supplied, but no value supplied for {.arg wt}.")
  }
  if (length(wt_shared) > 0L && !has_wt)
    cli::cli_abort("{.arg wt_shared} supplied, but no value supplied for {.arg wt}.")
  nms_data <- names(data)
  for (nm in c("count", "rate", "prob", "mean", "ratio", "wt")) {
    for (i in ans[[nm]]) {
      x <- data[[i]]
      nm_x <- nms_data[[i]]
      check_numeric(x = x,
                    x_arg = nm_x,
                    check_na = FALSE,
                    check_positive = FALSE,
                    check_nonneg = nm %in% c("rate", "wt"),
                    check_whole = FALSE)
      if (nm == "prob")
        check_prob_le_one(x = x, nm_x = nm_x, is_sum = FALSE)
    }
  }
  get_n_draw <- function(x) if (rvec::is_rvec(x)) as.integer(rvec::n_draw(x)) else NA_integer_
  n_draws <- vapply(data[all], get_n_draw, 1L)
  n_draws <- unique(n_draws[!is.na(n_draws)])
  if (length(n_draws) > 1L)
    cli::cli_abort(c("Measures have different numbers of draws.",
                     i = "Numbers of draws: {.val {n_draws}}."))
  ans$wt_shared <- wt_shared
  ans$all <- all
  ans
}


## HAS_TESTS
#' Check That Probabilities Do Not Exceed 1
#'
#' @param x A numeric vector or rvec
#' @param nm_x Name used in error message
#' @param is_sum Whether 'x' was obtained by summing
#' over shared dimensions
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_prob_le_one <- function(x, nm_x, is_sum) {
  if (rvec::is_rvec(x))
    x <- as.matrix(x)
  tol <- sqrt(.Machine$double.eps)
  n_high <- sum(x > 1 + tol, na.rm = TRUE)
  if (n_high > 0L) {
    if (is_sum)
      cli::cli_abort(c("Sum of {.arg {nm_x}} over {.arg wt_shared} dimensions is greater than 1.",
                       i = "Are all the dimensions in {.arg wt_shared} really shared?"))
    else
      cli::cli_abort("{.arg {nm_x}} has {cli::qty(n_high)} value{?s} greater than 1.")
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check the Dimension Columns of a Data Frame
#'
#' Check that dimensions are not rvecs, and that,
#' if rates, probabilities, means, ratios, or weights
#' are being aggregated, no two rows have the same
#' dimensions.
#'
#' @param data A data frame
#' @param dims_colnums Positions of all columns that are not measures
#' @param measures List created by 'collapse_measures()'
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_collapse_dims <- function(data, dims_colnums, measures) {
  nms_data <- names(data)
  for (i in dims_colnums) {
    if (rvec::is_rvec(data[[i]])) {
      nm <- nms_data[[i]]
      cli::cli_abort(c("Dimension {.var {nm}} is an rvec.",
                       i = "Is {.var {nm}} a measure that needs to be named in {.arg count}, {.arg rate}, etc?"))
    }
  }
  has_wt <- length(measures$wt) > 0L
  if (has_wt && length(dims_colnums) > 0L)
    check_duplicated_rows(x = data,
                          nm_x = "data",
                          nms_cols = nms_data[dims_colnums])
  invisible(TRUE)
}


## HAS_TESTS
#' Aggregate Measures Within Groups Defined by Keys
#'
#' Aggregation is done in two stages. In stage 1,
#' rows that differ only on shared dimensions are merged:
#' rates etc are summed, and the common value for 'wt'
#' is used. In stage 2, the results of stage 1 that share
#' the same output keys are merged: rates etc are
#' weighted means, and 'wt' is summed.
#'
#' @param data A tibble
#' @param keys_out Tibble with the dimensions in the result,
#' one row per row of 'data'
#' @param keys_stage1 Tibble with the dimensions in
#' 'keys_out', plus the original values of
#' unshared dimensions that are being collapsed
#' @param unshared Tibble with the original values of
#' unshared dimensions that are being collapsed
#' @param nms_unshared Names of the unshared dimensions,
#' for use in messages
#' @param measures List created by 'collapse_measures()'
#'
#' @returns A list with elements 'keys' (a tibble) and
#' 'measures' (a named list of vectors)
#'
#' @noRd
collapse_inner <- function(data, keys_out, keys_stage1, unshared, nms_unshared, measures) {
  nms_data <- names(data)
  n <- nrow(data)
  g1 <- vctrs::vec_group_id(keys_stage1)
  n1 <- attr(g1, "n")
  g1 <- as.integer(g1)
  i_first1 <- match(seq_len(n1), g1)
  keys_out1 <- vctrs::vec_slice(keys_out, i_first1)
  g2 <- vctrs::vec_group_id(keys_out1)
  n2 <- attr(g2, "n")
  g2 <- as.integer(g2)
  i_first2 <- match(seq_len(n2), g2)
  keys <- vctrs::vec_slice(keys_out1, i_first2)
  vals <- list()
  for (i in measures$count) {
    x <- data[[i]]
    m <- sum_by_group(to_matrix_draws(x), g = g2[g1], n_group = n2)
    vals[[nms_data[[i]]]] <- from_matrix_draws(m, is_rvec = rvec::is_rvec(x))
  }
  has_wt <- length(measures$wt) > 0L
  if (has_wt) {
    i_wt <- measures$wt
    nm_wt <- nms_data[[i_wt]]
    wt <- data[[i_wt]]
    is_rvec_wt <- rvec::is_rvec(wt)
    m_wt <- to_matrix_draws(wt)
    check_wt_shared_constant(m_wt = m_wt,
                             g = g1,
                             keys = keys_stage1,
                             nm_wt = nm_wt,
                             nms_shared = nms_data[measures$wt_shared])
    check_wt_unshared_varies(m_wt = m_wt,
                             keys_stage1 = keys_stage1,
                             unshared = unshared,
                             nms_unshared = nms_unshared,
                             nm_wt = nm_wt)
    m_wt1 <- m_wt[i_first1, , drop = FALSE]
    m_wt2 <- sum_by_group(m_wt1, g = g2, n_group = n2)
    vals[[nm_wt]] <- from_matrix_draws(m_wt2, is_rvec = is_rvec_wt)
    n_members <- tabulate(g2, nbins = n2)
    is_single <- n_members == 1L
    i_single1 <- i_first2[is_single]
    for (nm_type in c("rate", "prob", "mean", "ratio")) {
      for (i in measures[[nm_type]]) {
        x <- data[[i]]
        nm_x <- nms_data[[i]]
        is_rvec <- rvec::is_rvec(x) || is_rvec_wt
        m_x <- to_matrix_draws(x)
        m_x1 <- sum_by_group(m_x, g = g1, n_group = n1)
        if (nm_type == "prob")
          check_prob_le_one(x = m_x1, nm_x = nm_x, is_sum = TRUE)
        n_draw <- max(ncol(m_x1), ncol(m_wt1))
        m_x1 <- expand_draws(m_x1, n_draw = n_draw)
        m_w1 <- expand_draws(m_wt1, n_draw = n_draw)
        num <- sum_by_group(m_x1 * m_w1, g = g2, n_group = n2)
        den <- sum_by_group(m_w1, g = g2, n_group = n2)
        m_x2 <- num / den
        m_x2[!is.na(den) & den == 0] <- NA_real_
        m_x2[is_single, ] <- m_x1[i_single1, , drop = FALSE]
        vals[[nm_x]] <- from_matrix_draws(m_x2, is_rvec = is_rvec)
      }
    }
  }
  list(keys = keys, measures = vals)
}


## HAS_TESTS
#' Check That Weights are Constant Across Shared Dimensions
#'
#' @param m_wt Matrix of weights, one row per row of data
#' @param g Integer vector of stage-1 groups
#' @param keys Tibble used to construct 'g'
#' @param nm_wt Name of the weight variable
#' @param nms_shared Names of the shared dimensions
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_wt_shared_constant <- function(m_wt, g, keys, nm_wt, nms_shared) {
  is_const <- is_const_by_group(m = m_wt, g = g)
  if (!all(is_const)) {
    i_bad <- match(FALSE, is_const)
    i_row <- match(i_bad, g)
    msg <- "{.arg {nm_wt}} varies across {.arg wt_shared} {cli::qty(length(nms_shared))} dimension{?s} {.var {nms_shared}}."
    if (ncol(keys) > 0L) {
      str_key <- make_str_key(vctrs::vec_slice(keys, i_row))
      cli::cli_abort(c(msg, i = paste("Problem occurred where", str_key)))
    }
    else
      cli::cli_abort(msg)
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Warn If Weights are Identical Across a Collapsed,
#' Unshared Dimension
#'
#' Weights that are the same for every level of a
#' collapsed dimension, in every group, suggest that
#' the dimension is shared.
#'
#' @param m_wt Matrix of weights, one row per row of data
#' @param keys_stage1 Tibble with keys for stage 1
#' @param unshared Tibble with the original values of
#' unshared dimensions being collapsed, which have names
#' that appear in 'keys_stage1'
#' @param nms_unshared Names of the unshared dimensions,
#' for use in messages
#' @param nm_wt Name of the weight variable
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_wt_unshared_varies <- function(m_wt, keys_stage1, unshared, nms_unshared, nm_wt) {
  nms_keys <- names(keys_stage1)
  for (j in seq_along(unshared)) {
    nm <- names(unshared)[[j]]
    keys_other <- keys_stage1[setdiff(nms_keys, nm)]
    g <- as.integer(vctrs::vec_group_id(keys_other))
    n_group <- max(c(0L, g))
    pairs <- vctrs::vec_unique(vctrs::data_frame(g = g, x = unshared[[nm]]))
    n_levels <- tabulate(pairs$g, nbins = n_group)
    is_merged <- n_levels > 1L
    if (any(is_merged)) {
      is_const <- is_const_by_group(m = m_wt, g = g)
      if (all(is_const[is_merged])) {
        nm_dim <- nms_unshared[[j]]
        cli::cli_warn(c("{.arg {nm_wt}} is identical across the levels of {.var {nm_dim}} being collapsed.",
                        i = "Should {.var {nm_dim}} be included in {.arg wt_shared}?"))
      }
    }
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Test Whether Rows of a Matrix are Equal Within Groups
#'
#' Uses a relative tolerance. NAs are equal to other NAs.
#'
#' @param m A numeric matrix
#' @param g Integer vector of groups, with values 1, ..., n_group
#'
#' @returns A logical vector with one element per group
#'
#' @noRd
is_const_by_group <- function(m, g) {
  n_group <- max(c(0L, g))
  i_first <- match(seq_len(n_group), g)
  first <- m[i_first[g], , drop = FALSE]
  tol <- sqrt(.Machine$double.eps) * pmax(1, abs(first))
  is_na_m <- is.na(m)
  is_na_first <- is.na(first)
  is_same <- (is_na_m & is_na_first) | (!is_na_m & !is_na_first & abs(m - first) <= tol)
  is_same[is.na(is_same)] <- FALSE
  is_same_row <- rowSums(!is_same) == 0L
  n_bad <- tabulate(g[!is_same_row], nbins = n_group)
  n_bad == 0L
}


## HAS_TESTS
#' Convert a Vector or Rvec to a Matrix of Draws
#'
#' @param x A vector or rvec
#'
#' @returns A matrix with one row per element of 'x'
#'
#' @noRd
to_matrix_draws <- function(x) {
  if (rvec::is_rvec(x))
    unname(as.matrix(x))
  else
    matrix(x, ncol = 1L)
}


## HAS_TESTS
#' Convert a Matrix of Draws to a Vector or Rvec
#'
#' @param m A matrix
#' @param is_rvec Whether to return an rvec
#'
#' @returns A vector or rvec
#'
#' @noRd
from_matrix_draws <- function(m, is_rvec) {
  if (is_rvec)
    rvec::rvec(unname(m))
  else
    m[, 1L]
}


## HAS_TESTS
#' Replicate a One-Column Matrix to Match a Number of Draws
#'
#' @param m A matrix
#' @param n_draw Number of columns needed
#'
#' @returns A matrix with 'n_draw' columns
#'
#' @noRd
expand_draws <- function(m, n_draw) {
  if (ncol(m) == n_draw)
    m
  else
    m[, rep(1L, times = n_draw), drop = FALSE]
}


## HAS_TESTS
#' Sum the Rows of a Matrix Within Groups
#'
#' @param m A matrix
#' @param g Integer vector of groups, with values 1, ..., n_group
#' @param n_group Number of groups
#'
#' @returns A matrix with 'n_group' rows
#'
#' @noRd
sum_by_group <- function(m, g, n_group) {
  ans <- rowsum(m, group = g, reorder = TRUE)
  if (nrow(ans) != n_group)
    cli::cli_abort("Internal error: wrong number of groups.")
  unname(ans)
}


## HAS_TESTS
#' Make a Function That Recodes the Levels of a Dimension
#'
#' @param map A named list, a data frame, or a function
#'
#' @returns A function with arguments 'x' and 'nm_x'
#'
#' @noRd
make_collapse_recode <- function(map) {
  if (is.function(map)) {
    function(x, nm_x) {
      ans <- map(x)
      check_map_fun_return(x = x, ans = ans, nm_x = nm_x)
      ans
    }
  }
  else if (is.data.frame(map)) {
    check_map_df(map)
    old <- as.character(map[[1L]])
    new <- map[[2L]]
    function(x, nm_x) recode_lookup(x = x, old = old, new = new)
  }
  else if (is.list(map)) {
    check_map_list(map)
    old <- unlist(lapply(map, as.character), use.names = FALSE)
    new <- rep(names(map), times = lengths(map))
    function(x, nm_x) recode_lookup(x = x, old = old, new = new)
  }
  else
    cli::cli_abort(c("{.arg map} is not a named list, a data frame, or a function.",
                     i = "{.arg map} has class {.cls {class(map)}}."))
}


## HAS_TESTS
#' Recode a Vector Using a Lookup Table
#'
#' Values not found in 'old' are left unchanged.
#'
#' @param x A vector of levels
#' @param old Character vector of old levels
#' @param new Vector of new levels, the same length as 'old'
#'
#' @returns A vector with the same length as 'x'
#'
#' @noRd
recode_lookup <- function(x, old, new) {
  if (is.factor(new)) {
    x_chr <- as.character(x)
    i <- match(x_chr, old)
    is_found <- !is.na(i)
    levels_new <- levels(new)
    extra <- unique(x_chr[!is_found & !is.na(x_chr)])
    extra <- setdiff(extra, levels_new)
    ans <- x_chr
    ans[is_found] <- as.character(new)[i[is_found]]
    factor(ans, levels = c(levels_new, extra))
  }
  else if (is.factor(x)) {
    recode_chr <- function(y) {
      i <- match(y, old)
      is_found <- !is.na(i)
      y[is_found] <- as.character(new)[i[is_found]]
      y
    }
    levels_new <- unique(recode_chr(levels(x)))
    factor(recode_chr(as.character(x)), levels = levels_new)
  }
  else {
    i <- match(as.character(x), old)
    is_found <- !is.na(i)
    ans <- x
    ans[is_found] <- new[i[is_found]]
    ans
  }
}


## HAS_TESTS
#' Check a Named List Used as a Map
#'
#' @param map A list
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_map_list <- function(map) {
  nms <- names(map)
  if (length(map) == 0L)
    cli::cli_abort("{.arg map} has length 0.")
  if (is.null(nms) || anyNA(nms) || any(nms == ""))
    cli::cli_abort("Elements of {.arg map} do not all have names.")
  if (anyDuplicated(nms)) {
    nm_dup <- nms[duplicated(nms)][[1L]]
    cli::cli_abort("{.arg map} has two elements called {.val {nm_dup}}.")
  }
  for (nm in nms) {
    val <- map[[nm]]
    if (!is.atomic(val) && !is.factor(val))
      cli::cli_abort(c("Element {.val {nm}} of {.arg map} is not a vector.",
                       i = "Element {.val {nm}} has class {.cls {class(val)}}."))
    if (anyNA(val))
      cli::cli_abort("Element {.val {nm}} of {.arg map} has NA.")
  }
  old <- unlist(lapply(map, as.character), use.names = FALSE)
  if (anyDuplicated(old)) {
    old_dup <- old[duplicated(old)][[1L]]
    cli::cli_abort("Level {.val {old_dup}} appears more than once in {.arg map}.")
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check That Old Levels in a Named List Appear in Data
#'
#' @param map A named list
#' @param vals_dim Character vector with values
#' of the 'dim' variables
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_map_list_in_data <- function(map, vals_dim) {
  old <- unlist(lapply(map, as.character), use.names = FALSE)
  is_absent <- !(old %in% vals_dim)
  if (any(is_absent)) {
    absent <- old[is_absent]
    n <- length(absent)
    cli::cli_abort(c("{.arg map} has {cli::qty(n)} level{?s} not found in {.arg dim}: {.val {absent}}.",
                     i = "Is there a typo?"))
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check a Data Frame Used as a Map
#'
#' @param map A data frame
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_map_df <- function(map) {
  n_col <- ncol(map)
  if (n_col != 2L)
    cli::cli_abort(c("{.arg map} is a data frame but does not have 2 columns.",
                     i = "{.arg map} has {n_col} column{?s}.",
                     i = "{.arg map} should have a column of old levels and a column of new levels."))
  old <- map[[1L]]
  new <- map[[2L]]
  is_vec <- function(x) (is.atomic(x) || is.factor(x)) && !rvec::is_rvec(x)
  if (!is_vec(old) || !is_vec(new))
    cli::cli_abort("Columns of {.arg map} must be vectors.")
  if (anyDuplicated(old)) {
    old_dup <- as.character(old[duplicated(old)][[1L]])
    nm_old <- names(map)[[1L]]
    cli::cli_abort(c("Old level {.val {old_dup}} appears more than once in {.arg map}.",
                     i = "Each value in column {.var {nm_old}} of {.arg map} must be unique."))
  }
  if (anyNA(new))
    cli::cli_abort("New levels in {.arg map} have NA.")
  invisible(TRUE)
}


## HAS_TESTS
#' Check the Value Returned by a Function Used as a Map
#'
#' @param x Original levels
#' @param ans New levels
#' @param nm_x Name of the dimension
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_map_fun_return <- function(x, ans, nm_x) {
  if (!is.atomic(ans) && !is.factor(ans))
    cli::cli_abort(c("Function supplied for {.arg map} did not return a vector.",
                     i = "Return value has class {.cls {class(ans)}}."))
  n_x <- length(x)
  n_ans <- length(ans)
  if (n_ans != n_x)
    cli::cli_abort(c("Function supplied for {.arg map} returned vector with wrong length.",
                     i = "{.var {nm_x}} has length {n_x}.",
                     i = "Return value has length {n_ans}."))
  is_new_na <- is.na(ans) & !is.na(x)
  if (any(is_new_na)) {
    val <- as.character(x[is_new_na][[1L]])
    cli::cli_abort(c("Function supplied for {.arg map} returned NA for a non-NA level.",
                     i = "Level {.val {val}} of {.var {nm_x}} was converted to NA."))
  }
  n_old <- vctrs::vec_unique_count(x)
  n_pair <- vctrs::vec_unique_count(vctrs::data_frame(x = x, ans = ans))
  if (n_pair > n_old)
    cli::cli_abort(c("Function supplied for {.arg map} mapped a level of {.var {nm_x}} to more than one new level.",
                     i = "Each old level must map to a single new level."))
  invisible(TRUE)
}
