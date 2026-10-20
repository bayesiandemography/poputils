## HAS_TESTS
#' Decompose Differences in Life Expectancy
#'
#' Decompose differences in life expectancy between
#' two or more life tables into contributions from
#' each age group. Function `decompose_ex()` works with ordinary
#' life tables, and function `decompose_ex_decrement()` works with
#' multiple-decrement life tables, and decomposes differences by
#' age group and decrement, such as cause of death.
#'
#' @section Comparisons:
#'
#' The life tables being compared are stacked in a single
#' data frame, and distinguished by the variable specified
#' by `compare_along`, such as time or sex. Each value
#' of `compare_along` is compared with the value specified by
#' `compare_base`. For instance, with `compare_along = year` and
#' `compare_base = 2000`, the life tables for 2010 and 2020 are
#' each compared with the life table for 2000.
#' Contributions are to the difference between life expectancy
#' in the comparison table and life expectancy in the base table.
#'
#' To compare consecutive values, such as 2000 with 2010 and 2010
#' with 2020, stack the pairs of life tables, add a variable
#' identifying the pairs, and include that variable in `by`.
#'
#' Keeping the life tables in a single data frame means that,
#' when the life tables are [rvec][rvec::rvec()]s, each draw
#' from one life table is compared with the corresponding
#' draw from the other. When the life tables come from the
#' same model, such as a model fitted using package
#' **bage**, the decomposition then reflects correlations
#' between the two life tables.
#'
#' @section Methods:
#'
#' - `"Arriaga"` The method of Arriaga (1984). Contributions
#'   add up exactly to the difference in life expectancy.
#'   The method is not symmetric: swapping the comparison and
#'   base life tables changes the contributions,
#'   not just their sign. With `decompose_ex_decrement()`,
#'   the contribution of each age group is divided among
#'   decrements in proportion to the difference in
#'   decrement-specific rates, following Preston et al (2001).
#' - `"Horiuchi"` The method of Horiuchi et al (2008), which
#'   integrates the effect of changing rates gradually from the
#'   base life table to the comparison life table.
#'   The method is symmetric. Contributions add up to the
#'   difference in life expectancy, apart from a small numerical
#'   error. The rates are changed in 20 steps.
#'
#' The Horiuchi method recalculates life tables from rates.
#' Arguments `infant`, `child`, and `closed`
#' describe how the input life tables were constructed, and are
#' used in the recalculations. If one of these arguments is `NULL`
#' (the default), then the method is inferred, as with
#' [delete_decrements()]. When the method cannot be inferred,
#' or is `"CD"` or `"AK"`, the recalculations use the average time
#' lived by people who leave in each age group, interpolated between
#' the two life tables. With the Arriaga method, the input life
#' tables are used directly, and values for `infant`, `child`, and
#' `closed`, if supplied, are only used to check the life tables.
#'
#' With the Arriaga method, if the all-decrement rates in an age group
#' are the same in the base and comparison life tables, then the
#' contribution of the age group cannot be divided among decrements.
#' If the decrement-specific rates are also the same, the
#' contributions are 0. Otherwise, the contributions are `NaN`,
#' and `decompose_ex_decrement()` gives a warning.
#'
#' @section Output:
#'
#' The result has one row for each combination of population group,
#' value of `compare_along` other than `compare_base`, decrement cell
#' (with `decompose_ex_decrement()`), and age group at or above
#' `at`. Column `ex_contrib` holds contributions. Since contributions
#' are additive, they can be summarized using [collapse_dims()]. For
#' instance, contributions by cause of death are obtained with
#' `collapse_dims(result, count = ex_contrib, by = !age)`.
#'
#' Decrement cells that appear in some life tables but not others
#' are treated as having no exits in the life tables where they
#' do not appear.
#'
#' @section Population dimensions:
#'
#' Population dimensions, such as region, can be specified
#' using `by` or by [grouping][dplyr::group_by()] `data`.
#' Otherwise, they are inferred, as with [delete_decrements()].
#' `compare_along` is not a population dimension.
#'
#' @param data Life tables, stacked in a single data frame.
#' For `decompose_ex()`, life tables created by [lifetab()],
#' and for `decompose_ex_decrement()`, life tables created by
#' [lifetab_decrement()]. Must include columns `qx`, `lx`, `dx`,
#' and `Lx`, or, if `suffix` is used, columns such as `qx.<suffix>`.
#' @param decrement <[`tidyselect`][tidyselect::language]>
#' Decrement dimensions, such as cause of death
#' (`decompose_ex_decrement()` only).
#' @param compare_along <[`tidyselect`][tidyselect::language]>
#' Variable distinguishing the life tables being compared.
#' A single variable.
#' @param compare_base Value of `compare_along` for the
#' base life table.
#' @param method Decomposition method. Either `"Arriaga"`
#' or `"Horiuchi"`. No default.
#' @param at Age at which life expectancy is calculated.
#' Default is `0`. Must be the lower limit of an age group.
#' @param age <[`tidyselect`][tidyselect::language]>
#' Age group labels. Default is `age`.
#' @param by <[`tidyselect`][tidyselect::language]>
#' Population dimensions. If `NULL` (the default),
#' and `data` is not grouped, population dimensions
#' are inferred from `data`.
#' @param infant,child,closed Methods used to construct
#' `data`. If `NULL` (the default), the methods are inferred.
#' See [lifetab()] for the choices.
#' @param suffix Optional suffix used in the names of life table
#' columns in `data`, and added to the name of the `ex_contrib`
#' column in the result.
#' @param n_core Number of cores to use for parallel
#' processing. If `n_core` is `1` (the default),
#' no parallel processing is done.
#'
#' @returns A [tibble][tibble::tibble()].
#'
#' @seealso
#' - [lifetab()], [lifetab_decrement()] Calculate life tables
#' - [delete_decrements()] Calculate decrement-deleted life tables
#' - [collapse_dims()] Summarize contributions
#'
#' @references
#' - Arriaga EE. 1984. Measuring and explaining the change in
#' life expectancies. *Demography* 21(1): 83-96.
#' - Horiuchi S, Wilmoth JR, and Pletcher SD. 2008.
#' A decomposition method based on a model of continuous change.
#' *Demography* 45(4): 785-801.
#' - Preston SH, Heuveline P, and Guillot M. 2001.
#' *Demography: Measuring and Modeling Population Processes*
#' Oxford: Blackwell. Chapters 3 and 4.
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' library(tidyr)
#'
#' ## life tables for 2021 and 2022
#' tab <- nzl_mort |>
#'   lifetab(mx = mx, by = c(year, gender))
#'
#' ## contributions to change in life expectancy
#' ## between 2021 and 2022
#' tab |>
#'   decompose_ex(compare_along = year,
#'                compare_base = 2021,
#'                method = "Arriaga",
#'                by = gender)
#'
#' ## illustrative cause-specific death rates
#' mort_cause <- nzl_mort |>
#'   select(year, gender, age, mx) |>
#'   mutate(lower = age_lower(age),
#'          Cancer = 0.25,
#'          Circulatory = 0.7 * lower / (lower + 60),
#'          Other = 1 - Cancer - Circulatory) |>
#'   pivot_longer(c(Cancer, Circulatory, Other),
#'                names_to = "cause",
#'                values_to = "share") |>
#'   mutate(mx = mx * share) |>
#'   select(year, gender, age, cause, mx)
#'
#' ## contributions to the difference in life
#' ## expectancy between males and females,
#' ## by age and cause
#' contrib <- mort_cause |>
#'   lifetab_decrement(mx = mx,
#'                     decrement = cause,
#'                     by = c(year, gender)) |>
#'   decompose_ex_decrement(decrement = cause,
#'                          compare_along = gender,
#'                          compare_base = "Female",
#'                          method = "Horiuchi",
#'                          by = year)
#' contrib
#'
#' ## contributions by cause
#' contrib |>
#'   collapse_dims(count = ex_contrib,
#'                 by = !age)
#' @export
decompose_ex <- function(data,
                         compare_along,
                         compare_base,
                         method,
                         at = 0,
                         age = age,
                         by = NULL,
                         infant = NULL,
                         child = NULL,
                         closed = NULL,
                         suffix = NULL,
                         n_core = 1) {
  if (missing(compare_along))
    cli::cli_abort("No value supplied for {.arg compare_along}.")
  if (missing(compare_base))
    cli::cli_abort("No value supplied for {.arg compare_base}.")
  if (missing(method))
    cli::cli_abort("No value supplied for {.arg method}.")
  decompose_inner(data = data,
                  decrement_quo = NULL,
                  compare_along_quo = rlang::enquo(compare_along),
                  compare_base = compare_base,
                  method = method,
                  at = at,
                  age_quo = rlang::enquo(age),
                  by_quo = rlang::enquo(by),
                  infant = infant,
                  child = child,
                  closed = closed,
                  suffix = suffix,
                  n_core = n_core)
}


#' @rdname decompose_ex
#' @export
decompose_ex_decrement <- function(data,
                                   decrement,
                                   compare_along,
                                   compare_base,
                                   method,
                                   at = 0,
                                   age = age,
                                   by = NULL,
                                   infant = NULL,
                                   child = NULL,
                                   closed = NULL,
                                   suffix = NULL,
                                   n_core = 1) {
  if (missing(decrement))
    cli::cli_abort("No value supplied for {.arg decrement}.")
  if (missing(compare_along))
    cli::cli_abort("No value supplied for {.arg compare_along}.")
  if (missing(compare_base))
    cli::cli_abort("No value supplied for {.arg compare_base}.")
  if (missing(method))
    cli::cli_abort("No value supplied for {.arg method}.")
  decompose_inner(data = data,
                  decrement_quo = rlang::enquo(decrement),
                  compare_along_quo = rlang::enquo(compare_along),
                  compare_base = compare_base,
                  method = method,
                  at = at,
                  age_quo = rlang::enquo(age),
                  by_quo = rlang::enquo(by),
                  infant = infant,
                  child = child,
                  closed = closed,
                  suffix = suffix,
                  n_core = n_core)
}


## HAS_TESTS
#' Check Inputs and Do Decomposition, for Ordinary or
#' Multiple-Decrement Life Tables
#'
#' @param data A data frame
#' @param decrement_quo Quosure identifying decrement
#' dimensions, or NULL for ordinary life tables
#' @param compare_along_quo Quosure identifying 'compare_along'
#' @param compare_base Value for base life table
#' @param method "Arriaga" or "Horiuchi"
#' @param at Age at which life expectancy calculated
#' @param age_quo,by_quo Quosures
#' @param infant,child,closed NULL or strings
#' @param suffix NULL or string
#' @param n_core Number of cores
#'
#' @returns A tibble
#'
#' @noRd
decompose_inner <- function(data,
                            decrement_quo,
                            compare_along_quo,
                            compare_base,
                            method,
                            at,
                            age_quo,
                            by_quo,
                            infant,
                            child,
                            closed,
                            suffix,
                            n_core) {
  if (!is.data.frame(data))
    cli::cli_abort(c("{.arg data} is not a data frame.",
                     i = "{.arg data} has class {.cls {class(data)}}."))
  is_decrement <- !is.null(decrement_quo)
  methods_choices <- c("Arriaga", "Horiuchi")
  if (!is.character(method) || length(method) != 1L || !(method %in% methods_choices))
    cli::cli_abort(c("Invalid value for {.arg method}.",
                     i = "{.arg method} must be {.val {methods_choices[[1L]]}} or {.val {methods_choices[[2L]]}}."))
  methods_ax <- list(infant = check_deleted_method(infant,
                                                   nm = "infant",
                                                   choices = c("constant", "linear", "CD", "AK")),
                     child = check_deleted_method(child,
                                                  nm = "child",
                                                  choices = c("constant", "linear", "CD")),
                     closed = check_deleted_method(closed,
                                                   nm = "closed",
                                                   choices = c("constant", "linear")))
  if (!is.null(suffix))
    check_string(x = suffix, x_arg = "suffix")
  check_n(n = n_core,
          nm_n = "n_core",
          min = 1L,
          max = NULL,
          divisible_by = NULL)
  nms_data <- names(data)
  if (is_decrement)
    decrement_colnums <- tidyselect::eval_select(decrement_quo, data = data)
  else
    decrement_colnums <- structure(integer(), names = character())
  compare_along_colnum <- tidyselect::eval_select(compare_along_quo, data = data)
  age_colnum <- tidyselect::eval_select(age_quo, data = data)
  by_colnums <- tidyselect::eval_select(by_quo, data = data)
  groups_colnums <- groups_colnums(data)
  if (length(compare_along_colnum) == 0L)
    cli::cli_abort("No value supplied for {.arg compare_along}.")
  if (length(age_colnum) == 0L)
    cli::cli_abort("No value supplied for {.arg age}.")
  check_at_most_one_colnum(list(compare_along = compare_along_colnum,
                                age = age_colnum))
  if (length(by_colnums) > 0L && length(groups_colnums) > 0L)
    cli::cli_abort("Can't supply {.arg by} when {.arg data} is a grouped data frame.")
  nms_lt <- c("qx", "lx", "dx", "Lx")
  nm_ex <- "ex"
  if (!is.null(suffix)) {
    nms_lt <- paste(nms_lt, suffix, sep = ".")
    nm_ex <- paste(nm_ex, suffix, sep = ".")
  }
  is_lt_missing <- !(nms_lt %in% nms_data)
  if (any(is_lt_missing)) {
    nm_fun <- if (is_decrement) "lifetab_decrement" else "lifetab"
    cli::cli_abort(c("{.arg data} does not have life table {cli::qty(sum(is_lt_missing))} column{?s} {.var {nms_lt[is_lt_missing]}}.",
                     i = "{.arg data} should be life tables created by {.fn {nm_fun}}."))
  }
  lt_colnums <- match(nms_lt, nms_data)
  names(lt_colnums) <- nms_lt
  ex_colnum <- match(nm_ex, nms_data, nomatch = 0L)
  ex_colnum <- ex_colnum[ex_colnum > 0L]
  if (is_decrement)
    check_decrement_colnums(decrement_colnums = decrement_colnums,
                            others = list(compare_along = compare_along_colnum,
                                          age = age_colnum,
                                          lifetab = lt_colnums,
                                          by = by_colnums,
                                          groups = groups_colnums))
  check_no_overlap_colnums(list(compare_along = compare_along_colnum,
                                age = age_colnum,
                                lifetab = lt_colnums,
                                by = by_colnums,
                                groups = groups_colnums))
  for (i in c(decrement_colnums, compare_along_colnum)) {
    if (rvec::is_rvec(data[[i]]))
      cli::cli_abort("{.var {nms_data[[i]]}} is an rvec.")
  }
  compare_along <- data[[compare_along_colnum]]
  nm_compare_along <- nms_data[[compare_along_colnum]]
  compare_base <- check_compare_base(compare_base = compare_base,
                                     compare_along = compare_along,
                                     nm_compare_along = nm_compare_along)
  check_at(at = at, age = data[[age_colnum]])
  if (length(at) != 1L)
    cli::cli_abort(c("{.arg at} has length {length(at)}.",
                     i = "{.arg at} must be a single value."))
  at <- as.integer(at)
  used_colnums <- c(decrement_colnums, compare_along_colnum, age_colnum, lt_colnums, ex_colnum)
  pop_colnums <- classify_pop_colnums(data = data,
                                      used_colnums = used_colnums,
                                      by_colnums = by_colnums,
                                      groups_colnums = groups_colnums,
                                      age_colnum = age_colnum)
  data <- tibble::tibble(data)
  if (length(pop_colnums) > 0L) {
    inputs <- vctrs::vec_split(x = data, by = data[pop_colnums])
    vals <- inputs$val
    key <- inputs$key
  }
  else {
    vals <- list(data)
    key <- tibble::new_tibble(list(), nrow = 1L)
  }
  n_group <- length(vals)
  keys <- lapply(seq_len(n_group), function(i) vctrs::vec_slice(key, i))
  with_key <- function(expr_fun, i) {
    tryCatch(expr_fun(),
             error = function(cnd) {
               if (ncol(key) == 0L)
                 stop(cnd)
               str_key <- make_str_key(keys[[i]])
               cli::cli_abort(c("Problem decomposing life expectancy.",
                                i = paste("Problem occurred where", str_key)),
                              parent = cnd)
             })
  }
  groups <- lapply(seq_len(n_group), function(i) {
    with_key(function() prep_decompose_group(data = vals[[i]],
                                             decrement_colnums = decrement_colnums,
                                             compare_along_colnum = compare_along_colnum,
                                             compare_base = compare_base,
                                             age_colnum = age_colnum,
                                             lt_colnums = lt_colnums),
             i = i)
  })
  cells_missing <- unique(unlist(lapply(groups, function(g) g$cells_missing)))
  if (length(cells_missing) > 0L)
    cli::cli_alert_info(paste("{cli::qty(length(cells_missing))}Decrement cell{?s} {.val {cells_missing}}",
                              "{?does/do} not appear in every life table being compared.",
                              "Treating exits as 0 where {?it is/they are} absent."))
  preps <- unlist(lapply(groups, function(g) g$preps), recursive = FALSE)
  is_check_needed <- (method == "Horiuchi") ||
    any(vapply(methods_ax, function(x) !is.null(x), TRUE))
  if (is_check_needed)
    methods_ax <- choose_ax_methods(preps = preps, methods = methods_ax)
  compute_group <- function(g) {
    decompose_group(group = g,
                    method = method,
                    methods_ax = methods_ax,
                    at = at)
  }
  if (n_core > 1L && n_group > 1L) {
    cl <- parallel::makeCluster(n_core)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    contribs <- parallel::parLapply(cl = cl, X = groups, fun = compute_group)
  }
  else
    contribs <- lapply(groups, compute_group)
  n_nan <- sum(vapply(contribs, function(x) x$n_nan, 0L))
  if (n_nan > 0L)
    cli::cli_warn(c(paste("All-decrement rates are equal in base and comparison life tables,",
                          "but decrement-specific rates differ."),
                    i = paste("Contributions for {n_nan} combination{?s} of age group",
                              "and draw cannot be divided among decrements, and are {.val {NaN}}."),
                    i = "Consider using {.code method = \"Horiuchi\"}."))
  nm_contrib <- "ex_contrib"
  if (!is.null(suffix))
    nm_contrib <- paste(nm_contrib, suffix, sep = ".")
  ans <- lapply(seq_len(n_group), function(i) {
    ans_i <- assemble_decompose_group(group = groups[[i]],
                                      contrib = contribs[[i]]$contrib,
                                      nm_compare_along = nm_compare_along,
                                      nm_age = nms_data[[age_colnum]],
                                      nm_contrib = nm_contrib)
    key_i <- vctrs::vec_rep(keys[[i]], times = nrow(ans_i))
    vctrs::vec_cbind(key_i, ans_i)
  })
  ans <- vctrs::vec_rbind(!!!ans)
  nms_keys <- nms_data[sort(c(pop_colnums, compare_along_colnum, decrement_colnums, age_colnum))]
  ans[c(nms_keys, nm_contrib)]
}


## HAS_TESTS
#' Check 'compare_base' and Cast to Type of 'compare_along'
#'
#' @param compare_base Value supplied by user
#' @param compare_along Vector of values
#' @param nm_compare_along Name of 'compare_along' variable
#'
#' @returns 'compare_base', cast to the type of 'compare_along'
#'
#' @noRd
check_compare_base <- function(compare_base, compare_along, nm_compare_along) {
  if (length(compare_base) != 1L)
    cli::cli_abort(c("{.arg compare_base} does not have length 1.",
                     i = "{.arg compare_base} has length {length(compare_base)}."))
  if (is.na(compare_base))
    cli::cli_abort("{.arg compare_base} is {.val {NA}}.")
  ans <- tryCatch(vctrs::vec_cast(compare_base, to = vctrs::vec_ptype(compare_along)),
                  error = function(e) NULL)
  if (is.null(ans) || is.na(ans))
    cli::cli_abort(c("{.arg compare_base} is not a valid value for {.var {nm_compare_along}}.",
                     i = "{.arg compare_base} has class {.cls {class(compare_base)}}.",
                     i = "{.var {nm_compare_along}} has class {.cls {class(compare_along)}}."))
  if (!vctrs::vec_in(ans, compare_along))
    cli::cli_abort(c("{.arg compare_base} not found in {.var {nm_compare_along}}.",
                     i = "{.arg compare_base}: {.val {as.character(compare_base)}}."))
  ans
}


## HAS_TESTS
#' Prepare Life Tables for One Population Group
#'
#' Split by 'compare_along', prepare each life table using
#' 'prep_deleted_one()', check that age groups match,
#' and align decrement cells.
#'
#' @param data A tibble with rows for one population group
#' @param decrement_colnums,compare_along_colnum,age_colnum
#' Named integer vectors
#' @param compare_base Value for base life table
#' @param lt_colnums Named integer vector identifying
#' 'qx', 'lx', 'dx', and 'Lx'
#'
#' @returns A named list
#'
#' @noRd
prep_decompose_group <- function(data,
                                 decrement_colnums,
                                 compare_along_colnum,
                                 compare_base,
                                 age_colnum,
                                 lt_colnums) {
  nm_compare_along <- names(data)[[compare_along_colnum]]
  compare_along <- data[[compare_along_colnum]]
  levels <- vctrs::vec_unique(compare_along)
  if (is.factor(levels))
    levels <- levels[order(as.integer(levels))]
  if (!vctrs::vec_in(compare_base, levels))
    cli::cli_abort("{.arg compare_base} ({.val {as.character(compare_base)}}) not found in {.var {nm_compare_along}}.")
  if (length(levels) < 2L)
    cli::cli_abort(c("{.var {nm_compare_along}} has only one value.",
                     i = "Need at least two life tables to compare."))
  i_base <- vctrs::vec_match(compare_base, levels)
  levels <- vctrs::vec_c(vctrs::vec_slice(levels, i_base),
                         vctrs::vec_slice(levels, -i_base))
  preps <- lapply(seq_along(levels), function(i) {
    level <- vctrs::vec_slice(levels, i)
    is_level <- vctrs::vec_equal(compare_along, level, na_equal = TRUE)
    tryCatch(prep_deleted_one(data = vctrs::vec_slice(data, is_level),
                              decrement_colnums = decrement_colnums,
                              age_colnum = age_colnum,
                              lt_colnums = lt_colnums),
             error = function(cnd) {
               cli::cli_abort("Problem with life table where {.var {nm_compare_along}} is {.val {as.character(level)}}.",
                              parent = cnd)
             })
  })
  age_base <- preps[[1L]]$age
  for (i in seq_along(preps)[-1L]) {
    age_i <- preps[[i]]$age
    is_same <- length(age_i) == length(age_base) &&
      all(age_lower(age_i) == age_lower(age_base)) &&
      all(age_upper(age_i) == age_upper(age_base))
    if (!is_same) {
      level <- as.character(vctrs::vec_slice(levels, i))
      cli::cli_abort(c("Life tables being compared have different age groups.",
                       i = "Age groups where {.var {nm_compare_along}} is {.val {as.character(compare_base)}}: {.val {as.character(age_base)}}.",
                       i = "Age groups where {.var {nm_compare_along}} is {.val {level}}: {.val {as.character(age_i)}}."))
    }
  }
  cells <- vctrs::vec_unique(vctrs::vec_rbind(!!!lapply(preps, function(p) p$cells)))
  n_cell <- nrow(cells)
  cells_missing <- character()
  for (i in seq_along(preps)) {
    prep <- preps[[i]]
    i_cell <- vctrs::vec_match(prep$cells, cells)
    dims <- dim(prep$dx_k)
    dx_k <- array(0, dim = c(dims[[1L]], dims[[2L]], n_cell))
    dx_k[, , i_cell] <- prep$dx_k
    prep$dx_k <- dx_k
    if (length(i_cell) < n_cell) {
      is_missing <- !(seq_len(n_cell) %in% i_cell)
      missing <- vctrs::vec_slice(cells, is_missing)
      cells_missing <- c(cells_missing,
                         do.call(paste, c(lapply(missing, as.character), sep = "/")))
    }
    prep$cells <- cells
    preps[[i]] <- prep
  }
  list(levels = levels,
       preps = preps,
       cells = cells,
       cells_missing = unique(cells_missing))
}


## HAS_TESTS
#' Decompose Differences for One Population Group
#'
#' @param group Output from 'prep_decompose_group()'
#' @param method "Arriaga" or "Horiuchi"
#' @param methods_ax List or named character vector
#' of methods for age bands
#' @param at Age at which life expectancy is calculated
#'
#' @returns A list with elements 'contrib' (a list of arrays, one
#' for each comparison life table, with dimensions age, draw, cell)
#' and 'n_nan' (number of age-draw combinations where
#' contributions could not be divided among decrements)
#'
#' @noRd
decompose_group <- function(group, method, methods_ax, at) {
  preps <- group$preps
  base <- preps[[1L]]
  i_at <- match(at, age_lower(base$age))
  n_comp <- length(preps) - 1L
  contrib <- vector(mode = "list", length = n_comp)
  n_nan <- 0L
  for (i in seq_len(n_comp)) {
    comp <- preps[[i + 1L]]
    if (method == "Arriaga") {
      val <- decompose_arriaga(base = base, comp = comp, i_at = i_at)
      contrib[[i]] <- val$contrib
      n_nan <- n_nan + val$n_nan
    }
    else
      contrib[[i]] <- decompose_horiuchi(base = base,
                                         comp = comp,
                                         i_at = i_at,
                                         methods_ax = methods_ax)
  }
  list(contrib = contrib, n_nan = n_nan)
}


## HAS_TESTS
#' Arriaga Decomposition of Difference in Life Expectancy
#'
#' Contributions by age are calculated from the all-decrement
#' life tables, and then divided among decrement cells in
#' proportion to differences in decrement-specific rates.
#'
#' @param base,comp Outputs from 'prep_deleted_one()', with
#' aligned decrement cells
#' @param i_at Index of age group where life expectancy calculated
#'
#' @returns A list with elements 'contrib' (an array with
#' dimensions age, draw, cell) and 'n_nan'
#'
#' @noRd
decompose_arriaga <- function(base, comp, i_at) {
  n_age <- length(base$age)
  rows <- seq.int(from = i_at, to = n_age)
  n_row <- length(rows)
  l1 <- base$lx[rows, , drop = FALSE]
  L1 <- base$Lx[rows, , drop = FALSE]
  l2 <- comp$lx[rows, , drop = FALSE]
  L2 <- comp$Lx[rows, , drop = FALSE]
  n_draw <- max(ncol(l1), ncol(l2))
  l1 <- expand_draws(l1, n_draw)
  L1 <- expand_draws(L1, n_draw)
  l2 <- expand_draws(l2, n_draw)
  L2 <- expand_draws(L2, n_draw)
  e1 <- rev_cumsum_cols(L1) / l1
  e2 <- rev_cumsum_cols(L2) / l2
  shift_up <- function(x) rbind(x[-1L, , drop = FALSE], 0)
  l1_next <- shift_up(l1)
  e1_next <- shift_up(e1)
  e2_next <- shift_up(e2)
  l1_at <- matrix(l1[1L, ], nrow = n_row, ncol = n_draw, byrow = TRUE)
  total <- (l1 / l1_at) * (e2 - e1) - (l1_next / l1_at) * (e2_next - e1_next)
  dx_k1 <- expand_draws_array(base$dx_k[rows, , , drop = FALSE], n_draw)
  dx_k2 <- expand_draws_array(comp$dx_k[rows, , , drop = FALSE], n_draw)
  n_cell <- dim(dx_k1)[[3L]]
  if (n_cell == 1L) {
    contrib <- array(total, dim = c(n_row, n_draw, 1L))
    return(list(contrib = contrib, n_nan = 0L))
  }
  d1 <- expand_draws(base$dx[rows, , drop = FALSE], n_draw)
  d2 <- expand_draws(comp$dx[rows, , drop = FALSE], n_draw)
  m1 <- d1 / L1
  m2 <- d2 / L2
  m1_k <- dx_k1 / as.vector(L1)
  m2_k <- dx_k2 / as.vector(L2)
  diff_m <- m2 - m1
  diff_m_k <- m2_k - m1_k
  share <- diff_m_k / as.vector(diff_m)
  is_equal <- !is.na(diff_m) & diff_m == 0
  n_nan <- 0L
  if (any(is_equal)) {
    is_equal_k <- apply(abs(diff_m_k) == 0, c(1L, 2L), all)
    is_zero <- is_equal & is_equal_k
    is_nan <- is_equal & !is_equal_k
    n_nan <- sum(is_nan)
    for (k in seq_len(n_cell)) {
      share_k <- matrix(share[, , k], nrow = n_row, ncol = n_draw)
      share_k[is_zero] <- 0
      share_k[is_nan] <- NaN
      share[, , k] <- share_k
    }
  }
  contrib <- share * as.vector(total)
  list(contrib = contrib, n_nan = n_nan)
}


## HAS_TESTS
#' Horiuchi Decomposition of Difference in Life Expectancy
#'
#' Decrement-specific rates are changed linearly from the base
#' to the comparison values, in 'n_step' steps. At each step,
#' the change in life expectancy from each rate is estimated
#' by changing that rate by half a step in each direction.
#'
#' @param base,comp Outputs from 'prep_deleted_one()', with
#' aligned decrement cells
#' @param i_at Index of age group where life expectancy calculated
#' @param methods_ax Named character vector of methods for age bands
#' @param n_step Number of steps
#'
#' @returns An array with dimensions age, draw, cell
#'
#' @noRd
decompose_horiuchi <- function(base, comp, i_at, methods_ax, n_step = 20L) {
  n_age <- length(base$age)
  rows <- seq.int(from = i_at, to = n_age)
  n_row <- length(rows)
  n_draw <- max(ncol(base$lx), ncol(comp$lx))
  nx <- base$nx[rows]
  categ <- base$categ[rows]
  band <- ifelse(categ == "0", "infant", ifelse(categ == "1-4", "child", "closed"))
  method_row <- unname(methods_ax[band])
  method_row[n_row] <- "open"
  get_rates <- function(prep) {
    L <- expand_draws(prep$Lx[rows, , drop = FALSE], n_draw)
    dx_k <- expand_draws_array(prep$dx_k[rows, , , drop = FALSE], n_draw)
    dx_k / as.vector(L)
  }
  get_ax <- function(prep) {
    lx <- expand_draws(prep$lx[rows, , drop = FALSE], n_draw)
    dx <- expand_draws(prep$dx[rows, , drop = FALSE], n_draw)
    Lx <- expand_draws(prep$Lx[rows, , drop = FALSE], n_draw)
    lx_end <- rbind(lx[-1L, , drop = FALSE], 0)
    nx_closed <- nx
    nx_closed[n_row] <- 0
    (Lx - nx_closed * lx_end) / dx
  }
  m1_k <- get_rates(base)
  m2_k <- get_rates(comp)
  a1 <- get_ax(base)
  a2 <- get_ax(comp)
  a1_na <- is.na(a1)
  a2_na <- is.na(a2)
  a1[a1_na] <- a2[a1_na]
  a2[a2_na] <- a1[a2_na]
  a_default <- matrix(nx / 2, nrow = n_row, ncol = n_draw)
  a1[is.na(a1)] <- a_default[is.na(a1)]
  a2[is.na(a2)] <- a_default[is.na(a2)]
  diff_k <- m2_k - m1_k
  step_k <- diff_k / n_step
  n_cell <- dim(m1_k)[[3L]]
  contrib <- array(0, dim = dim(m1_k))
  for (s in seq_len(n_step)) {
    t <- (s - 0.5) / n_step
    m_k <- m1_k + t * diff_k
    m <- matrix(apply(m_k, c(1L, 2L), sum), nrow = n_row, ncol = n_draw)
    a_table <- a1 + t * (a2 - a1)
    for (k in seq_len(n_cell)) {
      for (i in seq_len(n_row)) {
        step_ik <- step_k[i, , k]
        if (all(!is.na(step_ik) & step_ik == 0))
          next
        m_plus <- m
        m_minus <- m
        m_plus[i, ] <- m[i, ] + step_ik / 2
        m_minus[i, ] <- m[i, ] - step_ik / 2
        e_plus <- ex_from_rates(mx = m_plus, a_table = a_table, nx = nx, method_row = method_row)
        e_minus <- ex_from_rates(mx = m_minus, a_table = a_table, nx = nx, method_row = method_row)
        contrib[i, , k] <- contrib[i, , k] + e_plus - e_minus
      }
    }
  }
  contrib
}


## HAS_TESTS
#' Calculate Life Expectancy at the Start of the First Age Group
#'
#' Used by the Horiuchi method. Closed age groups use
#' the constant or linear methods, or values of 'ax' supplied
#' through 'a_table'. The final age group uses the constant method.
#'
#' @param mx Matrix of all-decrement rates, with dimensions age, draw
#' @param a_table Matrix of 'ax' used for age groups where
#' 'method_row' is "table"
#' @param nx Widths of age groups
#' @param method_row Character vector, with "constant", "linear",
#' "table", or "open" for each age group
#'
#' @returns A numeric vector, with one element per draw
#'
#' @noRd
ex_from_rates <- function(mx, a_table, nx, method_row) {
  n_row <- nrow(mx)
  lx <- rep(1, times = ncol(mx))
  ans <- rep(0, times = ncol(mx))
  for (i in seq_len(n_row - 1L)) {
    m <- mx[i, ]
    n <- nx[[i]]
    a <- switch(method_row[[i]],
                constant = n * ax_const_prop(n * m),
                linear = rep(n / 2, times = length(m)),
                table = a_table[i, ],
                cli::cli_abort("Internal error: Invalid value for 'method_row'.")) # nocov
    q <- n * m / (1 + (n - a) * m)
    q[!is.na(m) & m == 0] <- 0
    q[!is.na(m) & is.infinite(m)] <- 1
    q <- pmin(q, 1)
    lx_end <- lx * (1 - q)
    ans <- ans + (lx - lx_end) * a + lx_end * n
    lx <- lx_end
  }
  ans + lx / mx[n_row, ]
}


## HAS_TESTS
#' Reverse Cumulative Sums of the Columns of a Matrix
#'
#' @param m A matrix
#'
#' @returns A matrix with same dimensions as 'm'
#'
#' @noRd
rev_cumsum_cols <- function(m) {
  ans <- apply(m, 2L, function(x) rev(cumsum(rev(x))))
  matrix(ans, nrow = nrow(m), ncol = ncol(m))
}


## HAS_TESTS
#' Replicate a One-Draw Array to Match a Number of Draws
#'
#' @param x Array with dimensions age, draw, cell
#' @param n_draw Number of draws needed
#'
#' @returns An array
#'
#' @noRd
expand_draws_array <- function(x, n_draw) {
  dims <- dim(x)
  if (dims[[2L]] == n_draw)
    x
  else
    x[, rep(1L, times = n_draw), , drop = FALSE]
}


## HAS_TESTS
#' Assemble Results for One Population Group
#'
#' @param group Output from 'prep_decompose_group()'
#' @param contrib List of arrays from 'decompose_group()'
#' @param nm_compare_along,nm_age,nm_contrib Names of columns
#'
#' @returns A tibble
#'
#' @noRd
assemble_decompose_group <- function(group, contrib, nm_compare_along, nm_age, nm_contrib) {
  levels <- vctrs::vec_slice(group$levels, -1L)
  cells <- group$cells
  n_cell <- nrow(cells)
  base <- group$preps[[1L]]
  n_row <- dim(contrib[[1L]])[[1L]]
  n_age <- length(base$age)
  age <- base$age[seq.int(to = n_age, length.out = n_row)]
  ans <- lapply(seq_along(contrib), function(i) {
    x <- contrib[[i]]
    n_draw <- dim(x)[[2L]]
    x <- aperm(x, perm = c(1L, 3L, 2L))
    x <- matrix(x, nrow = n_row * n_cell, ncol = n_draw)
    vals <- if (n_draw > 1L) rvec::rvec_dbl(x) else as.double(x)
    level <- vctrs::vec_rep(vctrs::vec_slice(levels, i), times = n_row * n_cell)
    cells_i <- vctrs::vec_slice(cells, rep(seq_len(n_cell), each = n_row))
    age_i <- vctrs::vec_rep(age, times = n_cell)
    cols_compare <- list(level)
    names(cols_compare) <- nm_compare_along
    cols_age_contrib <- list(age_i, vals)
    names(cols_age_contrib) <- c(nm_age, nm_contrib)
    vctrs::vec_cbind(tibble::as_tibble(cols_compare),
                     cells_i,
                     tibble::as_tibble(cols_age_contrib))
  })
  vctrs::vec_rbind(!!!ans)
}
