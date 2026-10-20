## HAS_TESTS
#' Delete Decrements from a Multiple-Decrement Life Table
#'
#' Calculate 'decrement-deleted' life tables (also called
#' 'cause-deleted' life tables), showing what
#' the life table would look like if one decrement,
#' such as a cause of death, were removed and the
#' other decrements were unchanged.
#' Each decrement cell is deleted in turn.
#'
#' The input to `delete_decrements()` is a multiple-decrement
#' life table, in the format produced by [lifetab_decrement()].
#' Each cell in the input gives one ordinary life table
#' in the output.
#'
#' @section Output:
#'
#' The result has one row for each combination of population
#' group, deleted decrement cell, and age group.
#' Decrement variables are renamed by adding `"_deleted"`,
#' so that, for instance, `cause` becomes `cause_deleted`.
#' The life table columns, `qx`, `lx`, `dx`, `Lx`, and `ex`, describe
#' an ordinary, single-decrement life table, for a population in
#' which exits through the deleted cell do not occur. The life tables
#' have the same radix as the input.
#'
#' @section Calculation methods:
#'
#' `delete_decrements()` assumes that, within each age group,
#' the rates for different decrements are proportional to
#' each other, as in [lifetab_decrement()],
#' and that deleting one decrement does not change the others.
#' The second assumption cannot be checked from data
#' (Tsiatis 1975), and is often unrealistic. Deleting
#' cardiovascular disease, for instance, may change the
#' risk of dying from other causes.
#'
#' With proportional rates, the probability of
#' surviving an age group when decrement `k` has been deleted is
#' \eqn{p_x^{-k} = p_x^{R_x}}, where \eqn{p_x} is the all-decrement
#' probability of surviving and \eqn{R_x} is the share of exits in age
#' group `x` that are not due to `k`.
#'
#' Person-years lived, `Lx`, depend on how rates vary
#' within age groups, which is controlled by arguments
#' `infant`, `child`, and `closed`.
#'
#' - `"constant"` and `"linear"` give exact results under the
#'   corresponding assumptions about rates within age groups,
#'   as described in [lifetab()].
#' - `"CD"` and `"AK"` give results based on the formula for
#'   cause-deleted life tables in Preston et al (2001),
#'   which uses the average time lived by people who leave
#'   in the age group in the input life table.
#'
#' The final, open age group assumes constant rates.
#'
#' If `infant`, `child`, or `closed` is `NULL` (the default),
#' then `delete_decrements()` checks whether the input life
#' table is consistent with the `"constant"` method or the
#' `"linear"` method in that age band, for all population groups
#' and draws, and if so, uses that method. Otherwise it uses
#' the formula from Preston et al (2001).
#' A life table produced by `lifetab_decrement()` with its
#' default methods is recognized as `"constant"`.
#' Supplying values for `infant`, `child`, and `closed`
#' makes results more predictable, and is recommended for
#' production code. If the value is `"constant"` or `"linear"`, then
#' `delete_decrements()` checks that the input life table is
#' consistent with that method.
#'
#' With the `"constant"` method, deleting decrement `k` gives the
#' same result as calculating an ordinary life table with [lifetab()],
#' using the all-decrement rate minus the rate for `k`.
#'
#' If all exits in the final age group are due to the
#' deleted decrement, then no one ever leaves the
#' final age group, and `Lx` and `ex` are `Inf`.
#'
#' @section Population dimensions:
#'
#' Population dimensions, such as sex or region, can be specified
#' using `by` or by [grouping][dplyr::group_by()] `data`.
#' Otherwise, `delete_decrements()` treats a variable as a population
#' dimension if each of its values appears with every age group.
#' Variables that are not population dimensions, decrement
#' dimensions, `age`, or life table columns, are dropped,
#' with a message. Specifying `by` explicitly is recommended
#' for production code.
#'
#' @section Using rvecs to represent uncertainty:
#'
#' An [rvec][rvec::rvec()] is a 'random vector',
#' holding multiple draws from a distribution.
#' If the life table columns in `data` are rvecs, then
#' the life table columns in the result are also rvecs.
#'
#' @param data A multiple-decrement life table, typically
#' created by [lifetab_decrement()]. Must include
#' columns `qx`, `lx`, `dx`, and `Lx`, or, if `suffix` is
#' used, columns such as `qx.<suffix>`.
#' @param decrement <[`tidyselect`][tidyselect::language]>
#' Decrement dimensions, such as cause of death.
#' One or more variables.
#' @param age <[`tidyselect`][tidyselect::language]>
#' Age group labels. Default is `age`.
#' @param by <[`tidyselect`][tidyselect::language]>
#' Population dimensions. If `NULL` (the default),
#' and `data` is not grouped, population dimensions
#' are inferred from `data`.
#' @param infant,child,closed Methods used to construct
#' `data`, for age group `"0"`, age group `"1-4"`, and other closed
#' age groups. If `NULL` (the default), the method is inferred.
#' See [lifetab()] for the choices.
#' @param suffix Optional suffix used in the names of life table
#' columns in `data`, and added to the names of life
#' table columns in the result.
#'
#' @returns A [tibble][tibble::tibble()].
#'
#' @seealso
#' - [lifetab_decrement()] Calculate a multiple-decrement life table
#' - [lifetab()] Calculate an ordinary life table
#'
#' @references
#' - Preston SH, Heuveline P, and Guillot M. 2001.
#' *Demography: Measuring and Modeling Population Processes*
#' Oxford: Blackwell. Chapter 4.
#' - Chiang CL. 1968. *Introduction to Stochastic
#' Processes in Biostatistics*. New York: Wiley.
#' - Tsiatis A. 1975. A nonidentifiability aspect of the
#' problem of competing risks. *Proceedings of the National
#' Academy of Sciences* 72(1): 20-22.
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' library(tidyr)
#'
#' ## illustrative cause-specific death rates
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
#'
#' ## cause-deleted life tables
#' deleted <- tab |>
#'   delete_decrements(decrement = cause,
#'                     by = gender)
#' deleted
#'
#' ## life expectancy at birth
#' ## with each cause deleted
#' deleted |>
#'   filter(age == "Infant") |>
#'   select(gender, cause_deleted, ex)
#' @export
delete_decrements <- function(data,
                              decrement,
                              age = age,
                              by = NULL,
                              infant = NULL,
                              child = NULL,
                              closed = NULL,
                              suffix = NULL) {
  if (!is.data.frame(data))
    cli::cli_abort(c("{.arg data} is not a data frame.",
                     i = "{.arg data} has class {.cls {class(data)}}."))
  if (missing(decrement))
    cli::cli_abort("No value supplied for {.arg decrement}.")
  methods <- list(infant = check_deleted_method(infant,
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
  decrement_colnums <- tidyselect::eval_select(rlang::enquo(decrement), data = data)
  age_colnum <- tidyselect::eval_select(rlang::enquo(age), data = data)
  by_colnums <- tidyselect::eval_select(rlang::enquo(by), data = data)
  groups_colnums <- groups_colnums(data)
  if (length(age_colnum) == 0L)
    cli::cli_abort("No value supplied for {.arg age}.")
  check_at_most_one_colnum(list(age = age_colnum))
  if (length(by_colnums) > 0L && length(groups_colnums) > 0L)
    cli::cli_abort("Can't supply {.arg by} when {.arg data} is a grouped data frame.")
  nms_lt <- c("qx", "lx", "dx", "Lx")
  if (!is.null(suffix))
    nms_lt <- paste(nms_lt, suffix, sep = ".")
  nms_data <- names(data)
  is_lt_missing <- !(nms_lt %in% nms_data)
  if (any(is_lt_missing))
    cli::cli_abort(c("{.arg data} does not have life table {cli::qty(sum(is_lt_missing))} column{?s} {.var {nms_lt[is_lt_missing]}}.",
                     i = "{.arg data} should be a life table created by {.fn lifetab_decrement}."))
  lt_colnums <- match(nms_lt, nms_data)
  names(lt_colnums) <- nms_lt
  is_by_supplied <- length(by_colnums) > 0L
  is_groups_supplied <- length(groups_colnums) > 0L
  pop_colnums <- if (is_by_supplied) by_colnums else groups_colnums
  check_decrement_colnums(decrement_colnums = decrement_colnums,
                          others = list(age = age_colnum,
                                        lifetab = lt_colnums,
                                        by = by_colnums,
                                        groups = groups_colnums))
  check_no_overlap_colnums(list(age = age_colnum,
                                lifetab = lt_colnums,
                                pop = pop_colnums))
  for (i in decrement_colnums) {
    if (rvec::is_rvec(data[[i]]))
      cli::cli_abort("Decrement variable {.var {nms_data[[i]]}} is an rvec.")
  }
  used <- c(decrement_colnums, age_colnum, lt_colnums)
  other_colnums <- setdiff(seq_along(data), c(used, pop_colnums))
  is_inferred <- !is_by_supplied && !is_groups_supplied
  if (is_inferred) {
    is_pop <- vapply(other_colnums,
                     function(i) is_pop_dim(x = data[[i]], age = data[[age_colnum]]),
                     TRUE)
    pop_colnums <- other_colnums[is_pop]
    names(pop_colnums) <- nms_data[pop_colnums]
    other_colnums <- other_colnums[!is_pop]
    if (length(pop_colnums) > 0L)
      cli::cli_alert_info(paste("Using population {cli::qty(length(pop_colnums))}dimension{?s}",
                                "{.var {names(pop_colnums)}}.",
                                "Supply {.arg by} to set population dimensions explicitly."))
    else
      cli::cli_alert_info("Treating {.arg data} as a single population.")
  }
  for (i in pop_colnums) {
    if (rvec::is_rvec(data[[i]]))
      cli::cli_abort("Population dimension {.var {nms_data[[i]]}} is an rvec.")
  }
  if (length(other_colnums) > 0L) {
    nms_other <- nms_data[other_colnums]
    reason <- if (is_inferred) "not a population dimension"
              else if (is_by_supplied) "not a {.arg by} variable"
              else "not a grouping variable"
    cli::cli_alert_info(paste("Dropping {cli::qty(length(nms_other))}column{?s} {.var {nms_other}},",
                              paste0("which {?is/are} ", reason, ".")))
  }
  data <- tibble::tibble(data)
  pop_colnums <- sort(pop_colnums)
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
               cli::cli_abort(c("Problem deleting decrements.",
                                i = paste("Problem occurred where", str_key)),
                              parent = cnd)
             })
  }
  preps <- lapply(seq_len(n_group), function(i) {
    with_key(function() prep_deleted_one(data = vals[[i]],
                                         decrement_colnums = decrement_colnums,
                                         age_colnum = age_colnum,
                                         lt_colnums = lt_colnums),
             i = i)
  })
  methods <- choose_deleted_methods(preps = preps, methods = methods)
  ans <- lapply(seq_len(n_group), function(i) {
    ans_i <- make_deleted_one(prep = preps[[i]],
                              methods = methods,
                              nm_age = nms_data[[age_colnum]],
                              suffix = suffix)
    key_i <- vctrs::vec_rep(keys[[i]], times = nrow(ans_i))
    vctrs::vec_cbind(key_i, ans_i)
  })
  vctrs::vec_rbind(!!!ans)
}


## HAS_TESTS
#' Check a Method Argument for 'delete_decrements()'
#'
#' @param x NULL or a string
#' @param nm Name of the argument
#' @param choices Valid values
#'
#' @returns 'x'
#'
#' @noRd
check_deleted_method <- function(x, nm, choices) {
  if (is.null(x))
    return(NULL)
  if (!is.character(x) || length(x) != 1L || is.na(x) || !(x %in% choices))
    cli::cli_abort(c("Invalid value for {.arg {nm}}.",
                     i = "{.arg {nm}} must be {.val {NULL}} or one of {.val {choices}}."))
  x
}


## HAS_TESTS
#' Test Whether a Variable Looks Like a Population Dimension
#'
#' A variable is treated as a population dimension if it is
#' not an rvec, and if each of its values appears with every
#' age group.
#'
#' @param x A vector
#' @param age Age labels
#'
#' @returns TRUE or FALSE
#'
#' @noRd
is_pop_dim <- function(x, age) {
  if (rvec::is_rvec(x))
    return(FALSE)
  age <- as.character(age)
  n_age <- length(unique(age))
  pairs <- vctrs::vec_unique(vctrs::data_frame(x = x, age = age))
  n_age_per_x <- vctrs::vec_count(pairs$x)$count
  all(n_age_per_x == n_age)
}


## HAS_TESTS
#' Prepare a Multiple-Decrement Life Table for One Population Group
#'
#' Sort rows, check the table, and form all-decrement totals.
#'
#' @param data A tibble with rows for one population group
#' @param decrement_colnums,age_colnum Named integer vectors
#' identifying columns
#' @param lt_colnums Named integer vector identifying
#' 'qx', 'lx', 'dx', and 'Lx'
#'
#' @returns A named list
#'
#' @noRd
prep_deleted_one <- function(data, decrement_colnums, age_colnum, lt_colnums) {
  cell <- vctrs::vec_group_id(data[decrement_colnums])
  n_cell <- attr(cell, "n")
  cell <- as.integer(cell)
  age <- data[[age_colnum]]
  check_age(x = age,
            complete = FALSE,
            unique = FALSE,
            zero = TRUE,
            open = TRUE)
  ord <- order(cell, age_lower(age))
  data <- vctrs::vec_slice(data, ord)
  cell <- cell[ord]
  age <- data[[age_colnum]]
  age_ref <- age[cell == 1L]
  cells <- vctrs::vec_slice(data[decrement_colnums], match(seq_len(n_cell), cell))
  check_decrement_rectangle(age = age,
                            cell = cell,
                            age_ref = age_ref,
                            cells = cells[cell, , drop = FALSE])
  check_age(x = age_ref,
            complete = TRUE,
            unique = TRUE,
            zero = TRUE,
            open = TRUE)
  n_age <- length(age_ref)
  to_array <- function(x) {
    nm <- names(lt_colnums)[[match(x, lt_colnums)]]
    vals <- data[[x]]
    check_numeric(x = vals,
                  x_arg = nm,
                  check_na = FALSE,
                  check_positive = FALSE,
                  check_nonneg = TRUE,
                  check_whole = FALSE)
    vals <- as.matrix(vals)
    n_draw <- ncol(vals)
    vals <- array(vals, dim = c(n_age, n_cell, n_draw))
    aperm(vals, perm = c(1L, 3L, 2L))
  }
  qx_k <- to_array(lt_colnums[[1L]])
  lx_k <- to_array(lt_colnums[[2L]])
  dx_k <- to_array(lt_colnums[[3L]])
  Lx_k <- to_array(lt_colnums[[4L]])
  n_draws <- c(dim(qx_k)[[2L]], dim(lx_k)[[2L]], dim(dx_k)[[2L]], dim(Lx_k)[[2L]])
  if (length(unique(n_draws)) > 1L)
    cli::cli_abort("Life table columns have different numbers of draws.")
  sum_cells <- function(x) matrix(apply(x, c(1L, 2L), sum), nrow = n_age)
  lx <- sum_cells(lx_k)
  dx <- sum_cells(dx_k)
  Lx <- sum_cells(Lx_k)
  check_deleted_qx_denominator(qx_k = qx_k, dx_k = dx_k, lx = lx, age = age_ref)
  nx <- age_upper(age_ref) - age_lower(age_ref)
  list(age = age_ref,
       categ = age_group_categ(age_ref),
       nx = nx,
       cells = cells,
       lx = lx,
       dx = dx,
       Lx = Lx,
       dx_k = dx_k)
}


## HAS_TESTS
#' Check That Crude Probabilities Imply the Total of 'lx'
#'
#' Crude probabilities 'qx' use the all-decrement 'lx' as the
#' denominator, so 'dx / qx' should equal the sum of 'lx' across
#' cells. If it does not, cells are missing, or rows from different
#' populations have been combined.
#'
#' @param qx_k,dx_k Arrays with dimensions age, draw, cell
#' @param lx Matrix of all-decrement 'lx', with dimensions age, draw
#' @param age Age labels
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_deleted_qx_denominator <- function(qx_k, dx_k, lx, age) {
  implied <- dx_k / qx_k
  lx_rep <- array(lx, dim = dim(implied))
  is_used <- !is.na(qx_k) & qx_k > 0 & !is.na(implied) & !is.na(lx_rep)
  tol <- 1e-6
  is_bad <- is_used & abs(implied - lx_rep) > tol * pmax(abs(lx_rep), 1e-12)
  if (any(is_bad)) {
    i_age <- which(apply(is_bad, 1L, any))[[1L]]
    age_bad <- as.character(age[[i_age]])
    cli::cli_abort(c("Life table columns are not consistent with a complete set of decrement cells.",
                     i = "{.var dx} / {.var qx} should equal the sum of {.var lx} across decrement cells.",
                     i = "Problem with age group {.val {age_bad}}.",
                     i = paste("Are some decrement cells missing, or does a population",
                               "dimension need to be added to {.arg by}?")))
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Choose Methods for Each Age Band
#'
#' For each age band, check whether the life tables are consistent
#' with the "constant" and "linear" methods, across all
#' population groups and draws. If a method was supplied, check it.
#' If not, choose "constant", then "linear", then "preston".
#'
#' @param preps List of outputs from 'prep_deleted_one()'
#' @param methods Named list with elements 'infant', 'child',
#' and 'closed', each NULL or a string
#'
#' @returns A named character vector with values
#' "constant", "linear", or "preston"
#'
#' @noRd
choose_deleted_methods <- function(preps, methods) {
  bands <- c("infant", "child", "closed")
  consistent <- lapply(bands, function(band) c(constant = TRUE, linear = TRUE))
  names(consistent) <- bands
  first_bad <- list()
  for (prep in preps) {
    implied <- implied_ax_fit(prep)
    for (band in bands) {
      is_band <- implied$band == band
      if (any(is_band)) {
        for (method in c("constant", "linear")) {
          ok <- implied[[method]][is_band, , drop = FALSE]
          if (!all(ok)) {
            consistent[[band]][[method]] <- FALSE
            key <- paste(band, method)
            if (is.null(first_bad[[key]])) {
              i_age <- which(rowSums(!ok) > 0L)[[1L]]
              first_bad[[key]] <- as.character(prep$age[is_band][[i_age]])
            }
          }
        }
      }
    }
  }
  ans <- character()
  for (band in bands) {
    val <- methods[[band]]
    if (is.null(val)) {
      if (consistent[[band]][["constant"]])
        val <- "constant"
      else if (consistent[[band]][["linear"]])
        val <- "linear"
      else
        val <- "preston"
    }
    else if (val %in% c("constant", "linear")) {
      if (!consistent[[band]][[val]]) {
        age_bad <- first_bad[[paste(band, val)]]
        cli::cli_abort(c("{.arg {band}} is {.val {val}}, but the life table is not consistent with method {.val {val}}.",
                         i = "Problem with age group {.val {age_bad}}.",
                         i = "Was the life table created using a different method?"))
      }
    }
    else
      val <- "preston"
    ans[[band]] <- val
  }
  ans
}


## HAS_TESTS
#' Check Whether Implied 'ax' Fits the Constant and Linear Methods
#'
#' Compare the average time lived by people leaving in each
#' closed age group, implied by the life table, with the values
#' implied by the constant and linear methods. Age groups where the
#' two methods give almost the same values, or where there are
#' no exits, are treated as consistent with both.
#'
#' @param prep Output from 'prep_deleted_one()'
#'
#' @returns A list with element 'band' (a character vector, one
#' element per closed age group) and logical matrices 'constant'
#' and 'linear' (dimensions closed age group, draw)
#'
#' @noRd
implied_ax_fit <- function(prep) {
  n_age <- length(prep$age)
  i_closed <- seq_len(n_age - 1L)
  categ <- prep$categ[i_closed]
  band <- ifelse(categ == "0", "infant", ifelse(categ == "1-4", "child", "closed"))
  nx <- prep$nx[i_closed]
  lx <- prep$lx[i_closed, , drop = FALSE]
  lx_end <- prep$lx[i_closed + 1L, , drop = FALSE]
  dx <- prep$dx[i_closed, , drop = FALSE]
  Lx <- prep$Lx[i_closed, , drop = FALSE]
  ax <- (Lx - nx * lx_end) / dx
  qx <- dx / lx
  ax_const <- nx * ax_const_prop(-log1p(-qx))
  ax_lin <- matrix(nx / 2, nrow = nrow(ax), ncol = ncol(ax))
  tol <- 1e-6 * nx
  is_informative <- !is.na(ax) & !is.na(ax_const) & dx > 0 & lx > 0 &
    abs(ax_const - ax_lin) > 100 * tol
  fits <- function(target) !is_informative | (abs(ax - target) <= tol)
  list(band = band,
       constant = fits(ax_const),
       linear = fits(ax_lin))
}


## HAS_TESTS
#' Proportion of an Interval Lived by People Who Die in It,
#' With Constant Mortality
#'
#' R version of the function in lifetab.cpp.
#' Equals 1/x - 1/(exp(x) - 1), where x = nx * mx.
#'
#' @param x Numeric vector or matrix
#'
#' @returns Object with same dimensions as 'x'
#'
#' @noRd
ax_const_prop <- function(x) {
  ans <- 1 / x - 1 / expm1(x)
  is_small <- !is.na(x) & x < 1e-4
  ans[is_small] <- 0.5 - x[is_small] / 12 + x[is_small]^3 / 720
  ans
}


## HAS_TESTS
#' Construct Decrement-Deleted Life Tables for One Population Group
#'
#' @param prep Output from 'prep_deleted_one()'
#' @param methods Named character vector from 'choose_deleted_methods()'
#' @param nm_age Name of age variable
#' @param suffix Suffix for life table columns, or NULL
#'
#' @returns A tibble
#'
#' @noRd
make_deleted_one <- function(prep, methods, nm_age, suffix) {
  age <- prep$age
  n_age <- length(age)
  n_cell <- nrow(prep$cells)
  n_draw <- ncol(prep$lx)
  lx <- prep$lx
  dx <- prep$dx
  Lx <- prep$Lx
  nx <- prep$nx
  categ <- prep$categ
  band <- ifelse(categ == "0", "infant", ifelse(categ == "1-4", "child", "closed"))
  method_age <- unname(methods[band])
  method_age[n_age] <- "open"
  qx <- dx / lx
  ans <- list(qx = vector("list", n_cell),
              lx = vector("list", n_cell),
              dx = vector("list", n_cell),
              Lx = vector("list", n_cell),
              ex = vector("list", n_cell))
  for (k in seq_len(n_cell)) {
    dx_k <- matrix(prep$dx_k[, , k], nrow = n_age, ncol = n_draw)
    R <- 1 - dx_k / dx
    R[!is.na(dx) & dx == 0] <- 1
    lx_del <- matrix(NA_real_, nrow = n_age, ncol = n_draw)
    Lx_del <- matrix(NA_real_, nrow = n_age, ncol = n_draw)
    lx_del[1L, ] <- lx[1L, ]
    for (i in seq_len(n_age)) {
      l_i <- lx_del[i, ]
      R_i <- R[i, ]
      is_zero <- !is.na(l_i) & l_i == 0
      if (i < n_age) {
        q_i <- qx[i, ]
        p_del <- (1 - q_i)^R_i
        lx_del[i + 1L, ] <- ifelse(is_zero, 0, l_i * p_del)
        L_i <- deleted_Lx_closed(l = l_i,
                                 q = q_i,
                                 R = R_i,
                                 nx = nx[[i]],
                                 L_over_l = Lx[i, ] / lx[i, ],
                                 method = method_age[[i]])
      }
      else
        L_i <- l_i * (Lx[i, ] / lx[i, ]) / R_i
      Lx_del[i, ] <- ifelse(is_zero, 0, L_i)
    }
    ans$qx[[k]] <- lx_to_qx(lx_del)
    ans$lx[[k]] <- lx_del
    ans$dx[[k]] <- lx_to_dx(lx_del)
    ans$Lx[[k]] <- Lx_del
    ans$ex[[k]] <- Lx_to_ex(Lx = Lx_del, lx = lx_del)
  }
  to_col <- function(x) {
    x <- do.call(rbind, x)
    if (n_draw > 1L)
      rvec::rvec_dbl(x)
    else
      as.double(x)
  }
  ans <- lapply(ans, to_col)
  if (!is.null(suffix))
    names(ans) <- paste(names(ans), suffix, sep = ".")
  cells <- vctrs::vec_slice(prep$cells, rep(seq_len(n_cell), each = n_age))
  names(cells) <- paste0(names(cells), "_deleted")
  age_col <- list(vctrs::vec_rep(age, times = n_cell))
  names(age_col) <- nm_age
  vctrs::vec_cbind(cells,
                   tibble::as_tibble(age_col),
                   tibble::as_tibble(ans))
}


## HAS_TESTS
#' Person-Years Lived in a Closed Age Group When a Decrement is Deleted
#'
#' @param l Number alive at start of age group, in the deleted table
#' @param q All-decrement probability of leaving in original table
#' @param R Share of exits not due to deleted decrement
#' @param nx Width of age group
#' @param L_over_l Person-years per person alive at start of age group,
#' in original table
#' @param method "constant", "linear", or "preston"
#'
#' @returns Numeric vector
#'
#' @noRd
deleted_Lx_closed <- function(l, q, R, nx, L_over_l, method) {
  if (method == "constant") {
    x <- -log1p(-q)
    x_del <- ifelse(R == 0, 0, R * x)
    prop <- ifelse(x_del == 0, 1, -expm1(-x_del) / x_del)
    nx * l * prop
  }
  else if (method == "linear") {
    prop <- ifelse(q == 0, 1, (1 - (1 - q)^(R + 1)) / (q * (R + 1)))
    nx * l * prop
  }
  else if (method == "preston") {
    L_over_l <- ifelse(is.na(L_over_l) & !is.na(q), nx, L_over_l)
    l * (nx - R * (nx - L_over_l))
  }
  else
    cli::cli_abort("Internal error: Invalid value for {.arg method}.") # nocov
}
