
#' Derive Age-Specific Fertility Rates that
#' Match Total Fertility Rates by Scaling
#'
#' Turn total fertility rates (TFRs) into sets
#' of age-specific fertility rates, by scaling
#' a set of standard rates upwards or downwards.
#'
#' @section Method:
#'
#' The age-specific rates are derived by finding
#' a value \eqn{\alpha} such that
#' 
#' \deqn{f_x = \alpha f_x^{\mathrm{std}}}
#'
#' and
#'
#' \deqn{sum_x f_x = F}
#'
#' where
#'
#' - \eqn{f_x} is the age-specific fertility rate;
#' - \eqn{f_x^{\mathrm{std}}} is the standard schedule of rates;
#' - \eqn{\alpha} is a multiplier shared by all age groups; and
#' - \eqn{F} is the target total fertility rate.
#'
#' @section `target` argument:
#'
#' `target` is a data frame specifying
#' total fertility rates for each population being modelled,
#' Values in `target` are not age-specific.
#'
#' Variables in `target`:
#'
#' - A variable called `"tfr"`. Can be an ordinary
#'   numeric variable, or an [rvec()][rvec].
#' - Optionally, 'by' variables distinguishing
#'   populations, such as `"region"` or `"time"`.
#'
#' @section `standard` argument:
#'
#' `standard` is a data frame specifying
#' standard fertility scedules to be used
#' with each life expectancy
#' in `target`. Values in `standard` are age-specific.
#'
#' Variables in `standard`:
#' 
#' - A variable called `"age"`, with labels that
#'   can be parsed by [reformat_age()].
#' - A variable called `"value"`, containing
#'   non-negative values.  Cannot be an rvec.
#' - Additional variables used to match rows in `standard`
#'   to rows in `target`.
#'
#' Internally, `standard` is merged with
#' `target` using a left join from `target`,
#' on any variables that `target`
#' and `standard` have in common.
#' 
#'
#' @param target A data frame containing a variable called
#' `"tfr"`, and possibly others. See Details.
#' @param standard A data frame containing variables
#' called `age` and `asfr`, and possibly others.
#' See details.
#' @param suffix Optional suffix added to
#' `asfr` column in results.
#'
#' @returns A [tibble][tibble::tibble()].
#'
#' @seealso
#' - [ex_to_lifetab_brass()] Life table equivalent
#'   of `tfr_to_asfr_scale()`.
#' - [booth_standard] The 'Booth standard' fertility schedule
#' - [tfr] Calculate total fertility rate from age-specific
#'   fertility rates
#'
#' @examples
#' ## create age-specific fertility rates
#' ## based on the [Booth standard][booth_standard]
#' library(dplyr, warn.conflicts = FALSE)
#' target <- data.frame(region = c("A", "B"), 
#'                      tfr = c(5.5, 4.7))
#' asfr <- tfr_to_asfr_scale(target = target,
#'                           standard = booth_standard)
#' asfr
#'
#' ## check consistency with original TFRs
#' asfr |>
#'   tfr(asfr = asfr, by = region)
#' @export
tfr_to_asfr_scale <- function(target,
                              standard,
                              suffix = NULL) {
  ## check inputs
  check_target_tfr_to_asfr(target)
  check_standard_tfr_to_asfr(standard)
  if (!is.null(suffix))
    check_string(suffix, x_arg = "suffix")
  combined <- combine_target_standard_tfr_to_asfr(target = target,
                                                  standard = standard)
  key <- combined$key
  n_val <- nrow(combined)
  ans_val <- vector(mode = "list", length = n_val)
  for (i in seq_len(n_val)) {
    val_i <- combined$val[[i]]
    ans_val[[i]] <- tfr_to_asfr_scale_one(val = val_i,
                                          suffix = suffix)
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
#' Includes checking for gaps in 'standard',
#'
#' @param target,standard Data frames
#'
#' @returns A data frame with columns 'key' and 'value'
#'
#' @noRd
combine_target_standard_tfr_to_asfr <- function(target, standard) {
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
  nms_val <- c("tfr", "age", "value")
  nms_key <- setdiff(nms_all, nms_val)
  vctrs::vec_split(ans[nms_val], ans[nms_key])
}


#' Calculate ASFR, Given Processed Inputs
#'
#' @param val Data frame with following columns:
#' - tfr Numeric vector of life expectancies at birth
#' - value Fertility standard
#' - age Labels for age groups
#' @param suffix Suffix added to columns of life table,
#' or NULL.
#'
#' @returns A data frame
#'
#' @noRd
tfr_to_asfr_scale_one <- function(val, suffix) {
  tfr <- val$tfr[[1L]]
  age <- val$age
  value <- val$value
  width <- age_upper(age) - age_lower(age)
  asfr <- (tfr * value) / (sum(value) * width)
  nm <- "asfr"
  if (!is.null(suffix))
    nm <- paste(nm, suffix, sep = ".")
  val[[nm]] <- asfr
  i_remove <- match(c("tfr", "value"), names(val))
  val <- val[-i_remove]
  val
}

