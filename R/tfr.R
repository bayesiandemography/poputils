
## HAS_TESTS
#' Calculate Total Fertility Rates
#'
#' @section Sex or gender variable:
#'
#' Not used in official publications, but maybe in model.
#'
#' @section Denominator:
#'
#' 1000 is common.
#'
#' @section Using rvecs to represent uncertainty:
#'
#' An [rvec][rvec::rvec()] is a 'random vector',
#' holding multiple draws from a distribution.
#' Using an rvec for the `mx` argument to
#' `lifetab()` or `lifeexp()` is a way of representing
#' uncertainty. This uncertainty is propagated
#' through to the life table values, which will
#' also be rvecs.
#'
#' @param data Data frame with age-specific fertility rates
#' and age
#' @param asfr Age-specific fertility rates.
#' Possibly an [rvec][rvec::rvec()].
#' @param age <[`tidyselect`][tidyselect::language]>
#' Age group labels. The labels must be
#' interpretable by functions
#' such as [reformat_age()] and [age_group_type()].
#' The age groups must not have gaps,
#' and the highest age group must be "closed"
#' (ie have an upper limit.)
#' @param sex <[`tidyselect`][tidyselect::language]>
#' Biological sex, with labels that can be
#' interpreted by [reformat_sex()]. 
#' @param by <[`tidyselect`][tidyselect::language]>
#' Separate total fertility rates are calculated
#' for each combination the `by` variables.
#' If `data` is a
#' [grouped](https://dplyr.tidyverse.org/reference/group_data.html)
#' data frame, then the grouping variables
#' take precedence over `by`.
#' @param denominator The denominator used to
#' calculate the rates. Default is 1. 
#' @param suffix Optional suffix added to `"tfr"`
#' column in result.
#'
#' @returns A [tibble][tibble::tibble()].
#'
#' @seealso
#' - [lifeexp()] Calculate life expectancy from age-specific
#'   mortality rates.
#'
#' @examples
#' iran_fertility |>
#'   tfr(asfr = rates,
#'       by = c(area, time),
#'       denominator = 1000)
#' @export
tfr <- function(data,
                asfr = NULL,
                age = age,
                sex = NULL,
                by = NULL,
                denominator = 1,
                suffix = NULL) {
  asfr_quo <- rlang::enquo(asfr)
  age_quo <- rlang::enquo(age)
  sex_quo <- rlang::enquo(sex)
  by_quo <- rlang::enquo(by)
  asfr_colnum <- tidyselect::eval_select(asfr_quo, data = data)
  age_colnum <- tidyselect::eval_select(age_quo, data = data)
  sex_colnum <- tidyselect::eval_select(sex_quo, data = data)
  by_colnums <- tidyselect::eval_select(by_quo, data = data)
  groups_colnums <- groups_colnums(data)
  check_tfr_colnums(asfr_colnum = asfr_colnum,
                    age_colnum = age_colnum,
                    sex_colnum = sex_colnum,
                    by_colnums = by_colnums,
                    groups_colnums = groups_colnums)
  data <- remove_existing_tfr_col(data = data,
                                  suffix = suffix)
  is_sex_supplied <- length(sex_colnum) > 0L
  is_by_supplied <- length(by_colnums) > 0L
  is_groups_supplied <- length(groups_colnums) > 0L
  by_colnums <- if (is_by_supplied) by_colnums else groups_colnums
  has_by <- length(by_colnums) > 0L
  if (has_by) {
    tfr_by <- function(val, key) {
      tryCatch(tfr_inner(data = val,
                         asfr_colnum = asfr_colnum,
                         age_colnum = age_colnum,
                         sex_colnum = sex_colnum,
                         denominator = denominator,
                         suffix = suffix),
               error = function(cnd) {
                 str_key <- make_str_key(key)
                 msg1 <- "Problem calculating total fertility rate."
                 msg2 <- paste("Problem occurred where", str_key)
                 cli::cli_abort(c(msg1, i = msg2), parent = cnd)
               })
    }
    inputs <- vctrs::vec_split(x = data, by = data[by_colnums])
    vals <- inputs$val
    key <- inputs$key
    keys <- lapply(nrow(key), function(i) key[i, , drop = FALSE])
    ans <- .mapply(FUN = lfr_by,
                   data = list(val = vals, key = keys),
                   MoreArgs = list())
    ans <- vctrs::vec_cbind(key, ans)
  }
  else {
    ans <- tfr_inner(data = data,
                     asfr_colnum = asfr_colnum,
                     age_colnum = age_colnum,
                     sex_colnum = sex_colnum,
                     denominator = denominator,
                     suffix = suffix)
  }
  ans
}


make_age_sex_tfr <- function(age, sex) {
  has_sex <- !is.null(sex)
  if (has_sex) {
    age <- split(age, sex)


  }
  else {
    
    
  


tfr_inner <- function(data,
                      asfr_colnum,
                      age_colnum,
                      sex_colnum,
                      denominator, suffix) {
  asfr <- data[[asfr_colnum]]
  check_numeric(x = asfr,
                x_arg = "asfr",
                check_na = TRUE,
                check_positive = FALSE,
                check_nonneg = TRUE,
                check_whole = FALSE)
  age <- data[[age_colnum]]
  has_sex <- length(sex_colnum) > 0L
  sex <- if (has_sex) data[[sex_colnum]] else NULL
  age_sex <- make_age_sex_tfr(age = age, sex = sex)
  check_number(x = denominator,
               x_arg = "denominator",
               check_na = TRUE,
               check_positive = TRUE,
               check_nonneg = FALSE,
               check_whole = FALSE)
  check_string(x = suffix,
               x_arg = "suffix")
  width <- age_upper(age) - age_lower(age)
  ans <- sum(asfr * width) / denominator
  ans <- tibble::tibble(tfr = ans)
  if (!is.null(suffix))
    names(ans) <- paste(names(ans), suffix, sep = ".")
  ans
}

      
      



    check_age_tfr <- function(age) {
      check_age(x = age,
                complete = TRUE,
                unique = TRUE,
                zero = FALSE)
        age_upper <- age_upper(age)
        is_open <- is.infinite(age_upper)
        i_open <- match(TRUE, is_open, nomatch = 0L)
        if (i_open > 0L)
          cli::cli_abort(c("{.arg age} has open age group.",
                           i = "Open age group: {.val age[[i_open]]}."))
        invisible(TRUE)
      }

