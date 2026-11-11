
## HAS_TESTS
#' Calculate Stable Age or Age-Sex Composition
#'
#' Calculate the age composition, or age-sex composition, of a
#' stable population: the composition that a closed population
#' would eventually reach if it were subject to constant fertility
#' and mortality indefinitely.
#'
#' In a stable population with intrinsic growth rate \eqn{r}, the
#' share of the population in age group \eqn{a} is proportional to
#' \deqn{e^{-r x_a} L_a,}
#' where \eqn{x_a} is the representative age of age group \eqn{a}
#' (typically the midpoint) and \eqn{L_a} is the life table
#' quantity \eqn{{}_nL_x} for that age group, with radix
#' \eqn{l_0 = 1}.
#'
#' - `.stable_composition_one_sex()` returns shares that sum to 1
#'   over age groups.
#' - `.stable_composition_two_sex()` splits births between females
#'   and males using `sex_ratio`, the number of male births per 100
#'   female births, and returns shares that sum to 1 over age groups
#'   and both sexes. The share of age group \eqn{a} and sex \eqn{s}
#'   is proportional to \eqn{p_s e^{-r x_a} L_{as}}, where \eqn{p_s}
#'   is the proportion of births that are of sex \eqn{s}. The same
#'   growth rate `r` applies to both sexes.
#'
#' The growth rate `r` is typically calculated with
#' [.intrinsic_growth_rate()], from female fertility and mortality
#' schedules.
#'
#' The representative age for an open age group (e.g. 85+) has no
#' natural value. The calculations use whatever value is supplied
#' in `age_mid`.
#'
#' @param Lx Life table quantity \eqn{{}_nL_x}, with radix
#' \eqn{l_0 = 1}, for every age group, including the open age
#' group. An ordinary numeric vector or an [rvec()][rvec::rvec()].
#' @param Lx_female,Lx_male `Lx` for females and males. The same
#' length as `age_mid`.
#' @param age_mid Representative age (typically the midpoint) of
#' each age group. Numeric vector, strictly increasing.
#' @param r Growth rate. A single number, or an
#' [rvec()][rvec::rvec()] of length 1.
#' @param sex_ratio Number of male births per 100 female births.
#' A single positive number. Default is `105`.
#'
#' @returns
#' - `.stable_composition_one_sex()`: A vector the same length as
#'   `Lx`.
#' - `.stable_composition_two_sex()`: A named list with elements
#'   `female` and `male`, each a vector the same length as `age_mid`.
#'
#' Results are rvecs if any of `Lx`, `Lx_female`, `Lx_male`, or `r`
#' is an rvec, and ordinary numeric vectors otherwise.
#'
#' @seealso
#' - [.intrinsic_growth_rate()] Intrinsic growth rate implied by
#'   fertility and mortality schedules
#' - [lifetab()] Life tables, including `Lx`
#'
#' @examples
#' Lx <- c(4.6, 4.5, 4.5, 4.4, 4.4, 4.3, 25)
#' age_mid <- c(2.5, 7.5, 12.5, 17.5, 22.5, 27.5, 40)
#'
#' ## one sex
#' .stable_composition_one_sex(Lx = Lx,
#'                             age_mid = age_mid,
#'                             r = 0.02)
#'
#' ## two sexes
#' Lx_male <- c(4.5, 4.4, 4.4, 4.3, 4.2, 4.1, 21)
#' .stable_composition_two_sex(Lx_female = Lx,
#'                             Lx_male = Lx_male,
#'                             age_mid = age_mid,
#'                             r = 0.02)
#'
#' ## draws
#' r <- rvec::rvec(matrix(c(0.01, 0.02, 0.03), nrow = 1))
#' .stable_composition_one_sex(Lx = Lx,
#'                             age_mid = age_mid,
#'                             r = r)
#' @export
.stable_composition_one_sex <- function(Lx, age_mid, r) {
  check_stable_inputs(Lx = list(Lx = Lx),
                      age_mid = age_mid,
                      r = r)
  args <- align_draws(list(Lx = Lx, r = r))
  vals <- stable_unnormalised(Lx = args$vals$Lx,
                              age_mid = age_mid,
                              r = args$vals$r)
  total <- colSums(vals)
  check_stable_total(total, nms = "Lx")
  ans <- sweep(vals, MARGIN = 2L, STATS = total, FUN = "/")
  stable_return(ans, has_draws = args$has_draws)
}


#' @rdname dot-stable_composition_one_sex
#' @export
.stable_composition_two_sex <- function(Lx_female,
                                        Lx_male,
                                        age_mid,
                                        r,
                                        sex_ratio = 105) {
  check_stable_inputs(Lx = list(Lx_female = Lx_female,
                                Lx_male = Lx_male),
                      age_mid = age_mid,
                      r = r)
  check_not_rvec(sex_ratio, nm_x = "sex_ratio")
  check_number(sex_ratio,
               x_arg = "sex_ratio",
               check_na = TRUE,
               check_positive = TRUE,
               check_nonneg = FALSE,
               check_whole = FALSE)
  args <- align_draws(list(Lx_female = Lx_female,
                           Lx_male = Lx_male,
                           r = r))
  p_female <- 100 / (100 + sex_ratio)
  p_male <- sex_ratio / (100 + sex_ratio)
  vals_female <- p_female * stable_unnormalised(Lx = args$vals$Lx_female,
                                                age_mid = age_mid,
                                                r = args$vals$r)
  vals_male <- p_male * stable_unnormalised(Lx = args$vals$Lx_male,
                                            age_mid = age_mid,
                                            r = args$vals$r)
  total <- colSums(vals_female) + colSums(vals_male)
  check_stable_total(total, nms = c("Lx_female", "Lx_male"))
  female <- sweep(vals_female, MARGIN = 2L, STATS = total, FUN = "/")
  male <- sweep(vals_male, MARGIN = 2L, STATS = total, FUN = "/")
  list(female = stable_return(female, has_draws = args$has_draws),
       male = stable_return(male, has_draws = args$has_draws))
}


## Helpers --------------------------------------------------------------------

## HAS_TESTS
#' Align Numeric Vectors and Rvecs to Matrices with a Common Number of Draws
#'
#' Each element of `x` is converted to a matrix with one row per
#' element and one column per draw. Ordinary vectors, and rvecs with
#' one draw, are recycled to the common number of draws.
#'
#' @param x A named list of numeric vectors and rvecs.
#' @param call Call used in error messages.
#'
#' @returns A named list with elements `vals` (a named list of
#' matrices) and `has_draws` (whether any element of `x` is an rvec).
#'
#' @noRd
align_draws <- function(x, call = rlang::caller_env()) {
  is_rv <- vapply(x, rvec::is_rvec, TRUE)
  n_draw <- vapply(x, function(e) if (rvec::is_rvec(e)) rvec::n_draw(e) else 1L, 1L)
  n_draw_max <- max(n_draw)
  is_bad <- is_rv & n_draw != 1L & n_draw != n_draw_max
  if (any(is_bad)) {
    nms <- names(x)[is_rv]
    n <- n_draw[is_rv]
    bullets <- vapply(seq_along(nms),
                      function(i) cli::format_inline("{.arg {nms[[i]]}} has {n[[i]]} draw{?s}."),
                      "")
    names(bullets) <- rep("i", length(bullets))
    cli::cli_abort(c("Rvecs have different numbers of draws.",
                     bullets),
                   call = call)
  }
  to_matrix <- function(e) {
    m <- if (rvec::is_rvec(e)) as.matrix(e) else matrix(as.double(e), ncol = 1L)
    storage.mode(m) <- "double"
    if (ncol(m) != n_draw_max)
      m <- matrix(m, nrow = nrow(m), ncol = n_draw_max)
    m
  }
  vals <- lapply(x, to_matrix)
  list(vals = vals,
       has_draws = any(is_rv))
}


## HAS_TESTS
#' Check Inputs Shared by the Stable Composition Functions
#'
#' @param Lx Named list of `Lx` vectors (one or two)
#' @param age_mid Numeric vector
#' @param r Number or rvec of length 1
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_stable_inputs <- function(Lx, age_mid, r) {
  check_not_rvec(age_mid, nm_x = "age_mid")
  check_numeric(x = age_mid,
                x_arg = "age_mid",
                check_na = TRUE,
                check_positive = FALSE,
                check_nonneg = TRUE,
                check_whole = FALSE)
  if (length(age_mid) == 0L)
    cli::cli_abort("{.arg age_mid} has length 0.")
  if (any(diff(age_mid) <= 0))
    cli::cli_abort("{.arg age_mid} not strictly increasing.")
  for (nm in names(Lx)) {
    check_numeric(x = Lx[[nm]],
                  x_arg = nm,
                  check_na = TRUE,
                  check_positive = FALSE,
                  check_nonneg = TRUE,
                  check_whole = FALSE)
    check_equal_length(x = Lx[[nm]],
                       y = age_mid,
                       nm_x = nm,
                       nm_y = "age_mid")
  }
  check_number(x = r,
               x_arg = "r",
               check_na = TRUE,
               check_positive = FALSE,
               check_nonneg = FALSE,
               check_whole = FALSE)
  invisible(TRUE)
}


## HAS_TESTS
#' Check that the Total of Unnormalised Stable Values is Positive
#'
#' Values can only be 0 if every value of `Lx` is 0
#' (in some draw), since `Lx` is non-negative.
#'
#' @param total Numeric vector, one value per draw
#' @param nms Names of the `Lx` arguments
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_stable_total <- function(total, nms) {
  if (any(total <= 0))
    cli::cli_abort("{.arg {nms}} {?is/are} 0 for every age group.")
  invisible(TRUE)
}


## HAS_TESTS
#' Unnormalised Stable Composition: exp(-r * age_mid) * Lx
#'
#' @param Lx Matrix, one row per age group, one column per draw
#' @param age_mid Numeric vector
#' @param r Matrix with one row, one column per draw
#'
#' @returns A matrix the same size as `Lx`
#'
#' @noRd
stable_unnormalised <- function(Lx, age_mid, r) {
  exp(-outer(age_mid, r[1L, ])) * Lx
}


## HAS_TESTS
#' Return a Matrix as an Rvec or an Ordinary Vector
#'
#' @param x Matrix, one row per age group, one column per draw
#' @param has_draws Whether to return an rvec
#'
#' @returns An rvec or a numeric vector
#'
#' @noRd
stable_return <- function(x, has_draws) {
  if (has_draws)
    rvec::rvec_dbl(unname(x))
  else
    as.double(x[, 1L])
}
