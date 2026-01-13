
#' Calculate Intrinsic Growth Rate
#'
#' Calculate the intrinsic growth rate implied
#' by fertility and mortality schedules. The
#' intrinsic growth rate is the rate at which
#' all age groups in a population would eventually
#' grow if the population was subject to the fertility
#' and mortality schedules indefinitely. The fertility
#' and mortality schedules apply to a single sex,
#' typically females.
#'
#' The fertility scehdule `mx` and mortality schedule
#' `Lx` refer only to the reproductive ages. The
#' mortality schedule `Lx` is the number of years that
#' a newborn baby would be expected to spend in each
#' age group under prevailing mortality rates. It is
#' the life table quantity `nLx` where the radix
#' `l0` has been set to 1.
#'
#' @param mx Age specific fertility rates,
#' for a single sex (typically daughters).
#' An ordinary numeric vector or an [rvec()][rvec].
#' @param Lx Life table mortality measure,
#' for a single sex (typically females).
#' An ordinary numeric vector or an [rvec()][rvec].
#' @param age_mid The midpoint of each age group.
#' Numeric vector.
#'
#' @returns A numeric vector of length 1.
#'
#' @examples
#' mx <- c(0.072, 0.148, 0.156, 0.140, 0.108, 0.054, 0.013)
#' Lx <- c(3.49, 3.37, 3.23, 3.07, 2.91, 2.74, 2.56)
#' age_mid <- c(17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5)
#' .intrinsic_growth_rate(mx = mx, Lx = Lx, age_mid = age_mid)
#' @export
.intrinsic_growth_rate <- function(mx, Lx, age_mid) {
  check_equal_length(x = mx,
                     y = Lx,
                     nm_x = "mx",
                     nm_y = "Lx")
  check_equal_length(x = mx,
                     y = age_mid,
                     nm_x = "mx",
                     nm_y = "age_mid")
  check_numeric(x = mx,
                x_arg = "mx",
                check_na = TRUE,
                check_positive = FALSE,
                check_nonneg = TRUE,
                check_whole = FALSE)
  check_numeric(x = Lx,
                x_arg = "Lx",
                check_na = TRUE,
                check_positive = FALSE,
                check_nonneg = TRUE,
                check_whole = FALSE)
  check_numeric(x = age_mid,
                x_arg = "age_mid",
                check_na = TRUE,
                check_positive = TRUE,
                check_nonneg = FALSE,
                check_whole = FALSE)
  if (any(diff(age_mid) <= 0))
    cli::cli_abort("{.arg age_mid} not monotonically increasing.")
  is_rvec_mx <- rvec::is_rvec(mx)
  is_rvec_Lx <- rvec::is_rvec(Lx)
  FUN <- function(mx, Lx)
    intrinsic_growth_rate_cpp11(mx = mx,
                                Lx = Lx,
                                age_mid = age_mid,
                                max_iter = 50L,
                                tol = 1e-10,
                                deriv_tol = 1e-14)
  if (!is_rvec_mx && !is_rvec_Lx) {
    ans <- FUN(mx = mx, Lx = Lx)
    return(ans)
  }
  if (is_rvec_mx && !is_rvec_Lx) {
    n_draw <- rvec::n_draw(mx)
    mx <- as.matrix(mx)
    Lx <- matrix(Lx, nrow = length(Lx), ncol = n_draw)
  }
  else if (!is_rvec_mx && is_rvec_Lx) {
    n_draw <- rvec::n_draw(Lx)
    mx <- matrix(mx, nrow = length(mx), ncol = n_draw)
    Lx <- as.matrix(Lx)
  }
  else {
    n_draw_mx <- rvec::n_draw(mx)
    n_draw_Lx <- rvec::n_draw(Lx)
    if (n_draw_mx == n_draw_Lx) {
      mx <- as.matrix(mx)
      Lx <- as.matrix(Lx)
    }
    else if (n_draw_mx == 1L) {
      mx <- matrix(as.numeric(mx), nrow = length(mx), ncol = n_draw_Lx)
      Lx <- as.matrix(Lx)
    }
    else if (n_draw_Lx == 1L) {
      mx <- as.matrix(mx)
      Lx <- matrix(as.numeric(Lx), nrow = length(Lx), ncol = n_draw_mx)
    }
    else {
      cli::cli_abort(c("{.arg mx} and {.arg Lx} have different numbers of draws.",
                       i = "{.arg mx} has {.val {n_draw_mx}} draws.",
                       i = "{.arg Lx} has {.val {n_draw_Lx}} draws."))
    }
  }
  ans <- matrix(nrow = 1L, ncol = ncol(mx))
  for (j in seq_len(ncol(ans)))
    ans[1L, j] <- FUN(mx = mx[, j], Lx = Lx[, j])
  ans <- rvec::rvec(ans)
  ans
}


  
  
    
    
