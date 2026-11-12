## Frozen copies of bage's functions (bage 0.10.11, R/util.R and
## R/make-outputs.R), used as references for the poputils versions.
## Not bage itself: bage imports poputils, so a test dependency on
## bage would be circular.

bage_safe_chol_prec <- function(Q, max_jitter) {
  jitter0 <- 1e-12
  Q <- Matrix::forceSymmetric(Q, uplo = "L")
  if (!methods::is(Q, "dsCMatrix"))
    Q <- methods::as(Q, "CsparseMatrix")
  n <- nrow(Q)
  jitter <- 0
  repeat {
    Qj <- if (jitter > 0) Q + Matrix::Diagonal(n = n, x = jitter) else Q
    ans <- suppressWarnings(try(Matrix::Cholesky(Qj, LDL = FALSE, perm = TRUE, super = NA),
                                silent = TRUE))
    if (!inherits(ans, "try-error"))
      return(ans)
    jitter <- if (jitter == 0) jitter0 else jitter * 10
    if (jitter > max_jitter)
      stop("Cholesky factorization failed.")
  }
}

bage_rmvnorm_chol <- function(n, mean, CH) {
  n_val <- length(mean)
  Z <- matrix(stats::rnorm(n = n_val * n), nrow = n_val, ncol = n)
  L <- Matrix::expand(CH)$L
  E <- Matrix::solve(Matrix::t(L), Z)
  mean + E
}

bage_make_is_fixed <- function(est, map) {
  if (is.null(map)) {
    n <- length(unlist(est))
    ans <- rep(FALSE, times = n)
  }
  else {
    ans <- est
    nms_est <- names(est)
    nms_map <- names(map)
    for (nm in nms_est) {
      if (nm %in% nms_map)
        ans[[nm]] <- is.na(map[[nm]])
      else
        ans[[nm]] <- rep(FALSE, times = length(ans[[nm]]))
    }
    ans <- unlist(ans)
  }
  ans
}

bage_symmetry_grade <- function(Q,
                                thresh_abs_near = 1e-11,
                                thresh_rel_near = 1e-9,
                                thresh_abs_moderate = 1e-8,
                                thresh_rel_moderate = 1e-7) {
  if (!inherits(Q, "Matrix"))
    Q <- Matrix::Matrix(Q, sparse = FALSE)
  if (inherits(Q, "sparseMatrix"))
    Q <- methods::as(Q, "generalMatrix")
  if (Matrix::isSymmetric(Q, tol = 0))
    return("near")
  U <- Matrix::triu(Q, k = 1L)
  L <- Matrix::tril(Q, k = -1L)
  diff_U_L <- U - Matrix::t(L)
  max_abs_diff <- if (length(diff_U_L@x) > 0L) max(abs(diff_U_L@x)) else 0
  max_element_Q <- if (length(Q@x) > 0L) max(abs(Q@x)) else 0
  max_element_Q <- max(max_element_Q, 1)
  max_rel_diff <- max_abs_diff / max_element_Q
  if (max_abs_diff <= thresh_abs_near && max_rel_diff <= thresh_rel_near)
    "near"
  else if (max_abs_diff <= thresh_abs_moderate && max_rel_diff <= thresh_rel_moderate)
    "moderate"
  else
    "severe"
}
