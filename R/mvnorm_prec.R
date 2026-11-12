## Gaussian approximation: developer functions -----------------------------------

#' Draw from a Multivariate Normal Specified by Its Precision,
#' for Developers
#'
#' Draw from a multivariate normal distribution specified by a
#' mean and a precision matrix (the inverse of the covariance
#' matrix), as returned, for instance, by `TMB::sdreport()`. The
#' functions work with sparse precision matrices, which keeps
#' factorisation and drawing fast when there are many parameters.
#'
#' - `.chol_prec()` calculates a sparse Cholesky factorisation of
#'   a precision matrix, with a fill-reducing permutation. If the
#'   factorisation fails, it tries again with a small quantity added
#'   to the diagonal, starting at \eqn{10^{-12}} and increasing by
#'   factors of 10 up to `max_jitter`, with a warning if this was
#'   needed, and an error if it still fails.
#' - `.rmvnorm_prec_chol()` draws from the multivariate normal,
#'   given the mean and the factorisation from `.chol_prec()`.
#' - `.draws_free_fixed()` draws values for all parameters of a
#'   model, where some parameters are held fixed and the remainder
#'   ("free" parameters) have a multivariate normal distribution.
#'   Fixed parameters take their values from `est`.
#'
#' Factorisation is the expensive step. Storing the result of
#' `.chol_prec()` allows draws to be made later, cheaply. The result
#' is an ordinary S4 object, which can be saved with [saveRDS()].
#'
#' @section Symmetry:
#'
#' A precision matrix should be symmetric. Precision matrices
#' calculated numerically are sometimes slightly asymmetric.
#' `.chol_prec()` uses only the lower triangle. If the asymmetry is
#' moderate (largest absolute difference above \eqn{10^{-11}} or
#' relative difference above \eqn{10^{-9}}), `.chol_prec()` gives a
#' warning; if it is severe (above \eqn{10^{-8}} or \eqn{10^{-7}}),
#' an error.
#'
#' @section Method:
#'
#' `.chol_prec()` finds \eqn{L} and a permutation \eqn{P} such that
#' \eqn{P Q P^{\top} = L L^{\top}}, where \eqn{Q} is the precision.
#' `.rmvnorm_prec_chol()` then draws
#' \eqn{x = \mu + P^{\top} L^{-\top} z}, with
#' \eqn{z \sim \text{N}(0, I)}, so that \eqn{x} has covariance
#' \eqn{Q^{-1}}.
#'
#' @param prec A symmetric, positive definite precision matrix. A
#' sparse matrix from the Matrix package, or an ordinary matrix.
#' @param max_jitter Largest quantity to add to the diagonal of
#' `prec` if factorisation fails. Default is `1e-4`.
#' @param n Number of draws.
#' @param mean Mean of the distribution. A numeric vector, with one
#' element per row of the precision matrix.
#' @param chol A factorisation created by `.chol_prec()`.
#' @param est Values for all parameters. A numeric vector, or a
#' list of numeric vectors (such as the point estimates from
#' `TMB::sdreport()`), combined with [unlist()].
#' @param is_free Logical vector, one element per value in `est`:
#' `TRUE` for parameters drawn from the multivariate normal, `FALSE`
#' for parameters held fixed.
#'
#' @returns
#' - `.chol_prec()`: An object of class
#'   [CHMfactor][Matrix::CHMfactor-class].
#' - `.rmvnorm_prec_chol()`: A matrix, with one row per element of
#'   `mean` and one column per draw.
#' - `.draws_free_fixed()`: A matrix, with one row per value in
#'   `est`, named as in `unlist(est)`, and one column per draw.
#'
#' @seealso [.fit_tmb()] Fit a TMB model and prepare for drawing
#' from the posterior.
#'
#' @examples
#' ## precision matrix of a random walk, plus a little on the diagonal
#' n <- 5
#' D <- diff(diag(n))
#' prec <- crossprod(D) + diag(0.1, n)
#' chol <- .chol_prec(prec)
#' draws <- .rmvnorm_prec_chol(n = 1000, mean = rep(0, n), chol = chol)
#' round(cov(t(draws)), 1)
#' round(solve(prec), 1)
#'
#' ## first and third parameters fixed
#' est <- list(a = 1, b = c(0, 0), c = 3)
#' is_free <- c(FALSE, TRUE, TRUE, FALSE)
#' chol <- .chol_prec(diag(2))
#' .draws_free_fixed(est, is_free = is_free, chol = chol, n = 3)
#' @name mvnorm_prec
NULL


#' @rdname mvnorm_prec
#' @export
.chol_prec <- function(prec, max_jitter = 1e-4) {
  check_prec(prec)
  if (!is.numeric(max_jitter) || length(max_jitter) != 1L || is.na(max_jitter) || max_jitter < 0)
    cli::cli_abort("{.arg max_jitter} must be a single non-negative number.")
  grade <- symmetry_grade(prec)
  if (grade == "severe")
    cli::cli_abort("{.arg prec} is severely asymmetric.")
  if (grade == "moderate")
    cli::cli_warn(c("{.arg prec} is moderately asymmetric.",
                    i = "Proceeding using the lower triangle."))
  prec <- Matrix::forceSymmetric(prec, uplo = "L")
  prec <- as_cholmod_preferred(prec)
  n <- nrow(prec)
  jitter0 <- 1e-12
  jitter <- 0
  repeat {
    prec_jitter <- if (jitter > 0) prec + Matrix::Diagonal(n = n, x = jitter) else prec
    ans <- suppressWarnings(try(Matrix::Cholesky(prec_jitter, LDL = FALSE, perm = TRUE, super = NA),
                                silent = TRUE))
    if (!inherits(ans, "try-error")) {
      if (jitter > 0)
        cli::cli_warn(c("Cholesky factorisation only possible after adding a small quantity to the diagonal.",
                        i = sprintf("Quantity added: %.1e.", jitter),
                        i = paste("Factorisation problems sometimes mean that a model is too complex,",
                                  "or is weakly identified.")))
      return(ans)
    }
    jitter <- if (jitter == 0) jitter0 else jitter * 10
    if (jitter > max_jitter)
      cli::cli_abort(c("Cholesky factorisation failed.",
                       i = paste("Factorisation problems sometimes mean that a model is too complex,",
                                 "or is weakly identified."),
                       i = paste("Increasing {.arg max_jitter} may allow factorisation to proceed,",
                                 "with reduced accuracy.")))
  }
}


#' @rdname mvnorm_prec
#' @export
.rmvnorm_prec_chol <- function(n, mean, chol) {
  check_n(n = n, nm_n = "n", min = 1L, max = NULL, divisible_by = NULL)
  if (!methods::is(chol, "CHMfactor"))
    cli::cli_abort(c("{.arg chol} must be a factorisation created by {.fn .chol_prec}.",
                     i = "{.arg chol} has class {.cls {class(chol)}}."))
  n_val <- chol@Dim[[1L]]
  if (!is.numeric(mean) || length(mean) != n_val || anyNA(mean))
    cli::cli_abort(c("{.arg mean} must be a numeric vector with one element per row of the precision matrix, and no NAs.",
                     i = "{.arg mean} has length {length(mean)}; the precision matrix has {n_val} row{?s}."))
  n <- as.integer(n)
  z <- matrix(stats::rnorm(n = n_val * n), nrow = n_val, ncol = n)
  x <- Matrix::solve(chol, z, system = "Lt")
  x <- Matrix::solve(chol, x, system = "Pt")
  ans <- as.matrix(x) + mean
  dimnames(ans) <- NULL
  ans
}


#' @rdname mvnorm_prec
#' @export
.draws_free_fixed <- function(est, is_free, chol, n) {
  check_n(n = n, nm_n = "n", min = 1L, max = NULL, divisible_by = NULL)
  n <- as.integer(n)
  val <- unlist(est)
  if (!is.numeric(val) || anyNA(val))
    cli::cli_abort("{.arg est} must contain only numbers, with no NAs.")
  storage.mode(val) <- "double"
  if (!is.logical(is_free) || length(is_free) != length(val) || anyNA(is_free))
    cli::cli_abort(c("{.arg is_free} must be a logical vector with one element per value in {.arg est}, and no NAs.",
                     i = "{.arg is_free} has length {length(is_free)}; {.arg est} has {length(val)} value{?s}."))
  n_free <- sum(is_free)
  ans <- matrix(val, nrow = length(val), ncol = n, dimnames = list(names(val), NULL))
  if (n_free > 0L)
    ans[is_free, ] <- .rmvnorm_prec_chol(n = n, mean = val[is_free], chol = chol)
  ans
}


## Helpers ----------------------------------------------------------------------

#' Check that a precision matrix is square and numeric
#'
#' @param prec Object to check
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_prec <- function(prec) {
  is_mat <- is.matrix(prec) || methods::is(prec, "Matrix")
  if (!is_mat)
    cli::cli_abort(c("{.arg prec} must be a matrix.",
                     i = "{.arg prec} has class {.cls {class(prec)}}."))
  if (is.matrix(prec) && !is.numeric(prec))
    cli::cli_abort("{.arg prec} must be numeric.")
  dim <- dim(prec)
  if (dim[[1L]] != dim[[2L]] || dim[[1L]] == 0L)
    cli::cli_abort(c("{.arg prec} must be square, with at least one row.",
                     i = "{.arg prec} has {dim[[1L]]} row{?s} and {dim[[2L]]} column{?s}."))
  if (anyNA(Matrix::diag(prec)))
    cli::cli_abort("Diagonal of {.arg prec} has NAs.")
  invisible(TRUE)
}


#' Grade the asymmetry of a matrix
#'
#' From bage's symmetry_grade(), unchanged thresholds.
#'
#' @param Q A square matrix, dense or sparse
#' @param thresh_abs_near,thresh_rel_near Thresholds for "near"
#' @param thresh_abs_moderate,thresh_rel_moderate Thresholds for
#' "moderate"
#'
#' @returns "near", "moderate", or "severe"
#'
#' @noRd
symmetry_grade <- function(Q,
                           thresh_abs_near = 1e-11,
                           thresh_rel_near = 1e-9,
                           thresh_abs_moderate = 1e-8,
                           thresh_rel_moderate = 1e-7) {
  if (!inherits(Q, "Matrix"))
    Q <- Matrix::Matrix(Q, sparse = FALSE)
  Q <- methods::as(Q, "generalMatrix")
  if (Matrix::isSymmetric(Q, tol = 0))
    return("near")
  diff <- Matrix::triu(Q, k = 1L) - Matrix::t(Matrix::tril(Q, k = -1L))
  max_abs_diff <- max(abs(as.numeric(diff[diff != 0])), 0)
  max_element <- max(abs(as.numeric(Q[Q != 0])), 1)
  max_rel_diff <- max_abs_diff / max_element
  if (max_abs_diff <= thresh_abs_near && max_rel_diff <= thresh_rel_near)
    "near"
  else if (max_abs_diff <= thresh_abs_moderate && max_rel_diff <= thresh_rel_moderate)
    "moderate"
  else
    "severe"
}


#' Coerce a symmetric matrix to a class suitable for Cholesky
#'
#' Prefers sparse symmetric, so that Matrix::Cholesky() returns a
#' CHMfactor. From bage's as_cholmod_preferred().
#'
#' @param Q A symmetric Matrix
#'
#' @returns A Matrix
#'
#' @noRd
as_cholmod_preferred <- function(Q) {
  if (methods::is(Q, "dsCMatrix"))
    return(Q)
  methods::as(Q, "CsparseMatrix")
}
