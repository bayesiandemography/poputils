## TMB: developer functions ------------------------------------------------------

#' Fit a TMB Model and Prepare to Draw from the Posterior,
#' for Developers
#'
#' Functions for packages that fit models with TMB and approximate
#' the posterior with a multivariate normal distribution: the
#' "outer" parameters (TMB's fixed effects) at the mode of their
#' Laplace-approximated marginal posterior, and the "inner"
#' parameters (TMB's random effects) given the outer ones, as in
#' `TMB::sdreport()` with `getJointPrecision = TRUE`.
#'
#' - `.fit_tmb()` does the whole job: minimises the objective
#'   function with [.optimize()], warns if the optimizer did not
#'   converge, extracts point estimates and the precision matrix
#'   with `.tmb_est_prec()`, identifies the free parameters with
#'   `.tmb_is_free()`, and factorises the precision with
#'   [.chol_prec()]. Draws can then be made, at any time, with
#'   [.draws_free_fixed()].
#' - `.tmb_est_prec()` extracts point estimates of all parameters,
#'   and the precision matrix of the free parameters, from a fitted
#'   TMB object. With random effects, the precision is TMB's joint
#'   precision of fixed and random effects; without, the inverse of
#'   the covariance of the fixed effects.
#' - `.tmb_is_free()` identifies, from the `map` argument passed to
#'   `TMB::MakeADFun()`, which parameters are free (estimated) and
#'   which are fixed.
#'
#' The caller creates `f` with `TMB::MakeADFun()`, and is
#' responsible for freeing it (`TMB::FreeADFun()`) afterwards.
#'
#' The functions require the TMB package, which poputils suggests
#' but does not import.
#'
#' @section Map:
#'
#' In `TMB::MakeADFun()`, `map` can fix parameters (factor levels
#' `NA`) and can also make parameters share a value (repeated
#' levels). These functions support only the first: non-`NA` levels
#' within each element of `map` must be distinct.
#'
#' @param f An object created by `TMB::MakeADFun()`.
#' @param map The `map` argument passed to `TMB::MakeADFun()`: a
#' named list of factors, or `NULL` or an empty list if no parameters
#' are fixed.
#' @param est Point estimates of all parameters: a named list, as
#' returned by `.tmb_est_prec()`.
#' @param optimizer Which optimizer to use. See [.optimize()].
#' Default is `"multi"`.
#' @param quiet Whether to suppress the optimizer's warnings and
#' progress reports. Default is `TRUE`.
#' @param max_jitter Largest quantity to add to the diagonal of the
#' precision matrix if factorisation fails. See [.chol_prec()].
#' Default is `1e-4`.
#'
#' @returns
#' - `.fit_tmb()`: A named list:
#'   - `est` Point estimates of all parameters, a named list.
#'   - `is_free` Logical vector, one element per value in
#'     `unlist(est)`.
#'   - `chol` Factorisation of the precision matrix of the free
#'     parameters, from [.chol_prec()].
#'   - `optimizer` A list with elements `optimizer`, `converged`,
#'     `iter`, and `message`, from [.optimize()].
#'   - `times` A named numeric vector: seconds spent optimising
#'     (`optimize`), in `TMB::sdreport()` (`sdreport`), and
#'     factorising (`chol`).
#' - `.tmb_est_prec()`: A named list with elements `est` (a named
#'   list) and `prec` (a matrix, one row per free parameter).
#' - `.tmb_is_free()`: A logical vector, one element per value in
#'   `unlist(est)`.
#'
#' @seealso
#' - [.optimize()] Optimizers
#' - [.chol_prec()], [.draws_free_fixed()] Draws from the
#'   multivariate normal
#'
#' @name fit_tmb
NULL


#' @rdname fit_tmb
#' @export
.fit_tmb <- function(f, map = NULL, optimizer = "multi", quiet = TRUE, max_jitter = 1e-4) {
  rlang::check_installed("TMB")
  check_tmb_object(f)
  t_start <- Sys.time()
  out <- .optimize(par = f$par, fn = f$fn, gr = f$gr, optimizer = optimizer, quiet = quiet)
  if (!out$converged)
    cli::cli_warn(c("Optimizer did not converge.",
                    i = "Optimizer: {.val {out$optimizer}}.",
                    i = "Message: {out$message}"))
  t_optimize <- Sys.time()
  est_prec <- .tmb_est_prec(f)
  t_sdreport <- Sys.time()
  est <- est_prec$est
  is_free <- .tmb_is_free(est = est, map = map)
  n_free <- sum(is_free)
  if (n_free != nrow(est_prec$prec))
    cli::cli_abort(paste("Internal error: {n_free} free parameter{?s} in {.arg map},",
                         "but precision matrix has {nrow(est_prec$prec)} row{?s}."))
  chol <- .chol_prec(est_prec$prec, max_jitter = max_jitter)
  t_chol <- Sys.time()
  seconds <- function(t1, t0) as.double(difftime(t1, t0, units = "secs"))
  list(est = est,
       is_free = is_free,
       chol = chol,
       optimizer = list(optimizer = out$optimizer,
                        converged = out$converged,
                        iter = out$iter,
                        message = out$message),
       times = c(optimize = seconds(t_optimize, t_start),
                 sdreport = seconds(t_sdreport, t_optimize),
                 chol = seconds(t_chol, t_sdreport)))
}


#' @rdname fit_tmb
#' @export
.tmb_est_prec <- function(f) {
  rlang::check_installed("TMB")
  check_tmb_object(f)
  has_random <- length(f$env$random) > 0L
  if (has_random) {
    sdreport <- TMB::sdreport(f, getJointPrecision = TRUE)
    prec <- sdreport$jointPrecision
  }
  else {
    sdreport <- TMB::sdreport(f)
    var <- sdreport$cov.fixed
    if (anyNA(diag(var)))
      prec <- var
    else
      prec <- solve(var)
  }
  est <- as.list(sdreport, what = "Est")
  check_est_prec(est = est, prec = prec)
  list(est = est, prec = prec)
}


#' @rdname fit_tmb
#' @export
.tmb_is_free <- function(est, map = NULL) {
  if (!is.list(est) || is.null(names(est)))
    cli::cli_abort("{.arg est} must be a named list.")
  if (is.null(map))
    map <- list()
  if (!is.list(map) || (length(map) > 0L && is.null(names(map))))
    cli::cli_abort("{.arg map} must be a named list or {.code NULL}.")
  nms_unknown <- setdiff(names(map), names(est))
  if (length(nms_unknown) > 0L)
    cli::cli_abort("{.arg map} has element{?s} {.val {nms_unknown}} not found in {.arg est}.")
  ans <- lapply(names(est), function(nm) {
    n <- length(est[[nm]])
    m <- map[[nm]]
    if (is.null(m))
      return(rep(TRUE, times = n))
    if (!is.factor(m) || length(m) != n)
      cli::cli_abort("Element {.val {nm}} of {.arg map} must be a factor the same length as the parameter.")
    levels_used <- as.integer(m[!is.na(m)])
    if (anyDuplicated(levels_used))
      cli::cli_abort(c("Element {.val {nm}} of {.arg map} makes parameters share values.",
                       i = "Only fixing parameters (level {.val {NA}}) is supported."))
    !is.na(m)
  })
  unlist(ans, use.names = FALSE)
}


## Helpers ----------------------------------------------------------------------

#' Check that estimates and the diagonal of the precision have no NAs
#'
#' @param est Named list of estimates
#' @param prec Precision matrix, with row names
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_est_prec <- function(est, prec) {
  val <- unlist(est)
  if (anyNA(val)) {
    nms <- unique(sub("[0-9]+$", "", names(val)[is.na(val)]))
    cli::cli_abort(c("Problem deriving posterior distribution.",
                     i = "Estimate of {.val {nms}} is {.val {NA}}."))
  }
  is_na_prec <- is.na(Matrix::diag(prec))
  if (any(is_na_prec)) {
    nms <- unique(rownames(prec)[is_na_prec])
    cli::cli_abort(c("Problem deriving posterior distribution.",
                     i = "Variance of {.val {nms}} is {.val {NA}}."))
  }
  invisible(TRUE)
}

#' Check that an object was created by TMB::MakeADFun()
#'
#' @param f Object to check
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_tmb_object <- function(f) {
  is_valid <- is.list(f) &&
    all(c("par", "fn", "gr", "env") %in% names(f)) &&
    is.function(f$fn) &&
    is.function(f$gr)
  if (!is_valid)
    cli::cli_abort("{.arg f} must be an object created by {.fn TMB::MakeADFun}.")
  invisible(TRUE)
}
