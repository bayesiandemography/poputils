## Optimisation: developer function ----------------------------------------------

#' Minimise a Function, With a Fallback Optimizer, for Developers
#'
#' Minimise a function, given its gradient, using [stats::nlminb()]
#' or [stats::optim()]. Designed for objective functions created by
#' `TMB::MakeADFun()`, but works with any function and gradient.
#'
#' Choices for `optimizer`:
#'
#' - `"multi"` Try `"nlminb"`, and if that does not converge,
#'   continue with `"BFGS"` from the values where `"nlminb"` stopped.
#'   The default.
#' - `"nlminb"` [stats::nlminb()], with at most 300 iterations and
#'   400 function evaluations.
#' - `"BFGS"` [stats::optim()] with method `"BFGS"`, with at most
#'   300 iterations.
#' - `"CG"` [stats::optim()] with method `"CG"` (conjugate
#'   gradients), with at most 300 iterations.
#'
#' `.optimize()` does not warn when the optimizer fails to converge:
#' the caller decides what to do, using `converged`.
#'
#' @param par Numeric vector of starting values.
#' @param fn Function to minimise. Takes a vector like `par` and
#' returns a number.
#' @param gr Gradient of `fn`. Takes a vector like `par` and returns
#' a vector the same length.
#' @param optimizer Which optimizer to use. See below.
#' @param quiet Whether to suppress the optimizer's warnings and
#' progress reports. Default is `TRUE`.
#'
#' @returns A named list:
#' - `par` Values at the minimum.
#' - `value` Value of `fn` at the minimum.
#' - `converged` `TRUE` if the (last) optimizer reported
#'   convergence.
#' - `iter` Number of iterations. With `"multi"`, when both
#'   optimizers ran, a string giving both, e.g. `"300 + 25"`.
#' - `message` Message from the optimizer, or `NULL`. With
#'   `"multi"`, when both ran, both messages.
#' - `optimizer` Optimizer(s) used, e.g. `"nlminb"` or
#'   `"nlminb + BFGS"`.
#'
#' @seealso [.fit_tmb()] Fit a TMB model and prepare for drawing
#' from the posterior.
#'
#' @examples
#' rosenbrock <- function(x) 100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2
#' rosenbrock_gr <- function(x)
#'   c(-400 * x[1] * (x[2] - x[1]^2) - 2 * (1 - x[1]),
#'     200 * (x[2] - x[1]^2))
#' ans <- .optimize(c(-1.2, 1), fn = rosenbrock, gr = rosenbrock_gr)
#' ans$par
#' ans$converged
#' @export
.optimize <- function(par,
                      fn,
                      gr,
                      optimizer = c("multi", "nlminb", "BFGS", "CG"),
                      quiet = TRUE) {
  if (!is.numeric(par) || length(par) == 0L || anyNA(par))
    cli::cli_abort("{.arg par} must be a numeric vector with at least one element and no NAs.")
  if (!is.function(fn))
    cli::cli_abort("{.arg fn} must be a function.")
  if (!is.function(gr))
    cli::cli_abort("{.arg gr} must be a function.")
  optimizer <- match.arg(optimizer)
  if (!isTRUE(quiet) && !isFALSE(quiet))
    cli::cli_abort("{.arg quiet} must be {.val {TRUE}} or {.val {FALSE}}.")
  switch(optimizer,
         multi = optimize_multi(par = par, fn = fn, gr = gr, quiet = quiet),
         nlminb = optimize_nlminb(par = par, fn = fn, gr = gr, quiet = quiet),
         BFGS = optimize_optim(par = par, fn = fn, gr = gr, method = "BFGS", quiet = quiet),
         CG = optimize_optim(par = par, fn = fn, gr = gr, method = "CG", quiet = quiet))
}


## Helpers ----------------------------------------------------------------------

#' Optimise with nlminb, falling back to BFGS
#'
#' @param par,fn,gr,quiet As for .optimize()
#' @param force_fallback Whether to continue with BFGS even if nlminb
#' converged. For testing.
#' @param iter_max_nlminb Maximum iterations for nlminb. For testing.
#'
#' @returns As for .optimize()
#'
#' @noRd
optimize_multi <- function(par, fn, gr, quiet, force_fallback = FALSE, iter_max_nlminb = 300L) {
  out <- optimize_nlminb(par = par, fn = fn, gr = gr, quiet = quiet, iter_max = iter_max_nlminb)
  if (out$converged && !force_fallback)
    return(out)
  if (!quiet)
    cli::cli_alert_info("{.val nlminb} did not converge: continuing with {.val BFGS}.")
  out_new <- optimize_optim(par = out$par, fn = fn, gr = gr, method = "BFGS", quiet = quiet)
  na_to_text <- function(x) if (is.null(x)) "<no message>" else x
  out_new$iter <- paste(out$iter, out_new$iter, sep = " + ")
  out_new$message <- paste(na_to_text(out$message), na_to_text(out_new$message), sep = " + ")
  out_new$optimizer <- "nlminb + BFGS"
  out_new
}


#' Optimise with nlminb
#'
#' @param par,fn,gr,quiet As for .optimize()
#' @param iter_max Maximum number of iterations
#'
#' @returns As for .optimize()
#'
#' @noRd
optimize_nlminb <- function(par, fn, gr, quiet, iter_max = 300L) {
  control <- list(iter.max = iter_max,
                  eval.max = 400L,
                  trace = if (quiet) 0L else 1L)
  call_optimizer <- function()
    stats::nlminb(start = par, objective = fn, gradient = gr, control = control)
  out <- run_optimizer(call_optimizer, nm = "nlminb", quiet = quiet)
  list(par = out$par,
       value = out$objective,
       converged = identical(out$convergence, 0L),
       iter = out$iterations,
       message = out$message,
       optimizer = "nlminb")
}


#' Optimise with optim
#'
#' @param par,fn,gr,quiet As for .optimize()
#' @param method "BFGS" or "CG"
#'
#' @returns As for .optimize()
#'
#' @noRd
optimize_optim <- function(par, fn, gr, method, quiet) {
  control <- list(maxit = 300L, trace = if (quiet) 0L else 1L)
  if (!quiet)
    control$REPORT <- 1L
  call_optimizer <- function()
    stats::optim(par = par, fn = fn, gr = gr, method = method, control = control)
  out <- run_optimizer(call_optimizer, nm = "optim", quiet = quiet)
  list(par = out$par,
       value = out$value,
       converged = identical(out$convergence, 0L),
       iter = out$counts[["gradient"]],
       message = out$message,
       optimizer = method)
}


#' Run an optimizer quietly or with a header
#'
#' @param call_optimizer Function with no arguments that runs the
#' optimizer
#' @param nm Name of the optimizer, for the header
#' @param quiet Whether to suppress warnings and output
#'
#' @returns Output of call_optimizer()
#'
#' @noRd
run_optimizer <- function(call_optimizer, nm, quiet) {
  if (quiet)
    return(suppressWarnings(call_optimizer()))
  cli::cli_alert("Output from {.fun {nm}}:")
  call_optimizer()
}
