## Seeds: developer functions ----------------------------------------------------

#' Seeds for Reproducible Random Draws, for Developers
#'
#' Helpers for packages that store seeds in their objects, so that
#' random draws are the same each time they are made.
#'
#' - `.make_seeds()` makes one random seed per name.
#' - `.with_seed()` evaluates an expression with a given seed, and
#'   then restores R's random number state exactly as it was, so
#'   the user's own random numbers are unaffected. If there was no
#'   random number state (no `.Random.seed` in the global
#'   environment) before the call, there is none afterwards. The
#'   state is restored even if `expr` gives an error.
#'
#' `.make_seeds()` uses R's random number generator, so calling
#' `set.seed()` first gives reproducible seeds.
#'
#' @param names Character vector of names for the seeds.
#' @param seed An integer seed, for instance from `.make_seeds()`.
#' @param expr Expression to evaluate.
#'
#' @returns
#' - `.make_seeds()`: A named integer vector, the same length as
#'   `names`.
#' - `.with_seed()`: The value of `expr`.
#'
#' @examples
#' seeds <- .make_seeds(c("prior", "posterior"))
#' seeds
#'
#' ## same draws each time
#' .with_seed(seeds[["prior"]], rnorm(2))
#' .with_seed(seeds[["prior"]], rnorm(2))
#' @name seeds
NULL


#' @rdname seeds
#' @export
.make_seeds <- function(names) {
  if (!is.character(names) || anyNA(names))
    cli::cli_abort("{.arg names} must be a character vector with no NAs.")
  ans <- sample.int(n = .Machine$integer.max, size = length(names), replace = TRUE)
  names(ans) <- names
  ans
}


#' @rdname seeds
#' @export
.with_seed <- function(seed, expr) {
  is_valid <- is.numeric(seed) &&
    length(seed) == 1L &&
    !is.na(seed) &&
    is.finite(seed) &&
    seed == round(seed)
  if (!is_valid)
    cli::cli_abort("{.arg seed} must be a single whole number.")
  env <- globalenv()
  has_old <- exists(".Random.seed", envir = env, inherits = FALSE)
  if (has_old)
    old <- get(".Random.seed", envir = env, inherits = FALSE)
  on.exit({
    if (has_old)
      assign(".Random.seed", old, envir = env)
    else if (exists(".Random.seed", envir = env, inherits = FALSE))
      rm(".Random.seed", envir = env)
  })
  set.seed(seed)
  expr
}
