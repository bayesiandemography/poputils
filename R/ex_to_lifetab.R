
#' Derive Life Tables that Match Life Expectancies,
#' using a Brass Logit Model
#'
#' This function contained an error, and is deprecated.
#' Please use function [e0_to_lifetab_logit()] instead.
#'
#' Function `ex_to_lifetab_brass()` used formula
#' 
#' \deqn{\text{logit}(l_x^{\text{B}}) \approx \alpha + \beta \text{logit}(l_x^{\text{A}})},
#'
#' instead of the conventional 
#'
#' \deqn{\text{logit}(1-l_x^{\text{B}}) \approx \alpha + \beta \text{logit}(1-l_x^{\text{A}})}.
#' 
#' @param target A data frame containing a variable called
#' `"ex"`, and possibly other varibles. See Details.
#' @param standard A data frame containing variables
#' called `age` and `lx`, and possibly others.
#' See Details.
#' @param infant,child,closed,open Methods used to
#' calculate life expectancy. See [lifetab()] for details.
#' @param radix Initial population for the
#' `lx` column in the derived life table(s).
#' Default is `100000`.
#' @param suffix Optional suffix added to life table columns.
#'
#' @returns A [tibble][tibble::tibble()].
#'
#' @export
ex_to_lifetab_brass <- function(target,
                                standard,
                                infant = c("constant", "linear", "CD", "AK"),
                                child = c("constant", "linear", "CD"),
                                closed = c("constant", "linear"),
                                open = "constant",
                                radix = 100000,
                                suffix = NULL) {
  lifecycle::deprecate_stop(when = "0.5.1",                  # nocov
                            what = "ex_to_lifetab_brass()",  # nocov
                            with = "e0_to_lifetab_logit()")  # nocov
}


