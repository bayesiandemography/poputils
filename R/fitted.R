## 'is_fitted' and 'unfit' -----------------------------------------------------

#' Test Whether a Model Has Been Fitted
#'
#' Test whether a model has been fitted, for instance by calling
#' `fit()`.
#'
#' `is_fitted()` is a generic function. poputils does not supply
#' any methods: these come from modelling packages, such as bage and
#' popcounts, which re-export the generic.
#'
#' @param x A model object.
#' @param ... Arguments passed to methods.
#'
#' @returns `TRUE` or `FALSE`.
#'
#' @seealso [unfit()] Reset a model to its unfitted state.
#'
#' @export
is_fitted <- function(x, ...) {
  UseMethod("is_fitted")
}


#' Reset a Model to Its Unfitted State
#'
#' Remove the results of fitting a model, leaving the model
#' specification (data, priors and other settings) unchanged.
#'
#' `unfit()` is a generic function. poputils does not supply any
#' methods: these come from modelling packages, such as bage and
#' popcounts, which re-export the generic.
#'
#' Methods should return an object of the same class as `x`, for
#' which [is_fitted()] returns `FALSE`. Calling `unfit()` on a model
#' that has not been fitted should return the model unchanged.
#'
#' @param x A model object.
#' @param ... Arguments passed to methods.
#'
#' @returns A model object.
#'
#' @seealso [is_fitted()] Test whether a model has been fitted.
#'
#' @export
unfit <- function(x, ...) {
  UseMethod("unfit")
}
