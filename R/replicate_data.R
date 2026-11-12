## 'replicate_data' ------------------------------------------------------------

#' Create Replicate Data
#'
#' Use a model to create replicate datasets, typically as a way of
#' checking the model.
#'
#' `replicate_data()` is a generic function. poputils does not
#' supply any methods: these come from modelling packages, such as
#' bage and popcounts, which re-export the generic. Each method
#' documents its own arguments.
#'
#' @details
#' Methods should return a [tibble][tibble::tibble()] containing
#' the original data, stacked with replicate datasets, with an extra
#' first column, `.replicate`, identifying the datasets:
#'
#' | `.replicate` | data |
#' |---|---|
#' | `"Original"` | Original data |
#' | `"Replicate 1"` | Simulated data |
#' | `"Replicate 2"` | Simulated data |
#' | ... | ... |
#' | `"Replicate <n>"` | Simulated data |
#'
#' If the model is working well, the replicate datasets should look
#' similar to the original data.
#'
#' @param x A model object.
#' @param ... Arguments passed to methods.
#'
#' @returns A [tibble][tibble::tibble()].
#'
#' @export
replicate_data <- function(x, ...) {
  UseMethod("replicate_data")
}
