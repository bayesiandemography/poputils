
#' Calculate life expectancy from mortality rates
#'
#' Given mortality rates, a description of the age groups,
#' and a calculation method, derive life expectancies.
#'
#' Mortality rates \code{mx} are held in a matrix,
#' with one set of age-specific rates per row.
#'
#' There are three choices for argument \code{age_groups}:
#' \describe{
#'   \item{\code{"lt"}}{Life table age groups
#'         \code{"0", "1-4", "5-9", \dots, "A+"}}
#'   \item{\code{"single"}}{\code{"0", "1", "2", \dots, "A+"}}
#'   \item{\code{"five"}}{\code{"0-4", "5-9", \dots, "A+"}}
#' }
#' The last interval is always assumed to be open, meaning that
#' it includes everyone over a certain age.
#'
#' There are six choices for the \code{method} argument.
#' Each method makes different assumptions about
#' the shape of mortality rates, the survival curve,
#' or the average of deaths within each age interval:
#' \describe{
#'   \item{\code{"const"}}{Mortality rates are constant
#'         within each age group; equivalently, the
#'         life table function \code{lx} is an exponential
#'         curve within each age interval.}
#'   \item{\code{"mid"}}{On average, people die half way
#'         through each age group; equivalently, the
#'         life table function \code{lx} is a straight
#'         line within each age group.}
#'   \item{\code{"CD-Female"}, \code{"CD-Male"}}{As for
#'         \code{"mid"}, except that the average age at
#'         which infants die (and, if \code{age_groups} is
#'         \code{"lt"}, the average age at which
#'         children aged 1-4 die), is determined by
#'         formulas developed by Coale and Demeny (1983),
#'         and reported in Preston et al (2001).}
#'   \item{\code{"HMD-Female"}, \code{"HMD-Male"}}{The
#'         approach used in the Human Mortality Database.
#'         As for \code{"mid"}, except that the average
#'         age at which infants die is determined by
#'         formulas developed by Andreev and Kingkade (2015),
#'         and reported in Wilmoth et al (2019).}
#' }
lifeexp <- function(mx, age, sex = NULL, classif_vars = NULL) {
}

lifeexp_num <- function(mx, age, sex, classif_vars) {

}

lifeexp_list <- function(mx, age, sex, classif_vars) {

}

lifeexp_mat <- function(mx, age, sex, classif_vars) {

}
