
#' Mortality rates from Coale-Demeny model life tables
#'
#' Age-sex-specific mortality rates for "West"
#' from Coale-Demeny model life tables.
#'
#' @format A data frame with 1,050 rows and the
#' following variables:
#' - `age`: Age, in life table age groups, with an open age
#'    group of 95+.
#' - `sex`: `"Female"`, and `"Male"`.
#' - `level`: Index for life table. Lower level implies
#'    lower life expectancy.
#' - `mx`: Mortality rate.
#'
#' @source Coale, A., P. Demeny, and B. Vaughn. 1983.
#' Regional model life tables and stable populations.
#' 2nd ed. New York: Academic Press,
#' accessed via `demogR::cdmltw()`.
"west_mx"
