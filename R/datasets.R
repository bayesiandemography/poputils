
#' Deaths and population for New Zealand
#'
#' Counts of deaths and population, by age, sex, and calendar
#' year, for New Zealand, 2021-2022.
#'
#' @format A data frame with 84 rows and the
#' following variables:
#' - `year`: Calendar year.
#' - `gender`: `"Female"`, and `"Male"`.
#' - `age`: Age, in life table age groups, with an open age
#'    group of 95+.
#' - `deaths`: Counts of deaths, randomly rounded to base 3.
#' - `popn`: Estimates of average annual population.
#'
#' @source Modified from data in tables
#' "Deaths by age and sex (Annual-Dec)" and
#' "Estimated Resident Population by Age and Sex (1991+) (Annual-Dec)"
#' from Stats NZ online database *Infoshare*,
#' downloaded on 24 September 2023.
"nzmort"


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
#' @source Coale A, Demeny P, and Vaughn B. 1983.
#' Regional model life tables and stable populations.
#' 2nd ed. New York: Academic Press,
#' accessed via `demogR::cdmltw()`.
"west_mx"
