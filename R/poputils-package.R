
#' Functions for working with demographic data
#'
#' Functions for common tasks in demographic
#' analyses. Some functions are aimed
#' at end-users, and others at developers.
#'
#' @section For end users:
#'
#' **Life expectancy, life tables**
#' 
#' - [lifeexp()] Calculate life expectancy from mortality rates.
#' - [lifetab()] Create a full life table from mortality rates.
#'
#' **Labels**
#' 
#' - [age_labels()] Create age labels.
#' - [age_lower()], [age_mid()], [age_upper()] Limits and midpoints of age groups.
#' - [combine_age()] Merge age group labels.
#' - [reformat_age()] Reformat age group labels.
#' - [reformat_sex()] Reformat sex labels.
#' - [set_age_open()] Specify oldest age group.
#'
#'
#' @section For developers:
#'
#' **Data manipulation**
#' 
#' - [check_no_overlap_colnums()] Checking for argument clash.
#' - [groups_colnums()] Get column numbers for grouping variables.
#' - [matrix_to_list_of_cols()], [matrix_to_list_of_rows()] Split matrix.
#' - [to_matrix()] Convert data frame to matrix.
#'
#' **Labels**
#'
#' - [age_group_type()] Infer type of age group label.
#' - [check_age()] Validity checks for age group labels.
#' - [find_label_female()], [find_label_male()] Identify sex or gender labels.
#' - [find_var_age()], [find_var_sexgender()], [find_var_time()] Identify age, sex/gender, time variables.
#'
#' 
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @useDynLib poputils, .registration = TRUE
## usethis namespace: end
NULL
