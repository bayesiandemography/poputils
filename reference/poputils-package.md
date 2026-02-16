# Functions for working with demographic data

Functions for common tasks in demographic analyses. Some functions are
aimed at end-users, and others at developers.

## For end users

**Data manipulation**

- [`logit()`](https://bayesiandemography.github.io/poputils/reference/logit.md),[`invlogit()`](https://bayesiandemography.github.io/poputils/reference/logit.md)
  Logistic transformation

- [`trim_01()`](https://bayesiandemography.github.io/poputils/reference/trim_01.md)
  Trim values to interval (0, 1)

- [`rr3()`](https://bayesiandemography.github.io/poputils/reference/rr3.md)
  Randomly round to base 3

**Labels**

- [`age_labels()`](https://bayesiandemography.github.io/poputils/reference/age_labels.md)
  Create age labels.

- [`age_lower()`](https://bayesiandemography.github.io/poputils/reference/age_lower.md),
  [`age_mid()`](https://bayesiandemography.github.io/poputils/reference/age_lower.md),
  [`age_upper()`](https://bayesiandemography.github.io/poputils/reference/age_lower.md)
  Limits and midpoints of age groups

- [`combine_age()`](https://bayesiandemography.github.io/poputils/reference/combine_age.md)
  Merge age group labels

- [`reformat_age()`](https://bayesiandemography.github.io/poputils/reference/reformat_age.md)
  Reformat age group labels

- [`reformat_sex()`](https://bayesiandemography.github.io/poputils/reference/reformat_sex.md)
  Reformat sex labels

- [`set_age_open()`](https://bayesiandemography.github.io/poputils/reference/set_age_open.md)
  Specify oldest age group

**Life expectancy, life tables**

- [`e0_to_lifetab_logit()`](https://bayesiandemography.github.io/poputils/reference/e0_to_lifetab_logit.md)
  Lifetable from Brass logit model.

- [`lifeexp()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md)
  Life expectancy

- [`lifetab()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md)
  Full life table

- [`q0_to_m0()`](https://bayesiandemography.github.io/poputils/reference/q0_to_m0.md)
  Infant mortality

**Fertility**

- [`tfr()`](https://bayesiandemography.github.io/poputils/reference/tfr.md)
  Total fertility rate

- [`tfr_to_asfr_scale()`](https://bayesiandemography.github.io/poputils/reference/tfr_to_asfr_scale.md)
  Age-specific fertility rates from scaling

**Data**

- [booth_standard](https://bayesiandemography.github.io/poputils/reference/booth_standard.md)
  Age-pattern for fertility

- [irn_fert](https://bayesiandemography.github.io/poputils/reference/irn_fert.md)
  Fertility rates in Iran

- [nzl_mort](https://bayesiandemography.github.io/poputils/reference/nzl_mort.md)
  Death rates and counts in New Zealand

- [nzl_mort_rvec](https://bayesiandemography.github.io/poputils/reference/nzl_mort_rvec.md)
  Probabilistic version of
  [nzl_mort](https://bayesiandemography.github.io/poputils/reference/nzl_mort.md)

- [west_lifetab](https://bayesiandemography.github.io/poputils/reference/west_lifetab.md)
  "West" model life table

## For developers

**Checking arguments**

- [`check_n()`](https://bayesiandemography.github.io/poputils/reference/check_n.md)
  Check an integer scalar

**Data manipulation**

- [`check_no_overlap_colnums()`](https://bayesiandemography.github.io/poputils/reference/check_no_overlap_colnums.md)
  Checking for argument clash

- [`groups_colnums()`](https://bayesiandemography.github.io/poputils/reference/groups_colnums.md)
  Get column numbers for grouping variables

- [`matrix_to_list_of_cols()`](https://bayesiandemography.github.io/poputils/reference/matrix_to_list_of_cols.md),
  [`matrix_to_list_of_rows()`](https://bayesiandemography.github.io/poputils/reference/matrix_to_list_of_cols.md)
  Split matrix

- [`to_matrix()`](https://bayesiandemography.github.io/poputils/reference/to_matrix.md)
  Convert data frame to matrix

**Labels**

- [`age_group_type()`](https://bayesiandemography.github.io/poputils/reference/age_group_type.md)
  Infer type of age group label

- [`check_age()`](https://bayesiandemography.github.io/poputils/reference/check_age.md)
  Validity checks for age group labels

- [`find_label_female()`](https://bayesiandemography.github.io/poputils/reference/find_label_female.md),
  [`find_label_male()`](https://bayesiandemography.github.io/poputils/reference/find_label_male.md)
  Identify sex or gender labels

- [`find_var_age()`](https://bayesiandemography.github.io/poputils/reference/find_var_age.md),
  [`find_var_sexgender()`](https://bayesiandemography.github.io/poputils/reference/find_var_sexgender.md),
  [`find_var_time()`](https://bayesiandemography.github.io/poputils/reference/find_var_time.md)
  Identify age, sex/gender, time variables

**Stable populations**

- [`.intrinsic_growth_rate()`](https://bayesiandemography.github.io/poputils/reference/dot-intrinsic_growth_rate.md)
  Growth rate implied by mortality, fertility schedules

## See also

Useful links:

- <https://bayesiandemography.github.io/poputils/>

- <https://github.com/bayesiandemography/poputils>

- Report bugs at <https://github.com/bayesiandemography/poputils/issues>

## Author

**Maintainer**: John Bryant <john@bayesiandemography.com>

Other contributors:

- Bayesian Demography Limited \[copyright holder\]
