# poputils

Manipulate and analyse demographic data.

## Installation

``` r
install.packages("poputils")
```

## For end users

### Data manipulation

- [`logit()`](https://bayesiandemography.github.io/poputils/reference/logit.md),
  [`invlogit()`](https://bayesiandemography.github.io/poputils/reference/logit.md)
  Logistic transformation
- [`trim_01()`](https://bayesiandemography.github.io/poputils/reference/trim_01.md)
  Trim values to interval (0, 1)
- [`rr3()`](https://bayesiandemography.github.io/poputils/reference/rr3.md)
  Randomly round to base 3

### Life expectancy, life tables

- [`ex_to_lifetab_brass()`](https://bayesiandemography.github.io/poputils/reference/ex_to_lifetab_brass.md)
  Use the Brass logit model to derive life tables with specified life
  expectancies
- [`lifeexp()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md)
  Calculate life expectancy from mortality rates
- [`lifetab()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md)
  Calculate life tables from mortality rates
- [`q0_to_m0()`](https://bayesiandemography.github.io/poputils/reference/q0_to_m0.md)
  Infant mortality

### Fertility

- [`tfr()`](https://bayesiandemography.github.io/poputils/reference/tfr.md)
  Calculate total fertility rates

### Labels

- [`age_labels()`](https://bayesiandemography.github.io/poputils/reference/age_labels.md)
  Create age labels
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

## For developers

### Checking arguments

- [`check_n()`](https://bayesiandemography.github.io/poputils/reference/check_n.md)
  Check an integer scalar.

### Data manipulation

- [`check_no_overlap_colnums()`](https://bayesiandemography.github.io/poputils/reference/check_no_overlap_colnums.md)
  Check for argument clashes
- [`groups_colnums()`](https://bayesiandemography.github.io/poputils/reference/groups_colnums.md)
  Get column numbers for grouping variables
- [`matrix_to_list_of_cols()`](https://bayesiandemography.github.io/poputils/reference/matrix_to_list_of_cols.md),
  [`matrix_to_list_of_rows()`](https://bayesiandemography.github.io/poputils/reference/matrix_to_list_of_cols.md)
  Split matrix
- [`to_matrix()`](https://bayesiandemography.github.io/poputils/reference/to_matrix.md)
  Convert data frame to matrix

### Labels

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
