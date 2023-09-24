
<!-- README.md is generated from README.Rmd. Please edit that file -->

# poputils

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/bayesiandemography/poputils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bayesiandemography/poputils/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/bayesiandemography/poputils/branch/main/graph/badge.svg)](https://app.codecov.io/gh/bayesiandemography/poputils?branch=main)
<!-- badges: end -->

Miscellaneous functions for demographic analysis.

## Installation

``` r
## library(devtools)
devtools::install_github("bayesiandemography/poputils")
```

## For end users

### Life expectancy, life tables

- `lifeexp()` Calculate life expectancy from mortality rates.
- `lifetab()` Calculate life tables from mortality rates.

### Labels

- `age_labels()` Create age labels.
- `age_lower()`, `age_mid()`, `age_upper()` Limits and midpoints of age
  groups.
- `combine_age()` Merge age group labels.
- `reformat_age()` Reformat age group labels.
- `reformat_sex()` Reformat sex labels.
- `set_age_open()` Specify oldest age group.

## For developers

### Data manipulation

- `check_no_overlap_colnums()` Check for argument clashes.
- `groups_colnums()` Get column numbers for grouping variables.
- `matrix_to_list_of_cols()`, `matrix_to_list_of_rows()` Split matrix.
- `to_matrix()` Convert data frame to matrix.

### Labels

- `age_group_type()` Infer type of age group label.
- `check_age()` Validity checks for age group labels.
- `find_label_female()`, `find_label_male()` Identify sex or gender
  labels.
- `find_var_age()`, `find_var_sexgender()`, `find_var_time()` Identify
  age, sex/gender, time variables.
