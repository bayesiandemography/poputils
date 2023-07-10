
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

### Labels

- `age_labels()` Create age labels.
- `age_lower()`, `age_mid()`, `age_upper()` Limits and midpoint of age
  groups.
- `reformat_age()` Reform age group labels.
- `reformat_sex()` Reformat binary sex labels.
- `set_age_open()` Specify oldest age group.

### Data manipulation

- `collapse_age()` Aggregate age groups.

## For developers

### Labels

- `age_groups()` Infer type of age group label.
- `check_age()` Validity checks for age group labels.
- `find_label_female()`, `find_label_male()` Identify sex or gender
  labels.
- `find_var_age()`, `find_var_sexgender()`, `find_var_time()` Identify
  age, sex/gender, time variables.

### Data manipulation

- `to_matrix()` Reformat data frame to matrix.
