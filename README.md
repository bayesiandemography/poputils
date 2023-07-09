
<!-- README.md is generated from README.Rmd. Please edit that file -->

# poputils

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/bayesiandemography/poputils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bayesiandemography/poputils/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Miscellaneous functions for demographic analysis.

The functions are low level in the sense that they work with simple data
structures, and are general-purpose. They are also fast.

## Installation

``` r
## library(devtools)
devtools::install_github("bayesiandemography/poputils")
```

## Functions

### Life expectancy, life tables

- `lifeexp()` Calculate life expectancy from data on mortality rates.
- `Lx()` Calculate life table function *L<sub>x</sub>* (person-years
  lived) from data on mortality rates. *Not written yet*

### Age group labels *Not written yet*

- `parse_age_group()` Extract lower and upper limits from age group
  labels.
- `find_age_group()` Locate one set of age group labels in another set.
- `recode_age_group()` Recode one set of age group labels to align to
  another set.

### Generate counts of demographic events *Not written yet*

- `means_popn_one()` Compute mean values for stocks and flows for a
  single population.
- `draw_popn_one()` Randomly generate stocks and flows for a single
  population.
- `prob_popn_one()` Calculate the probability of values for stocks and
  flows for a single population.
- `means_popn_inter()` Compute mean values for stocks and flows for
  multiple interacting populations.
- `draw_popn_inter()` Randomly generate stocks and flows for multiple
  interacting populations.
- `prob_popn_inter()` Calculate the probability of values for stocks and
  flows for multiple interacting populations.

*More to be added*
