# Derive Age-Specific Fertility Rates that Match Total Fertility Rates by Scaling

Turn total fertility rates (TFRs) into sets of age-specific fertility
rates, by scaling a set of standard rates upwards or downwards.

## Usage

``` r
tfr_to_asfr_scale(target, standard, suffix = NULL)
```

## Arguments

- target:

  A data frame containing a variable called `"tfr"`, and possibly
  others. See Details.

- standard:

  A data frame containing variables called `age` and `asfr`, and
  possibly others. See Details.

- suffix:

  Optional suffix added to `asfr` column in results.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html).

## Method

Let \\{}\_nf_x\\ be the age-specific fertility rate for people aged
between \\x\\ and \\x+n\\. Values for \\{}\_nf_x\\ are obtained by
scaling the standard rates \\{}\_nf_x^{\mathrm{std}}\\ so that they
agree with the target total fertility rate \\F\\. That is,
`tfr_to_asfr_scale()` sets

\$\${}\_nf_x = \alpha \times {}\_nf_x^{\mathrm{std}}\$\$

where

\$\$\alpha = \frac{F}{\sum_x n \times {}\_nf_x^{\mathrm{std}}}\$\$

## The `target` argument

`target` is a data frame specifying total fertility rates for each
population being modelled.

`target` contains the following variables:

- A variable called `"tfr"`. An ordinary numeric vector or an
  [rvec()](https://bayesiandemography.github.io/rvec/reference/rvec.html).

- Optionally, 'by' variables. Typical examples are time, region, and
  model variant.

## The `standard` argument

`standard` is a data frame specifying standard fertility schedules to be
used with each life expectancy in `target`. Values in `standard` are
age-specific.

`standard` contains the following variables:

- A variable called `"age"`, with labels that can be parsed by
  [`reformat_age()`](https://bayesiandemography.github.io/poputils/reference/reformat_age.md).

- A variable called `"value"`, containing non-negative values. Cannot be
  an rvec.

- Additional variables used to match rows in `standard` to rows in
  `target`.

## See also

- [`ex_to_lifetab_brass()`](https://bayesiandemography.github.io/poputils/reference/ex_to_lifetab_brass.md)
  Life table equivalent of `tfr_to_asfr_scale()`.

- [booth_standard](https://bayesiandemography.github.io/poputils/reference/booth_standard.md)
  The 'Booth standard' fertility schedule

- [tfr](https://bayesiandemography.github.io/poputils/reference/tfr.md)
  Calculate total fertility rate from age-specific fertility rates

## Examples

``` r
## create age-specific fertility rates
## based on the [Booth standard][booth_standard]
library(dplyr, warn.conflicts = FALSE)
target <- data.frame(region = c("A", "B"), 
                     tfr = c(5.5, 4.7))
asfr <- tfr_to_asfr_scale(target = target,
                          standard = booth_standard)
asfr
#> # A tibble: 16 × 3
#>    region age      asfr
#>    <chr>  <chr>   <dbl>
#>  1 A      10-14 0.00305
#>  2 A      15-19 0.146  
#>  3 A      20-24 0.266  
#>  4 A      25-29 0.254  
#>  5 A      30-34 0.206  
#>  6 A      35-39 0.147  
#>  7 A      40-44 0.0679 
#>  8 A      45-49 0.00893
#>  9 B      10-14 0.00260
#> 10 B      15-19 0.125  
#> 11 B      20-24 0.227  
#> 12 B      25-29 0.217  
#> 13 B      30-34 0.176  
#> 14 B      35-39 0.126  
#> 15 B      40-44 0.0580 
#> 16 B      45-49 0.00763

## check consistency with original TFRs
asfr |>
  tfr(asfr = asfr, by = region)
#> # A tibble: 2 × 2
#>   region   tfr
#>   <chr>  <dbl>
#> 1 A        5.5
#> 2 B        4.7

## target is an rvec
library(rvec, warn.conflicts = FALSE)
target_rvec <- data.frame(region = c("A", "B"), 
                          tfr = rnorm_rvec(n = 2,
                                           mean = c(5.5, 4.7),
                                           n_draw = 1000))
tfr_to_asfr_scale(target = target_rvec,
                  standard = booth_standard)
#> # A tibble: 16 × 3
#>    region age                      asfr
#>    <chr>  <chr>            <rdbl<1000>>
#>  1 A      10-14  0.003 (0.0019, 0.0041)
#>  2 A      15-19       0.15 (0.093, 0.2)
#>  3 A      20-24       0.27 (0.17, 0.36)
#>  4 A      25-29       0.25 (0.16, 0.35)
#>  5 A      30-34       0.21 (0.13, 0.28)
#>  6 A      35-39       0.15 (0.094, 0.2)
#>  7 A      40-44    0.068 (0.043, 0.092)
#>  8 A      45-49  0.0089 (0.0057, 0.012)
#>  9 B      10-14 0.0026 (0.0015, 0.0037)
#> 10 B      15-19      0.13 (0.072, 0.18)
#> 11 B      20-24       0.23 (0.13, 0.32)
#> 12 B      25-29       0.22 (0.13, 0.31)
#> 13 B      30-34        0.18 (0.1, 0.25)
#> 14 B      35-39      0.13 (0.073, 0.18)
#> 15 B      40-44    0.058 (0.034, 0.082)
#> 16 B      45-49  0.0076 (0.0044, 0.011)
```
