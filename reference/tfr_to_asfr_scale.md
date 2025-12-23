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
  possibly others. See details.

- suffix:

  Optional suffix added to `asfr` column in results.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html).

## Method

The age-specific rates are derived by finding a value \\\alpha\\ such
that

\$\$f_x = \alpha f_x^{\mathrm{std}}\$\$

and

\$\$sum_x f_x = F\$\$

where

- \\f_x\\ is the age-specific fertility rate;

- \\f_x^{\mathrm{std}}\\ is the standard schedule of rates;

- \\\alpha\\ is a multiplier shared by all age groups; and

- \\F\\ is the target total fertility rate.

## `target` argument

`target` is a data frame specifying total fertility rates for each
population being modelled, Values in `target` are not age-specific.

Variables in `target`:

- A variable called `"tfr"`. Can be an ordinary numeric variable, or an
  [rvec()](https://bayesiandemography.github.io/rvec/reference/rvec.html).

- Optionally, 'by' variables distinguishing populations, such as
  `"region"` or `"time"`.

## `standard` argument

`standard` is a data frame specifying standard fertility scedules to be
used with each life expectancy in `target`. Values in `standard` are
age-specific.

Variables in `standard`:

- A variable called `"age"`, with labels that can be parsed by
  [`reformat_age()`](https://bayesiandemography.github.io/poputils/reference/reformat_age.md).

- A variable called `"value"`, containing non-negative values. Cannot be
  an rvec.

- Additional variables used to match rows in `standard` to rows in
  `target`.

Internally, `standard` is merged with `target` using a left join from
`target`, on any variables that `target` and `standard` have in common.

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
```
