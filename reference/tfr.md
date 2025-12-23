# Calculate Total Fertility Rates

Calculate the total fertility rate (TFR) from age-specific fertility
rates.

## Usage

``` r
tfr(
  data,
  asfr = asfr,
  age = age,
  sex = NULL,
  by = NULL,
  denominator = 1,
  suffix = NULL
)
```

## Arguments

- data:

  Data frame with age-specific fertility rates and age

- asfr:

  \<[`tidyselect`](https://tidyselect.r-lib.org/reference/language.html)\>
  Age-specific fertility rates. Possibly an
  [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.html).
  Default is `asfr`.

- age:

  \<[`tidyselect`](https://tidyselect.r-lib.org/reference/language.html)\>
  Age group labels. The labels must be interpretable by functions such
  as
  [`reformat_age()`](https://bayesiandemography.github.io/poputils/reference/reformat_age.md)
  and
  [`age_group_type()`](https://bayesiandemography.github.io/poputils/reference/age_group_type.md).
  The age groups must not have gaps, and the highest age group must be
  "closed" (ie have an upper limit.) Default is `age`.

- sex:

  \<[`tidyselect`](https://tidyselect.r-lib.org/reference/language.html)\>
  Sex/gender of the child (not the parent).

- by:

  \<[`tidyselect`](https://tidyselect.r-lib.org/reference/language.html)\>
  Separate total fertility rates are calculated for each combination the
  `by` variables. If `data` is a
  [grouped](https://dplyr.tidyverse.org/reference/group_data.html) data
  frame, then the grouping variables take precedence over `by`.

- denominator:

  The denominator used to calculate `asfr`. Default is 1.

- suffix:

  Optional suffix added to `"tfr"` column in result.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html).

## Details

The total fertility rate is a summary measures for current fertility
levels that removes the effect of age structure. Is obtained by summing
up age-specific fertility rates, multiplying each rate by the width of
the corresponding age group. For instance, the rate for age group
"15-19" is multiplied by 5, and the rate for age group "15" is
multiplied by 1.

The total fertility rate can be interpreted as the number of average
children that a person would have, under prevailing fertility rates, if
the person survived to the maximum age of reproduction. The hypothetical
person is normally a woman, since age-specific fertility rates normally
use person-years lived by women as the denominator. But it can apply to
men, if the age-specific fertility rates are "paternity rates", ie rates
that use person-years lived by men as the denominator.

## Sex-specific fertility rates

Age-specific fertility rates do not normally specify the sex of the
children who are born. In cases where they do, however, rates have to be
summed across sexes to give the total fertility rates. If `tfr()` is
supplied with a `sex` argument, it assumes that `sex` applies to the
births, and sums over the sexes.

## Denominator

Published tables of age-specific fertility rates often express the rates
as births per 1000 person-years lived, rather than per person-year
lived. (Sometimes this is expressed as "births per 1000 women".) In
these cases

## Using rvecs to represent uncertainty

An [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.html)
is a 'random vector', holding multiple draws from a distribution. Using
an rvec for the `asfr` argument to `tfr()` is a way of representing
uncertainty. This uncertainty is propagated through to the TFR, which
will also be rvecs.

## See also

- [`lifeexp()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md)
  Calculate life expectancy from age-specific mortality rates.

## Examples

``` r
irn_fert |>
  tfr(asfr = rate,
      by = c(area, time),
      denominator = 1000)
#> # A tibble: 30 × 3
#>    area   time   tfr
#>    <chr> <dbl> <dbl>
#>  1 Rural  1986  7.08
#>  2 Rural  1987  6.87
#>  3 Rural  1988  6.62
#>  4 Rural  1989  6.55
#>  5 Rural  1990  6.43
#>  6 Rural  1991  6.01
#>  7 Rural  1992  5.29
#>  8 Rural  1993  4.90
#>  9 Rural  1994  4.37
#> 10 Rural  1995  3.82
#> # ℹ 20 more rows
```
