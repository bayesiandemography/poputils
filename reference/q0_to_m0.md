# Convert q0 to m0

Convert the probability of dying during infancy (q0) to the mortality
rate for infancy (m0).

## Usage

``` r
q0_to_m0(
  q0,
  sex = NULL,
  a0 = NULL,
  infant = c("constant", "linear", "CD", "AK")
)
```

## Arguments

- q0:

  Probability of dying in first year of life. A numeric vector or an
  [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.html).

- sex:

  Biological sex. A vector the same length as `q0`, with labels that can
  be interpreted by
  [`reformat_sex()`](https://bayesiandemography.github.io/poputils/reference/reformat_sex.md).
  Needed only when `infant` is `"CD"` or `"AK"`.

- a0:

  Average age at death for infants who die. Optional. See help for
  [`lifetab()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md).

- infant:

  Calculation method. See help for
  [`lifetab()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md).
  Default is `"constant"`.

## Value

A numeric vector or
[rvec](https://bayesiandemography.github.io/rvec/reference/rvec.html).

## Warning

The term "infant mortality rate" is ambiguous. Demographers sometimes
use it to refer to m0 (which is an actual rate) and sometimes use it to
refer to q0 (which is a probability.)

## See also

- [`lifetab()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md)
  Calculate a full life table.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)
west_lifetab |>
 filter(age == 0, level <= 5) |>
 select(level, sex, age, mx, qx) |>
 mutate(m0 = q0_to_m0(q0 = qx, sex = sex, infant = "CD"))
#> # A tibble: 10 × 6
#>    level sex    age      mx    qx    m0
#>    <int> <chr>  <fct> <dbl> <dbl> <dbl>
#>  1     1 Female 0     0.479 0.366 0.479
#>  2     1 Male   0     0.584 0.420 0.584
#>  3     2 Female 0     0.427 0.334 0.427
#>  4     2 Male   0     0.517 0.384 0.517
#>  5     3 Female 0     0.381 0.306 0.381
#>  6     3 Male   0     0.460 0.352 0.460
#>  7     4 Female 0     0.342 0.280 0.342
#>  8     4 Male   0     0.411 0.323 0.411
#>  9     5 Female 0     0.307 0.256 0.307
#> 10     5 Male   0     0.369 0.296 0.369
```
