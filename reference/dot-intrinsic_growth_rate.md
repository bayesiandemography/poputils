# Calculate Intrinsic Growth Rate

Calculate the intrinsic growth rate implied by fertility and mortality
schedules. The intrinsic growth rate is the rate at which all age groups
in a population would eventually grow if the population was subject to
the fertility and mortality schedules indefinitely. The fertility and
mortality schedules apply to a single sex, typically females.

## Usage

``` r
.intrinsic_growth_rate(mx, Lx, age_mid)
```

## Arguments

- mx:

  Age specific fertility rates, for a single sex (typically daughters).
  An ordinary numeric vector or an
  [rvec()](https://bayesiandemography.github.io/rvec/reference/rvec.html).

- Lx:

  Life table mortality measure, for a single sex (typically females). An
  ordinary numeric vector or an
  [rvec()](https://bayesiandemography.github.io/rvec/reference/rvec.html).

- age_mid:

  The midpoint of each age group. Numeric vector.

## Value

A numeric vector of length 1.

## Details

The fertility scehdule `mx` and mortality schedule `Lx` refer only to
the reproductive ages. The mortality schedule `Lx` is the number of
years that a newborn baby would be expected to spend in each age group
under prevailing mortality rates. It is the life table quantity `nLx`
where the radix `l0` has been set to 1.

## Examples

``` r
mx <- c(0.072, 0.148, 0.156, 0.140, 0.108, 0.054, 0.013)
Lx <- c(3.49, 3.37, 3.23, 3.07, 2.91, 2.74, 2.56)
age_mid <- c(17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5)
.intrinsic_growth_rate(mx = mx, Lx = Lx, age_mid = age_mid)
#> [1] 0.02762209
```
