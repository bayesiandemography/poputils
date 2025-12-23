# Lower Limits, Midpoints, and Upper Limits of Age Groups

Given a vector `x` of age group labels, return a numeric vector.

- `age_lower()` returns the lower limits of each age group,

- `age_mid()` returns the midpoints, and

- `age_upper()` returns the upper limits.

Vector `x` must describe 1-year, 5-year or life-table age groups: see
[`age_labels()`](https://bayesiandemography.github.io/poputils/reference/age_labels.md)
for examples. `x` can format these age groups in any way understood by
[`reformat_age()`](https://bayesiandemography.github.io/poputils/reference/reformat_age.md).

## Usage

``` r
age_lower(x)

age_mid(x)

age_upper(x)
```

## Arguments

- x:

  A vector of age group labels.

## Value

A numeric vector, the same length as `x`.

## Details

These functions can make age groups easier to work with. Lower and upper
limits can be used for selecting on age. Replacing age group with
midpoints can improve graphs.

## See also

[`reformat_age()`](https://bayesiandemography.github.io/poputils/reference/reformat_age.md)
[`age_labels()`](https://bayesiandemography.github.io/poputils/reference/age_labels.md)

## Examples

``` r
x <- c("15-19", "5-9", "50+")
age_lower(x)
#> [1] 15  5 50
age_mid(x)
#> [1] 17.5  7.5 52.5
age_upper(x)
#> [1]  20  10 Inf

## non-standard formats are OK
age_lower(c("infants", "100 and over"))
#> [1]   0 100

df <- data.frame(age = c("1-4", "10-14", "5-9", "0"),
                 rate = c(0.023, 0.015, 0.007, 0.068))
df
#>     age  rate
#> 1   1-4 0.023
#> 2 10-14 0.015
#> 3   5-9 0.007
#> 4     0 0.068
subset(df, age_lower(age) >= 5)
#>     age  rate
#> 2 10-14 0.015
#> 3   5-9 0.007
```
