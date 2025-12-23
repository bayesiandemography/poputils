# Trim Values So They Are Between 0 and 1

Trim a vector so that all values are greater than 0 and less than 1.

## Usage

``` r
trim_01(x)
```

## Arguments

- x:

  A numeric vector. Can be an
  [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.html).

## Value

A trimmed version of `x`

## Details

If

- `min` is lowest element of `x` that is higher than 0, and

- `max` is the highest element of `x` that is lower than 1, then
  `trim_01()`

- shifts all elements of `x` that are lower than `min` upwards, so that
  they equal `min`, and

- shifts all elements of `x` that are higher than `max` downwards, so
  that they equal `max`.

## See also

- [`logit()`](https://bayesiandemography.github.io/poputils/reference/logit.md),
  [`invlogit()`](https://bayesiandemography.github.io/poputils/reference/logit.md)
  Logit transformation

## Examples

``` r
x <- c(1, 0.98, -0.001, 0.5, 0.01)
trim_01(x)
#> [1] 0.98 0.98 0.01 0.50 0.01
```
