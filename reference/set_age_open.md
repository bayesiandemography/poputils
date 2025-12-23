# Specify Open Age Group

Set the lower limit of the open age group. Given a vector of age group
labels, recode all age groups with a lower limit greater than or equal
to `<lower>` to `<lower>+`.

## Usage

``` r
set_age_open(x, lower)
```

## Arguments

- x:

  A vector of age labels.

- lower:

  An integer. The lower limit for the open age group.

## Value

A modified version of `x`.

## Details

`set_age_open()` requires that `x` and the return value have a a
five-year, single-year, or life table format, as described in
[`age_labels()`](https://bayesiandemography.github.io/poputils/reference/age_labels.md).

## See also

- `set_age_open()` uses
  [`age_lower()`](https://bayesiandemography.github.io/poputils/reference/age_lower.md)
  to identify lower limits

- [`age_labels()`](https://bayesiandemography.github.io/poputils/reference/age_labels.md)
  for creating age labels from scratch

## Examples

``` r
x <- c("100+", "80-84", "95-99", "20-24")
set_age_open(x, 90)
#> [1] "90+"   "80-84" "90+"   "20-24"
set_age_open(x, 25)
#> [1] "25+"   "25+"   "25+"   "20-24"
```
