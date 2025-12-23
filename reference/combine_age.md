# Aggregate Age Group Labels

Convert age group labels to a less detailed classification. The three
classifications recognized by `combine_age()` are `"single"`, `"five"`,
and `"lt"`, as defined on
[`age_labels()`](https://bayesiandemography.github.io/poputils/reference/age_labels.md).
The following conversions are permitted:

- `"single"` —\> `"lt"`

- `"single"` —\> `"five"`

- `"lt"` —\> `"five"`

## Usage

``` r
combine_age(x, to = c("five", "lt"))
```

## Arguments

- x:

  A vector of age labels

- to:

  Type of age classification to convert to: `"five"` or `"lt"`. Defaults
  to `"five"`.

## Value

If `x` is a factor, then `combine_age()` returns a factor; otherwise it
returns a character vector.

## See also

- [`age_labels()`](https://bayesiandemography.github.io/poputils/reference/age_labels.md)
  to create age group labels

- [`reformat_age()`](https://bayesiandemography.github.io/poputils/reference/reformat_age.md)
  to convert existing age group labels to a standard format

- [`set_age_open()`](https://bayesiandemography.github.io/poputils/reference/set_age_open.md)
  to set the lower limit of the open age group

## Examples

``` r
x <- c("0", "5", "3", "12")
combine_age(x)
#> [1] "0-4"   "5-9"   "0-4"   "10-14"
combine_age(x, to = "lt")
#> [1] "0"     "5-9"   "1-4"   "10-14"
```
