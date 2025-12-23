# Reformat Age Group Labels

Convert age group labels to one of three formats:

- Single-year age groups, eg `"0"`, `"1"`, ..., `"99"`, `"100+"`.

- Life table age groups, eg `"0"`,
  `"1-4", `"5-9"`, ..., `"95-99"`, `"100+"\`.

- Five-year age groups, eg `"0-4"`, `"5-9"`, ..., `"95-99"`, `"100+"`.

By default `reformat_age()` returns a factor that includes all
intermediate age groups. See below for examples.

## Usage

``` r
reformat_age(x, factor = TRUE)
```

## Arguments

- x:

  A vector.

- factor:

  Whether the return value should be a factor.

## Value

If `factor` is `TRUE`, then `reformat_age()` returns a factor; otherwise
it returns a character vector.

## Details

`reformat_age()` applies the following algorithm:

1.  Tidy and translate text, eg convert `"20 to 24 years"` to `"20-24"`,
    convert `"infant"` to `"0"`, or convert `"100 or more"` to `"100+"`.

2.  Check whether the resulting labels could have been produced by
    [`age_labels()`](https://bayesiandemography.github.io/poputils/reference/age_labels.md).
    If not, throw an error.

3.  If `factor` is `TRUE` (the default), then return a factor. The
    levels of this factor include all intermediate age groups. Otherwise
    return a character vector.

When `x` consists entirely of numbers, `reformat_age()` also checks for
two special cases:

- If every element of `x` is a multiple of 5, and if `max(x) >= 50`,
  then `x` is assumed to describe 5-year age groups

- If every element of `x` is 0, 1, or a multiple of 5, with
  `max(x) >= 50`, then `x` is assumed to describe life table age groups.

## See also

[`age_labels()`](https://bayesiandemography.github.io/poputils/reference/age_labels.md),
[`reformat_sex()`](https://bayesiandemography.github.io/poputils/reference/reformat_sex.md)

## Examples

``` r
reformat_age(c("80 to 84", "90 or more", "85 to 89"))
#> [1] 80-84 90+   85-89
#> Levels: 80-84 85-89 90+

## factor contains intermediate level missing from 'x'
reformat_age(c("80 to 84", "90 or more"))
#> [1] 80-84 90+  
#> Levels: 80-84 85-89 90+

## non-factor
reformat_age(c("80 to 84", "90 or more"),
          factor = FALSE)
#> [1] "80-84" "90+"  

## single
reformat_age(c("80", "90plus"))
#> [1] 80  90+
#> Levels: 80 81 82 83 84 85 86 87 88 89 90+

## life table
reformat_age(c("0",
            "30-34",
            "10--14",
            "1-4 years"))
#> [1] 0     30-34 10-14 1-4  
#> Levels: 0 1-4 5-9 10-14 15-19 20-24 25-29 30-34
```
