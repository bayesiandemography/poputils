# Reformat a Binary Sex Variable

Reformat a binary sex variable so that it consists entirely of values
`"Female"`, `"Male"`, and possibly `NA` and any values included in
`except`.

## Usage

``` r
reformat_sex(x, except = NULL, factor = TRUE)
```

## Arguments

- x:

  A vector.

- except:

  Values to exclude when reformatting.

- factor:

  Whether the return value should be a factor.

## Value

If `factor` is `TRUE`, then
[`reformat_age()`](https://bayesiandemography.github.io/poputils/reference/reformat_age.md)
returns a factor; otherwise it returns a character vector.

## Details

When parsing labels, `reformat_sex()` ignores case: `"FEMALE"` and
`"fEmAlE"` are equivalent.

White space is removed from the beginning and end of labels.

`reformat_sex()` does not try to interpreting numeric codes (eg `1`,
`2`).

## See also

[`age_labels()`](https://bayesiandemography.github.io/poputils/reference/age_labels.md),
[`reformat_age()`](https://bayesiandemography.github.io/poputils/reference/reformat_age.md)

## Examples

``` r
reformat_sex(c("F", "female", NA, "MALES"))
#> [1] Female Female <NA>   Male  
#> Levels: Female Male <NA>

## values supplied for 'except'
reformat_sex(c("Fem", "Other", "Male", "M"),
             except = c("Other", "Diverse"))
#> [1] Female Other  Male   Male  
#> Levels: Female Male Other Diverse

## return an ordinary character vector
reformat_sex(c("F", "female", NA, "MALES"),
             factor = FALSE)
#> [1] "Female" "Female" NA       "Male"  
```
