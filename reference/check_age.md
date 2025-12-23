# Validity Checks for Age Labels

Check that age labels can be parsed and, optionally, whether the labels
are complete, unique, start at zero, and end with an open age group.

## Usage

``` r
check_age(
  x,
  complete = FALSE,
  unique = FALSE,
  zero = FALSE,
  open = FALSE,
  closed = FALSE
)
```

## Arguments

- x:

  A vector of age labels.

- complete:

  If `TRUE`, test whether `x` has gaps.

- unique:

  If `TRUE`, test whether `x` has duplicates.

- zero:

  If `TRUE`, test whether youngest age group in `x` starts at 0.

- open:

  If `TRUE`, test whether oldest age group in `x` is open.

- closed:

  If `TRUE`, test whether oldest age group in `x` is closed.

## Value

`TRUE`, invisibly, or raises an error if a test fails.

## Details

By default, `check_age()` only tests whether a set of labels can be
parsed as single-year, five-year, or life table age groups. (See
[`age_group_type()`](https://bayesiandemography.github.io/poputils/reference/age_group_type.md)
for more on the three types of age group.) However, it can also apply
the following tests:

- `complete`. Whether `x` includes all intermediate age groups, with no
  gaps. For instance, the labels `c("10-14", "15-19", "5-9")` are
  complete, while the labels`c("15-19", "5-9")` are not (because they
  are missing `"10-14"`.)

- `unique`. Whether `x` has duplicated labels.

- `zero`. Whether the youngest age group in `x` starts at age 0, ie
  whether it includes `"0"` or `"0-4"`.

- `open`. Whether the oldest age group in `x` is "open", with no upper
  limit, eg `"100+"` or `"65+"`.

- `closed`. Whether the oldest age group in `x` is "closed", with an
  upper limit, eg `"100-104+"` or `"65"`.

## See also

- [`reformat_age()`](https://bayesiandemography.github.io/poputils/reference/reformat_age.md)
  to convert age labels to the format used by **poputils**.

## Examples

``` r
try(
  check_age(c("10-14", "0-4", "15+"),
            complete = TRUE)  
)
#> Error in check_age(c("10-14", "0-4", "15+"), complete = TRUE) : 
#>   Age group "5-9" is missing.

try(
  check_age(c("10-14", "5-9", "0-4", "5-9", "15+"),
            unique = TRUE)
)
#> Error in check_age(c("10-14", "5-9", "0-4", "5-9", "15+"), unique = TRUE) : 
#>   Age group "5-9" is duplicated.

try(
  check_age(c("10-14", "5-9", "15+"),
            zero = TRUE)
)
#> Error in check_age(c("10-14", "5-9", "15+"), zero = TRUE) : 
#>   Youngest age group does not start at 0.
#> ℹ Youngest age group is "5-9".

try(
  check_age(c("10-14", "0-4", "5-9"),
            open = TRUE)
)
#> Error in check_age(c("10-14", "0-4", "5-9"), open = TRUE) : 
#>   Oldest age group is not open.
#> ℹ Oldest age group is "10-14".

try(
  check_age(c("10+", "0-4", "5-9"),
            closed = TRUE)
)
#> Error in check_age(c("10+", "0-4", "5-9"), closed = TRUE) : 
#>   Oldest age group is not closed.
#> ℹ Oldest age group is "10+".
```
