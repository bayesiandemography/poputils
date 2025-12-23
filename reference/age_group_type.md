# Infer Age Label Type

Determine whether a set of age labels refer to one-year, five-year, or
life-table age groups.

## Usage

``` r
age_group_type(x)
```

## Arguments

- x:

  A vector of age labels

## Value

`"single"`, `"five"`, or `"lt"`.

## Details

The valid types of age labels are:

- `"single"`. One-year age groups, eg `"0"` or `"55"`, and possibly an
  open age group, eg `"90+"`.

- `"five"`. Five-year age groups, eg `"0-4"` or `"55-59"`, and possibly
  an open age group, eg `"100+"`.

- `"lt"`. Life table age groups, eg `"0"`, `"1-4"`, `"5-9"`, `"55-59"`,
  or `"80+"`.

If `x` does not fit any of these descriptions, `then age_group_type()`
throws an error.

If `x` could belong to more than one type, then `age_group_type()`
prefers `"single"` to `"five"` and `"lt"`, and prefers `"five"` to
`"lt"`.

## Examples

``` r
age_group_type(c("5-9", "0-4", "100+"))
#> [1] "five"
age_group_type(c("2", "5", "1"))
#> [1] "single"
age_group_type(c("0", "1-4"))
#> [1] "lt"

## could be any "single" or "lt"
age_group_type("0")
#> [1] "single"

## could be "five" or "lt"
age_group_type("80-84")
#> [1] "five"
```
