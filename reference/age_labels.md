# Create Age Labels

Create labels for age groups. The labels depend on the `type` argument:

- `"single"`. One-year age groups, eg `"0"` or `"55"`, and possibly an
  open age group, eg `"90+"`.

- `"five"`. Five-year age groups, eg `"0-4"` or `"55-59"`, and possibly
  an open age group, eg `"100+"`.

- `"lt"`. Life table age groups, eg `"0"`, `"1-4"`, `"5-9"`, `"55-59"`,
  or `"80+"`.

## Usage

``` r
age_labels(type, min = 0, max = 100, open = NULL)
```

## Arguments

- type:

  Type of age group labels: `"single"`, `"five"`, or `"lt"`.

- min:

  Minimum age. Defaults to 0.

- max:

  Maximum age for closed age groups. Defaults to 100.

- open:

  Whether the last age group is "open", ie has no upper limit.

## Value

A character vector.

## Details

The first age group starts at the age specified by `min`. If `open` is
`TRUE`, then the final age group starts at the age specified by `max`.
Otherwise, the final age group ends at the age specified by `max`.

`open` defaults to `TRUE` when `min` equals zero, and to `FALSE`
otherwise.

## See also

[`reformat_age()`](https://bayesiandemography.github.io/poputils/reference/reformat_age.md)

## Examples

``` r
age_labels(type = "single", min = 15, max = 40)
#>  [1] "15" "16" "17" "18" "19" "20" "21" "22" "23" "24" "25" "26" "27" "28" "29"
#> [16] "30" "31" "32" "33" "34" "35" "36" "37" "38" "39"
age_labels(type = "five")
#>  [1] "0-4"   "5-9"   "10-14" "15-19" "20-24" "25-29" "30-34" "35-39" "40-44"
#> [10] "45-49" "50-54" "55-59" "60-64" "65-69" "70-74" "75-79" "80-84" "85-89"
#> [19] "90-94" "95-99" "100+" 
age_labels(type = "lt", max = 80)
#>  [1] "0"     "1-4"   "5-9"   "10-14" "15-19" "20-24" "25-29" "30-34" "35-39"
#> [10] "40-44" "45-49" "50-54" "55-59" "60-64" "65-69" "70-74" "75-79" "80+"  
```
