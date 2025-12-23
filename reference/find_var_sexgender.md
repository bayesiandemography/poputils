# Identify a Sex or Gender Variable

Find the element of `nms` that looks like a sex or gender variable. If
no elements look like a sex or gender variable, or if two or more
elements do, then return `NULL`.

## Usage

``` r
find_var_sexgender(nms)
```

## Arguments

- nms:

  A character vector

## Value

An element of `nms`, or `NULL`.

## See also

[`find_var_age()`](https://bayesiandemography.github.io/poputils/reference/find_var_age.md),
[`find_var_time()`](https://bayesiandemography.github.io/poputils/reference/find_var_time.md),
[`find_label_female()`](https://bayesiandemography.github.io/poputils/reference/find_label_female.md),
[`find_label_male()`](https://bayesiandemography.github.io/poputils/reference/find_label_male.md)

## Examples

``` r
find_var_sexgender(c("Sex", "Year", "AgeGroup", NA)) ## one valid
#> [1] "Sex"
find_var_sexgender(c("Age", "Region"))               ## none valid
#> NULL
find_var_sexgender(c("sexgender", "sexes"))          ## two valid
#> NULL
```
