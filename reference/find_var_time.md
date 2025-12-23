# Identify a Time Variable

Find the element of `nms` that looks like an time variable. If no
elements look like a time variable, or if two or more elements do, then
return `NULL`.

## Usage

``` r
find_var_time(nms)
```

## Arguments

- nms:

  A character vector

## Value

An element of `nms`, or `NULL`.

## See also

[`find_var_age()`](https://bayesiandemography.github.io/poputils/reference/find_var_age.md),
[`find_var_sexgender()`](https://bayesiandemography.github.io/poputils/reference/find_var_sexgender.md)

## Examples

``` r
find_var_time(c("Sex", "Year", "AgeGroup", NA)) ## one valid
#> [1] "Year"
find_var_time(c("Sex", "Region"))               ## none valid
#> NULL
find_var_time(c("time", "year"))                ## two valid
#> NULL
```
