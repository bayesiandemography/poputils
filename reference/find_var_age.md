# Identify an Age Variable

Find the element of `nms` that looks like an age variable. If no
elements look like an age variable, or if two or more elements do, then
return `NULL`.

## Usage

``` r
find_var_age(nms)
```

## Arguments

- nms:

  A character vector

## Value

An element of `nms`, or `NULL`.

## See also

[`find_var_time()`](https://bayesiandemography.github.io/poputils/reference/find_var_time.md),
[`find_var_sexgender()`](https://bayesiandemography.github.io/poputils/reference/find_var_sexgender.md)

## Examples

``` r
find_var_age(c("Sex", "Year", "AgeGroup", NA)) ## one valid
#> [1] "AgeGroup"
find_var_age(c("Sex", "Year"))                 ## none valid
#> NULL
find_var_age(c("age", "age.years"))            ## two valid
#> NULL
```
