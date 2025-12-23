# Identify Sex or Gender Labels Referring to Females

Given labels for sex or gender, try to infer which (if any) refer to
females. If no elements look like a label for females, or if two or more
elements do, then return `NULL`.

## Usage

``` r
find_label_female(nms)
```

## Arguments

- nms:

  A character vector

## Value

An element of `nms` or `NULL`.

## See also

[`find_label_male()`](https://bayesiandemography.github.io/poputils/reference/find_label_male.md),
[`find_var_sexgender()`](https://bayesiandemography.github.io/poputils/reference/find_var_sexgender.md)

## Examples

``` r
find_label_female(c("Female", "Male")) ## one valid
#> [1] "Female"
find_label_female(c("0-4", "5-9"))     ## none valid
#> NULL
find_label_female(c("F", "Fem"))       ## two valid
#> NULL
```
