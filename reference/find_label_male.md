# Identify Sex or Gender Labels Referring to Males

Given labels for sex or gender, try to infer which (if any) refer to
males. If no elements look like a label for males, or if two or more
elements do, then return `NULL`.

## Usage

``` r
find_label_male(nms)
```

## Arguments

- nms:

  A character vector

## Value

An element of `nms` or `NULL`.

## See also

[`find_label_female()`](https://bayesiandemography.github.io/poputils/reference/find_label_female.md),
[`find_var_sexgender()`](https://bayesiandemography.github.io/poputils/reference/find_var_sexgender.md)

## Examples

``` r
find_label_male(c("Female", "Male")) ## one valid
#> [1] "Male"
find_label_male(c("0-4", "5-9"))     ## none valid
#> NULL
find_label_male(c("male", "m"))      ## two valid
#> NULL
```
