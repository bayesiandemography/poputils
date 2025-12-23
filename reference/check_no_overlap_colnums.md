# Check that Colnum Vectors do not Overlap

Given a named list of colnum vectors, like those produced by
[`tidyselect::eval_select()`](https://tidyselect.r-lib.org/reference/eval_select.html),
throw an error if there is an overlap.

## Usage

``` r
check_no_overlap_colnums(x)
```

## Arguments

- x:

  A named list of integer vectors.

## Value

`TRUE`, invisibly

## See also

[`tidyselect::eval_select()`](https://tidyselect.r-lib.org/reference/eval_select.html)

## Examples

``` r
x <- list(arg1 = c(age = 1L),
          arg2 = c(gender = 4L, region = 5L))
check_no_overlap_colnums(x)
```
