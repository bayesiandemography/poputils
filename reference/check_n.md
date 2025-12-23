# Check Whole Number

Check that `n` is finite, non-NA scalar that is an integer or integerish
(ie is equal to `round(n)`), and optionally within a specified range and
divisible by a specified number.

## Usage

``` r
check_n(n, nm_n, min, max, divisible_by)
```

## Arguments

- n:

  A whole number

- nm_n:

  Name for 'n' to be used in error messages

- min:

  Minimum value 'n' can take. Can be NULL.

- max:

  Maximum values 'n' can take. Can be NULL.

- divisible_by:

  'n' must be divisible by this. Can be NULL.

## Value

If all tests pass, `check_n()` returns `TRUE` invisibly. Otherwise it
throws an error.

## Examples

``` r
check_n(10, nm_n = "count", min = 0, max = NULL, divisible_by = 1)
check_n(10, nm_n = "count", min = NULL, max = NULL, divisible_by = NULL)
check_n(10, nm_n = "n", min = 5, max = 10, divisible_by = 2)
```
