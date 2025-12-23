# Check that Arguments have Same Length

Check that `x` and `y` have the same length.

## Usage

``` r
check_equal_length(x, y, nm_x, nm_y)
```

## Arguments

- x, y:

  Arguments to compare

- nm_x, nm_y:

  Names to use in error message

## Value

'TRUE', invisibly.

## Examples

``` r
x <- 1:3
y <- 3:1
check_equal_length(x = x,
                   y = y,
                   nm_x = "x",
                   nm_y = "y")
```
