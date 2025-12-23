# Build a Matrix from Measure and ID Variables

Build a matrix where the elements are values of a measure variable, and
the rows and columns are formed by observed combinations of ID
variables. The ID variables picked out by `rows` and `cols` must
uniquely identify cells. `to_matrix()`, unlike
[`stats::xtabs()`](https://rdrr.io/r/stats/xtabs.html), does not sum
across multiple combinations of ID variables.

## Usage

``` r
to_matrix(x, rows, cols, measure)
```

## Arguments

- x:

  A data frame.

- rows:

  The ID variable(s) used to distinguish rows in the matrix.

- cols:

  The ID variable(s) used to distinguish columns in the matrix.

- measure:

  The measure variable, eg rates or counts.

## Value

A matrix

## Examples

``` r
x <- expand.grid(age = c(0, 1, 2),
                 sex = c("F", "M"),
                 region = c("A", "B"),
                 year = 2000:2001)
x$count <- 1:24

to_matrix(x,
          rows = c(age, sex),
          cols = c(region, year),
          measure = count)
#>     A.2000 B.2000 A.2001 B.2001
#> 0.F      1      7     13     19
#> 1.F      2      8     14     20
#> 2.F      3      9     15     21
#> 0.M      4     10     16     22
#> 1.M      5     11     17     23
#> 2.M      6     12     18     24

to_matrix(x,
          rows = c(age, sex, region),
          cols = year,
          measure = count)
#>       2000 2001
#> 0.F.A    1   13
#> 1.F.A    2   14
#> 2.F.A    3   15
#> 0.M.A    4   16
#> 1.M.A    5   17
#> 2.M.A    6   18
#> 0.F.B    7   19
#> 1.F.B    8   20
#> 2.F.B    9   21
#> 0.M.B   10   22
#> 1.M.B   11   23
#> 2.M.B   12   24

## cells not uniquely identified
try(
to_matrix(x,
          rows = age,
          cols = sex,
          measure = count)
)
#> Error in to_matrix(x, rows = age, cols = sex, measure = count) : 
#>   `x` has two rows with values age="0", sex="F".
```
