# Get a named vector of column indices for the grouping variables in a grouped data frame

Constructed a named vector of indices equivalent to the vectors produced
by tidyselect::eval_select, but for the grouping variables in an object
of class "grouped_df".

## Usage

``` r
groups_colnums(data)
```

## Arguments

- data:

  A data frame.

## Value

A named integer vector.

## Details

If `data` is not grouped, then `groups_colnums` returns a zero-length
vector.

## Examples

``` r
library(dplyr)
df <- data.frame(x = 1:4,
                 g = c(1, 1, 2, 2))
groups_colnums(df)
#> named integer(0)
df <- group_by(df, g)
groups_colnums(df)
#> g 
#> 2 
```
