# Turn a Matrix Into a List of Columns or Rows

Given a matrix, create a list, each element of which contains a column
or row from the matrix.

## Usage

``` r
matrix_to_list_of_cols(m)

matrix_to_list_of_rows(m)
```

## Arguments

- m:

  A matrix

## Value

- `matrix_to_list_of_cols()` A list of vectors, each of which is a
  column from `x`.

- `matrix_to_list_of_rows()`, A list of vectors, each of which is a row
  from `x`.

## Details

`matrix_to_list_of_cols()` and \`matrix_to_list_of_rows() are internal
functions, for use by developers, and would not normally be called
directly by end users.

## Examples

``` r
m <- matrix(1:12, nrow = 3)
matrix_to_list_of_cols(m)
#> [[1]]
#> [1] 1 2 3
#> 
#> [[2]]
#> [1] 4 5 6
#> 
#> [[3]]
#> [1] 7 8 9
#> 
#> [[4]]
#> [1] 10 11 12
#> 
matrix_to_list_of_rows(m)
#> [[1]]
#> [1]  1  4  7 10
#> 
#> [[2]]
#> [1]  2  5  8 11
#> 
#> [[3]]
#> [1]  3  6  9 12
#> 
```
