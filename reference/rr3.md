# Randomly Round A Vector of Integers to Base 3

Apply the 'Random Round to Base 3' (RR3) algorithm to a vector of
integers (or doubles where `round(x) == x`.)

## Usage

``` r
rr3(x)
```

## Arguments

- x:

  A vector of integers (in the sense that `round(x) == x`.) Can be an
  [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.html).

## Value

A randomly-rounded version of `x`.

## Details

The RR3 algorithm is used by statistical agencies to confidentialize
data. Under the RR3 algorithm, an integer \\n\\ is randomly rounded as
follows:

- If \\n\\ is divisible by 3, leave it unchanged

- If dividing \\n\\ by 3 leaves a remainder of 1, then round down
  (subtract 1) with probability 2/3, and round up (add 2) with
  probability 1/3.

- If dividing \\n\\ by 3 leaves a remainder of 1, then round down
  (subtract 2) with probability 1/3, and round up (add 1) with
  probability 2/3.

RR3 has some nice properties:

- The randomly-rounded version of \\n\\ has expected value \\n\\.

- If \\n\\ non-negative, then the randomly rounded version of \\n\\ is
  non-negative.

- If \\n\\ is non-positive, then the randomly rounded version of \\n\\
  is non-positive.

## Examples

``` r
x <- c(1, 5, 2, 0, -1, 3, NA)
rr3(x)
#> [1]  3  6  3  0  0  3 NA
```
