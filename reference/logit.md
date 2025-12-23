# Logit and Inverse-Logit Functions

Transform values to and from the logit scale. `logit()` calculates

## Usage

``` r
logit(p)

invlogit(x)
```

## Arguments

- p:

  Values between 0 and 1. Can be an atomic vector, a matrix, or an
  [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.html).

- x:

  Values in the interval `(-Inf, Inf)`. Can be an atomic vector, a
  matrix, or an
  [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.html).

## Value

- A vector of doubles, if `p` or `x` is a vector.

- A matrix of doubles, if `p` or `x` is a matrix.

- An object of class `rvec_dbl`, if `p` or `x` is an rvec.

## Details

\$\$x = \log \left(\frac{p}{1 - p}\right)\$\$

and `invlogit()` calculates

\$\$p = \frac{e^x}{1 + e^x}\$\$

To avoid overflow, `invlogit()` uses \\p = \frac{1}{1 + e^{-x}}\\
internally for \\x\\ where \\x \> 0\\.

In some of the demographic literature, the logit function is defined as

\$\$x = \frac{1}{2} \log \left(\frac{p}{1 - p}\right).\$\$

`logit()` and `invlogit()` follow the conventions in statistics and
machine learning, and omit the \\\frac{1}{2}\\.

## Examples

``` r
p <- c(0.5, 1, 0.2)
logit(p)
#> [1]  0.000000       Inf -1.386294
invlogit(logit(p))
#> [1] 0.5 1.0 0.2
```
