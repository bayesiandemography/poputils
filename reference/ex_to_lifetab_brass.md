# Derive Life Tables that Match Life Expectancies, using a Brass Logit Model

This function contained an error, and is deprecated. Please use function
[`e0_to_lifetab_logit()`](https://bayesiandemography.github.io/poputils/reference/e0_to_lifetab_logit.md)
instead.

## Usage

``` r
ex_to_lifetab_brass(
  target,
  standard,
  infant = c("constant", "linear", "CD", "AK"),
  child = c("constant", "linear", "CD"),
  closed = c("constant", "linear"),
  open = "constant",
  radix = 1e+05,
  suffix = NULL
)
```

## Arguments

- target:

  A data frame containing a variable called `"ex"`, and possibly other
  varibles. See Details.

- standard:

  A data frame containing variables called `age` and `lx`, and possibly
  others. See Details.

- infant, child, closed, open:

  Methods used to calculate life expectancy. See
  [`lifetab()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md)
  for details.

- radix:

  Initial population for the `lx` column in the derived life table(s).
  Default is `100000`.

- suffix:

  Optional suffix added to life table columns.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html).

## Details

Function `ex_to_lifetab_brass()` used formula

\$\$\text{logit}(l_x^{\text{B}}) \approx \alpha + \beta
\text{logit}(l_x^{\text{A}})\$\$,

instead of the conventional

\$\$\text{logit}(1-l_x^{\text{B}}) \approx \alpha + \beta
\text{logit}(1-l_x^{\text{A}})\$\$.
