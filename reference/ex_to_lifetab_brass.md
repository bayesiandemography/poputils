# Derive Life Tables that Match Life Expectancies, using a Brass Logit Model

Turn life expectancies at birth into full life tables, using the Brass
logit model. The method is simple and is designed for simulations or for
settings with little or no data on age-specific mortality rates. In
settings where data on age-specific mortality is available, other
methods might be more appropriate.

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

  A data frame containing a variable called `"ex"`, and possibly others.
  See Details.

- standard:

  A data frame containing variables called `age` and `lx`, and possibly
  others. See details.

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

## Method

The method implemented by `ex_to_lifetab_brass()` is based on the
observation that, if populations A and B are demographically similar,
then, in many cases,

\$\$\text{logit}(l_x^{\text{B}}) \approx \alpha + \beta
\text{logit}(l_x^{\text{A}})\$\$

where \\l_x\\ is the "survivorship probability" quantity from a life
table. When populations are similar, \\beta\\ is often close to 1.

Given (i) target life expectancy, (ii) a set of \\l_x^{\text{A}}\\),
(referred to as a "standard"), and (iii) a value for \\\beta\\,
`ex_to_lifetab_brass()` finds a value for \\\alpha\\ that yields a set
of \\l_x^{\text{B}}\\) with the required life expectancy.

## `target` argument

`target` is a data frame specifying life expectancies for each
population being modelled, and, optionally, inputs to the calculations,
and 'by' variables. Values in `target` are not age-specific.

Variables in `target`:

- A variable called `"ex"`, with life expectancy at birth must be
  included in `target`.

- A variable called `"beta"` with values for `beta` can be included in
  `target`. This variable can be an
  [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.html).
  If no `"beta"` variable is included in `target`, then
  `ex_to_lifetab_brass()` assumes that \\beta \equiv 1\\.

- A variable called `"sex"`. If the `infant` argument to
  `ex_to_lifetab_brass()` is is `"CD"` or `"AK"`, or if the `child`
  argument is `"CD"`, `target` must include a
  `"sex" variable, and the labels for this variable must be interpretable by function [format_sex()]. Otherwise, the `"sex"\`
  variable is optional, and there is no restriction on labels.

- 'by' variables used to distinguish between life expectancies, such as
  time, region, or model variant.

## `standard` argument

`standard` is a data frame specifying the \\l_x\\ to be used with each
life expectancy in `target`, and, optionally, values for the average age
person-years lived by people who die in each group, \\\_na_x\\. Values
in `standard` are age-specific.

Variables in `standard`:

- A variable called `"age"`, with labels that can be parsed by
  [`reformat_age()`](https://bayesiandemography.github.io/poputils/reference/reformat_age.md).

- A variable called `"lx"`. Internally each set of \\l_x\\ is are
  standardized so that the value for age 0 equals 1. Within each set,
  values must be non-increasing. Cannot be an rvec.

- Additional variables used to match rows in `standard` to rows in
  `target`.

Internally, `standard` is merged with `target` using a left join from
`target`, on any variables that `target` and `standard` have in common.

## References

Brass W, Coale AJ. 1968. “Methods of analysis and estimation,” in Brass,
W, Coale AJ, Demeny P, Heisel DF, et al. (eds). The Demography of
Tropical Africa. Princeton NJ: Princeton University Press, pp. 88–139.

Moultrie TA, Timæus IM. 2013. Introduction to Model Life Tables. In
Moultrie T, Dorrington R, Hill A, Hill K, Timæus I, Zaba B. (eds). Tools
for Demographic Estimation. Paris: International Union for the
Scientific Study of Population. [online
version](https://demographicestimation.iussp.org/content/using-models-derive-life-tables-incomplete-data).

## See also

- [tfr_to_asfr_scale](https://bayesiandemography.github.io/poputils/reference/tfr_to_asfr_scale.md)
  Fertility equivalent of `ex_to_lifetab_brass()`

- [`logit()`](https://bayesiandemography.github.io/poputils/reference/logit.md),
  [`invlogit()`](https://bayesiandemography.github.io/poputils/reference/logit.md)
  Logit function

- [`lifeexp()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md)
  Calculate life expectancy from detailed inputs

## Examples

``` r
## create new life tables based on level-1
## 'West' model life tables, but with lower
## life expectancy

library(dplyr, warn.conflicts = FALSE)

target <- data.frame(sex = c("Female", "Male"), 
                     ex = c(17.5, 15.6))

standard <- west_lifetab |>
    filter(level == 1) |>
    select(sex, age, lx)
    
ex_to_lifetab_brass(target = target,
                    standard = standard,
                    infant = "CD",
                    child = "CD")
#> # A tibble: 42 × 7
#>    sex    age       qx      lx     dx      Lx    ex
#>    <chr>  <fct>  <dbl>   <dbl>  <dbl>   <dbl> <dbl>
#>  1 Female 0     0.412  100000  41204.  73218.  17.5
#>  2 Female 1-4   0.286   58796. 16790. 190877.  28.5
#>  3 Female 5-9   0.0793  42006.  3331. 201590.  35.4
#>  4 Female 10-14 0.0617  38675.  2387. 187346.  33.2
#>  5 Female 15-19 0.0793  36288.  2879. 174145.  30.2
#>  6 Female 20-24 0.0979  33409.  3270. 158729.  27.6
#>  7 Female 25-29 0.109   30139.  3273. 142356.  25.4
#>  8 Female 30-34 0.122   26866.  3264. 125994.  23.1
#>  9 Female 35-39 0.131   23602.  3103. 110070.  21.0
#> 10 Female 40-44 0.138   20499.  2838.  95223.  18.8
#> # ℹ 32 more rows
```
