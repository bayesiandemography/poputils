# Derive Life Tables that Match Life Expectancies, using a Brass Logit Model

Turn life expectancies at birth into full life tables, using the Brass
logit model. The method is simple and is designed for simulations or for
settings with little or no data on age-specific mortality rates. In
settings where data on age-specific mortality is available, other
methods might be more appropriate.

## Usage

``` r
e0_to_lifetab_logit(
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

  A data frame containing a variable called `"e0"`, and possibly other
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

## Method

The method implemented by `e0_to_lifetab_logit()` is based on the
observation that, if populations A and B are demographically similar,
then, in many cases,

\$\$\text{logit}(q_x^{\text{B}}) \approx \alpha + \beta
\text{logit}(q_x^{\text{A}})\$\$

where \\q_x\\ is the life table probability of dying between birth and
age \\x\\. By definition, \\q_x = 1 - l_x\\, where \\l_x\\ is the
standard life table function, with radix (\\l_0\\) equal to 1.

Given (i) target life expectancy, (ii) a set of \\l_x^{\text{A}}\\),
(referred to as a "standard"), and (iii) a value for \\\beta\\,
`e0_to_lifetab_logit()` finds a value for \\\alpha\\ that yields a set
of \\q_x^{\text{B}}\\) with the required life expectancy. If populations
A and B are similar, then \\beta\\ is likely to close to 1.

## The `target` argument

`target` is a data frame specifying life expectancies for each
population being modelled, and, optionally, inputs to the calculations,
and 'by' variables.

`target` contains the following variables:

- A variable called `"e0"` giving life expectancy at birth.

- Optionally, a variable called `"beta"` with values for \\\beta\\. Can
  be an ordinary numeric vector or an
  [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.html).
  If `target` does not include a `"beta"` variable, then
  `e0_to_lifetab_logit()` sets \\\beta\\ to 1.

- A variable called `"sex"`. The `"sex"` variable must be supplied if
  the `infant` argument to `e0_to_lifetab_logit()` is `"CD"` or `"AK"`,
  or if the `child` argument is `"CD"`.

- Optionally, 'by' variables. Typical examples are time, region, and
  model variant.

## The `standard` argument

`standard` is a data frame specifying the \\l_x\\ to be used with each
life expectancy in `target`, and, optionally, values for the average age
person-years lived by people who die in each group, \\\_na_x\\. Values
in `standard` are age-specific.

`standard` contains the following variables:

- A variable called `"age"`, with labels that can be parsed by
  [`reformat_age()`](https://bayesiandemography.github.io/poputils/reference/reformat_age.md).

- A variable called `"lx"`. Cannot be an rvec.

- Additional variables used to match rows in `standard` to rows in
  `target`.

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
  Fertility equivalent of `e0_to_lifetab_logit()`

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
                     e0 = c(17.5, 15.6))

standard <- west_lifetab |>
    filter(level == 1) |>
    select(sex, age, lx)
    
e0_to_lifetab_logit(target = target,
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

## target is an rvec
library(rvec, warn.conflicts = FALSE)
target_rvec <- data.frame(sex = c("Female", "Male"), 
                          e0 = rnorm_rvec(n = 2,
                                          mean = c(17.5, 15.6),
                                          n_draw = 1000))
e0_to_lifetab_logit(target = target_rvec,
                    standard = standard)
#> # A tibble: 42 × 7
#>    sex    age                     qx                   lx                   dx
#>    <chr>  <fct>         <rdbl<1000>>         <rdbl<1000>>         <rdbl<1000>>
#>  1 Female 0        0.42 (0.38, 0.46) 1e+05 (1e+05, 1e+05) 41526 (37788, 45580)
#>  2 Female 1-4      0.29 (0.27, 0.31) 58474 (54420, 62212) 16777 (16660, 16791)
#>  3 Female 5-9    0.08 (0.075, 0.085) 41683 (37735, 45524)    3322 (3194, 3408)
#>  4 Female 10-14 0.062 (0.058, 0.066) 38360 (34541, 42117)    2379 (2266, 2462)
#>  5 Female 15-19  0.08 (0.075, 0.084) 35981 (32275, 39655)    2868 (2709, 2993)
#>  6 Female 20-24   0.098 (0.094, 0.1) 33114 (29567, 36662)    3254 (3044, 3430)
#>  7 Female 25-29     0.11 (0.1, 0.11) 29859 (26523, 33232)    3254 (3012, 3465)
#>  8 Female 30-34    0.12 (0.12, 0.13) 26606 (23510, 29767)    3243 (2971, 3490)
#>  9 Female 35-39    0.13 (0.13, 0.14) 23363 (20539, 26277)    3080 (2794, 3349)
#> 10 Female 40-44    0.14 (0.13, 0.14) 20283 (17746, 22928)    2815 (2530, 3091)
#> # ℹ 32 more rows
#> # ℹ 2 more variables: Lx <rdbl<1000>>, ex <rdbl<1000>>
```
