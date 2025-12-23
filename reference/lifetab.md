# Calculate Life Tables or Life Expectancies

Calculate life table quantities. Function `lifetab()` returns an entire
life table. Function `lifeexp()` returns life expectancy at birth. The
inputs can be mortality rates (`mx`) or probabilities of dying (`qx`),
though not both.

## Usage

``` r
lifetab(
  data,
  mx = NULL,
  qx = NULL,
  age = age,
  sex = NULL,
  ax = NULL,
  by = NULL,
  infant = c("constant", "linear", "CD", "AK"),
  child = c("constant", "linear", "CD"),
  closed = c("constant", "linear"),
  open = "constant",
  radix = 1e+05,
  suffix = NULL,
  n_core = 1
)

lifeexp(
  data,
  mx = NULL,
  qx = NULL,
  at = 0,
  age = age,
  sex = NULL,
  ax = NULL,
  by = NULL,
  infant = c("constant", "linear", "CD", "AK"),
  child = c("constant", "linear", "CD"),
  closed = c("constant", "linear"),
  open = "constant",
  suffix = NULL,
  n_core = 1
)
```

## Arguments

- data:

  Data frame with mortality data.

- mx:

  \<[`tidyselect`](https://tidyselect.r-lib.org/reference/language.html)\>
  Mortality rates, expressed as deaths per person-year lived. Possibly
  an
  [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.html).

- qx:

  \<[`tidyselect`](https://tidyselect.r-lib.org/reference/language.html)\>
  Probability of dying within age interval. An alternative to `mx`.
  Possibly an
  [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.html).

- age:

  \<[`tidyselect`](https://tidyselect.r-lib.org/reference/language.html)\>
  Age group labels. The labels must be interpretable by functions such
  as
  [`reformat_age()`](https://bayesiandemography.github.io/poputils/reference/reformat_age.md)
  and
  [`age_group_type()`](https://bayesiandemography.github.io/poputils/reference/age_group_type.md).
  The first age group must start at age 0, and the last age group must
  be "open", with no upper limit.

- sex:

  \<[`tidyselect`](https://tidyselect.r-lib.org/reference/language.html)\>
  Biological sex, with labels that can be interpreted by
  [`reformat_sex()`](https://bayesiandemography.github.io/poputils/reference/reformat_sex.md).
  Needed only when `infant` is `"CD"` or `"AK"`, or `child` is `"CD"`.

- ax:

  \<[`tidyselect`](https://tidyselect.r-lib.org/reference/language.html)\>
  Average age at death within age group. Optional. See Details.

- by:

  \<[`tidyselect`](https://tidyselect.r-lib.org/reference/language.html)\>
  Separate life tables, or life expectancies, calculated for each
  combination the `by` variables. If a `sex` variable was specified,
  then that variable is automatically included among the `by` variables.
  If `data` is a
  [grouped](https://dplyr.tidyverse.org/reference/group_data.html) data
  frame, then the grouping variables take precedence over `by`.

- infant:

  Method used to calculate life table values in age group `"0"`. Ignored
  if `age` does not include age group `"0"`. Default is `"constant"`.

- child:

  Method used to calculate life table values in age group `"1-4"`.
  Ignored if `age` does not include age group `"0"`. Default is
  `"constant"`.

- closed:

  Method used to calculate life table values in closed age intervals
  other than `"0"` and `"1-4"` (ie intervals such as "10-14" or "12").
  Default is `"constant"`.

- open:

  Method used to calculate life table values in the final, open age
  group (eg `"80+"` or `"110+"`). Currently the only option is
  \`"constant".

- radix:

  Initial population for the `lx` column. Default is `100000`.

- suffix:

  Optional suffix added to new columns in result.

- n_core:

  Number of cores to use for parallel processing. If `n_core` is `1`
  (the default), no parallel processing is done.

- at:

  Age at which life expectancy is calculated
  (`lifeexp() only). Default is `0\`. Can be a vector with length \> 1.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html).

## Definitions of life table quantities

- `mx` Deaths per person-year lived.

- `qx` Probability of surviving from the start of age group 'x' to the
  end.

- `lx` Number of people alive at the start of age group `x`.

- `dx` Number of deaths in age group `x`

- `Lx` Expected number of person years lived in age group `x`.

- `ex` Life expectancy, calculated at the start of age group `x`.

Mortality rates `mx` are sometimes expressed as deaths per 1000
person-years lived, or per 100,000 person-years lived. `lifetab()` and
`lifeexp()` assumed that they are expressed as deaths per person-year
lived.

## Calculation methods

`lifetab()` and `lifeexp()` implement several methods for calculating
life table quantities from mortality rates. Each method makes different
assumptions about the way that mortality rates vary within age
intervals:

- `"constant"` Mortality rates are constant within each interval.

- `"linear"`. Life table quantity `lx` is a straight line within each
  interval. Equivalently, deaths are distributed uniformly within each
  interval.

- `"CD"`. Used only with age groups "0" and "1-4". Mortality rates
  decline over the age interval, with the slope depending on the
  absolute level of infant mortality. The formulas were developed by
  Coale and Demeny (1983), and used in Preston et al (2001).

- `"AK"`. Used only with age group "0". Mortality rates decline over the
  age interval, with the slope depending on the absolute level of infant
  mortality. The formulas were formulas developed by Andreev and
  Kingkade (2015), and are used in the Human Mortality Database [methods
  protocol](https://www.mortality.org/File/GetDocument/Public/Docs/MethodsProtocolV6.pdf).

For a detailed description of the methods, see the vignette for
**poputils**.

## ax

`ax` is the average number of years lived in an age interval by people
who die in that interval. Demographers sometimes refer to it as the
'separation factor'. If a non-`NA` value of `ax` is supplied for an age
group, then the results for that age group are based on the formula

\$\$m_x = d_x / (n_x l_x + a_x d_x)\$\$,

(where `n_x` is the width of the age interval), over-riding any methods
specified via the `infant`, `child`, `closed` and `open` arguments.

## Open age group when inputs are qx

The probability of dying, `qx`, is always 1 in the final (open) age
group. `qx` therefore provides no direct information on mortality
conditions within the final age group. `lifetab()` and `lifeexp()` use
conditions in the second-to-final age group as a proxy for conditions in
the final age group. When `open` is `"constant"` (which is currently the
only option), and no value for `ax` in the final age group is provided,
`lifetab()` and `lifeexp()` assume that \\m_A = m\_{A-1}\\, and set
\\L\_{A} = l_A / m_A\\.

In practice, mortality is likely to be higher in the final age group
than in the second-to-final age group, so the default procedure is
likely to lead to inaccuracies. When the size of the final age group is
very small, these inaccuracies will be inconsequential. But in other
cases, it may be necessary to supply an explicit value for `ax` for the
final age group, or to use `mx` rather than `qx` as inputs.

## Using rvecs to represent uncertainty

An [rvec](https://bayesiandemography.github.io/rvec/reference/rvec.html)
is a 'random vector', holding multiple draws from a distribution. Using
an rvec for the `mx` argument to `lifetab()` or `lifeexp()` is a way of
representing uncertainty. This uncertainty is propagated through to the
life table values, which will also be rvecs.

## Parallel processing

Calculations can be slow when working with rvecs and many combinations
of 'by' variables. In these cases, setting `n_core` to a number greater
than 1, which triggers parallel processing, may help.

## References

- Preston SH, Heuveline P, and Guillot M. 2001. *Demography: Measuring
  and Modeling Population Processes* Oxford: Blackwell.

- Coale AJ, Demeny P, and Vaughn B. 1983. *Regional model life tables
  and stable populations* New York: Academic Press.

- Andreev, E.M. and Kingkade, W.W., 2015. Average age at death in
  infancy and infant mortality level: Reconsidering the Coale-Demeny
  formulas at current levels of low mortality. *Demographic Research*,
  33, pp.363-390.

- Human Mortality Database [Methods
  Protocol](https://www.mortality.org/File/GetDocument/Public/Docs/MethodsProtocolV6.pdf).

- [Tools for Demographic
  Estimation](https://demographicestimation.iussp.org).

## See also

- [`ex_to_lifetab_brass()`](https://bayesiandemography.github.io/poputils/reference/ex_to_lifetab_brass.md)
  Calculate life table from minimal inputs

- [`q0_to_m0()`](https://bayesiandemography.github.io/poputils/reference/q0_to_m0.md)
  Convert between infant mortality measures

- [`tfr()`](https://bayesiandemography.github.io/poputils/reference/tfr.md)
  Calculate total fertility rate

## Examples

``` r
library(dplyr)

## life table for females based on 'level 1'
## mortality rates "West" model life table
west_lifetab |>
    filter(sex == "Female",
           level == 1) |>
    lifetab(mx = mx)
#> ℹ Overwriting existing columns `lx`, `dx`, `Lx`, and `ex` in `data`.
#> New names:
#> • `qx` -> `qx...5`
#> • `qx` -> `qx...6`
#> # A tibble: 21 × 10
#>    level sex    age      ax qx...5 qx...6      lx     dx      Lx    ex
#>    <int> <chr>  <fct> <dbl>  <dbl>  <dbl>   <dbl>  <dbl>   <dbl> <dbl>
#>  1     1 Female 0      0.35 0.366  0.381  100000  38090.  79439.  19.4
#>  2     1 Female 1-4    1.36 0.262  0.271   61910. 16793. 212286.  30.0
#>  3     1 Female 5-9    2.25 0.0732 0.0734  45117.  3312. 217198.  36.5
#>  4     1 Female 10-14  2.6  0.0572 0.0571  41805.  2389. 202992.  34.2
#>  5     1 Female 15-19  2.6  0.0740 0.0739  39416.  2911. 189708.  31.1
#>  6     1 Female 20-24  2.6  0.0919 0.0917  36505.  3346. 174024.  28.4
#>  7     1 Female 25-29  2.6  0.103  0.102   33159.  3393. 157159.  26.1
#>  8     1 Female 30-34  2.6  0.116  0.115   29766.  3429. 140082.  23.7
#>  9     1 Female 35-39  2.6  0.126  0.125   26337.  3302. 123243.  21.5
#> 10     1 Female 40-44  2.6  0.133  0.133   23034.  3059. 107343.  19.2
#> # ℹ 11 more rows

## change method for infant and children from
## default ("constant") to "CD"
west_lifetab |>
    filter(sex == "Female",
           level == 1) |>
    lifetab(mx = mx,
            sex = sex,
            infant = "CD",
            child = "CD")
#> ℹ Overwriting existing columns `lx`, `dx`, `Lx`, and `ex` in `data`.
#> New names:
#> • `qx` -> `qx...5`
#> • `qx` -> `qx...6`
#> # A tibble: 21 × 10
#>    level sex    age      ax qx...5 qx...6      lx     dx      Lx    ex
#>    <int> <chr>  <fct> <dbl>  <dbl>  <dbl>   <dbl>  <dbl>   <dbl> <dbl>
#>  1     1 Female 0      0.35 0.366  0.366  100000  36555.  76239.  20.0
#>  2     1 Female 1-4    1.36 0.262  0.262   63445. 16608. 209949.  30.3
#>  3     1 Female 5-9    2.25 0.0732 0.0734  46836.  3438. 225475.  36.5
#>  4     1 Female 10-14  2.6  0.0572 0.0571  43398.  2480. 210728.  34.2
#>  5     1 Female 15-19  2.6  0.0740 0.0739  40918.  3022. 196937.  31.1
#>  6     1 Female 20-24  2.6  0.0919 0.0917  37896.  3474. 180656.  28.4
#>  7     1 Female 25-29  2.6  0.103  0.102   34422.  3522. 163148.  26.1
#>  8     1 Female 30-34  2.6  0.116  0.115   30900.  3560. 145420.  23.7
#>  9     1 Female 35-39  2.6  0.126  0.125   27340.  3428. 127940.  21.5
#> 10     1 Female 40-44  2.6  0.133  0.133   23912.  3176. 111433.  19.2
#> # ℹ 11 more rows

## calculate life expectancies
## for all levels, using the 'by'
## argument to distinguish levels
west_lifetab |>
    lifeexp(mx = mx,
            sex = sex,
            infant = "CD",
            child = "CD",
            by = level)
#> ℹ Overwriting existing column `ex` in `data`.
#> # A tibble: 50 × 3
#>    level sex       ex
#>    <int> <chr>  <dbl>
#>  1     1 Female  20.0
#>  2     1 Male    18.0
#>  3     2 Female  22.5
#>  4     2 Male    20.4
#>  5     3 Female  25.0
#>  6     3 Male    22.8
#>  7     4 Female  27.5
#>  8     4 Male    25.2
#>  9     5 Female  30.0
#> 10     5 Male    27.6
#> # ℹ 40 more rows

## obtain the same result using
## 'group_by'
west_lifetab |>
  group_by(level) |>
  lifeexp(mx = mx,
          sex = sex,
          infant = "CD",
          child = "CD")
#> ℹ Overwriting existing column `ex` in `data`.
#> # A tibble: 50 × 3
#>    level sex       ex
#>    <int> <chr>  <dbl>
#>  1     1 Female  20.0
#>  2     1 Male    18.0
#>  3     2 Female  22.5
#>  4     2 Male    20.4
#>  5     3 Female  25.0
#>  6     3 Male    22.8
#>  7     4 Female  27.5
#>  8     4 Male    25.2
#>  9     5 Female  30.0
#> 10     5 Male    27.6
#> # ℹ 40 more rows

## calculations based on 'qx'
west_lifetab |>
  lifeexp(qx = qx,
          sex = sex,
          by = level)
#> ℹ Overwriting existing column `ex` in `data`.
#> # A tibble: 50 × 3
#>    level sex       ex
#>    <int> <chr>  <dbl>
#>  1     1 Female  20.1
#>  2     1 Male    18.1
#>  3     2 Female  22.5
#>  4     2 Male    20.5
#>  5     3 Female  25.0
#>  6     3 Male    22.9
#>  7     4 Female  27.5
#>  8     4 Male    25.3
#>  9     5 Female  30.0
#> 10     5 Male    27.7
#> # ℹ 40 more rows

## life expectancy at age 60
west_lifetab |>
  filter(level == 10) |>
  lifeexp(mx = mx,
          at = 60,
          sex = sex)
#> ℹ Overwriting existing column `ex` in `data`.
#> # A tibble: 2 × 2
#>   sex       ex
#>   <chr>  <dbl>
#> 1 Female  13.5
#> 2 Male    12.3

## life expectancy at ages 0 and 60
west_lifetab |>
  filter(level == 10) |>
  lifeexp(mx = mx,
          at = c(0, 60),
          sex = sex)
#> ℹ Overwriting existing column `ex` in `data`.
#> # A tibble: 4 × 3
#>   sex       at    ex
#>   <chr>  <int> <dbl>
#> 1 Female     0  42.3
#> 2 Female    60  13.5
#> 3 Male       0  39.4
#> 4 Male      60  12.3
```
