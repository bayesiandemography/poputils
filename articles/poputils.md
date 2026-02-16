# poputils

## 1 Aims

`poputils` provides tools for common tasks with demographic data. It has
some distinctive features:

- *Tidyverse compliance*. `poputils` tries to into
  [tidyverse](https://tidyverse.org/) workflows. For instance,
  `poputils` functions use data frames for inputs and outputs, use
  [tidyselect](https://tidyselect.r-lib.org/reference/language.html)
  methods to specify variables, and follow tidyverse conventions for
  variable names.
- *Uncertainty*. `poputils` handles uncertainty through the use of
  [rvecs](https://bayesiandemography.github.io/rvec/reference/rvec.html).
  An rvec is an object holding multiple draws from a distribution that
  behaves similarly to an ordinary R vector.
- *Age and time labels*. `poputils` allows users to work directly with
  age and time labels, based on common set of methods.

Some functions in `poputils` are designed for data analysts. Others are
designed for programmers creating functions to be used by data analysts.

## 2 Tools for data analysts

### 2.1 Labels

#### 2.1.1 Age

Producers of demographic data follow a wide variety of styles for
labeling age groups. `poputils` contains tools for parsing and
manipulating these labels.

Age label functions in `poputils` require that age labels belong to one
of three types:

- `"single"`. Single years of age, possibly including an open age group,
  eg `"0"`, `"81"`, `"17"`, `"100+"`.
- `"five"`. Five-year age groups, possibly including an open age group,
  eg `"0-4"`, `"80-84"`, `"15-19"`, `"100+"`.
- `"lt"`. Life table age groups. Like `"five"`, but with the `"0-4"` age
  group split into `"0"` and `"1-4"`.

Age labels created by `poputils` functions such as
[`age_labels()`](https://bayesiandemography.github.io/poputils/reference/age_labels.md)
follow a standard set of rules. Many age labels created using other
rules can, however, be parsed by `poputils` functions,

``` r
library(poputils)
library(dplyr, warn.conflicts = FALSE)
tibble(original = c("5 to 9", "5_9", "05-09"),
       reformated = reformat_age(original))
#> # A tibble: 3 × 2
#>   original reformated
#>   <chr>    <fct>     
#> 1 5 to 9   5-9       
#> 2 5_9      5-9       
#> 3 05-09    5-9
```

Functions
[`age_lower()`](https://bayesiandemography.github.io/poputils/reference/age_lower.md),
[`age_upper()`](https://bayesiandemography.github.io/poputils/reference/age_lower.md),
and
[`age_mid()`](https://bayesiandemography.github.io/poputils/reference/age_lower.md)
extract information about lower limits, upper limits, and centers of age
groups. This can be useful for ordering data

``` r
df <- data.frame(age = c("5-9", "0-4", "15-19", "10-14"),
                 population = c(3, 7, 2, 4))
df
#>     age population
#> 1   5-9          3
#> 2   0-4          7
#> 3 15-19          2
#> 4 10-14          4
df |>
  arrange(age_lower(age))
#>     age population
#> 1   0-4          7
#> 2   5-9          3
#> 3 10-14          4
#> 4 15-19          2
```

and plotting

``` r
library(ggplot2)
ggplot(df, aes(x = age_mid(age), y = population)) +
  geom_point()
```

![](poputils_files/figure-html/unnamed-chunk-4-1.png)

among other things.

Functions
[`combine_age()`](https://bayesiandemography.github.io/poputils/reference/combine_age.md)
and
[`set_age_open()`](https://bayesiandemography.github.io/poputils/reference/set_age_open.md)
can be used to collapse age groups,

``` r
tibble(age = age_labels("lt", max = 30),
       age_5 = combine_age(age, to = "five"),
       age_25plus = set_age_open(age, lower = 20))
#> # A tibble: 8 × 3
#>   age   age_5 age_25plus
#>   <chr> <chr> <chr>     
#> 1 0     0-4   0         
#> 2 1-4   0-4   1-4       
#> 3 5-9   5-9   5-9       
#> 4 10-14 10-14 10-14     
#> 5 15-19 15-19 15-19     
#> 6 20-24 20-24 20+       
#> 7 25-29 25-29 20+       
#> 8 30+   30+   20+
```

#### 2.1.2 Sex/gender

Function
[`reformat_sex()`](https://bayesiandemography.github.io/poputils/reference/reformat_sex.md)
converts sex/gender categories to `"Female"`, `"Male"`, and any
additional categories specified through the `except` argument,

``` r
reformat_sex(c("M", "F", "Diverse", "Fem"), except = "Diverse")
#> [1] Male    Female  Diverse Female 
#> Levels: Female Male Diverse
```

### 2.2 Life tables and life expectancy

A life table a way of describing mortality. The best known life table
quantity is life expectancy at birth.

#### 2.2.1 Basic functionality

Life tables can be calculated from age-specific mortality rates using
function
[`lifetab()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md).

``` r
nzl_mort |>
  filter(year == 2022,
         gender == "Female") |>
  lifetab(mx = mx)  
#> # A tibble: 21 × 10
#>     year gender age         deaths   popn       qx      lx    dx      Lx    ex
#>    <int> <chr>  <fct>        <int>  <int>    <dbl>   <dbl> <dbl>   <dbl> <dbl>
#>  1  2022 Female Infant          84  29680 0.00283  100000  283.   99859.  83.4
#>  2  2022 Female 1-4 years       18 118420 0.000608  99717.  60.6 398748.  82.6
#>  3  2022 Female 5-9 years       12 156820 0.000383  99657.  38.1 498189.  78.7
#>  4  2022 Female 10-14 years     15 164830 0.000455  99619.  45.3 497980.  73.7
#>  5  2022 Female 15-19 years     42 154150 0.00136   99573. 136.  497528.  68.8
#>  6  2022 Female 20-24 years     63 156860 0.00201   99438. 199.  496690.  63.8
#>  7  2022 Female 25-29 years     72 172770 0.00208   99238. 207.  495675.  59.0
#>  8  2022 Female 30-34 years     78 194570 0.00200   99032. 198.  494663.  54.1
#>  9  2022 Female 35-39 years    111 175050 0.00317   98833. 313.  493385.  49.2
#> 10  2022 Female 40-44 years    147 160070 0.00458   98521. 451.  491474.  44.3
#> # ℹ 11 more rows
```

[`lifetab()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md)
and
[`lifeexp()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md)
both have a `by` argument. Separate results are calculated for each
combination of the `by` variables,

``` r
nzl_mort |>
  lifeexp(mx = mx,
          by = c(gender, year))  
#> # A tibble: 4 × 3
#>   gender  year    ex
#>   <chr>  <int> <dbl>
#> 1 Female  2021  84.0
#> 2 Male    2021  80.5
#> 3 Female  2022  83.4
#> 4 Male    2022  80.0
```

The same effect can be obtained using
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),

``` r
nzl_mort |>
  group_by(gender, year) |>
  lifeexp(mx = mx)
#> # A tibble: 4 × 3
#>   gender  year    ex
#>   <chr>  <int> <dbl>
#> 1 Female  2021  84.0
#> 2 Male    2021  80.5
#> 3 Female  2022  83.4
#> 4 Male    2022  80.0
```

The input data for life tables and life expectancies can be
probabilities of dying (`qx`), rather than mortality rates (`mx`)

``` r
west_lifetab |>
  group_by(level, sex) |>
  lifeexp(qx = qx)
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
```

By default,
[`lifeexp()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md)
calculates life expectancy at age zero. It can, however, be used to
calculate life expectancy at other ages.

``` r
nzl_mort |>
  lifeexp(mx = mx,
          at = c(0, 65),
          by = c(gender, year))  
#> # A tibble: 8 × 4
#>   gender  year    at    ex
#>   <chr>  <int> <int> <dbl>
#> 1 Female  2021     0  84.0
#> 2 Female  2021    65  22.0
#> 3 Male    2021     0  80.5
#> 4 Male    2021    65  19.8
#> 5 Female  2022     0  83.4
#> 6 Female  2022    65  21.4
#> 7 Male    2022     0  80.0
#> 8 Male    2022    65  19.3
```

#### 2.2.2 Calculation methods

Alternative methods for calculating life tables differ mainly in their
assumptions about how mortality varies within age groups (Preston,
Heuveline, and Guillot 2001; Keyfitz and Caswell 2005). It turns out
that, for the purposes of constructing life tables, all the relevant
information about the way that mortality varies within age groups can be
captured by a single number: the average length of time lived in an
interval by people who die in that interval (Preston, Heuveline, and
Guillot 2001, 43). This number is denoted \\\_na_x\\, where \\x\\ is
exact age at the start of the internal, and \\n\\ is the length of the
interval. The quantity \\\_5a\_{20}\\, for instance, refers to the
average number of years lived after their 20th birthday by people who
die between their 20th and 25th birthdays. When \\n=1\\, the \\n\\
subscript is typically omitted.

Functions
[`lifetab()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md)
and
[`lifeexp()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md)
have four arguments for specifying calculation methods:

- `infant`, which specifies how \\a_0\\ is calculated,
- `child`, which specifies how \\\_4a_1\\ is calculated,
- `closed`, which specifies how \\\_na_x\\ for all other closed
  intervals are calculated, and
- `open`, which specifies how the final interval, \\\_{\infty}a_x\\ is
  calculated.

Different choices of method are available for each argument. In some
cases, different formulas are used for females and males. The formulas
can also differ depending on whether the input data is of mortality
rates or probabilities of dying.

| `argument` | `sex`   | `method`     | input |                                                                               formula                                                                               |
|:-----------|:--------|:-------------|:-----:|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------:|
| `infant`   | \<any\> | `"constant"` | `mx`  |                                                     \\a_0 = \frac{1 - (m_0 + 1) e^{-m_0}}{m_0 (1 - e^{-m_0})}\\                                                     |
| `infant`   | \<any\> | `"constant"` | `qx`  |                                                \\a_0 = \frac{(1 - \log(1 - q_0) (1 - q_0)) - 1}{\log(1 - q_0) q_0}\\                                                |
| `infant`   | \<any\> | `"linear"`   | `mx`  |                                                                            \\a_0 = 0.5\\                                                                            |
| `infant`   | \<any\> | `"linear"`   | `qx`  |                                                                            \\a_0 = 0.5\\                                                                            |
| `infant`   | Female  | `"CD"`       | `mx`  |                                  \\a_0 = \begin{cases} 0.053 + 2.8 m_0 & 0 \le m_0 \< 0.107 \\ 0.35 & m_0 \ge 0.107 \end{cases}\\                                   |
| `infant`   | Female  | `"CD"`       | `qx`  |                                      \\a_0 = \begin{cases} 0.05 + 3 q_0 & 0 \le m_0 \< 0.1 \\ 0.35 & q_0 \ge 0.1 \end{cases}\\                                      |
| `infant`   | Male    | `"CD"`       | `mx`  |                                 \\a_0 = \begin{cases} 0.045 + 2.684 m_0 & 0 \le m_0 \< 0.107 \\ b0.33 & m_0 \ge 0.107 \end{cases}\\                                 |
| `infant`   | Male    | `"CD"`       | `qx`  |                                   \\a_0 = \begin{cases} 0.0425 + 2.875 q_0 & 0 \le q_0 \< 0.1 \\ 0.33 & q_0 \ge 0.1 \end{cases}\\                                   |
| `infant`   | Female  | `"AK"`       | `mx`  | \\a_0 = \begin{cases} 0.14903 - 2.05527 m_0 & 0 \le m_0 \< 0.01724 \\ 0.04667 + 3.88089 m_0 & 0.01724 \le m_0 \< 0.06891 \\ 0.31411 & m_0 \ge 0.06891 \end{cases}\\ |
| `infant`   | Female  | `"AK"`       | `qx`  |       \\a_0 = \begin{cases} 0.149 - 2.0867 q_0 & 0 \le q_0 \< 0.017 \\ 0.0438 + 4.1075 q_0 & 0.017 \le q_0 \< 0.0658 \\ 0.3141 & q_0 \ge 0.0658 \end{cases}\\       |
| `infant`   | Male    | `"AK"`       | `mx`  |   \\a_0 = \begin{cases} 0.14929 - 1.99545 m_0 & 0 \le m_0 \< 0.023 \\ 0.02832 + 3.26021 m_0 & 0.023 \le m_0 \< 0.08307 \\ 0.29915 & m_0 \ge 0.08307 \end{cases}\\   |
| `infant`   | Male    | `"AK"`       | `qx`  |     \\a_0 = \begin{cases} 0.1493 - 2.0367 q_0 & 0 \le q_0 \< 0.0226 \\ 0.0244 + 3.4994 q_0 & 0.0226 \le q_0 \< 0.0785 \\ 0.2991 & q_0 \ge 0.0785 \end{cases}\\      |
| `child`    | \<any\> | `"constant"` | `mx`  |                             \\\_4a_1 = \frac{1 - (4 \times {\_4}m_1 + 1) e^{-4 \times {\_4}m_1}}{\_4m_1 (1 - e^{-4 \times {\_4}m_1})}\\                             |
| `child`    | \<any\> | `"constant"` | `qx`  |                                    \\\_4a_1 = \frac{4((1 - \log(1-{\_4}q_1)) (1 - {\_4}m_1) - 1)}{\log(1 - {\_4q_1}) {\_4}q_1}\\                                    |
| `child`    | \<any\> | `"linear"`   | `mx`  |                                                                           \\\_4a_1 = 2\\                                                                            |
| `child`    | \<any\> | `"linear"`   | `qx`  |                                                                           \\\_4a_1 = 2\\                                                                            |
| `child`    | Female  | `"CD"`       | `mx`  |                               \\\_4a_1 = \begin{cases} 1.522 - 1.518 m_0 & 0 \le m_0 \< 0.107 \\ 1.361 & m_0 \ge 0.107 \end{cases}\\                                |
| `child`    | Female  | `"CD"`       | `qx`  |                                 \\\_4a_1 = \begin{cases} 1.542 - 1.625 q_0 & 0 \le q_0 \< 0.1 \\ 1.361 & q_0 \ge 0.1 \end{cases}\\                                  |
| `child`    | Male    | `"CD"`       | `mx`  |                               \\\_4a_1 = \begin{cases} 1.651 - 2.816 m_0 & 0 \le m_0 \< 0.107 \\ 1.352 & m_0 \ge 0.107 \end{cases}\\                                |
| `child`    | Male    | `"CD"`       | `qx`  |                                 \\\_4a_1 = \begin{cases} 1.653 - 3.013 q_0 & 0 \le q_0 \< 0.1 \\ 1.352 & q_0 \ge 0.1 \end{cases}\\                                  |
| `closed`   | \<any\> | `"constant"` | `mx`  |                             \\\_na_x = \frac{1 - (n \times {\_n}m_x + 1) e^{-n \times {\_n}m_x}}{\_nm_x (1 - e^{-n \times {\_n}m_x})}\\                             |
| `closed`   | \<any\> | `"constant"` | `qx`  |                                   \\\_na_x = \frac{n((1 - \log(1 - {\_n}q_x))(1 - {\_nq_x}) - 1)}{\log(1 - {\_nq_x}) {\_n}q_x}\\                                    |
| `closed`   | \<any\> | `"linear"`   | `mx`  |                                                                         \\\_na_x = 0.5 n\\                                                                          |
| `closed`   | \<any\> | `"linear"`   | `qx`  |                                                                         \\\_na_x = 0.5 n\\                                                                          |
| `open`     | \<any\> | `"constant"` | `mx`  |                                                     \\\_{\infty}a\_{\omega} = \frac{1}{\_{\infty}m\_{\omega}}\\                                                     |
| `open`     | \<any\> | `"constant"` | `qx`  |                                                      \\\_{\infty}a\_{\omega} = \frac{1}{\_{n}m\_{\omega-n}}\\                                                       |

In the table above,

- values for `"CD"` are from Coale, Demeny, and Vaughan (1983), p20, and
  Preston, Heuveline, and Guillot (2001), p48;
- values for `"AK"` are from Andreev and Kingkade (2015), p376, and
  Wilmoth et al. (2021), p37; and
- values for `"constant"` are expected values for an exponential
  distribution that has been right-truncated at \\n\\.

When the inputs data are \\\_nq_x\\, the value of \\\_na_x\\ for the
last age group is based in mortality rates in the second-to-last age
group. This is an expedient to deal with the fact that \\\_nq_x\\ is
always 1 in the last age group, and therefore provides no information
about mortality in that age group.

Once the \\\_na_x\\ have been determined, the life table is fully
specified, and the required calculations can be carried out with no
further input from the user.

The probability of dying within each interval is

\\\_nq_x = \frac{n \times {\_n}m_x}{1 + (n - {\_n}a_x) \times
{\_nm_x}},\\ with \\\_{\infty}q\_{\omega} = 1\\. Quantity \\l_x\\ is the
number of people surviving to exact age \\x\\. In
[`lifetab()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md),
by default, \\l_0 = 100,000\\. Remaining values are calculated using

\\l\_{x+n} = (1 - {\_nq_x}) \times l_x.\\ Quantity \\\_nd_x\\ is the
number of people who die between exact ages \\x\\ and \\x+n\\,

\\\_nd_x = l_x - l\_{x+n}.\\

Quantity \\\_nL_x\\ is the number of person-years lived between exact
ages \\x\\ and \\x+n\\. It consists of person-years lived by people who
survive the interval, plus person-years lived by people who die within
the interval,

\\\_nL_x = l\_{x+n} \times n + {\_nd_x} \times {\_na_x}.\\ Finally,
\\e_x\\, the number of years of life remaining to a person aged exactly
\\x\\, is \\e_x = {\_nL_x} + {\_nL\_{x+n}} + \cdots +
{\_{\infty}L\_{\omega}}\\.

Although the results for
[`lifetab()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md)
and
[`lifeexp()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md)
do vary with the values for `infant`, `child`, or `closed`, the
differences are typically small,

``` r
lin <- nzl_mort |>
  lifeexp(mx = mx,
          by = c(gender, year),
          infant = "linear",
          suffix = "lin")
ak <- nzl_mort |>
  lifeexp(mx = mx,
          sex = gender,
          by = year,
          infant = "AK", 
          suffix = "ak")
inner_join(lin, ak, by = c("year", "gender")) |>
  mutate(diff = ex.lin - ex.ak)
#> # A tibble: 4 × 5
#>   gender  year ex.lin ex.ak     diff
#>   <chr>  <int>  <dbl> <dbl>    <dbl>
#> 1 Female  2021   84.0  84.0 0.000906
#> 2 Male    2021   80.5  80.5 0.00110 
#> 3 Female  2022   83.4  83.4 0.000771
#> 4 Male    2022   80.0  80.0 0.000965
```

#### 2.2.3 Deriving life tables from life expectancies

When working with very limited data, or when constructing simulations,
it is sometimes helpful to be able to derive a full life table that is
consistent with a given life expectancy. This is what function
[`e0_to_lifetab_logit()`](https://bayesiandemography.github.io/poputils/reference/e0_to_lifetab_logit.md)
does:

``` r
target_e0 <- data.frame(sex = "Female", e0 = 51.4)

west10 <- west_lifetab |>
  filter(level == 10) |>
  select(age, sex, lx)

e0_to_lifetab_logit(target = target_e0,
                    standard = west10)
#> # A tibble: 21 × 7
#>    sex    age       qx      lx     dx      Lx    ex
#>    <chr>  <fct>  <dbl>   <dbl>  <dbl>   <dbl> <dbl>
#>  1 Female 0     0.102  100000  10189.  94814.  51.4
#>  2 Female 1-4   0.0693  89811.  6223. 346647.  56.2
#>  3 Female 5-9   0.0202  83587.  1688. 413704.  56.2
#>  4 Female 10-14 0.0159  81900.  1301. 406237.  52.3
#>  5 Female 15-19 0.0214  80599.  1722. 398673.  48.1
#>  6 Female 20-24 0.0273  78877.  2155. 388973.  44.1
#>  7 Female 25-29 0.0312  76722.  2394. 377595.  40.3
#>  8 Female 30-34 0.0358  74329.  2664. 364942.  36.5
#>  9 Female 35-39 0.0404  71664.  2894. 351036.  32.8
#> 10 Female 40-44 0.0450  68770.  3097. 336049.  29.0
#> # ℹ 11 more rows
```

### 2.3 Fertility

Function
[`tfr()`](https://bayesiandemography.github.io/poputils/reference/tfr.md)
calculates total fertility rates from age-specific fertility rates. The
interface is similar to that of
[`lifeexp()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md),

``` r
irn_fert |>
  tfr(asfr = rate,
      by = c(area, time),
      denominator = 1000)      
#> # A tibble: 30 × 3
#>    area   time   tfr
#>    <chr> <dbl> <dbl>
#>  1 Rural  1986  7.08
#>  2 Rural  1987  6.87
#>  3 Rural  1988  6.62
#>  4 Rural  1989  6.55
#>  5 Rural  1990  6.43
#>  6 Rural  1991  6.01
#>  7 Rural  1992  5.29
#>  8 Rural  1993  4.90
#>  9 Rural  1994  4.37
#> 10 Rural  1995  3.82
#> # ℹ 20 more rows
```

Function
[`tfr_to_asfr_scale()`](https://bayesiandemography.github.io/poputils/reference/tfr_to_asfr_scale.md)
is the fertility equivalent of
[`e0_to_lifetab_logit()`](https://bayesiandemography.github.io/poputils/reference/e0_to_lifetab_logit.md).

### 2.4 Uncertainty

The examples so far have all used deterministic inputs. The data frame
`nzl_mort_rvec`, in constrast, uses a rvec to represent mortality rates,

``` r
library(rvec)
#> 
#> Attaching package: 'rvec'
#> The following objects are masked from 'package:stats':
#> 
#>     sd, var
#> The following object is masked from 'package:base':
#> 
#>     rank
nzl_mort_rvec
#> # A tibble: 84 × 4
#>     year gender age                                 mx
#>    <int> <chr>  <fct>                     <rdbl<1000>>
#>  1  2021 Female Infant         0.0033 (0.0029, 0.0038)
#>  2  2021 Female 1-4 years   0.00017 (0.00014, 0.00021)
#>  3  2021 Female 5-9 years   9.3e-05 (7.9e-05, 0.00011)
#>  4  2021 Female 10-14 years 0.00011 (9.5e-05, 0.00013)
#>  5  2021 Female 15-19 years 0.00025 (0.00022, 0.00028)
#>  6  2021 Female 20-24 years   3e-04 (0.00027, 0.00034)
#>  7  2021 Female 25-29 years   0.00033 (3e-04, 0.00037)
#>  8  2021 Female 30-34 years 0.00043 (0.00038, 0.00047)
#>  9  2021 Female 35-39 years 0.00061 (0.00056, 0.00067)
#> 10  2021 Female 40-44 years   0.00094 (0.00087, 0.001)
#> # ℹ 74 more rows
```

The `mx` rvec holds 1000 draws from the posterior distribution from a
Bayesian model of mortality. The posterior distribution for infant
mortality for females in 2021, for instance, has a posterior median of
0.0032, and a 95% credible interval of (0.0028, 0.0037).

If the input to
[`lifetab()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md),
[`lifeexp()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md),
or
[`tfr()`](https://bayesiandemography.github.io/poputils/reference/tfr.md)
is an rvec, then the output will be too. Uncertainty in the input is
propagated through to the output.

``` r
library(rvec)
nzl_mort_rvec |>
  filter(year == 2022,
         gender == "Female") |>
  lifetab(mx = mx) |>
  select(age, qx, lx)
#> # A tibble: 21 × 3
#>    age                                 qx                   lx
#>    <fct>                     <rdbl<1000>>         <rdbl<1000>>
#>  1 Infant           0.0035 (0.003, 0.004) 1e+05 (1e+05, 1e+05)
#>  2 1-4 years   0.00073 (0.00061, 0.00089) 99651 (99605, 99698)
#>  3 5-9 years     5e-04 (0.00042, 0.00059) 99578 (99524, 99630)
#>  4 10-14 years 0.00059 (0.00051, 0.00068) 99528 (99469, 99585)
#>  5 15-19 years    0.0013 (0.0012, 0.0015) 99470 (99407, 99529)
#>  6 20-24 years    0.0016 (0.0014, 0.0018) 99339 (99272, 99401)
#>  7 25-29 years     0.0018 (0.0016, 0.002) 99176 (99103, 99248)
#>  8 30-34 years    0.0023 (0.0021, 0.0025) 98997 (98915, 99076)
#>  9 35-39 years     0.0033 (0.003, 0.0035) 98771 (98687, 98861)
#> 10 40-44 years    0.0049 (0.0046, 0.0053) 98447 (98354, 98548)
#> # ℹ 11 more rows
```

## 3 Tools for developers

`poputils` provides functions that developers creating packages to be
used by demographers may find useful.

### 3.1 Labels

[`check_age()`](https://bayesiandemography.github.io/poputils/reference/check_age.md)
and
[`age_group_type()`](https://bayesiandemography.github.io/poputils/reference/age_group_type.md)
can be useful in functions that involve age group labels.
[`check_age()`](https://bayesiandemography.github.io/poputils/reference/check_age.md)
performs some basic validity checks, while
[`age_group_type()`](https://bayesiandemography.github.io/poputils/reference/age_group_type.md)
assesses whether a set of labels belongs to type `"single"`, `"five"`,
or `"lt"`.

It is often possible to guess the nature of a demographic variable, or
of categories within a demographic variable, based on names and labels.
Functions
[`find_var_age()`](https://bayesiandemography.github.io/poputils/reference/find_var_age.md),
[`find_var_sexgender()`](https://bayesiandemography.github.io/poputils/reference/find_var_sexgender.md),
[`find_var_time()`](https://bayesiandemography.github.io/poputils/reference/find_var_time.md),
[`find_label_female()`](https://bayesiandemography.github.io/poputils/reference/find_label_female.md),
and
[`find_label_male()`](https://bayesiandemography.github.io/poputils/reference/find_label_male.md)
help with these sorts of inferences.

### 3.2 Data manipulation

Function
[`groups_colnums()`](https://bayesiandemography.github.io/poputils/reference/groups_colnums.md)
is helpful when implementing
[tidyselect](https://tidyselect.r-lib.org/reference/language.html)
methods when the data are held in a grouped data frame.

[`matrix_to_list_of_cols()`](https://bayesiandemography.github.io/poputils/reference/matrix_to_list_of_cols.md)
and
[`matrix_to_list_of_rows()`](https://bayesiandemography.github.io/poputils/reference/matrix_to_list_of_cols.md)
convert from matrices to lists of vectors.

[`to_matrix()`](https://bayesiandemography.github.io/poputils/reference/to_matrix.md)
converts a data frame to a matrix. The data frame potentially has more
than two classification variables, and the rows and/or columns of the
matrix can be formed from combinations of these variables.

## References

Andreev, Evgeny M, and W Ward Kingkade. 2015. “Average Age at Death in
Infancy and Infant Mortality Level: Reconsidering the Coale-Demeny
Formulas at Current Levels of Low Mortality.” *Demographic Research* 33:
363–90.

Coale, Ansley J, Paul Demeny, and Barbara Vaughan. 1983. *Regional Model
Life Tables and Stable Populations: Studies in Population*. Academic
Press.

Keyfitz, Nathan, and Hal Caswell. 2005. *Applied Mathematical
Demography*. Springer.

Preston, Samuel H., Patrick Heuveline, and Michel Guillot. 2001.
*Demography: Measuring and Modeling Population Processes*. Blackwell.

Wilmoth, John R, Kirill Andreev, Dmitri Jdanov, Dana A Glei, and Tim
Riffe. 2021. “Methods Protocol for the Human Mortality Database. Version
6.”
