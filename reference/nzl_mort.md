# Mortality Data for New Zealand

Counts of deaths and population, by age, sex, and calendar year, plus
mortality rates, for New Zealand, 2021-2022.

## Usage

``` r
nzl_mort
```

## Format

A data frame with 84 rows and the following variables:

- `year` Calendar year.

- `gender` `"Female"`, and `"Male"`.

- `age` Age, in life table age groups, with an open age group of 95+.

- `deaths` Counts of deaths, randomly rounded to base 3.

- `popn` Estimates of average annual population.

- `mx` Mortality rates (deaths / popn).

## Source

Modified from data in tables "Deaths by age and sex (Annual-Dec)" and
"Estimated Resident Population by Age and Sex (1991+) (Annual-Dec)" from
Stats NZ online database *Infoshare*, downloaded on 24 September 2023.
