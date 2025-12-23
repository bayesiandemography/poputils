# Coale-Demeny West Model Life Tables

Life table quantities from the "West" family of Coale-Demeny model life
tables.

## Usage

``` r
west_lifetab
```

## Format

A data frame with 1,050 rows and the following variables:

- `level` Index for life table. Lower level implies lower life
  expectancy.

- `sex` `"Female"`, and `"Male"`.

- `age` Age, in life table age groups, with an open age group of 95+.

- `mx` Mortality rate.

- `ax` Average years lived in age interval by people who die in that
  interval.

- `qx` Probability some alive at start of age interval dies during
  interval.

- `lx` Number of people still alive at start of age interval.

- `dx` Number of people dying during age interval.

- `Lx` Number of person-years lived during age interval.

- `ex` Expectation of life at start of age interval.

## Source

Coale A, Demeny P, and Vaughn B. 1983. Regional model life tables and
stable populations. 2nd ed. New York: Academic Press, accessed via
`demogR::cdmltw()`.
