# Booth Standard

A "standard" distribution for age-specific fertility rates proposed in
Booth (1984), and widely used for estimating fertility rates in settings
with high fertility and limited data.

## Usage

``` r
booth_standard
```

## Format

A tibble with 8 rows and the following columns:

- `age` Five-year age group, from `"10-14"` to `"45-49"`

- `value` Proportion of total fertility occurring within the age group

## Source

Booth H (1984) Transforming Gompertz's function for fertility analysis:
The development of a standard for the relational Gompertz function.
*Population Studies* 38(3): 495-506.

## Details

The key input for `booth_standard` is 'Yx' from table 2 of Booth (1984).
