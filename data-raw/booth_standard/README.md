# Booth fertility standard

Source for datasets `booth_standard` (five-year age groups) and `booth_standard_single` (single years of age).

## Source

Booth H (1984) Transforming Gompertz's function for fertility analysis: The development of a standard for the relational Gompertz function. *Population Studies* 38(3): 495–506. JSTOR: <http://www.jstor.org/stable/2174137>.

The article is not stored in this repository, for copyright reasons.

## Files

| File | Contents |
|---|---|
| `booth_standard.R` | Builds `booth_standard` from the standard values Ys(x) at exact ages 10, 15, ..., 50 in Table 2 of Booth (1984), which are typed into the script |
| `table_3.csv` | Table 3 of Booth (1984), "Average and standard values of ΔY and standard values of Y by single years of age", transcribed. Columns: `age_lower`, `age_upper` (the age interval); `adjusted_average_dY` ("Adjusted average ΔY"); `standard_dY` ("Standard ΔYs"); `Ys_upper` (Ys(x) at exact age x = `age_upper`). Infinite values are `Inf` |
| `booth_standard_single.R` | Builds `booth_standard_single` from `Ys_upper` in `table_3.csv`, with Ys(10) = −Inf |

In both scripts, cumulative standard fertility is F(x) = exp(−exp(−Ys(x))), and the proportion of fertility in an age group is the difference in F across the group.

## Checks

`booth_standard_single.R` checks that:

- **The transcription is consistent.** Differences in `Ys_upper` match `standard_dY` (typed separately) to within the rounding of the table (1e-5).
- **It agrees with the five-year standard.** Table 3's values of Ys at ages 15, 20, ..., 45 are identical to Table 2's, so single-year values summed to five-year groups reproduce `booth_standard`.

Column `adjusted_average_dY` is transcribed for completeness, and is not used. For ages 15–19 its ratio to `standard_dY` is 0.971–0.979, not the adjustment factor 0.99135 described in the text of the article.
