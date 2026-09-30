# Count Occurrences of a Value Across Columns

Counts how often a specific value appears in each row across the
selected variables. Useful for data quality checks (e.g., "How many
items did a respondent answer with -9?") or for creating count-based
indices.

## Usage

``` r
row_count(data, ..., count, na.rm = TRUE)
```

## Arguments

- data:

  Your survey data (a data frame or tibble).

- ...:

  The variables to check. Supports tidyselect.

- count:

  The value(s) to count: one value (`count = 5`) or a set
  (`count = c(4, 5)` counts cells equal to 4 or 5, like SPSS's
  `COUNT n = v1 TO v5 (4, 5)`). `NA` counts missing values (SPSS keyword
  `MISSING`). Missing codes of imported data (e.g. -9 read by
  [`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md)
  as a tagged NA) are counted when listed, as SPSS's `COUNT` counts
  user-missing values.

- na.rm:

  If `TRUE` (default), `NA` values are ignored. If `FALSE`, any row
  containing a missing value that is not counted returns `NA`.

## Value

An integer vector with one value per row — the count of how often
`count` appears.

## See also

[`row_sums()`](https://YannickDiehl.github.io/mariposa/reference/row_sums.md)
for row-wise sums,
[`row_means()`](https://YannickDiehl.github.io/mariposa/reference/row_means.md)
for row-wise means

Other scale:
[`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md),
[`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md),
[`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md),
[`row_means()`](https://YannickDiehl.github.io/mariposa/reference/row_means.md),
[`row_sums()`](https://YannickDiehl.github.io/mariposa/reference/row_sums.md)

## Examples

``` r
library(dplyr)
data(survey_data)

# How many items did each respondent answer with the highest value (5)?
survey_data <- survey_data %>%
  mutate(n_top = row_count(., trust_government, trust_media,
                           trust_science, count = 5))

# Top-2 box (4 or 5) and number of missing answers per respondent
survey_data <- survey_data %>%
  mutate(n_top2 = row_count(., trust_government, trust_media,
                            trust_science, count = c(4, 5)),
         n_miss = row_count(., trust_government, trust_media,
                            trust_science, count = NA))
```
