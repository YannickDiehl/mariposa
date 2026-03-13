# Transform Scores to Percent of Maximum Possible (POMPS)

`pomps()` transforms scores to a 0-100 scale using the Percent of
Maximum Possible Scores method. This makes different scales directly
comparable regardless of their original range.

This is the R equivalent of the SPSS formula:
`COMPUTE v81p = ((v81 - 1) / (7 - 1)) * 100`.

A score of 0 means the minimum possible score, 100 means the maximum.
The transformation preserves all correlations between variables.

## Usage

``` r
pomps(x, scale_min = NULL, scale_max = NULL)
```

## Arguments

- x:

  A numeric vector to transform (e.g., a column from your data).

- scale_min:

  The theoretical minimum of the scale. If `NULL` (default), the
  observed minimum of `x` is used.

- scale_max:

  The theoretical maximum of the scale. If `NULL` (default), the
  observed maximum of `x` is used.

## Value

A numeric vector of the same length as `x`, with values rescaled to the
0-100 range.

## Details

### The Formula

POMPS = ((score - scale_min) / (scale_max - scale_min)) \* 100

### Why Specify scale_min and scale_max?

By default, `pomps()` uses the observed minimum and maximum of your
data. However, for Likert scales you should specify the *theoretical*
range:

- A 1-5 Likert scale: `scale_min = 1, scale_max = 5`

- A 1-7 Likert scale: `scale_min = 1, scale_max = 7`

- A 0-10 scale: `scale_min = 0, scale_max = 10`

Using theoretical values ensures that the transformation is consistent
across samples and time points.

### When to Use This

- Comparing variables measured on different scales

- Creating profile plots across scales with different ranges

- Reporting scale scores in an intuitive 0-100 format

## See also

[`row_means`](https://YannickDiehl.github.io/mariposa/reference/row_means.md)
for creating mean indices across items.

Other scale:
[`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md),
[`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md),
[`row_count()`](https://YannickDiehl.github.io/mariposa/reference/row_count.md),
[`row_means()`](https://YannickDiehl.github.io/mariposa/reference/row_means.md),
[`row_sums()`](https://YannickDiehl.github.io/mariposa/reference/row_sums.md)

## Examples

``` r
library(dplyr)
data(survey_data)

# Transform a 1-5 Likert scale to POMPS
survey_data <- survey_data %>%
  mutate(trust_gov_pomps = pomps(trust_government, scale_min = 1, scale_max = 5))

# Transform multiple variables with the same scale
survey_data <- survey_data %>%
  mutate(across(
    c(trust_government, trust_media, trust_science),
    ~ pomps(.x, scale_min = 1, scale_max = 5),
    .names = "{.col}_pomps"
  ))

# Auto-detect range (uses observed min/max)
survey_data <- survey_data %>%
  mutate(age_pomps = pomps(age))
```
