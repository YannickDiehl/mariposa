# Create a Mean Index Across Items

`scale_index()` calculates the mean across multiple items for each
respondent - the standard way to create scale scores from survey items.
This is the R equivalent of SPSS's
`COMPUTE m_X = MEAN(var1, var2, var3)`.

For example, if you measured trust with 3 items (trust in government,
trust in media, trust in science), `scale_index()` creates a single
trust score by averaging across those items.

## Usage

``` r
scale_index(data, ..., min_valid = NULL, na.rm = TRUE)
```

## Arguments

- data:

  Your survey data (a data frame or tibble). When used inside
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html), pass
  `.` or use
  [`pick()`](https://dplyr.tidyverse.org/reference/pick.html).

- ...:

  The items to average. Use bare column names separated by commas, or
  tidyselect helpers like `starts_with("trust")`. If no variables are
  specified, all numeric columns in `data` are used (useful with
  [`pick()`](https://dplyr.tidyverse.org/reference/pick.html)).

- min_valid:

  Minimum number of non-missing items required to compute a mean. If a
  respondent has fewer valid items, `NA` is returned for that row. This
  works like SPSS's `MEAN.x()` syntax: `min_valid = 2` corresponds to
  `MEAN.2(a, b, c)` in SPSS. Default is `NULL` (compute mean if at least
  1 value is valid).

- na.rm:

  Remove missing values before calculating? (Default: TRUE)

## Value

A numeric vector with one value per row - the mean across the selected
items. Use inside
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
to add it as a new column to your data.

## Details

### How It Works

For each respondent (row), `scale_index()` computes the arithmetic mean
of the selected items. Missing values are ignored by default, so a
respondent who answered 2 out of 3 items still gets a score.

### The min_valid Parameter

In practice, you often want to require a minimum number of valid
responses. If someone only answered 1 out of 5 items, a mean based on a
single item may not be reliable. Set `min_valid` to control this:

- `min_valid = NULL` (default): Compute mean with any number of valid
  values (at least 1)

- `min_valid = 2`: Require at least 2 valid values

- `min_valid = 3`: Require at least 3 valid values

### When to Use This

Use `scale_index()` after checking reliability with
[`reliability`](https://YannickDiehl.github.io/mariposa/dev/reference/reliability.md):

1.  Run
    [`reliability()`](https://YannickDiehl.github.io/mariposa/dev/reference/reliability.md)
    to check if items form a reliable scale

2.  If Cronbach's Alpha is acceptable (typically \> .70), create the
    index

3.  Use the index in further analyses (t-tests, correlations,
    regression)

## See also

[`pomps`](https://YannickDiehl.github.io/mariposa/dev/reference/pomps.md)
for transforming scales to Percent of Maximum Possible Scores.

Other scale:
[`efa()`](https://YannickDiehl.github.io/mariposa/dev/reference/efa.md),
[`pomps()`](https://YannickDiehl.github.io/mariposa/dev/reference/pomps.md),
[`reliability()`](https://YannickDiehl.github.io/mariposa/dev/reference/reliability.md)

## Examples

``` r
library(dplyr)
data(survey_data)

# Create a trust scale from 3 items
survey_data <- survey_data %>%
  mutate(m_trust = scale_index(., trust_government, trust_media, trust_science))

# Using pick() - works with both %>% and |> pipes
survey_data <- survey_data %>%
  mutate(m_trust = scale_index(
    pick(trust_government, trust_media, trust_science)
  ))

# Using tidyselect helpers
survey_data <- survey_data %>%
  mutate(m_trust = scale_index(., starts_with("trust")))

# Require at least 2 valid items (like SPSS MEAN.2)
survey_data <- survey_data %>%
  mutate(m_trust = scale_index(., trust_government, trust_media,
                               trust_science, min_valid = 2))

# Standalone usage (returns vector)
scores <- scale_index(survey_data, trust_government, trust_media, trust_science)
```
