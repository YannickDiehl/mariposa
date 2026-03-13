# Compute Row Means Across Items

Calculates the mean across multiple variables for each row. This is the
standard way to create scale scores from survey items — the R equivalent
of SPSS's `COMPUTE score = MEAN(var1, var2, var3)`.

Use `min_valid` to require a minimum number of non-missing items,
mirroring SPSS's `MEAN.n()` syntax: `min_valid = 2` corresponds to
`MEAN.2(a, b, c)`.

## Usage

``` r
row_means(data, ..., min_valid = NULL, na.rm = TRUE)
```

## Arguments

- data:

  Your survey data (a data frame or tibble). When used inside
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html), pass
  `.` or use
  [`pick()`](https://dplyr.tidyverse.org/reference/pick.html).

- ...:

  The variables to average. Use bare column names separated by commas,
  or tidyselect helpers like `starts_with("trust")`. If no variables are
  specified, all numeric columns in `data` are used (useful with
  [`pick()`](https://dplyr.tidyverse.org/reference/pick.html)).

- min_valid:

  Minimum number of non-missing values required to compute a mean. If a
  row has fewer valid values, `NA` is returned. Default is `NULL`
  (compute mean if at least 1 value is valid).

- na.rm:

  Remove missing values before calculating? Default: `TRUE`.

## Value

A numeric vector with one value per row — the mean across the selected
variables. Use inside
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
to add it as a new column.

## Details

### How It Works

For each row, `row_means()` computes the arithmetic mean of the selected
variables. Missing values are ignored by default, so a respondent who
answered 2 out of 3 items still gets a score.

### The min_valid Parameter

In practice, you often want to require a minimum number of valid
responses. If someone only answered 1 out of 5 items, a mean based on a
single item may not be reliable. Set `min_valid` to control this:

- `min_valid = NULL` (default): Compute mean with any number of valid
  values (at least 1)

- `min_valid = 2`: Require at least 2 valid values

- `min_valid = 3`: Require at least 3 valid values

### When to Use This

Use `row_means()` after checking reliability with
[`reliability`](https://YannickDiehl.github.io/mariposa/reference/reliability.md):

1.  Run
    [`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md)
    to check if items form a reliable scale

2.  If Cronbach's Alpha is acceptable (typically \> .70), create the
    index

3.  Use the index in further analyses (t-tests, correlations,
    regression)

## See also

[`row_sums()`](https://YannickDiehl.github.io/mariposa/reference/row_sums.md)
for row-wise sums,
[`row_count()`](https://YannickDiehl.github.io/mariposa/reference/row_count.md)
for counting specific values,
[`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md)
for rescaling to 0-100

Other scale:
[`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md),
[`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md),
[`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md),
[`row_count()`](https://YannickDiehl.github.io/mariposa/reference/row_count.md),
[`row_sums()`](https://YannickDiehl.github.io/mariposa/reference/row_sums.md)

## Examples

``` r
library(dplyr)
data(survey_data)

# Create a trust scale from 3 items
survey_data <- survey_data %>%
  mutate(m_trust = row_means(., trust_government, trust_media, trust_science))

# Using pick()
survey_data <- survey_data %>%
  mutate(m_trust = row_means(pick(starts_with("trust"))))

# Require at least 2 valid items (like SPSS MEAN.2)
survey_data <- survey_data %>%
  mutate(m_trust = row_means(., trust_government, trust_media,
                             trust_science, min_valid = 2))
```
