# Compute Row Sums Across Items

Calculates the sum across multiple variables for each row. This is the R
equivalent of SPSS's `COMPUTE total = SUM(var1, var2, var3)`.

Use `min_valid` to require a minimum number of non-missing items,
mirroring SPSS's `SUM.n()` syntax.

## Usage

``` r
row_sums(data, ..., min_valid = NULL, na.rm = TRUE)
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

A numeric vector with one value per row — the sum across the selected
variables.

## Details

### When to Use row_sums() vs row_means()

- [`row_means()`](https://YannickDiehl.github.io/mariposa/reference/row_means.md):
  For Likert-type scales where you want an average score (preserves the
  original scale range)

- `row_sums()`: For count-based scores (e.g., number of symptoms
  endorsed) or when you need a total score

## See also

[`row_means()`](https://YannickDiehl.github.io/mariposa/reference/row_means.md)
for row-wise means,
[`row_count()`](https://YannickDiehl.github.io/mariposa/reference/row_count.md)
for counting specific values

Other scale:
[`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md),
[`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md),
[`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md),
[`row_count()`](https://YannickDiehl.github.io/mariposa/reference/row_count.md),
[`row_means()`](https://YannickDiehl.github.io/mariposa/reference/row_means.md)

## Examples

``` r
library(dplyr)
data(survey_data)

# Total score across items
survey_data <- survey_data %>%
  mutate(total = row_sums(., trust_government, trust_media, trust_science))

# With min_valid (like SPSS SUM.3)
survey_data <- survey_data %>%
  mutate(total = row_sums(., trust_government, trust_media,
                          trust_science, min_valid = 3))
```
