# Count How Many People Chose Each Option

`frequency()` helps you understand categorical data by showing how many
people chose each option. It's perfect for survey questions with fixed
choices like education level, yes/no questions, or rating scales.

Think of it as creating a summary table that shows:

- How many people chose each option

- What percentage that represents

- Running totals to see cumulative patterns

## Usage

``` r
frequency(
  data,
  ...,
  weights = NULL,
  sort_frq = "none",
  show_na = TRUE,
  show_prc = TRUE,
  show_valid = TRUE,
  show_sum = TRUE,
  show_labels = "auto",
  show_unused = FALSE,
  sort.frq = NULL,
  show.na = NULL,
  show.prc = NULL,
  show.valid = NULL,
  show.sum = NULL,
  show.labels = NULL,
  show.unused = NULL
)

fre(data, ..., weights = NULL, sort_frq = "none", show_na = TRUE,
  show_prc = TRUE, show_valid = TRUE, show_sum = TRUE, show_labels = "auto",
  show_unused = FALSE, sort.frq = NULL, show.na = NULL, show.prc = NULL,
  show.valid = NULL, show.sum = NULL, show.labels = NULL, show.unused = NULL)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- ...:

  The categorical variables you want to analyze. You can list multiple
  variables separated by commas, or use helpers like
  `starts_with("trust")`

- weights:

  Optional survey weights for population-representative results. Without
  weights, you get sample frequencies. With weights, you get population
  estimates.

- sort_frq:

  How to order the results:

  - `"none"` (default): Keep original order

  - `"asc"`: Sort from lowest to highest frequency

  - `"desc"`: Sort from highest to lowest frequency

- show_na:

  Include missing values in the table? (Default: TRUE)

- show_prc:

  Show raw percentages including missing values? (Default: TRUE)

- show_valid:

  Show percentages excluding missing values? (Default: TRUE)

- show_sum:

  Show cumulative totals? (Default: TRUE)

- show_labels:

  Show category labels if available? (Default: "auto" - shows labels
  when they exist)

- show_unused:

  Show all defined value labels, even those with zero observations?
  (Default: FALSE). When TRUE, values that have labels defined (e.g.,
  from statistical software files) but no cases in the data are included
  with frequency 0. This is useful for labelled datasets where unused
  categories should still appear in the output. Automatically enables
  label display.

- sort.frq, show.na, show.prc, show.valid, show.sum, show.labels,
  show.unused:

  Defunct dot-case argument names, removed in mariposa 0.6.9. Calling
  the function with any of them is an error; use the snake_case
  equivalents instead. (The formals are retained only so that the old
  names error clearly instead of being swallowed by `...`.)

## Value

A frequency table showing counts and percentages for each category

## Details

### Understanding the Results

The frequency table shows:

- **Freq**: Number of responses in each category

- **%**: Percentage including missing values (use for "response rate")

- **Valid %**: Percentage excluding missing values (use for "among those
  who answered")

- **Cum %**: Running total percentage (helps identify cutoff points)

### When to Use This

Use `frequency()` when you have:

- Categorical variables (gender, region, education level)

- Yes/No questions

- Rating scales (satisfied/neutral/dissatisfied)

- Any question with a fixed set of options

### Weights Make a Difference

Without weights, you're describing your sample. With weights, you're
estimating population values. Always use weights for population
inference.

### Tagged Missing Values

When data is imported with tagged NAs (e.g., via
[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md)
with `tag_na = TRUE`, or
[`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md),
[`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md),
[`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md)
with the `tag_na` parameter), `frequency()` automatically expands the
missing value section to show each missing type individually (with its
original missing value code and label), plus summary rows for **Total
Valid** and **Total Missing**.

## See also

[`table`](https://rdrr.io/r/base/table.html) for base R frequency
tables.

[`crosstab`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md)
for cross-tabulation of two variables.

[`chi_square`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
for testing relationships between categories.

[`describe`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
for numeric variable summaries.

Other descriptive:
[`crosstab()`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md),
[`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic categorical analysis
survey_data %>% frequency(gender)
#> Frequency: gender
#>   2 categories, N valid = 2500, missing = 0
#> Use summary() for detailed output.

# Multiple variables with weights
survey_data %>% frequency(gender, region, weights = sampling_weight)
#> Frequency: gender [Weighted]
#>   2 categories, N valid = 2516, missing = 0
#> Frequency: region [Weighted]
#>   2 categories, N valid = 2516, missing = 0
#> Use summary() for detailed output.

# Grouped analysis by region
survey_data %>% 
  group_by(region) %>% 
  frequency(gender, weights = sampling_weight)
#> Frequency: gender [Weighted]
#>   [region = East] 2 categories, N valid = 509, missing = 0
#>   [region = West] 2 categories, N valid = 2007, missing = 0
#> Use summary() for detailed output.

# Education levels with sorting
survey_data %>% frequency(education, sort_frq = "desc")
#> Frequency: education
#>   4 categories, N valid = 2500, missing = 0
#> Use summary() for detailed output.

# Employment status with custom display options
survey_data %>% frequency(employment, weights = sampling_weight, 
                         show_na = TRUE, show_sum = TRUE)
#> Frequency: employment [Weighted]
#>   5 categories, N valid = 2516, missing = 0
#> Use summary() for detailed output.
```
