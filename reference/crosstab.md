# Compare Two Categories: See How They Relate

`crosstab()` shows you how two categorical variables relate to each
other. It creates a table that reveals patterns - like whether education
level differs by region, or if gender influences product preferences.

Think of it as a two-way frequency table that shows:

- How many people fall into each combination of categories

- What percentage each cell represents

- Whether there are patterns or associations

## Usage

``` r
crosstab(
  data,
  row,
  col,
  weights = NULL,
  percentages = c("row", "none", "col", "total", "all"),
  na.rm = TRUE,
  digits = 1
)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- row:

  The variable for table rows (e.g., education, age_group)

- col:

  The variable for table columns (e.g., region, gender)

- weights:

  Optional survey weights for population-representative results

- percentages:

  Which percentages to show:

  - `"row"` (default): Percentages across each row (adds to 100%
    horizontally)

  - `"col"`: Percentages down each column (adds to 100% vertically)

  - `"total"`: Percentage of the entire table

  - `"all"`: Show all three types

  - `"none"`: Just counts, no percentages

- na.rm:

  Remove missing values before calculating? (Default: TRUE)

- digits:

  Decimal places for percentages (Default: 1)

## Value

A cross-tabulation table showing the relationship between two variables

## Details

### Understanding the Results

The crosstab table shows:

- **Cell counts**: Number of people in each combination

- **Row %**: Distribution within each row (e.g., "Among those with high
  school education, X% live in the East")

- **Column %**: Distribution within each column (e.g., "Among those in
  the East, X% have high school education")

- **Total %**: Percentage of the entire sample (e.g., "X% of all
  respondents have high school education AND live in the East")

### When to Use This

Use crosstab when you want to:

- See if two categorical variables are related

- Compare distributions across groups

- Find patterns in survey responses

- Create demographic breakdowns

### Choosing Percentages

- **Row %**: Use when your row variable is the grouping factor (e.g.,
  "How does region vary BY education level?")

- **Column %**: Use when your column variable is the grouping factor
  (e.g., "How does education vary BY region?")

- **Total %**: Use to understand the overall sample composition

### Tips for Success

- Start with row or column percentages, not both at once

- Use chi-squared test to check if the relationship is statistically
  significant

- Watch for small cell counts (\< 5) which may be unreliable

- Consider combining sparse categories if many cells are empty

## See also

[`table`](https://rdrr.io/r/base/table.html) for base R contingency
tables.

[`frequency`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
for single-variable frequency tables.

[`chi_square`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
for testing if the cross-tabulated variables are related.

Other descriptive:
[`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md),
[`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic crosstab
survey_data %>% crosstab(gender, region)
#> Crosstab: gender x region
#>   2 x 2 table, N = 2500
#>   Note: no significance test included - use chi_square() for a test of independence.
#> Use summary() for detailed output.

# With weights and all percentages
survey_data %>% crosstab(gender, education,
                         weights = sampling_weight,
                         percentages = "all")
#> Crosstab: gender x education [Weighted]
#>   2 x 4 table, N = 2516
#>   Note: no significance test included - use chi_square() for a test of independence.
#> Use summary() for detailed output.

# Grouped analysis
survey_data %>%
  group_by(employment) %>%
  crosstab(gender, region, weights = sampling_weight)
#> Crosstab: gender x region [Weighted]
#>   [employment = Student] 2 x 2 table, N = 80
#>   [employment = Employed] 2 x 2 table, N = 1603
#>   [employment = Unemployed] 2 x 2 table, N = 184
#>   [employment = Retired] 2 x 2 table, N = 534
#>   [employment = Other] 2 x 2 table, N = 115
#>   Note: no significance test included - use chi_square() for a test of independence.
#> Use summary() for detailed output.

# Column percentages only
survey_data %>% crosstab(education, employment, percentages = "col")
#> Crosstab: education x employment
#>   4 x 5 table, N = 2500
#>   Note: no significance test included - use chi_square() for a test of independence.
#> Use summary() for detailed output.
```
