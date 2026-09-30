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
  estimates. Give a column name (unquoted or as a string), an expression
  such as `sampling_weight * 2`, or a numeric vector with one weight per
  row.

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

  Show percentages excluding missing values? (Default: TRUE). The
  cumulative percentages are cumulative valid percentages, so
  `show_valid = FALSE` hides them too.

- show_sum:

  Show the cumulative (valid) percentages? (Default: TRUE)

- show_labels:

  Show category labels if available? (Default: "auto" - shows labels
  when they exist)

- show_unused:

  Show all defined value labels, even those with zero observations?
  (Default: FALSE). When TRUE, values that have labels defined (e.g.,
  from statistical software files) but no cases in the data are included
  with frequency 0. This is useful for labelled datasets where unused
  categories should still appear in the output. The same applies to
  empty factor levels, which are hidden by default (as SPSS lists only
  observed values). Automatically enables label display.

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

The frequency table follows the SPSS FREQUENCIES layout:

- **N**: Number of responses in each category (weighted: sum of weights,
  displayed rounded)

- **Raw %**: Percentage including missing values (use for "response
  rate")

- **Valid %**: Percentage excluding missing values (use for "among those
  who answered")

- **Cum. %**: Running total of the valid percentages (helps identify
  cutoff points)

- **Total valid**, the missing categories, **Total missing** (with two
  or more missing categories) and the grand **Total**; without missing
  values a single **Total** row ends the table.

Factors, character and logical variables show their categories in the
Value column; labelled numeric variables show the code and its label.
Long labels are never cut; they wrap when the table would be wider than
the console.

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
[`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md),
[`multiple_response()`](https://YannickDiehl.github.io/mariposa/reference/multiple_response.md),
[`normality_test()`](https://YannickDiehl.github.io/mariposa/reference/normality_test.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic categorical analysis
survey_data %>% frequency(gender)
#> 
#> Frequency Analysis Results
#> --------------------------
#> 
#> gender (Gender)
#> # total N=2500 valid N=2500
#> 
#> +--------+------+--------+---------+--------+
#> | Value  |    N |  Raw % | Valid % | Cum. % |
#> +--------+------+--------+---------+--------+
#> | Male   | 1194 |  47.76 |   47.76 |  47.76 |
#> | Female | 1306 |  52.24 |   52.24 | 100.00 |
#> +--------+------+--------+---------+--------+
#> | Total  | 2500 | 100.00 |  100.00 |        |
#> +--------+------+--------+---------+--------+
#> 

# Multiple variables with weights
survey_data %>% frequency(gender, region, weights = sampling_weight)
#> 
#> Weighted Frequency Analysis Results
#> -----------------------------------
#> 
#> gender (Gender)
#> # total N=2516 valid N=2516
#> 
#> +--------+------+--------+---------+--------+
#> | Value  |    N |  Raw % | Valid % | Cum. % |
#> +--------+------+--------+---------+--------+
#> | Male   | 1195 |  47.48 |   47.48 |  47.48 |
#> | Female | 1321 |  52.52 |   52.52 | 100.00 |
#> +--------+------+--------+---------+--------+
#> | Total  | 2516 | 100.00 |  100.00 |        |
#> +--------+------+--------+---------+--------+
#> 
#> 
#> region (Region (East/West))
#> # total N=2516 valid N=2516
#> 
#> +-------+------+--------+---------+--------+
#> | Value |    N |  Raw % | Valid % | Cum. % |
#> +-------+------+--------+---------+--------+
#> | East  |  509 |  20.23 |   20.23 |  20.23 |
#> | West  | 2007 |  79.77 |   79.77 | 100.00 |
#> +-------+------+--------+---------+--------+
#> | Total | 2516 | 100.00 |  100.00 |        |
#> +-------+------+--------+---------+--------+
#> 

# Grouped analysis by region
survey_data %>% 
  group_by(region) %>% 
  frequency(gender, weights = sampling_weight)
#> 
#> Weighted Frequency Analysis Results
#> -----------------------------------
#> 
#> gender (Gender)
#> 
#> Group: region = East
#> --------------------
#> # total N=509 valid N=509
#> 
#> +--------+-----+--------+---------+--------+
#> | Value  |   N |  Raw % | Valid % | Cum. % |
#> +--------+-----+--------+---------+--------+
#> | Male   | 249 |  49.01 |   49.01 |  49.01 |
#> | Female | 260 |  50.99 |   50.99 | 100.00 |
#> +--------+-----+--------+---------+--------+
#> | Total  | 509 | 100.00 |  100.00 |        |
#> +--------+-----+--------+---------+--------+
#> 
#> 
#> Group: region = West
#> --------------------
#> # total N=2007 valid N=2007
#> 
#> +--------+------+--------+---------+--------+
#> | Value  |    N |  Raw % | Valid % | Cum. % |
#> +--------+------+--------+---------+--------+
#> | Male   |  945 |  47.09 |   47.09 |  47.09 |
#> | Female | 1062 |  52.91 |   52.91 | 100.00 |
#> +--------+------+--------+---------+--------+
#> | Total  | 2007 | 100.00 |  100.00 |        |
#> +--------+------+--------+---------+--------+
#> 

# Education levels with sorting
survey_data %>% frequency(education, sort_frq = "desc")
#> 
#> Frequency Analysis Results
#> --------------------------
#> 
#> education (Highest educational attainment)
#> # total N=2500 valid N=2500
#> 
#> +------------------------+------+--------+---------+--------+
#> | Value                  |    N |  Raw % | Valid % | Cum. % |
#> +------------------------+------+--------+---------+--------+
#> | Basic Secondary        |  841 |  33.64 |   33.64 |  33.64 |
#> | Academic Secondary     |  631 |  25.24 |   25.24 |  58.88 |
#> | Intermediate Secondary |  629 |  25.16 |   25.16 |  84.04 |
#> | University             |  399 |  15.96 |   15.96 | 100.00 |
#> +------------------------+------+--------+---------+--------+
#> | Total                  | 2500 | 100.00 |  100.00 |        |
#> +------------------------+------+--------+---------+--------+
#> 

# Employment status with custom display options
survey_data %>% frequency(employment, weights = sampling_weight, 
                         show_na = TRUE, show_sum = TRUE)
#> 
#> Weighted Frequency Analysis Results
#> -----------------------------------
#> 
#> employment (Employment status)
#> # total N=2516 valid N=2516
#> 
#> +------------+------+--------+---------+--------+
#> | Value      |    N |  Raw % | Valid % | Cum. % |
#> +------------+------+--------+---------+--------+
#> | Student    |   80 |   3.18 |    3.18 |   3.18 |
#> | Employed   | 1603 |  63.71 |   63.71 |  66.89 |
#> | Unemployed |  184 |   7.32 |    7.32 |  74.21 |
#> | Retired    |  534 |  21.21 |   21.21 |  95.41 |
#> | Other      |  115 |   4.59 |    4.59 | 100.00 |
#> +------------+------+--------+---------+--------+
#> | Total      | 2516 | 100.00 |  100.00 |        |
#> +------------+------+--------+---------+--------+
#> 
```
