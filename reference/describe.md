# Get to Know Your Numeric Data

`describe()` gives you a complete summary of numeric variables - like
age, income, or satisfaction scores. It's your first step in any
analysis, helping you understand what's typical, what's unusual, and how
spread out your data is.

Think of it as a health check for your data that reveals:

- What's the average value?

- What's the middle value?

- How spread out are the responses?

- Are there unusual patterns or outliers?

## Usage

``` r
describe(
  data,
  ...,
  weights = NULL,
  show = "short",
  probs = c(0.25, 0.5, 0.75),
  na.rm = TRUE,
  excess = TRUE
)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- ...:

  The numeric variables you want to summarize. List them separated by
  commas, or use helpers like `starts_with("trust")`

- weights:

  Optional survey weights to get population-representative statistics.
  Without weights, you describe your sample. With weights, you describe
  the population.

- show:

  Which statistics to display:

  - `"short"` (default): Essential stats (mean, median, SD, range, IQR,
    skewness)

  - `"all"`: Everything including variance, kurtosis, mode, quantiles

  - Custom list: Choose specific stats like `c("mean", "sd", "range")`

- probs:

  For quantiles, which percentiles to show (default: 25th, 50th, 75th)

- na.rm:

  Remove missing values before calculating? (Default: TRUE)

- excess:

  For kurtosis, show excess kurtosis? (Default: TRUE, easier to
  interpret)

## Value

A summary table with descriptive statistics for each variable

## Details

### Understanding the Output

Key statistics and what they tell you:

- **n**: How many valid responses (watch for too many missing)

- **Mean**: The average value

- **Median**: The middle value (half above, half below)

- **SD**: Standard deviation - how spread out values are

- **Range**: The minimum and maximum values

- **IQR**: Interquartile range - the middle 50% of values

- **Skewness**: Whether data leans left (negative) or right (positive)

- **Kurtosis**: Whether you have unusual outliers

### When to Use This

Always start here! Use `describe()` to:

- Check data quality (impossible values?)

- Understand distributions before testing

- Spot outliers that might affect analyses

- Compare groups side by side

### Interpreting Patterns

- **Mean ≈ Median**: Data is roughly symmetric

- **Mean \> Median**: Right-skewed (tail extends right)

- **Mean \< Median**: Left-skewed (tail extends left)

- **Large SD**: Responses vary widely

- **Small SD**: Responses are similar

## See also

Other descriptive:
[`crosstab()`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md),
[`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic unweighted analysis
survey_data %>% describe(age)
#> 
#> ── Descriptive Statistics ──────────────────────────────────────────────────────
#>  Variable  Mean Median     SD Range IQR Skewness    N Missing
#>       age 50.55     50 16.976    77  24    0.172 2500       0
#> ────────────────────────────────────────────────────────────────────────────────

# Weighted analysis
survey_data %>% describe(age, weights = sampling_weight)
#> 
#> ── Weighted Descriptive Statistics ─────────────────────────────────────────────
#>  Variable   Mean Median     SD Range IQR Skewness Effective_N
#>       age 50.514     50 17.084    77  25    0.159      2468.8
#> ────────────────────────────────────────────────────────────────────────────────

# Multiple variables with custom statistics
survey_data %>% describe(age, income, life_satisfaction, 
                        weights = sampling_weight, 
                        show = c("mean", "sd", "skew"))
#> 
#> ── Weighted Descriptive Statistics ─────────────────────────────────────────────
#>           Variable     Mean       SD Skewness Effective_N
#>                age   50.514   17.084    0.159      2468.8
#>             income 3743.099 1423.966    0.724      2158.9
#>  life_satisfaction    3.625    1.152   -0.498      2390.9
#> ────────────────────────────────────────────────────────────────────────────────

# Grouped analysis
survey_data %>% 
  group_by(region) %>% 
  describe(age, weights = sampling_weight)
#> 
#> ── Weighted Descriptive Statistics ─────────────────────────────────────────────
#> 
#> 
#> ── Group: region = East ──
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#>  Variable   Mean Median     SD Range IQR Skewness Effective_N
#>       age 52.278     53 17.595    77  24    0.098         477
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> 
#> ── Group: region = West ──
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#>  Variable   Mean Median     SD Range IQR Skewness Effective_N
#>       age 50.067     49 16.927    77  24     0.17      1993.1
#> ────────────────────────────────────────────────────────────────────────────────
```
