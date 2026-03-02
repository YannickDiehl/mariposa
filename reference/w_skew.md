# Measure Population-Representative Skewness

`w_skew()` measures how asymmetric your data's distribution is, using
survey weights for population-representative results. Skewness tells you
whether values tend to trail off more to the left or right:

- **Positive skew**: A long tail to the right (e.g., income – many low,
  few very high)

- **Negative skew**: A long tail to the left (e.g., exam scores – many
  high, few very low)

- **Near zero**: Roughly symmetric (e.g., height in a population)

## Usage

``` r
w_skew(data, ..., weights = NULL, na.rm = TRUE)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- ...:

  The numeric variables you want to analyze. You can list multiple
  variables or use helpers like `starts_with("trust")`

- weights:

  Survey weights to make results representative of your population.
  Without weights, you get the simple sample skewness.

- na.rm:

  Remove missing values before calculating? (Default: TRUE)

## Value

Population-weighted skewness value(s) with sample size information,
including the weighted skewness, effective sample size (effective N),
and the number of valid observations used.

## Details

### Understanding the Results

- **Weighted Skewness**: The population-representative skewness value.

  - Near 0: Distribution is roughly symmetric

  - Between -0.5 and 0.5: Approximately symmetric

  - Between -1 and -0.5 or 0.5 and 1: Moderately skewed

  - Beyond -1 or 1: Highly skewed

- **Effective N**: How many independent observations your weighted data
  represents.

- **N**: The actual number of observations used.

High skewness suggests you might want to use the median instead of the
mean as a measure of center, and non-parametric tests instead of
t-tests.

### When to Use This

Use `w_skew()` when:

- You need to check if a variable is normally distributed

- You want to decide between parametric and non-parametric tests

- You need to assess whether the mean is a good summary measure

- You need SPSS-compatible weighted skewness values

### Formula

Uses the SPSS Type 2 (sample-corrected) skewness formula. First, the
population skewness (\\g_1\\) is calculated:

\\g_1 = \frac{m_3}{m_2^{3/2}}\\

where \\m_2 = \sum w_i(x_i - \bar{x}\_w)^2 / V_1\\ and \\m_3 = \sum
w_i(x_i - \bar{x}\_w)^3 / V_1\\ with \\V_1 = \sum w_i\\.

Then, the bias-corrected (Type 2) skewness is:

\\G_1 = g_1 \cdot \frac{\sqrt{n(n-1)}}{n-2}\\

where \\n = V_1\\ for weighted data.

## References

Joanes, D. N., & Gill, C. A. (1998). Comparing measures of sample
skewness and kurtosis. The Statistician, 47(1), 183-189.

IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.

## See also

[`w_kurtosis`](https://YannickDiehl.github.io/mariposa/reference/w_kurtosis.md)
for weighted kurtosis (tail heaviness).

[`describe`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
for comprehensive descriptive statistics including skewness.

Other weighted_statistics:
[`w_iqr()`](https://YannickDiehl.github.io/mariposa/reference/w_iqr.md),
[`w_kurtosis()`](https://YannickDiehl.github.io/mariposa/reference/w_kurtosis.md),
[`w_mean()`](https://YannickDiehl.github.io/mariposa/reference/w_mean.md),
[`w_median()`](https://YannickDiehl.github.io/mariposa/reference/w_median.md),
[`w_modus()`](https://YannickDiehl.github.io/mariposa/reference/w_modus.md),
[`w_quantile()`](https://YannickDiehl.github.io/mariposa/reference/w_quantile.md),
[`w_range()`](https://YannickDiehl.github.io/mariposa/reference/w_range.md),
[`w_sd()`](https://YannickDiehl.github.io/mariposa/reference/w_sd.md),
[`w_se()`](https://YannickDiehl.github.io/mariposa/reference/w_se.md),
[`w_var()`](https://YannickDiehl.github.io/mariposa/reference/w_var.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic weighted skewness
survey_data %>% w_skew(age, weights = sampling_weight)
#> 
#> ── Weighted Skewness Statistics ────────────────────────────────────────────────
#> 
#> --- age ---
#>  Variable weighted_skew Effective_N
#>       age         0.159      2468.8
#> 

# Multiple variables
survey_data %>% w_skew(age, income, life_satisfaction, weights = sampling_weight)
#> 
#> ── Weighted Skewness Statistics ────────────────────────────────────────────────
#> 
#> --- age ---
#>  Variable weighted_skew Effective_N
#>       age         0.159      2468.8
#> 
#> --- income ---
#>  Variable weighted_skew Effective_N
#>    income         0.725      2158.9
#> 
#> --- life_satisfaction ---
#>           Variable weighted_skew Effective_N
#>  life_satisfaction        -0.499      2390.9
#> 

# Grouped data
survey_data %>% group_by(region) %>% w_skew(age, weights = sampling_weight)
#> 
#> ── Weighted Skewness Statistics ────────────────────────────────────────────────
#> 
#> Group: region = East
#> Warning: Unknown or uninitialised column: `Variable`.
#> 
#> Group: region = West
#> Warning: Unknown or uninitialised column: `Variable`.
#> 

# In summarise context
survey_data %>% summarise(skew_age = w_skew(age, weights = sampling_weight))
#> # A tibble: 1 × 1
#>   skew_age
#>      <dbl>
#> 1    0.159

# Unweighted (for comparison)
survey_data %>% w_skew(age)
#> 
#> ── Skewness Statistics ─────────────────────────────────────────────────────────
#> 
#> --- age ---
#>  Variable  skew    N
#>       age 0.172 2500
#> 
```
