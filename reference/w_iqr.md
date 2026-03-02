# Measure Population-Representative Spread (IQR)

`w_iqr()` calculates the interquartile range using survey weights. The
IQR is the distance between the 25th and 75th percentiles – it tells you
the range that contains the middle 50% of your population. Unlike the
standard deviation, the IQR is not affected by outliers, making it a
robust measure of spread.

## Usage

``` r
w_iqr(data, ..., weights = NULL, na.rm = TRUE)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- ...:

  The numeric variables you want to analyze. You can list multiple
  variables or use helpers like `starts_with("income")`

- weights:

  Survey weights to make results representative of your population.
  Without weights, you get the simple sample IQR.

- na.rm:

  Remove missing values before calculating? (Default: TRUE)

## Value

Population-weighted IQR(s) with sample size information, including the
weighted IQR, effective sample size (effective N), and the number of
valid observations used.

## Details

### Understanding the Results

- **Weighted IQR**: The range that covers the middle 50% of the weighted
  population. A larger IQR means more spread in the central part of the
  data.

- **Effective N**: How many independent observations your weighted data
  represents.

- **N**: The actual number of observations used.

The IQR is especially useful when your data is skewed. For example, with
income data, the IQR gives a better sense of "typical spread" than the
SD because extreme incomes do not distort it.

### When to Use This

Use `w_iqr()` when:

- Your data has outliers or is skewed (e.g., income, response times)

- You want a robust measure of spread that is not influenced by extremes

- You need to describe the spread of the middle 50% of your population

- You need SPSS-compatible weighted IQR values

### Formula

\\IQR_w = Q\_{3,w} - Q\_{1,w}\\

where \\Q\_{1,w}\\ and \\Q\_{3,w}\\ are the weighted 25th and 75th
percentiles, calculated using cumulative weights (see
[`w_quantile`](https://YannickDiehl.github.io/mariposa/reference/w_quantile.md)).

## References

IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.

## See also

[`IQR`](https://rdrr.io/r/stats/IQR.html) for the base R IQR function.

[`w_quantile`](https://YannickDiehl.github.io/mariposa/reference/w_quantile.md)
for arbitrary weighted percentiles.

[`w_sd`](https://YannickDiehl.github.io/mariposa/reference/w_sd.md) for
weighted standard deviation (another spread measure).

[`w_range`](https://YannickDiehl.github.io/mariposa/reference/w_range.md)
for the full weighted range.

[`describe`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
for comprehensive descriptive statistics including IQR.

Other weighted_statistics:
[`w_kurtosis()`](https://YannickDiehl.github.io/mariposa/reference/w_kurtosis.md),
[`w_mean()`](https://YannickDiehl.github.io/mariposa/reference/w_mean.md),
[`w_median()`](https://YannickDiehl.github.io/mariposa/reference/w_median.md),
[`w_modus()`](https://YannickDiehl.github.io/mariposa/reference/w_modus.md),
[`w_quantile()`](https://YannickDiehl.github.io/mariposa/reference/w_quantile.md),
[`w_range()`](https://YannickDiehl.github.io/mariposa/reference/w_range.md),
[`w_sd()`](https://YannickDiehl.github.io/mariposa/reference/w_sd.md),
[`w_se()`](https://YannickDiehl.github.io/mariposa/reference/w_se.md),
[`w_skew()`](https://YannickDiehl.github.io/mariposa/reference/w_skew.md),
[`w_var()`](https://YannickDiehl.github.io/mariposa/reference/w_var.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic weighted IQR
survey_data %>% w_iqr(age, weights = sampling_weight)
#> 
#> ── Weighted Interquartile Range Statistics ─────────────────────────────────────
#> 
#> --- age ---
#>  Variable weighted_iqr Effective_N
#>       age           25      2468.8
#> 

# Multiple variables
survey_data %>% w_iqr(age, income, weights = sampling_weight)
#> 
#> ── Weighted Interquartile Range Statistics ─────────────────────────────────────
#> 
#> --- age ---
#>  Variable weighted_iqr Effective_N
#>       age           25      2468.8
#> 
#> --- income ---
#>  Variable weighted_iqr Effective_N
#>    income         1900      2158.9
#> 

# Grouped data
survey_data %>% group_by(region) %>% w_iqr(age, weights = sampling_weight)
#> 
#> ── Weighted Interquartile Range Statistics ─────────────────────────────────────
#> 
#> Group: region = East
#> Warning: Unknown or uninitialised column: `Variable`.
#> 
#> Group: region = West
#> Warning: Unknown or uninitialised column: `Variable`.
#> 

# In summarise context
survey_data %>% summarise(iqr_age = w_iqr(age, weights = sampling_weight))
#> # A tibble: 1 × 1
#>   iqr_age
#>     <dbl>
#> 1      25

# Unweighted (for comparison)
survey_data %>% w_iqr(age)
#> 
#> ── Interquartile Range Statistics ──────────────────────────────────────────────
#> 
#> --- age ---
#>  Variable iqr    N
#>       age  24 2500
#> 
```
