# Find the Population-Representative Middle Value

`w_median()` finds the median (middle value) of your data using survey
weights. The weighted median is the value where half the population
falls below and half falls above. Unlike the mean, the median is not
pulled by extreme values, making it a robust measure of the "typical"
value in your population.

## Usage

``` r
w_median(data, ..., weights = NULL, na.rm = TRUE)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- ...:

  The numeric variables you want to analyze. You can list multiple
  variables or use helpers like `starts_with("income")`

- weights:

  Survey weights to make results representative of your population.
  Without weights, you get the simple sample median.

- na.rm:

  Remove missing values before calculating? (Default: TRUE)

## Value

Population-weighted median(s) with sample size information, including
the weighted median, effective sample size (effective N), and the number
of valid observations used.

## Details

### Understanding the Results

- **Weighted Median**: The value that splits the weighted population in
  half. 50% of the population (by weight) falls below this value, 50%
  above.

- **Effective N**: How many independent observations your weighted data
  represents.

- **N**: The actual number of observations used.

Comparing the weighted median to the weighted mean is informative:

- If they are similar, the distribution is roughly symmetric.

- If the mean is much larger than the median, the distribution is
  right-skewed (a few very high values pull the mean up, e.g., income).

### When to Use This

Use `w_median()` when:

- Your data has outliers or is skewed (e.g., income, housing prices)

- You want a robust "typical value" not influenced by extremes

- You need the weighted 50th percentile

- You need SPSS-compatible weighted median values

### Formula

The weighted median is calculated using cumulative weights: observations
are sorted by value, weights are accumulated, and the median is the
value where the cumulative weight reaches 50% of the total weight.
Linear interpolation is used when the 50% point falls between two
observations.

## References

IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.

## See also

[`median`](https://rdrr.io/r/stats/median.html) for the base R median
function.

[`w_mean`](https://YannickDiehl.github.io/mariposa/reference/w_mean.md)
for weighted means.

[`w_quantile`](https://YannickDiehl.github.io/mariposa/reference/w_quantile.md)
for arbitrary weighted percentiles.

[`describe`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
for comprehensive descriptive statistics including the median.

Other weighted_statistics:
[`w_iqr()`](https://YannickDiehl.github.io/mariposa/reference/w_iqr.md),
[`w_kurtosis()`](https://YannickDiehl.github.io/mariposa/reference/w_kurtosis.md),
[`w_mean()`](https://YannickDiehl.github.io/mariposa/reference/w_mean.md),
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

# Basic weighted median
survey_data %>% w_median(age, weights = sampling_weight)
#> 
#> Weighted Median Statistics
#> --------------------------
#> 
#> --- age ---
#>  Variable weighted_median Effective_N
#>       age              50      2468.8
#> 

# Multiple variables
survey_data %>% w_median(age, income, weights = sampling_weight)
#> 
#> Weighted Median Statistics
#> --------------------------
#> 
#> --- age ---
#>  Variable weighted_median Effective_N
#>       age              50      2468.8
#> 
#> --- income ---
#>  Variable weighted_median Effective_N
#>    income            3500      2158.9
#> 

# Grouped data
survey_data %>% group_by(region) %>% w_median(age, weights = sampling_weight)
#> 
#> Weighted Median Statistics
#> --------------------------
#> 
#> Group: region = East
#> Warning: Unknown or uninitialised column: `Variable`.
#> 
#> Group: region = West
#> Warning: Unknown or uninitialised column: `Variable`.
#> 

# In summarise context
survey_data %>% summarise(med_age = w_median(age, weights = sampling_weight))
#> # A tibble: 1 × 1
#>   med_age
#>     <dbl>
#> 1      50

# Unweighted (for comparison)
survey_data %>% w_median(age)
#> 
#> Median Statistics
#> -----------------
#> 
#> --- age ---
#>  Variable median    N
#>       age     50 2500
#> 
```
