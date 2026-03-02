# Find the Range of Your Data

`w_range()` calculates the range (maximum minus minimum) of your data.
The range gives you the total spread from the smallest to the largest
observed value. It provides the `w_*` interface for consistency with
other weighted statistics, though the range itself is not affected by
weights (the minimum and maximum values remain the same regardless of
weighting).

## Usage

``` r
w_range(data, ..., weights = NULL, na.rm = TRUE)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- ...:

  The numeric variables you want to analyze. You can list multiple
  variables or use helpers like `starts_with("income")`

- weights:

  Survey weights are accepted for interface consistency, but do not
  affect the range calculation. The range depends only on the observed
  minimum and maximum values.

- na.rm:

  Remove missing values before calculating? (Default: TRUE)

## Value

The range (max - min) with sample size information, including the
effective sample size (effective N) when weights are provided, and the
number of valid observations used.

## Details

### Understanding the Results

- **Range**: The difference between the largest and smallest values. A
  large range indicates that at least some values are far apart.

- **Effective N**: Reported when weights are provided, for consistency
  with other weighted statistics.

- **N**: The actual number of observations used.

Note: The range is sensitive to outliers. A single extreme value can
dramatically increase the range. Consider using
[`w_iqr`](https://YannickDiehl.github.io/mariposa/reference/w_iqr.md)
for a more robust measure of spread.

### When to Use This

Use `w_range()` when:

- You want a quick overview of the total spread of your data

- You need to check for data entry errors (impossible values)

- You want to compare the total spread across groups

### Formula

\\Range = \max(x) - \min(x)\\

Note: This statistic is weight-invariant. The minimum and maximum
observed values do not change when weights are applied.

## References

IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.

## See also

[`range`](https://rdrr.io/r/base/range.html) for the base R range
function.

[`w_iqr`](https://YannickDiehl.github.io/mariposa/reference/w_iqr.md)
for the weighted interquartile range (more robust).

[`w_sd`](https://YannickDiehl.github.io/mariposa/reference/w_sd.md) for
weighted standard deviation (another spread measure).

[`describe`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
for comprehensive descriptive statistics including range.

Other weighted_statistics:
[`w_iqr()`](https://YannickDiehl.github.io/mariposa/reference/w_iqr.md),
[`w_kurtosis()`](https://YannickDiehl.github.io/mariposa/reference/w_kurtosis.md),
[`w_mean()`](https://YannickDiehl.github.io/mariposa/reference/w_mean.md),
[`w_median()`](https://YannickDiehl.github.io/mariposa/reference/w_median.md),
[`w_modus()`](https://YannickDiehl.github.io/mariposa/reference/w_modus.md),
[`w_quantile()`](https://YannickDiehl.github.io/mariposa/reference/w_quantile.md),
[`w_sd()`](https://YannickDiehl.github.io/mariposa/reference/w_sd.md),
[`w_se()`](https://YannickDiehl.github.io/mariposa/reference/w_se.md),
[`w_skew()`](https://YannickDiehl.github.io/mariposa/reference/w_skew.md),
[`w_var()`](https://YannickDiehl.github.io/mariposa/reference/w_var.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic range
survey_data %>% w_range(age, weights = sampling_weight)
#> 
#> Weighted Range Statistics
#> -------------------------
#> 
#> --- age ---
#>  Variable weighted_range Effective_N
#>       age             77      2468.8
#> 

# Multiple variables
survey_data %>% w_range(age, income, weights = sampling_weight)
#> 
#> Weighted Range Statistics
#> -------------------------
#> 
#> --- age ---
#>  Variable weighted_range Effective_N
#>       age             77      2468.8
#> 
#> --- income ---
#>  Variable weighted_range Effective_N
#>    income           7200      2158.9
#> 

# Grouped data
survey_data %>% group_by(region) %>% w_range(age, weights = sampling_weight)
#> 
#> Weighted Range Statistics
#> -------------------------
#> 
#> Group: region = East
#> Warning: Unknown or uninitialised column: `Variable`.
#> 
#> Group: region = West
#> Warning: Unknown or uninitialised column: `Variable`.
#> 

# In summarise context
survey_data %>% summarise(range_age = w_range(age, weights = sampling_weight))
#> # A tibble: 1 × 1
#>   range_age
#>       <dbl>
#> 1        77

# Unweighted (for comparison)
survey_data %>% w_range(age)
#> 
#> Range Statistics
#> ----------------
#> 
#> --- age ---
#>  Variable range    N
#>       age    77 2500
#> 
```
