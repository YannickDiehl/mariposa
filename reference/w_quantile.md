# Calculate Population-Representative Percentiles

`w_quantile()` calculates percentiles (quantiles) using survey weights
for population-representative results. Percentiles divide your data into
equal portions – for example, the 25th percentile is the value below
which 25% of your population falls. This is essential for understanding
the distribution of variables like income, age, or satisfaction scores.

## Usage

``` r
w_quantile(
  data,
  ...,
  weights = NULL,
  probs = c(0, 0.25, 0.5, 0.75, 1),
  na.rm = TRUE
)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- ...:

  The numeric variables you want to analyze. You can list multiple
  variables or use helpers like `starts_with("income")`

- weights:

  Survey weights to make results representative of your population.
  Without weights, you get the simple sample quantiles. Give a column
  name (unquoted or as a string), an expression such as
  `sampling_weight * 2`, or a numeric vector with one weight per row.

- probs:

  Which percentiles to calculate, as proportions between 0 and 1.
  Default: `c(0, 0.25, 0.5, 0.75, 1)` for the minimum, 25th percentile,
  median, 75th percentile, and maximum. Use `c(0.1, 0.5, 0.9)` for
  deciles, or `c(0.25, 0.5, 0.75)` for quartiles only.

- na.rm:

  Remove missing values before calculating? (Default: TRUE). With
  `FALSE`, the result for a variable that contains missing values is
  `NA` (as in base R).

## Value

Population-weighted quantile(s) with sample size information, including
the weighted percentile values, effective sample size (effective N), and
the number of valid observations used.

## Details

### Understanding the Results

- **Percentile values**: The data values at each requested percentile in
  the weighted population. For example, if the weighted 25th percentile
  of income is 35,000, then 25% of the population earns less than that.

- **Effective N**: How many independent observations your weighted data
  represents.

- **N / Missing**: Valid and missing cases. With weights, both are sums
  of weights (displayed rounded), as SPSS reports them under
  `WEIGHT BY`; Kish's effective N is shown by
  [`summary()`](https://rdrr.io/r/base/summary.html).

Common percentiles and their meaning:

- **0% (minimum)**: The smallest observed value

- **25% (Q1)**: One quarter of the population falls below this value

- **50% (median)**: Half the population falls below this value

- **75% (Q3)**: Three quarters of the population falls below this value

- **100% (maximum)**: The largest observed value

### When to Use This

Use `w_quantile()` when:

- You want to know at what values the population splits into groups

- You need to construct population-representative income brackets or age
  groups

- You want to identify the median or quartiles with proper weighting

- You need SPSS-compatible weighted percentile values

### Formula

Weighted quantiles are calculated using cumulative weights. Observations
are sorted by value, weights are accumulated, and the requested
percentile is found by linear interpolation at the point where the
cumulative weight proportion reaches the target probability.

## References

IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.

## See also

[`quantile`](https://rdrr.io/r/stats/quantile.html) for the base R
quantile function.

[`w_median`](https://YannickDiehl.github.io/mariposa/reference/w_median.md)
for the weighted median (50th percentile).

[`w_iqr`](https://YannickDiehl.github.io/mariposa/reference/w_iqr.md)
for the weighted interquartile range (Q3 - Q1).

[`describe`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
for comprehensive descriptive statistics including quantiles.

### Validation status

Unweighted values use quantile Type 6 (SPSS HAVERAGE). Weighted values
apply the HAVERAGE position to cumulative weights with linear
interpolation - an R-internal extension (Tier 4 of the Validation
Charter); SPSS reference validation for weighted percentiles is pending.

Other weighted_statistics:
[`w_iqr()`](https://YannickDiehl.github.io/mariposa/reference/w_iqr.md),
[`w_kurtosis()`](https://YannickDiehl.github.io/mariposa/reference/w_kurtosis.md),
[`w_mean()`](https://YannickDiehl.github.io/mariposa/reference/w_mean.md),
[`w_median()`](https://YannickDiehl.github.io/mariposa/reference/w_median.md),
[`w_modus()`](https://YannickDiehl.github.io/mariposa/reference/w_modus.md),
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

# Basic weighted quantiles (0%, 25%, 50%, 75%, 100%)
survey_data %>% w_quantile(age, weights = sampling_weight)
#> 
#> Weighted Quantile Statistics
#> ----------------------------
#> Weights: sampling_weight
#> 
#>   ---------------------------------------------------------------
#>   Variable     Min     25%     50%     75%     Max     N  Missing
#>   ---------------------------------------------------------------
#>   age       18.000  38.000  50.000  63.000  95.000  2516        0
#>   ---------------------------------------------------------------

# Custom quantiles
survey_data %>% w_quantile(income, weights = sampling_weight, probs = c(0.1, 0.5, 0.9))
#> 
#> Weighted Quantile Statistics
#> ----------------------------
#> Weights: sampling_weight
#> 
#>   -----------------------------------------------------
#>   Variable       10%       50%       90%     N  Missing
#>   -----------------------------------------------------
#>   income    2100.000  3500.000  5700.000  2201      315
#>   -----------------------------------------------------

# Multiple variables
survey_data %>% w_quantile(age, income, weights = sampling_weight)
#> 
#> Weighted Quantile Statistics
#> ----------------------------
#> Weights: sampling_weight
#> 
#>   ------------------------------------------------------------------------
#>   Variable      Min       25%       50%       75%       Max     N  Missing
#>   ------------------------------------------------------------------------
#>   age        18.000    38.000    50.000    63.000    95.000  2516        0
#>   income    800.000  2700.000  3500.000  4600.000  8000.000  2201      315
#>   ------------------------------------------------------------------------

# Grouped data
survey_data %>% group_by(region) %>% w_quantile(age, weights = sampling_weight)
#> 
#> Weighted Quantile Statistics
#> ----------------------------
#> Weights: sampling_weight
#> 
#> Group: region = East
#> --------------------
#> 
#>   --------------------------------------------------------------
#>   Variable     Min     25%     50%     75%     Max    N  Missing
#>   --------------------------------------------------------------
#>   age       18.000  40.000  53.000  64.000  95.000  509        0
#>   --------------------------------------------------------------
#> 
#> Group: region = West
#> --------------------
#> 
#>   ---------------------------------------------------------------
#>   Variable     Min     25%     50%     75%     Max     N  Missing
#>   ---------------------------------------------------------------
#>   age       18.000  38.000  49.000  62.000  95.000  2007        0
#>   ---------------------------------------------------------------

# Unweighted (for comparison)
survey_data %>% w_quantile(age)
#> 
#> Quantile Statistics
#> -------------------
#> 
#>   ---------------------------------------------------------------
#>   Variable     Min     25%     50%     75%     Max     N  Missing
#>   ---------------------------------------------------------------
#>   age       18.000  38.000  50.000  62.000  95.000  2500        0
#>   ---------------------------------------------------------------
```
