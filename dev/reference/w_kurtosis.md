# Measure Population-Representative Kurtosis

`w_kurtosis()` measures how heavy the tails of your data's distribution
are, using survey weights for population-representative results.
Kurtosis tells you whether your data has unusually many extreme values
(outliers) compared to a normal distribution:

- **Positive (excess) kurtosis**: Heavier tails than normal (more
  outliers)

- **Negative (excess) kurtosis**: Lighter tails than normal (fewer
  outliers)

- **Near zero**: Similar tail behavior to a normal distribution

## Usage

``` r
w_kurtosis(data, ..., weights = NULL, na.rm = TRUE, excess = TRUE)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- ...:

  The numeric variables you want to analyze. You can list multiple
  variables or use helpers like `starts_with("trust")`

- weights:

  Survey weights to make results representative of your population.
  Without weights, you get the simple sample kurtosis.

- na.rm:

  Remove missing values before calculating? (Default: TRUE)

- excess:

  Show excess kurtosis? (Default: TRUE, matching SPSS). Excess kurtosis
  subtracts 3 so that a normal distribution has kurtosis of 0, making
  interpretation easier.

## Value

Population-weighted kurtosis value(s) with sample size information,
including the weighted kurtosis, effective sample size (effective N),
and the number of valid observations used.

## Details

### Understanding the Results

- **Weighted Kurtosis** (excess, default):

  - Near 0: Tail behavior similar to a normal distribution

  - Positive (\> 0): Heavier tails, more extreme values than normal

  - Negative (\< 0): Lighter tails, fewer extreme values than normal

  - Values beyond 2 or below -2 indicate notable departure from
    normality

- **Effective N**: How many independent observations your weighted data
  represents.

- **N**: The actual number of observations used.

Kurtosis is often checked together with skewness to assess normality.
Both should be close to zero for normally distributed data.

### When to Use This

Use `w_kurtosis()` when:

- You need to assess whether a variable follows a normal distribution

- You want to check for unusually many outliers

- You are deciding whether parametric tests are appropriate

- You need SPSS-compatible weighted kurtosis values

### Formula

Uses the SPSS Type 2 (sample-corrected) excess kurtosis formula. First,
the population excess kurtosis (\\g_2\\) is calculated:

\\g_2 = \frac{m_4}{m_2^2} - 3\\

where \\m_2 = \sum w_i(x_i - \bar{x}\_w)^2 / V_1\\ and \\m_4 = \sum
w_i(x_i - \bar{x}\_w)^4 / V_1\\ with \\V_1 = \sum w_i\\.

Then, the bias-corrected (Type 2) kurtosis is:

\\G_2 = \frac{(n+1) \cdot g_2 + 6}{(n-2)(n-3)} \cdot (n-1)\\

where \\n = V_1\\ for weighted data.

## References

Joanes, D. N., & Gill, C. A. (1998). Comparing measures of sample
skewness and kurtosis. The Statistician, 47(1), 183-189.

IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.

## See also

[`w_skew`](https://YannickDiehl.github.io/mariposa/dev/reference/w_skew.md)
for weighted skewness (distribution asymmetry).

[`describe`](https://YannickDiehl.github.io/mariposa/dev/reference/describe.md)
for comprehensive descriptive statistics including kurtosis.

Other weighted_statistics:
[`w_iqr()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_iqr.md),
[`w_mean()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_mean.md),
[`w_median()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_median.md),
[`w_modus()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_modus.md),
[`w_quantile()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_quantile.md),
[`w_range()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_range.md),
[`w_sd()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_sd.md),
[`w_se()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_se.md),
[`w_skew()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_skew.md),
[`w_var()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_var.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic weighted kurtosis (excess kurtosis, default)
survey_data %>% w_kurtosis(age, weights = sampling_weight)
#> 
#> Weighted Excess Kurtosis Statistics
#> -----------------------------------
#> 
#> --- age ---
#>  Variable weighted_kurtosis Effective_N
#>       age            -0.396      2468.8
#> 

# Multiple variables
survey_data %>% w_kurtosis(age, income, life_satisfaction, weights = sampling_weight)
#> 
#> Weighted Excess Kurtosis Statistics
#> -----------------------------------
#> 
#> --- age ---
#>  Variable weighted_kurtosis Effective_N
#>       age            -0.396      2468.8
#> 
#> --- income ---
#>  Variable weighted_kurtosis Effective_N
#>    income             0.388      2158.9
#> 
#> --- life_satisfaction ---
#>           Variable weighted_kurtosis Effective_N
#>  life_satisfaction            -0.598      2390.9
#> 

# Grouped data
survey_data %>% group_by(region) %>% w_kurtosis(age, weights = sampling_weight)
#> 
#> Weighted Excess Kurtosis Statistics
#> -----------------------------------
#> 
#> Group: region = East
#> Warning: Unknown or uninitialised column: `Variable`.
#> 
#> Group: region = West
#> Warning: Unknown or uninitialised column: `Variable`.
#> 

# Raw kurtosis (not excess)
survey_data %>% w_kurtosis(age, weights = sampling_weight, excess = FALSE)
#> 
#> Weighted Kurtosis Statistics
#> ----------------------------
#> 
#> --- age ---
#>  Variable weighted_kurtosis Effective_N
#>       age             2.604      2468.8
#> 

# In summarise context
survey_data %>% summarise(kurt_age = w_kurtosis(age, weights = sampling_weight))
#> # A tibble: 1 × 1
#>   kurt_age
#>      <dbl>
#> 1   -0.396

# Unweighted (for comparison)
survey_data %>% w_kurtosis(age)
#> 
#> Excess Kurtosis Statistics
#> --------------------------
#> 
#> --- age ---
#>  Variable kurtosis    N
#>       age   -0.364 2500
#> 
```
