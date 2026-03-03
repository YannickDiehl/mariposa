# Calculate Population-Representative Standard Errors

`w_se()` calculates the standard error of the mean using survey weights.
The standard error tells you how precisely you have estimated the
population mean – a smaller SE means your estimate is more precise. This
is essential for constructing confidence intervals and assessing the
reliability of your weighted mean estimates.

## Usage

``` r
w_se(data, ..., weights = NULL, na.rm = TRUE)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- ...:

  The numeric variables you want to analyze. You can list multiple
  variables or use helpers like `starts_with("trust")`

- weights:

  Survey weights to make results representative of your population.
  Without weights, you get the simple sample standard error.

- na.rm:

  Remove missing values before calculating? (Default: TRUE)

## Value

Population-weighted standard error(s) with sample size information,
including the weighted SE, effective sample size (effective N), and the
number of valid observations used.

## Details

### Understanding the Results

- **Weighted SE**: The precision of your weighted mean estimate. Smaller
  values mean more precise estimates. You can build a 95% confidence
  interval as: weighted mean +/- 1.96 \* weighted SE.

- **Effective N**: How many independent observations your weighted data
  represents. Weights that vary a lot reduce effective N, increasing the
  SE.

- **N**: The actual number of observations used.

### When to Use This

Use `w_se()` when:

- You need to report precision of mean estimates

- You want to construct confidence intervals for weighted means

- You need to compare precision across subgroups

- You need SPSS-compatible weighted standard errors

### Formula

The weighted standard error is calculated as:

\\SE_w = \frac{s_w}{\sqrt{V_1}}\\

where \\s_w\\ is the weighted standard deviation (see
[`w_sd`](https://YannickDiehl.github.io/mariposa/dev/reference/w_sd.md))
and \\V_1 = \sum w_i\\ is the sum of all weights.

For the unweighted case: \\SE = s / \sqrt{n}\\

## References

IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.

## See also

[`w_sd`](https://YannickDiehl.github.io/mariposa/dev/reference/w_sd.md)
for weighted standard deviation.

[`w_mean`](https://YannickDiehl.github.io/mariposa/dev/reference/w_mean.md)
for weighted means.

[`describe`](https://YannickDiehl.github.io/mariposa/dev/reference/describe.md)
for comprehensive descriptive statistics including SE.

Other weighted_statistics:
[`w_iqr()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_iqr.md),
[`w_kurtosis()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_kurtosis.md),
[`w_mean()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_mean.md),
[`w_median()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_median.md),
[`w_modus()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_modus.md),
[`w_quantile()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_quantile.md),
[`w_range()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_range.md),
[`w_sd()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_sd.md),
[`w_skew()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_skew.md),
[`w_var()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_var.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic weighted standard error
survey_data %>% w_se(age, weights = sampling_weight)
#> 
#> Weighted Standard Error Statistics
#> ----------------------------------
#> 
#> --- age ---
#>  Variable weighted_se Effective_N
#>       age       0.341      2468.8
#> 

# Multiple variables
survey_data %>% w_se(age, income, weights = sampling_weight)
#> 
#> Weighted Standard Error Statistics
#> ----------------------------------
#> 
#> --- age ---
#>  Variable weighted_se Effective_N
#>       age       0.341      2468.8
#> 
#> --- income ---
#>  Variable weighted_se Effective_N
#>    income      30.353      2158.9
#> 

# Grouped data
survey_data %>% group_by(region) %>% w_se(age, weights = sampling_weight)
#> 
#> Weighted Standard Error Statistics
#> ----------------------------------
#> 
#> Group: region = East
#> Warning: Unknown or uninitialised column: `Variable`.
#> 
#> Group: region = West
#> Warning: Unknown or uninitialised column: `Variable`.
#> 

# In summarise context
survey_data %>% summarise(se_age = w_se(age, weights = sampling_weight))
#> # A tibble: 1 × 1
#>   se_age
#>    <dbl>
#> 1  0.341

# Unweighted (for comparison)
survey_data %>% w_se(age)
#> 
#> Standard Error Statistics
#> -------------------------
#> 
#> --- age ---
#>  Variable   se    N
#>       age 0.34 2500
#> 
```
