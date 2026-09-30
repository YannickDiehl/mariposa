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
  Without weights, you get the simple sample standard error. Give a
  column name (unquoted or as a string), an expression such as
  `sampling_weight * 2`, or a numeric vector with one weight per row.

- na.rm:

  Remove missing values before calculating? (Default: TRUE). With
  `FALSE`, the result for a variable that contains missing values is
  `NA` (as in base R).

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

- **N / Missing**: Valid and missing cases. With weights, both are sums
  of weights (displayed rounded), as SPSS reports them under
  `WEIGHT BY`; Kish's effective N is shown by
  [`summary()`](https://rdrr.io/r/base/summary.html).

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
[`w_sd`](https://YannickDiehl.github.io/mariposa/reference/w_sd.md)) and
\\V_1 = \sum w_i\\ is the sum of all weights.

For the unweighted case: \\SE = s / \sqrt{n}\\

## References

IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.

## See also

[`w_sd`](https://YannickDiehl.github.io/mariposa/reference/w_sd.md) for
weighted standard deviation.

[`w_mean`](https://YannickDiehl.github.io/mariposa/reference/w_mean.md)
for weighted means.

[`describe`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
for comprehensive descriptive statistics including SE.

Other weighted_statistics:
[`w_iqr()`](https://YannickDiehl.github.io/mariposa/reference/w_iqr.md),
[`w_kurtosis()`](https://YannickDiehl.github.io/mariposa/reference/w_kurtosis.md),
[`w_mean()`](https://YannickDiehl.github.io/mariposa/reference/w_mean.md),
[`w_median()`](https://YannickDiehl.github.io/mariposa/reference/w_median.md),
[`w_modus()`](https://YannickDiehl.github.io/mariposa/reference/w_modus.md),
[`w_quantile()`](https://YannickDiehl.github.io/mariposa/reference/w_quantile.md),
[`w_range()`](https://YannickDiehl.github.io/mariposa/reference/w_range.md),
[`w_sd()`](https://YannickDiehl.github.io/mariposa/reference/w_sd.md),
[`w_skew()`](https://YannickDiehl.github.io/mariposa/reference/w_skew.md),
[`w_var()`](https://YannickDiehl.github.io/mariposa/reference/w_var.md)

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
#> Weights: sampling_weight
#> 
#>   ------------------------------
#>   Variable     SE     N  Missing
#>   ------------------------------
#>   age       0.341  2516        0
#>   ------------------------------

# Multiple variables
survey_data %>% w_se(age, income, weights = sampling_weight)
#> 
#> Weighted Standard Error Statistics
#> ----------------------------------
#> Weights: sampling_weight
#> 
#>   -------------------------------
#>   Variable      SE     N  Missing
#>   -------------------------------
#>   age        0.341  2516        0
#>   income    30.353  2201      315
#>   -------------------------------

# Grouped data
survey_data %>% group_by(region) %>% w_se(age, weights = sampling_weight)
#> 
#> Weighted Standard Error Statistics
#> ----------------------------------
#> Weights: sampling_weight
#> 
#> Group: region = East
#> --------------------
#> 
#>   -----------------------------
#>   Variable     SE    N  Missing
#>   -----------------------------
#>   age       0.780  509        0
#>   -----------------------------
#> 
#> Group: region = West
#> --------------------
#> 
#>   ------------------------------
#>   Variable     SE     N  Missing
#>   ------------------------------
#>   age       0.378  2007        0
#>   ------------------------------

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
#>   ------------------------------
#>   Variable     SE     N  Missing
#>   ------------------------------
#>   age       0.340  2500        0
#>   ------------------------------
```
