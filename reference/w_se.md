# Weighted Standard Error

Calculate weighted standard error for numeric variables, with support
for grouped data and multiple variables simultaneously. Formula: \\SE_w
= s_w / \sqrt{V_1}\\ where \\s_w\\ is the weighted SD and \\V_1 = \sum
w_i\\.

## Usage

``` r
w_se(data, ..., weights = NULL, na.rm = TRUE)
```

## Arguments

- data:

  A data frame, or a numeric vector when used in summarise() context

- ...:

  Variable names (unquoted) or tidyselect expressions

- weights:

  Name of the weights variable (unquoted), or a numeric vector of
  weights

- na.rm:

  Logical; if TRUE, missing values are removed (default: TRUE)

## Value

A w_se object (list) containing results and metadata, or numeric values
in summarise context

## See also

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
#> ── Weighted Standard Error Statistics ──────────────────────────────────────────
#> 
#> --- age ---
#>  Variable weighted_se Effective_N
#>       age       0.341      2468.8
#> 

# Multiple variables
survey_data %>% w_se(age, income, weights = sampling_weight)
#> 
#> ── Weighted Standard Error Statistics ──────────────────────────────────────────
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
#> ── Weighted Standard Error Statistics ──────────────────────────────────────────
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
#> ── Standard Error Statistics ───────────────────────────────────────────────────
#> 
#> --- age ---
#>  Variable   se    N
#>       age 0.34 2500
#> 
```
