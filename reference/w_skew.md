# Weighted Skewness

Calculate weighted skewness for numeric variables, with support for
grouped data and multiple variables simultaneously. Uses the SPSS Type 2
(sample-corrected) skewness formula.

## Usage

``` r
w_skew(data, ..., weights = NULL, na.rm = TRUE)
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

A w_skew object (list) containing results and metadata, or numeric
values in summarise context

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
