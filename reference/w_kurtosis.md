# Weighted Kurtosis

Calculate weighted kurtosis for numeric variables, with support for
grouped data and multiple variables simultaneously. Uses the SPSS Type 2
(sample-corrected) excess kurtosis formula. Reference: Joanes & Gill
(1998), "Comparing measures of sample skewness and kurtosis"

## Usage

``` r
w_kurtosis(data, ..., weights = NULL, na.rm = TRUE, excess = TRUE)
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

- excess:

  Logical; if TRUE, returns excess kurtosis (default: TRUE, matching
  SPSS)

## Value

A w_kurtosis object (list) containing results and metadata, or numeric
values in summarise context

## See also

Other weighted_statistics:
[`w_iqr()`](https://YannickDiehl.github.io/mariposa/reference/w_iqr.md),
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

# Basic weighted kurtosis (excess kurtosis, default)
survey_data %>% w_kurtosis(age, weights = sampling_weight)
#> 
#> ── Weighted Excess Kurtosis Statistics ─────────────────────────────────────────
#> 
#> --- age ---
#>  Variable weighted_kurtosis Effective_N
#>       age            -0.396      2468.8
#> 

# Multiple variables
survey_data %>% w_kurtosis(age, income, life_satisfaction, weights = sampling_weight)
#> 
#> ── Weighted Excess Kurtosis Statistics ─────────────────────────────────────────
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
#> ── Weighted Excess Kurtosis Statistics ─────────────────────────────────────────
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
#> ── Weighted Kurtosis Statistics ────────────────────────────────────────────────
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
#> ── Excess Kurtosis Statistics ──────────────────────────────────────────────────
#> 
#> --- age ---
#>  Variable kurtosis    N
#>       age   -0.364 2500
#> 
```
