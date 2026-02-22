# Weighted Variance

Calculate weighted variance for numeric variables, with support for
grouped data and multiple variables simultaneously. Uses the SPSS
frequency weights formula: \\s^2_w = \sum w_i (x_i - \bar{x}\_w)^2 /
(V_1 - 1)\\ where \\V_1 = \sum w_i\\.

## Usage

``` r
w_var(data, ..., weights = NULL, na.rm = TRUE)
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

A w_var object (list) containing results and metadata, or numeric values
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
[`w_se()`](https://YannickDiehl.github.io/mariposa/reference/w_se.md),
[`w_skew()`](https://YannickDiehl.github.io/mariposa/reference/w_skew.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic weighted variance
survey_data %>% w_var(age, weights = sampling_weight)
#> 
#> ── Weighted Variance Statistics ────────────────────────────────────────────────
#> 
#> --- age ---
#>  Variable weighted_var Effective_N
#>       age      291.857      2468.8
#> 

# Multiple variables
survey_data %>% w_var(age, income, weights = sampling_weight)
#> 
#> ── Weighted Variance Statistics ────────────────────────────────────────────────
#> 
#> --- age ---
#>  Variable weighted_var Effective_N
#>       age      291.857      2468.8
#> 
#> --- income ---
#>  Variable weighted_var Effective_N
#>    income      2027678      2158.9
#> 

# Grouped data
survey_data %>% group_by(region) %>% w_var(age, weights = sampling_weight)
#> 
#> ── Weighted Variance Statistics ────────────────────────────────────────────────
#> 
#> Group: region = East
#> Warning: Unknown or uninitialised column: `Variable`.
#> 
#> Group: region = West
#> Warning: Unknown or uninitialised column: `Variable`.
#> 

# In summarise context
survey_data %>% summarise(var_age = w_var(age, weights = sampling_weight))
#> # A tibble: 1 × 1
#>   var_age
#>     <dbl>
#> 1    292.

# Unweighted (for comparison)
survey_data %>% w_var(age)
#> 
#> ── Variance Statistics ─────────────────────────────────────────────────────────
#> 
#> --- age ---
#>  Variable     var    N
#>       age 288.185 2500
#> 
```
