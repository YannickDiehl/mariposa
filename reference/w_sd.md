# Weighted Standard Deviation

Calculate weighted standard deviation for numeric variables, with
support for grouped data and multiple variables simultaneously. Uses the
SPSS frequency weights formula: \\s_w = \sqrt{\sum w_i (x_i -
\bar{x}\_w)^2 / (V_1 - 1)}\\ where \\V_1 = \sum w_i\\.

## Usage

``` r
w_sd(data, ..., weights = NULL, na.rm = TRUE)
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

A w_sd object (list) containing results and metadata, or numeric values
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
[`w_se()`](https://YannickDiehl.github.io/mariposa/reference/w_se.md),
[`w_skew()`](https://YannickDiehl.github.io/mariposa/reference/w_skew.md),
[`w_var()`](https://YannickDiehl.github.io/mariposa/reference/w_var.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic weighted standard deviation
survey_data %>% w_sd(age, weights = sampling_weight)
#> 
#> ── Weighted Standard Deviation Statistics ──────────────────────────────────────
#> 
#> --- age ---
#>  Variable weighted_sd Effective_N
#>       age      17.084      2468.8
#> 

# Multiple variables
survey_data %>% w_sd(age, income, life_satisfaction, weights = sampling_weight)
#> 
#> ── Weighted Standard Deviation Statistics ──────────────────────────────────────
#> 
#> --- age ---
#>  Variable weighted_sd Effective_N
#>       age      17.084      2468.8
#> 
#> --- income ---
#>  Variable weighted_sd Effective_N
#>    income    1423.966      2158.9
#> 
#> --- life_satisfaction ---
#>           Variable weighted_sd Effective_N
#>  life_satisfaction       1.152      2390.9
#> 

# Grouped data
survey_data %>% group_by(region) %>% w_sd(age, weights = sampling_weight)
#> 
#> ── Weighted Standard Deviation Statistics ──────────────────────────────────────
#> 
#> Group: region = East
#> Warning: Unknown or uninitialised column: `Variable`.
#> 
#> Group: region = West
#> Warning: Unknown or uninitialised column: `Variable`.
#> 

# In summarise context
survey_data %>% summarise(sd_age = w_sd(age, weights = sampling_weight))
#> # A tibble: 1 × 1
#>   sd_age
#>    <dbl>
#> 1   17.1

# Unweighted (for comparison)
survey_data %>% w_sd(age)
#> 
#> ── Standard Deviation Statistics ───────────────────────────────────────────────
#> 
#> --- age ---
#>  Variable     sd    N
#>       age 16.976 2500
#> 
```
