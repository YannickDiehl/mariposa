# Weighted Median

Calculate weighted median for numeric variables, with support for
grouped data and multiple variables simultaneously.

## Usage

``` r
w_median(data, ..., weights = NULL, na.rm = TRUE)
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

A w_median object (list) containing results and metadata, or numeric
values in summarise context

## See also

Other weighted_statistics:
[`w_iqr()`](https://YannickDiehl.github.io/mariposa/reference/w_iqr.md),
[`w_kurtosis()`](https://YannickDiehl.github.io/mariposa/reference/w_kurtosis.md),
[`w_mean()`](https://YannickDiehl.github.io/mariposa/reference/w_mean.md),
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

# Basic weighted median
survey_data %>% w_median(age, weights = sampling_weight)
#> 
#> ── Weighted Median Statistics ──────────────────────────────────────────────────
#> 
#> --- age ---
#>  Variable weighted_median Effective_N
#>       age              50      2468.8
#> 

# Multiple variables
survey_data %>% w_median(age, income, weights = sampling_weight)
#> 
#> ── Weighted Median Statistics ──────────────────────────────────────────────────
#> 
#> --- age ---
#>  Variable weighted_median Effective_N
#>       age              50      2468.8
#> 
#> --- income ---
#>  Variable weighted_median Effective_N
#>    income            3500      2158.9
#> 

# Grouped data
survey_data %>% group_by(region) %>% w_median(age, weights = sampling_weight)
#> 
#> ── Weighted Median Statistics ──────────────────────────────────────────────────
#> 
#> Group: region = East
#> Warning: Unknown or uninitialised column: `Variable`.
#> 
#> Group: region = West
#> Warning: Unknown or uninitialised column: `Variable`.
#> 

# In summarise context
survey_data %>% summarise(med_age = w_median(age, weights = sampling_weight))
#> # A tibble: 1 × 1
#>   med_age
#>     <dbl>
#> 1      50

# Unweighted (for comparison)
survey_data %>% w_median(age)
#> 
#> ── Median Statistics ───────────────────────────────────────────────────────────
#> 
#> --- age ---
#>  Variable median    N
#>       age     50 2500
#> 
```
