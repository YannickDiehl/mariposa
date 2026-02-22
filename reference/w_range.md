# Weighted Range

Calculate range (max - min) for numeric variables, with support for
grouped data and multiple variables simultaneously. Note: Range is not
affected by weights (same data values regardless of weighting), but this
function supports the standard w\_\* interface for consistency.

## Usage

``` r
w_range(data, ..., weights = NULL, na.rm = TRUE)
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

A w_range object (list) containing results and metadata, or numeric
values in summarise context

## See also

Other weighted_statistics:
[`w_iqr()`](https://YannickDiehl.github.io/mariposa/reference/w_iqr.md),
[`w_kurtosis()`](https://YannickDiehl.github.io/mariposa/reference/w_kurtosis.md),
[`w_mean()`](https://YannickDiehl.github.io/mariposa/reference/w_mean.md),
[`w_median()`](https://YannickDiehl.github.io/mariposa/reference/w_median.md),
[`w_modus()`](https://YannickDiehl.github.io/mariposa/reference/w_modus.md),
[`w_quantile()`](https://YannickDiehl.github.io/mariposa/reference/w_quantile.md),
[`w_sd()`](https://YannickDiehl.github.io/mariposa/reference/w_sd.md),
[`w_se()`](https://YannickDiehl.github.io/mariposa/reference/w_se.md),
[`w_skew()`](https://YannickDiehl.github.io/mariposa/reference/w_skew.md),
[`w_var()`](https://YannickDiehl.github.io/mariposa/reference/w_var.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic range
survey_data %>% w_range(age, weights = sampling_weight)
#> 
#> ── Weighted Range Statistics ───────────────────────────────────────────────────
#> 
#> --- age ---
#>  Variable weighted_range Effective_N
#>       age             77      2468.8
#> 

# Multiple variables
survey_data %>% w_range(age, income, weights = sampling_weight)
#> 
#> ── Weighted Range Statistics ───────────────────────────────────────────────────
#> 
#> --- age ---
#>  Variable weighted_range Effective_N
#>       age             77      2468.8
#> 
#> --- income ---
#>  Variable weighted_range Effective_N
#>    income           7200      2158.9
#> 

# Grouped data
survey_data %>% group_by(region) %>% w_range(age, weights = sampling_weight)
#> 
#> ── Weighted Range Statistics ───────────────────────────────────────────────────
#> 
#> Group: region = East
#> Warning: Unknown or uninitialised column: `Variable`.
#> 
#> Group: region = West
#> Warning: Unknown or uninitialised column: `Variable`.
#> 

# In summarise context
survey_data %>% summarise(range_age = w_range(age, weights = sampling_weight))
#> # A tibble: 1 × 1
#>   range_age
#>       <dbl>
#> 1        77

# Unweighted (for comparison)
survey_data %>% w_range(age)
#> 
#> ── Range Statistics ────────────────────────────────────────────────────────────
#> 
#> --- age ---
#>  Variable range    N
#>       age    77 2500
#> 
```
