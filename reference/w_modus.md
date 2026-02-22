# Weighted Mode (Modus)

Calculate weighted mode for variables, with support for grouped data and
multiple variables simultaneously.

## Usage

``` r
w_modus(data, ..., weights = NULL, na.rm = TRUE)
```

## Arguments

- data:

  A data frame, or a vector when used in summarise() context

- ...:

  Variable names (unquoted) or tidyselect expressions

- weights:

  Name of the weights variable (unquoted), or a numeric vector of
  weights

- na.rm:

  Logical; if TRUE, missing values are removed

## Value

A w_modus object (list) containing results and metadata, or values in
summarise context

## See also

Other weighted_statistics:
[`w_iqr()`](https://YannickDiehl.github.io/mariposa/reference/w_iqr.md),
[`w_kurtosis()`](https://YannickDiehl.github.io/mariposa/reference/w_kurtosis.md),
[`w_mean()`](https://YannickDiehl.github.io/mariposa/reference/w_mean.md),
[`w_median()`](https://YannickDiehl.github.io/mariposa/reference/w_median.md),
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

# Basic weighted mode (most frequent value)
survey_data %>% w_modus(gender, weights = sampling_weight)
#> 
#> Weighted Mode Statistics
#> ------------------------ 
#> # A tibble: 1 × 3
#>   Variable weighted_mode effective_n
#>   <chr>    <fct>               <dbl>
#> 1 gender   Female              2469.
#> 

# Multiple variables (works best with categorical/discrete data)
survey_data %>% w_modus(gender, region, weights = sampling_weight)
#> 
#> Weighted Mode Statistics
#> ------------------------ 
#> # A tibble: 2 × 3
#>   Variable weighted_mode effective_n
#>   <chr>    <chr>               <dbl>
#> 1 gender   Female              2469.
#> 2 region   West                2469.
#> 

# Grouped data
survey_data %>% group_by(region) %>% w_modus(gender, weights = sampling_weight)
#> 
#> Weighted Mode Statistics
#> ------------------------ 
#> 
#> Group: region = East
#> # A tibble: 1 × 2
#>   weighted_mode effective_n
#>   <fct>               <dbl>
#> 1 Female                477
#> 
#> Group: region = West
#> # A tibble: 1 × 2
#>   weighted_mode effective_n
#>   <fct>               <dbl>
#> 1 Female              1993.
#> 

# In summarise context
survey_data %>% summarise(mode_gender = w_modus(gender, weights = sampling_weight))
#> # A tibble: 1 × 1
#>   mode_gender
#>   <fct>      
#> 1 Female     

# Unweighted (for comparison)
survey_data %>% w_modus(gender)
#> 
#> Mode Statistics
#> --------------- 
#> # A tibble: 1 × 3
#>   Variable mode       n
#>   <chr>    <chr>  <dbl>
#> 1 gender   Female  2500
#> 
```
