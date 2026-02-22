# Weighted Quantiles

Calculate weighted quantiles for numeric variables, with support for
grouped data and multiple variables simultaneously.

## Usage

``` r
w_quantile(
  data,
  ...,
  weights = NULL,
  probs = c(0, 0.25, 0.5, 0.75, 1),
  na.rm = TRUE
)
```

## Arguments

- data:

  A data frame, or a numeric vector when used in summarise() context

- ...:

  Variable names (unquoted) or tidyselect expressions

- weights:

  Name of the weights variable (unquoted), or a numeric vector of
  weights

- probs:

  Numeric vector of probabilities (default: c(0, 0.25, 0.5, 0.75, 1))

- na.rm:

  Logical; if TRUE, missing values are removed (default: TRUE)

## Value

A w_quantile object (list) containing results and metadata, or numeric
values in summarise context

## See also

Other weighted_statistics:
[`w_iqr()`](https://YannickDiehl.github.io/mariposa/reference/w_iqr.md),
[`w_kurtosis()`](https://YannickDiehl.github.io/mariposa/reference/w_kurtosis.md),
[`w_mean()`](https://YannickDiehl.github.io/mariposa/reference/w_mean.md),
[`w_median()`](https://YannickDiehl.github.io/mariposa/reference/w_median.md),
[`w_modus()`](https://YannickDiehl.github.io/mariposa/reference/w_modus.md),
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

# Basic weighted quantiles (0%, 25%, 50%, 75%, 100%)
survey_data %>% w_quantile(age, weights = sampling_weight)
#> 
#> ── Weighted Quantile Statistics ────────────────────────────────────────────────
#>  Variable Quantile    Value         Weights
#>       age      Min   18.000 sampling_weight
#>       age      25%   38.000 sampling_weight
#>       age      50%   50.000 sampling_weight
#>       age      75%   63.000 sampling_weight
#>       age      Max   95.000 sampling_weight
#>       age        n 2500.000 sampling_weight
#>       age    eff_n 2468.768 sampling_weight
#> ────────────────────────────────────────────────────────────────────────────────

# Custom quantiles
survey_data %>% w_quantile(income, weights = sampling_weight, probs = c(0.1, 0.5, 0.9))
#> 
#> ── Weighted Quantile Statistics ────────────────────────────────────────────────
#>  Variable Quantile    Value         Weights
#>    income      10% 2100.000 sampling_weight
#>    income      50% 3500.000 sampling_weight
#>    income      90% 5700.000 sampling_weight
#>    income        n 2186.000 sampling_weight
#>    income    eff_n 2158.917 sampling_weight
#> ────────────────────────────────────────────────────────────────────────────────

# Multiple variables
survey_data %>% w_quantile(age, income, weights = sampling_weight)
#> 
#> ── Weighted Quantile Statistics ────────────────────────────────────────────────
#>  Variable Quantile    Value         Weights
#>       age      Min   18.000 sampling_weight
#>       age      25%   38.000 sampling_weight
#>       age      50%   50.000 sampling_weight
#>       age      75%   63.000 sampling_weight
#>       age      Max   95.000 sampling_weight
#>       age        n 2500.000 sampling_weight
#>       age    eff_n 2468.768 sampling_weight
#>    income      Min  800.000 sampling_weight
#>    income      25% 2700.000 sampling_weight
#>    income      50% 3500.000 sampling_weight
#>    income      75% 4600.000 sampling_weight
#>    income      Max 8000.000 sampling_weight
#>    income        n 2186.000 sampling_weight
#>    income    eff_n 2158.917 sampling_weight
#> ────────────────────────────────────────────────────────────────────────────────

# Grouped data  
survey_data %>% group_by(region) %>% w_quantile(age, weights = sampling_weight)
#> 
#> ── Weighted Quantile Statistics ────────────────────────────────────────────────
#> 
#> Group: 
#> ────────────────────────────────────────────────────────────────────────────────
#>  Variable Quantile   Value         Weights
#>       age      Min  18.000 sampling_weight
#>       age      25%  40.000 sampling_weight
#>       age      50%  53.000 sampling_weight
#>       age      75%  64.000 sampling_weight
#>       age      Max  95.000 sampling_weight
#>       age        n 485.000 sampling_weight
#>       age    eff_n 477.027 sampling_weight
#> ────────────────────────────────────────────────────────────────────────────────

# Unweighted (for comparison)
survey_data %>% w_quantile(age)
#> 
#> ── Quantile Statistics ─────────────────────────────────────────────────────────
#>  Variable Quantile Value
#>       age      Min    18
#>       age      25%    38
#>       age      50%    50
#>       age      75%    62
#>       age      Max    95
#>       age        n  2500
#>       age    eff_n  2500
#> ────────────────────────────────────────────────────────────────────────────────
```
