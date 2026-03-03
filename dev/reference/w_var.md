# Calculate Population-Representative Variance

`w_var()` calculates variance that accurately represents your population
by using survey weights. Variance measures how far values spread out
from the average – it is the square of the standard deviation. A larger
variance means more dispersion in your population's responses.

Without weights, you describe the spread in your sample only. With
weights, you estimate how spread out values are in the entire
population.

## Usage

``` r
w_var(data, ..., weights = NULL, na.rm = TRUE)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- ...:

  The numeric variables you want to analyze. You can list multiple
  variables or use helpers like `starts_with("trust")`

- weights:

  Survey weights to make results representative of your population.
  Without weights, you get the simple sample variance.

- na.rm:

  Remove missing values before calculating? (Default: TRUE)

## Value

Population-weighted variance(s) with sample size information, including
the weighted variance, effective sample size (effective N), and the
number of valid observations used.

## Details

### Understanding the Results

- **Weighted Variance**: The population-representative variance. Because
  variance is in squared units (e.g., years-squared for age), it is
  often easier to interpret the standard deviation
  ([`w_sd`](https://YannickDiehl.github.io/mariposa/dev/reference/w_sd.md))
  instead.

- **Effective N**: How many independent observations your weighted data
  represents. Always less than or equal to the actual sample size.

- **N**: The actual number of observations used in the calculation.

### When to Use This

Use `w_var()` when:

- You need variance for further calculations (e.g., pooled variance,
  F-tests)

- You want to compare variability across groups with proper weighting

- Your analysis formulas require variance rather than SD

- You need SPSS-compatible weighted variance values

For reporting purposes,
[`w_sd`](https://YannickDiehl.github.io/mariposa/dev/reference/w_sd.md)
is usually preferred because the standard deviation is in the same units
as the original variable.

### Formula

The weighted variance uses the SPSS frequency weights formula:

\\s^2_w = \frac{\sum w_i (x_i - \bar{x}\_w)^2}{V_1 - 1}\\

where \\V_1 = \sum w_i\\ is the sum of all weights and \\\bar{x}\_w =
\sum w_i x_i / V_1\\ is the weighted mean.

Note: \\s^2_w = (s_w)^2\\, i.e., the weighted variance is the square of
the weighted standard deviation.

## References

IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.

## See also

[`var`](https://rdrr.io/r/stats/cor.html) for the base R variance
function.

[`w_sd`](https://YannickDiehl.github.io/mariposa/dev/reference/w_sd.md)
for weighted standard deviation (the square root of variance).

[`w_mean`](https://YannickDiehl.github.io/mariposa/dev/reference/w_mean.md)
for weighted means.

[`describe`](https://YannickDiehl.github.io/mariposa/dev/reference/describe.md)
for comprehensive descriptive statistics including variance.

Other weighted_statistics:
[`w_iqr()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_iqr.md),
[`w_kurtosis()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_kurtosis.md),
[`w_mean()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_mean.md),
[`w_median()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_median.md),
[`w_modus()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_modus.md),
[`w_quantile()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_quantile.md),
[`w_range()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_range.md),
[`w_sd()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_sd.md),
[`w_se()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_se.md),
[`w_skew()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_skew.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic weighted variance
survey_data %>% w_var(age, weights = sampling_weight)
#> 
#> Weighted Variance Statistics
#> ----------------------------
#> 
#> --- age ---
#>  Variable weighted_var Effective_N
#>       age      291.857      2468.8
#> 

# Multiple variables
survey_data %>% w_var(age, income, weights = sampling_weight)
#> 
#> Weighted Variance Statistics
#> ----------------------------
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
#> Weighted Variance Statistics
#> ----------------------------
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
#> Variance Statistics
#> -------------------
#> 
#> --- age ---
#>  Variable     var    N
#>       age 288.185 2500
#> 
```
