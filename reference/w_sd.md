# Calculate Population-Representative Standard Deviations

`w_sd()` calculates standard deviations that accurately represent your
population by using survey weights. The standard deviation tells you how
spread out your data is around the average – a larger SD means more
variation in responses, while a smaller SD means responses cluster
tightly around the mean.

Without weights, you describe spread in your sample only. With weights,
you estimate how spread out values are in the entire population.

## Usage

``` r
w_sd(data, ..., weights = NULL, na.rm = TRUE)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- ...:

  The numeric variables you want to analyze. You can list multiple
  variables or use helpers like `starts_with("trust")`

- weights:

  Survey weights to make results representative of your population.
  Without weights, you get the simple sample standard deviation.

- na.rm:

  Remove missing values before calculating? (Default: TRUE)

## Value

Population-weighted standard deviation(s) with sample size information,
including the weighted SD, effective sample size (effective N), and the
number of valid observations used.

## Details

### Understanding the Results

- **Weighted SD**: The population-representative standard deviation.
  Roughly 68% of your population falls within one SD of the weighted
  mean.

- **Effective N**: How many independent observations your weighted data
  represents. Always less than or equal to the actual sample size.

- **N**: The actual number of observations used in the calculation.

A large difference between weighted and unweighted SD suggests that the
variability in your sample does not accurately reflect the population.

### When to Use This

Use `w_sd()` when:

- You need to report how spread out a variable is in the population

- You want to compare variability across groups with proper weighting

- Your survey used complex sampling (oversampling, stratification)

- You need SPSS-compatible weighted standard deviations

### Formula

The weighted standard deviation uses the SPSS frequency weights formula:

\\s_w = \sqrt{\frac{\sum w_i (x_i - \bar{x}\_w)^2}{V_1 - 1}}\\

where \\V_1 = \sum w_i\\ is the sum of all weights and \\\bar{x}\_w =
\sum w_i x_i / V_1\\ is the weighted mean.

The effective sample size is: \\n\_{eff} = (\sum w_i)^2 / \sum w_i^2\\

## References

IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.

## See also

[`sd`](https://rdrr.io/r/stats/sd.html) for the base R standard
deviation function.

[`w_var`](https://YannickDiehl.github.io/mariposa/reference/w_var.md)
for weighted variance (the square of weighted SD).

[`w_mean`](https://YannickDiehl.github.io/mariposa/reference/w_mean.md)
for weighted means.

[`describe`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
for comprehensive descriptive statistics including SD.

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
#> Weighted Standard Deviation Statistics
#> --------------------------------------
#> 
#> --- age ---
#>  Variable weighted_sd Effective_N
#>       age      17.084      2468.8
#> 

# Multiple variables
survey_data %>% w_sd(age, income, life_satisfaction, weights = sampling_weight)
#> 
#> Weighted Standard Deviation Statistics
#> --------------------------------------
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
#> Weighted Standard Deviation Statistics
#> --------------------------------------
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
#> Standard Deviation Statistics
#> -----------------------------
#> 
#> --- age ---
#>  Variable     sd    N
#>       age 16.976 2500
#> 
```
