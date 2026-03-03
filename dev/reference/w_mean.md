# Calculate Population-Representative Averages

`w_mean()` calculates averages that accurately represent your population
by using survey weights. This ensures that groups who were over- or
under-sampled contribute appropriately to the final average.

## Usage

``` r
w_mean(data, ..., weights = NULL, na.rm = TRUE)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- ...:

  The numeric variables you want to average. You can list multiple
  variables or use helpers like `starts_with("income")`

- weights:

  Survey weights to make the average representative of your population.
  Without weights, you get the simple sample average.

- na.rm:

  Remove missing values before calculating? (Default: TRUE)

## Value

Population-weighted average(s) with sample size information

## Details

### When to Use This

Use `w_mean()` when your survey uses sampling weights and you need
population-representative averages. Weights correct for:

- Oversampling of certain groups (weights \< 1)

- Undersampling of other groups (weights \> 1)

- Non-response patterns

- Complex survey designs

### Understanding the Results

- **Weighted Mean**: The population-representative average

- **Effective N**: How many independent observations your weighted data
  represents

- **N**: Actual number of observations used

### Formula

\\\bar{x}\_w = \frac{\sum w_i \cdot x_i}{\sum w_i}\\

The effective sample size is: \\n\_{eff} = (\sum w_i)^2 / \sum w_i^2\\

## References

IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.

## See also

[`weighted.mean`](https://rdrr.io/r/stats/weighted.mean.html) for the
base R weighted mean function.

[`w_sd`](https://YannickDiehl.github.io/mariposa/dev/reference/w_sd.md)
for weighted standard deviation.

[`w_median`](https://YannickDiehl.github.io/mariposa/dev/reference/w_median.md)
for weighted median.

[`describe`](https://YannickDiehl.github.io/mariposa/dev/reference/describe.md)
for comprehensive descriptive statistics.

Other weighted_statistics:
[`w_iqr()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_iqr.md),
[`w_kurtosis()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_kurtosis.md),
[`w_median()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_median.md),
[`w_modus()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_modus.md),
[`w_quantile()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_quantile.md),
[`w_range()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_range.md),
[`w_sd()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_sd.md),
[`w_se()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_se.md),
[`w_skew()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_skew.md),
[`w_var()`](https://YannickDiehl.github.io/mariposa/dev/reference/w_var.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic weighted usage
survey_data %>% w_mean(age, weights = sampling_weight)
#> 
#> Weighted Mean Statistics
#> ------------------------
#> 
#> --- age ---
#>  Variable weighted_mean Effective_N
#>       age        50.514      2468.8
#> 

# Multiple variables
survey_data %>% w_mean(age, income, life_satisfaction, weights = sampling_weight)
#> 
#> Weighted Mean Statistics
#> ------------------------
#> 
#> --- age ---
#>  Variable weighted_mean Effective_N
#>       age        50.514      2468.8
#> 
#> --- income ---
#>  Variable weighted_mean Effective_N
#>    income      3743.099      2158.9
#> 
#> --- life_satisfaction ---
#>           Variable weighted_mean Effective_N
#>  life_satisfaction         3.625      2390.9
#> 

# Grouped data
survey_data %>% group_by(region) %>% w_mean(age, weights = sampling_weight)
#> 
#> Weighted Mean Statistics
#> ------------------------
#> 
#> Group: region = East
#> Warning: Unknown or uninitialised column: `Variable`.
#> 
#> Group: region = West
#> Warning: Unknown or uninitialised column: `Variable`.
#> 

# In summarise context
survey_data %>% summarise(mean_age = w_mean(age, weights = sampling_weight))
#> # A tibble: 1 × 1
#>   mean_age
#>      <dbl>
#> 1     50.5

# Unweighted (for comparison)
survey_data %>% w_mean(age)
#> 
#> Mean Statistics
#> ---------------
#> 
#> --- age ---
#>  Variable  mean    N
#>       age 50.55 2500
#> 
```
