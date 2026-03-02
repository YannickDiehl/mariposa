# Find the Most Common Value in Your Population

`w_modus()` finds the mode (most frequently occurring value) of your
data using survey weights for population-representative results. The
mode tells you which category or value is the most common in your
population. This is especially useful for categorical variables (e.g.,
the most common education level, the most frequent employment status).

Unlike mean and median, the mode works with both numeric and categorical
data.

## Usage

``` r
w_modus(data, ..., weights = NULL, na.rm = TRUE)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- ...:

  The variables you want to analyze. Works best with categorical or
  discrete numeric variables. You can list multiple variables or use
  helpers like `starts_with("trust")`

- weights:

  Survey weights to make results representative of your population.
  Without weights, the mode is simply the most frequent value in your
  sample.

- na.rm:

  Remove missing values before calculating? (Default: TRUE)

## Value

Population-weighted mode(s) with sample size information, including the
most common value (by weighted frequency), effective sample size
(effective N), and the number of valid observations used.

## Details

### Understanding the Results

- **Weighted Mode**: The value that occurs most frequently in the
  weighted population. For weighted data, the mode is the value whose
  observations have the largest total weight.

- **Effective N**: How many independent observations your weighted data
  represents.

- **N**: The actual number of observations used.

If multiple values share the highest weighted frequency (ties), the
first value encountered is returned.

### When to Use This

Use `w_modus()` when:

- You want to find the most common response (e.g., most popular
  education level)

- You are working with categorical or ordinal data

- You want the "typical" value for discrete data where mean is not
  meaningful

- You need SPSS-compatible weighted mode values

### Formula

The weighted mode is the value \\x_k\\ that maximizes the total weight:

\\\text{Mode}\_w = \arg\max\_{x_k} \sum\_{i: x_i = x_k} w_i\\

In other words, sum the weights for each unique value and pick the value
with the largest total weight.

## References

IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.

## See also

[`w_median`](https://YannickDiehl.github.io/mariposa/reference/w_median.md)
for the weighted middle value.

[`w_mean`](https://YannickDiehl.github.io/mariposa/reference/w_mean.md)
for weighted means.

[`frequency`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
for complete frequency tables of categorical variables.

[`describe`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
for comprehensive descriptive statistics including the mode.

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
