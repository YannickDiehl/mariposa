# Print Spearman correlation results (compact)

Compact print method for objects of class `"spearman_rho"`. Shows rank
correlation coefficient, p-value, and sample size per pair.

For the full detailed output including matrices, use
[`summary()`](https://rdrr.io/r/base/summary.html).

## Usage

``` r
# S3 method for class 'spearman_rho'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An object of class `"spearman_rho"` returned by
  [`spearman_rho`](https://YannickDiehl.github.io/mariposa/dev/reference/spearman_rho.md).

- digits:

  Number of decimal places to display. Default is `3`.

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## Examples

``` r
result <- spearman_rho(survey_data, age, life_satisfaction)
result              # compact one-line overview
#> Spearman Correlation: age x life_satisfaction
#>   rho = -0.024, p = 0.238 , N = 2421
summary(result)     # full correlation matrices
#> 
#> Spearman's Rank Correlation Analysis 
#> -------------------------------------
#> 
#> - Method: Spearman's rho (rank correlation)
#> - Variables: age, life_satisfaction
#> - Missing data handling: pairwise deletion
#> - Alternative hypothesis: two.sided
#> 
#> 
#>   Spearman's rho: rho = -0.024
#>   p-value: p = 0.238 
#>   N = 2421
#>   t-statistic: -1.180
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
