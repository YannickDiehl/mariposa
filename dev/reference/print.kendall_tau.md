# Print Kendall's tau results (compact)

Compact print method for objects of class `"kendall_tau"`. Shows tau
coefficient, p-value, and sample size per pair.

For the full detailed output including matrices, use
[`summary()`](https://rdrr.io/r/base/summary.html).

## Usage

``` r
# S3 method for class 'kendall_tau'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An object of class `"kendall_tau"` returned by
  [`kendall_tau`](https://YannickDiehl.github.io/mariposa/dev/reference/kendall_tau.md).

- digits:

  Number of decimal places to display. Default is `3`.

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## Examples

``` r
result <- kendall_tau(survey_data, age, life_satisfaction)
result              # compact one-line overview
#> Kendall's Tau: age x life_satisfaction
#>   tau = -0.018, p = 0.232 , N = 2421
summary(result)     # full correlation matrices
#> 
#> Kendall's Tau-b Correlation 
#> ----------------------------
#> 
#> - Missing data handling: pairwise deletion
#> - Alternative hypothesis: two.sided
#> 
#> 
#>   Kendall's tau-b: tau-b = -0.018
#>   p-value: p = 0.232 
#>   N = 2421
#>   z-score: -1.195
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
