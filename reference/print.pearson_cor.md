# Print Pearson correlation results (compact)

Compact print method for objects of class `"pearson_cor"`. Shows
correlation coefficient, p-value, and sample size per pair.

For the full detailed output including matrices, use
[`summary()`](https://rdrr.io/r/base/summary.html).

## Usage

``` r
# S3 method for class 'pearson_cor'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An object of class `"pearson_cor"` returned by
  [`pearson_cor`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md).

- digits:

  Number of decimal places to display. Default is `3`.

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## Examples

``` r
result <- pearson_cor(survey_data, age, life_satisfaction)
result              # compact one-line overview
#> Pearson Correlation: age x life_satisfaction
#>   r = -0.029, p = 0.158 , N = 2421
summary(result)     # full correlation matrices
#> 
#> Pearson Correlation 
#> --------------------
#> 
#> - Missing data handling: pairwise deletion
#> - Confidence level: 95.0%
#> 
#> 
#>   Correlation: r = -0.029
#>   p-value: p = 0.158 
#>   N = 2421
#>   95% CI: [-0.068, 0.011]
#>   r-squared: 0.001
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
