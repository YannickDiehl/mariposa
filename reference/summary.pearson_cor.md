# Summary method for Pearson correlation results

Creates a summary object that produces detailed output when printed,
including correlation matrices, p-value matrices, and sample size
matrices.

## Usage

``` r
# S3 method for class 'pearson_cor'
summary(
  object,
  correlation_matrix = TRUE,
  pvalue_matrix = TRUE,
  n_matrix = TRUE,
  digits = 3,
  ...
)
```

## Arguments

- object:

  A `pearson_cor` result object.

- correlation_matrix:

  Logical. Show the correlation coefficient matrix? (Default: TRUE)

- pvalue_matrix:

  Logical. Show the p-value matrix? (Default: TRUE)

- n_matrix:

  Logical. Show the sample size matrix? (Default: TRUE)

- digits:

  Number of decimal places for formatting (Default: 3).

- ...:

  Additional arguments (not used).

## Value

A `summary.pearson_cor` object.

## See also

[`pearson_cor`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md)
for the main analysis function.

## Examples

``` r
result <- pearson_cor(survey_data, trust_government, trust_media)
summary(result)
#> 
#> Pearson Correlation 
#> --------------------
#> 
#> - Missing data handling: pairwise deletion
#> - Confidence level: 95.0%
#> 
#> 
#>   Correlation: r = 0.009
#>   p-value: p = 0.674 
#>   N = 2227
#>   95% CI: [-0.033, 0.050]
#>   r-squared: 0.000
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
summary(result, pvalue_matrix = FALSE)
#> 
#> Pearson Correlation 
#> --------------------
#> 
#> - Missing data handling: pairwise deletion
#> - Confidence level: 95.0%
#> 
#> 
#>   Correlation: r = 0.009
#>   N = 2227
#>   95% CI: [-0.033, 0.050]
#>   r-squared: 0.000
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
