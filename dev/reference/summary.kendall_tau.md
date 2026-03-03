# Summary method for Kendall's tau correlation results

Creates a summary object that produces detailed output when printed,
including tau matrices, p-value matrices, and sample size matrices.

## Usage

``` r
# S3 method for class 'kendall_tau'
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

  A `kendall_tau` result object.

- correlation_matrix:

  Logical. Show the tau coefficient matrix? (Default: TRUE)

- pvalue_matrix:

  Logical. Show the p-value matrix? (Default: TRUE)

- n_matrix:

  Logical. Show the sample size matrix? (Default: TRUE)

- digits:

  Number of decimal places for formatting (Default: 3).

- ...:

  Additional arguments (not used).

## Value

A `summary.kendall_tau` object.

## See also

[`kendall_tau`](https://YannickDiehl.github.io/mariposa/dev/reference/kendall_tau.md)
for the main analysis function.

## Examples

``` r
result <- kendall_tau(survey_data, trust_government, trust_media)
summary(result)
#> 
#> Kendall's Tau-b Correlation 
#> ----------------------------
#> 
#> - Missing data handling: pairwise deletion
#> - Alternative hypothesis: two.sided
#> 
#> 
#>   Kendall's tau-b: tau-b = 0.006
#>   p-value: p = 0.721 
#>   N = 2227
#>   z-score: 0.357
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
summary(result, pvalue_matrix = FALSE)
#> 
#> Kendall's Tau-b Correlation 
#> ----------------------------
#> 
#> - Missing data handling: pairwise deletion
#> - Alternative hypothesis: two.sided
#> 
#> 
#>   Kendall's tau-b: tau-b = 0.006
#>   N = 2227
#>   z-score: 0.357
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
