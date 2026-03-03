# Summary method for Spearman correlation results

Creates a summary object that produces detailed output when printed,
including rho matrices, p-value matrices, and sample size matrices.

## Usage

``` r
# S3 method for class 'spearman_rho'
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

  A `spearman_rho` result object.

- correlation_matrix:

  Logical. Show the rho coefficient matrix? (Default: TRUE)

- pvalue_matrix:

  Logical. Show the p-value matrix? (Default: TRUE)

- n_matrix:

  Logical. Show the sample size matrix? (Default: TRUE)

- digits:

  Number of decimal places for formatting (Default: 3).

- ...:

  Additional arguments (not used).

## Value

A `summary.spearman_rho` object.

## See also

[`spearman_rho`](https://YannickDiehl.github.io/mariposa/dev/reference/spearman_rho.md)
for the main analysis function.

## Examples

``` r
result <- spearman_rho(survey_data, trust_government, trust_media)
summary(result)
#> 
#> Spearman's Rank Correlation Analysis 
#> -------------------------------------
#> 
#> - Method: Spearman's rho (rank correlation)
#> - Variables: trust_government, trust_media
#> - Missing data handling: pairwise deletion
#> - Alternative hypothesis: two.sided
#> 
#> 
#>   Spearman's rho: rho = 0.008
#>   p-value: p = 0.723 
#>   N = 2227
#>   t-statistic: 0.355
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
summary(result, pvalue_matrix = FALSE)
#> 
#> Spearman's Rank Correlation Analysis 
#> -------------------------------------
#> 
#> - Method: Spearman's rho (rank correlation)
#> - Variables: trust_government, trust_media
#> - Missing data handling: pairwise deletion
#> - Alternative hypothesis: two.sided
#> 
#> 
#>   Spearman's rho: rho = 0.008
#>   N = 2227
#>   t-statistic: 0.355
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
