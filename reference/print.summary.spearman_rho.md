# Print summary of Spearman correlation results (detailed output)

Displays the detailed SPSS-style output for a Spearman rank correlation,
with sections controlled by the boolean parameters passed to
[`summary.spearman_rho`](https://YannickDiehl.github.io/mariposa/reference/summary.spearman_rho.md).
Sections include the correlation matrix, p-value matrix, and sample size
matrix.

## Usage

``` r
# S3 method for class 'summary.spearman_rho'
print(x, ...)
```

## Arguments

- x:

  A `summary.spearman_rho` object created by
  [`summary.spearman_rho`](https://YannickDiehl.github.io/mariposa/reference/summary.spearman_rho.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`spearman_rho`](https://YannickDiehl.github.io/mariposa/reference/spearman_rho.md)
for the main analysis,
[`summary.spearman_rho`](https://YannickDiehl.github.io/mariposa/reference/summary.spearman_rho.md)
for summary options.

## Examples

``` r
result <- spearman_rho(survey_data, age, life_satisfaction)
summary(result)                             # all matrices
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
summary(result, pvalue_matrix = FALSE)      # hide p-values
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
#>   N = 2421
#>   t-statistic: -1.180
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
