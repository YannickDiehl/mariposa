# Print summary of Kendall's tau correlation results (detailed output)

Displays the detailed SPSS-style output for a Kendall's tau correlation,
with sections controlled by the boolean parameters passed to
[`summary.kendall_tau`](https://YannickDiehl.github.io/mariposa/reference/summary.kendall_tau.md).
Sections include the correlation matrix, p-value matrix, and sample size
matrix.

## Usage

``` r
# S3 method for class 'summary.kendall_tau'
print(x, ...)
```

## Arguments

- x:

  A `summary.kendall_tau` object created by
  [`summary.kendall_tau`](https://YannickDiehl.github.io/mariposa/reference/summary.kendall_tau.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`kendall_tau`](https://YannickDiehl.github.io/mariposa/reference/kendall_tau.md)
for the main analysis,
[`summary.kendall_tau`](https://YannickDiehl.github.io/mariposa/reference/summary.kendall_tau.md)
for summary options.

## Examples

``` r
result <- kendall_tau(survey_data[1:300, ], age, life_satisfaction)
summary(result)                             # all matrices
#> 
#> Kendall's Tau-b Correlation 
#> ----------------------------
#> 
#> - Missing data handling: pairwise deletion
#> - Alternative hypothesis: two.sided
#> 
#> 
#>   Kendall's tau-b: tau-b = -0.011
#>   p-value: p = 0.806 
#>   N = 287
#>   z-score: -0.245
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
summary(result, pvalue_matrix = FALSE)      # hide p-values
#> 
#> Kendall's Tau-b Correlation 
#> ----------------------------
#> 
#> - Missing data handling: pairwise deletion
#> - Alternative hypothesis: two.sided
#> 
#> 
#>   Kendall's tau-b: tau-b = -0.011
#>   N = 287
#>   z-score: -0.245
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
