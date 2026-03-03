# Print summary of Pearson correlation results (detailed output)

Displays the detailed SPSS-style output for a Pearson correlation, with
sections controlled by the boolean parameters passed to
[`summary.pearson_cor`](https://YannickDiehl.github.io/mariposa/dev/reference/summary.pearson_cor.md).
Sections include the correlation matrix, p-value matrix, and sample size
matrix.

## Usage

``` r
# S3 method for class 'summary.pearson_cor'
print(x, ...)
```

## Arguments

- x:

  A `summary.pearson_cor` object created by
  [`summary.pearson_cor`](https://YannickDiehl.github.io/mariposa/dev/reference/summary.pearson_cor.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`pearson_cor`](https://YannickDiehl.github.io/mariposa/dev/reference/pearson_cor.md)
for the main analysis,
[`summary.pearson_cor`](https://YannickDiehl.github.io/mariposa/dev/reference/summary.pearson_cor.md)
for summary options.

## Examples

``` r
result <- pearson_cor(survey_data, age, life_satisfaction)
summary(result)                             # all matrices
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
summary(result, pvalue_matrix = FALSE)      # hide p-values
#> 
#> Pearson Correlation 
#> --------------------
#> 
#> - Missing data handling: pairwise deletion
#> - Confidence level: 95.0%
#> 
#> 
#>   Correlation: r = -0.029
#>   N = 2421
#>   95% CI: [-0.068, 0.011]
#>   r-squared: 0.001
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
