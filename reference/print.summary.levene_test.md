# Print summary of Levene test results (detailed output)

Displays the detailed output for Levene's test of homogeneity of
variance, with sections controlled by the boolean parameters passed to
[`summary.levene_test`](https://YannickDiehl.github.io/mariposa/reference/summary.levene_test.md).
Sections include the results tables, interpretation, and recommendation.

## Usage

``` r
# S3 method for class 'summary.levene_test'
print(x, ...)
```

## Arguments

- x:

  A `summary.levene_test` object created by
  [`summary.levene_test`](https://YannickDiehl.github.io/mariposa/reference/summary.levene_test.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`levene_test`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md)
for the main analysis,
[`summary.levene_test`](https://YannickDiehl.github.io/mariposa/reference/summary.levene_test.md)
for summary options.

## Examples

``` r
result <- levene_test(survey_data, life_satisfaction, group = education)
summary(result)                          # all sections
#> Levene's Test for Homogeneity of Variance 
#> ------------------------------------------
#> 
#> - Grouping variable: education
#> - Center: mean
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Levene's Test Results:
#> -------------------------------------------------------------------- 
#>           Variable F_statistic df1  df2 p_value sig        Conclusion
#>  life_satisfaction      31.634   3 2417       0 *** Variances unequal
#> -------------------------------------------------------------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - p > 0.05: Variances are homogeneous (equal variances assumed)
#> - p <= 0.05: Variances are heterogeneous (equal variances NOT assumed)
summary(result, interpretation = FALSE)  # hide interpretation
#> Levene's Test for Homogeneity of Variance 
#> ------------------------------------------
#> 
#> - Grouping variable: education
#> - Center: mean
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Levene's Test Results:
#> -------------------------------------------------------------------- 
#>           Variable F_statistic df1  df2 p_value sig        Conclusion
#>  life_satisfaction      31.634   3 2417       0 *** Variances unequal
#> -------------------------------------------------------------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
