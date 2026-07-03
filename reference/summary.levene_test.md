# Summary method for Levene test results

Creates a summary object that produces detailed output when printed,
including the per-variable results tables, the interpretation of the
homogeneity-of-variance test, and the follow-up recommendation.

## Usage

``` r
# S3 method for class 'levene_test'
summary(
  object,
  results = TRUE,
  interpretation = TRUE,
  recommendation = TRUE,
  digits = 3,
  ...
)
```

## Arguments

- object:

  A `levene_test` result object.

- results:

  Logical. Show the test results tables? (Default: TRUE)

- interpretation:

  Logical. Show the interpretation section? (Default: TRUE)

- recommendation:

  Logical. Show the recommendation section (when available)? (Default:
  TRUE)

- digits:

  Number of decimal places for formatting (Default: 3).

- ...:

  Additional arguments (not used).

## Value

A `summary.levene_test` object.

## See also

[`levene_test`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md)
for the main analysis function.

## Examples

``` r
result <- levene_test(survey_data, life_satisfaction, group = education)
summary(result)
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
summary(result, interpretation = FALSE)
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
