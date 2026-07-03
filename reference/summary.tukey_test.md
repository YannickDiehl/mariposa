# Summary method for Tukey HSD test results

Creates a summary object that produces detailed output when printed,
including the pairwise comparison tables with mean differences,
confidence intervals, Tukey-adjusted p-values, and interpretation.

## Usage

``` r
# S3 method for class 'tukey_test'
summary(
  object,
  parameters = TRUE,
  comparisons = TRUE,
  interpretation = TRUE,
  digits = 3,
  ...
)
```

## Arguments

- object:

  A `tukey_test` result object.

- parameters:

  Logical. Show test parameters (confidence level, method notes)?
  (Default: TRUE)

- comparisons:

  Logical. Show the pairwise comparison tables? (Default: TRUE)

- interpretation:

  Logical. Show the interpretation section? (Default: TRUE)

- digits:

  Number of decimal places for formatting (Default: 3).

- ...:

  Additional arguments (not used).

## Value

A `summary.tukey_test` object.

## See also

[`tukey_test`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md)
for the main analysis function.

## Examples

``` r
result <- oneway_anova(survey_data, life_satisfaction,
                       group = education) |> tukey_test()
summary(result)
#> Tukey HSD Post-Hoc Test Results
#> -------------------------------
#> 
#> - Dependent variable: life_satisfaction
#> - Grouping variable: education
#> - Confidence level: 95.0%
#>   Family-wise error rate controlled using Tukey HSD
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Tukey Results:
#>   --------------------------------------------------------------------------------------- 
#>   Comparison                                 Difference  Lower CI  Upper CI  p-value  Sig 
#>   --------------------------------------------------------------------------------------- 
#>   Intermediate Secondary-Basic Secondary          0.497     0.344     0.649    <.001  *** 
#>   Academic Secondary-Basic Secondary              0.649     0.496     0.802    <.001  *** 
#>   University-Basic Secondary                      0.843     0.666     1.019    <.001  *** 
#>   Academic Secondary-Intermediate Secondary       0.153    -0.010     0.316     .075      
#>   University-Intermediate Secondary               0.346     0.161     0.531    <.001  *** 
#>   University-Academic Secondary                   0.193     0.008     0.379     .037    * 
#>   --------------------------------------------------------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - Positive differences: First group > Second group
#> - Negative differences: First group < Second group
#> - Confidence intervals not containing 0 indicate significant differences
#> - p-values are adjusted for multiple comparisons (family-wise error control)
summary(result, interpretation = FALSE)
#> Tukey HSD Post-Hoc Test Results
#> -------------------------------
#> 
#> - Dependent variable: life_satisfaction
#> - Grouping variable: education
#> - Confidence level: 95.0%
#>   Family-wise error rate controlled using Tukey HSD
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Tukey Results:
#>   --------------------------------------------------------------------------------------- 
#>   Comparison                                 Difference  Lower CI  Upper CI  p-value  Sig 
#>   --------------------------------------------------------------------------------------- 
#>   Intermediate Secondary-Basic Secondary          0.497     0.344     0.649    <.001  *** 
#>   Academic Secondary-Basic Secondary              0.649     0.496     0.802    <.001  *** 
#>   University-Basic Secondary                      0.843     0.666     1.019    <.001  *** 
#>   Academic Secondary-Intermediate Secondary       0.153    -0.010     0.316     .075      
#>   University-Intermediate Secondary               0.346     0.161     0.531    <.001  *** 
#>   University-Academic Secondary                   0.193     0.008     0.379     .037    * 
#>   --------------------------------------------------------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
