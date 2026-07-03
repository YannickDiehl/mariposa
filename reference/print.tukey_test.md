# Print Tukey HSD test results (compact)

Compact print method for objects of class `"tukey_test"`. Shows one line
per variable (or factor, or group combination) with the number of
pairwise comparisons and how many are significant at the .05 level.

For the full comparison tables (mean differences, confidence intervals,
adjusted p-values), use
[`summary()`](https://rdrr.io/r/base/summary.html).

## Usage

``` r
# S3 method for class 'tukey_test'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An object of class `"tukey_test"` returned by
  [`tukey_test`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md).

- digits:

  Number of decimal places to display (default: 3)

- ...:

  Additional arguments passed to
  [`print`](https://rdrr.io/r/base/print.html). Currently unused.

## Value

Invisibly returns the input object `x`.

## Examples

``` r
result <- oneway_anova(survey_data, life_satisfaction,
                       group = education) |> tukey_test()
result              # compact overview
#> Tukey HSD Post-Hoc Test by education
#>   life_satisfaction: 6 comparisons, 5 significant (p < .05)
#> Use summary() for the full comparison table.
summary(result)     # full comparison tables
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
```
