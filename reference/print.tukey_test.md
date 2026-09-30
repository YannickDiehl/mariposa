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
#>   ----------------------------------------------------------------------------------------------------------------
#>   (I) - (J)                                    Mean Difference (I-J)  Std. Error  p-value  Lower CI  Upper CI     
#>   ----------------------------------------------------------------------------------------------------------------
#>   Basic Secondary - Intermediate Secondary                    -0.497       0.059    <.001    -0.649    -0.344  ***
#>   Basic Secondary - Academic Secondary                        -0.649       0.060    <.001    -0.802    -0.496  ***
#>   Basic Secondary - University                                -0.843       0.069    <.001    -1.019    -0.666  ***
#>   Intermediate Secondary - Academic Secondary                 -0.153       0.063     .075    -0.316     0.010     
#>   Intermediate Secondary - University                         -0.346       0.072    <.001    -0.531    -0.161  ***
#>   Academic Secondary - University                             -0.193       0.072     .037    -0.379    -0.008    *
#>   ----------------------------------------------------------------------------------------------------------------
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - (I) - (J): mean of the first group (I) minus mean of the second (J)
#> - Positive differences: First group > Second group
#> - Negative differences: First group < Second group
#> - Confidence intervals not containing 0 indicate significant differences
#> - p-values are adjusted for multiple comparisons (family-wise error control)
```
