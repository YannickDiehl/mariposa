# Print Scheffe test results (compact)

Compact print method for objects of class `"scheffe_test"`. Shows one
line per variable (or factor, or group combination) with the number of
pairwise comparisons and how many are significant at the .05 level.

For the full comparison tables (mean differences, confidence intervals,
adjusted p-values), use
[`summary()`](https://rdrr.io/r/base/summary.html).

## Usage

``` r
# S3 method for class 'scheffe_test'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An object of class `"scheffe_test"` returned by
  [`scheffe_test`](https://YannickDiehl.github.io/mariposa/reference/scheffe_test.md).

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
                       group = education) |> scheffe_test()
result              # compact overview
#> Scheffe Post-Hoc Test by education
#>   life_satisfaction: 6 comparisons, 4 significant (p < .05)
#> Use summary() for the full comparison table.
summary(result)     # full comparison tables
#> Scheffe Post-Hoc Test Results
#> -----------------------------
#> 
#> - Dependent variable: life_satisfaction
#> - Grouping variable: education
#> - Confidence level: 95.0%
#>   Family-wise error rate controlled using Scheffe's method
#>   Note: Most conservative post-hoc test (widest confidence intervals)
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Scheffe Results:
#>   ----------------------------------------------------------------------------------------- 
#>   Comparison                                   Difference  Lower CI  Upper CI  p-value  Sig 
#>   ----------------------------------------------------------------------------------------- 
#>   Basic Secondary - Intermediate Secondary         -0.497    -0.662    -0.331    <.001  *** 
#>   Basic Secondary - Academic Secondary             -0.649    -0.816    -0.483    <.001  *** 
#>   Basic Secondary - University                     -0.843    -1.034    -0.651    <.001  *** 
#>   Intermediate Secondary - Academic Secondary      -0.153    -0.330     0.024     .121      
#>   Intermediate Secondary - University              -0.346    -0.547    -0.145    <.001  *** 
#>   Academic Secondary - University                  -0.193    -0.395     0.009     .067      
#>   ----------------------------------------------------------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - Positive differences: First group > Second group
#> - Negative differences: First group < Second group
#> - Confidence intervals not containing 0 indicate significant differences
#> - p-values are adjusted for all possible contrasts (most conservative)
#> - Scheffe test has wider CIs than Tukey HSD
```
