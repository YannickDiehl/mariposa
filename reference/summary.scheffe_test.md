# Summary method for Scheffe test results

Creates a summary object that produces detailed output when printed,
including the pairwise comparison tables with mean differences,
confidence intervals, Scheffe-adjusted p-values, and interpretation.

## Usage

``` r
# S3 method for class 'scheffe_test'
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

  A `scheffe_test` result object.

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

A `summary.scheffe_test` object.

## See also

[`scheffe_test`](https://YannickDiehl.github.io/mariposa/reference/scheffe_test.md)
for the main analysis function.

## Examples

``` r
result <- oneway_anova(survey_data, life_satisfaction,
                       group = education) |> scheffe_test()
summary(result)
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
summary(result, interpretation = FALSE)
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
```
