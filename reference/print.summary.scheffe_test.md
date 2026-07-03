# Print summary of Scheffe test results (detailed output)

Displays the detailed output for Scheffe post-hoc comparisons, with
sections controlled by the boolean parameters passed to
[`summary.scheffe_test`](https://YannickDiehl.github.io/mariposa/reference/summary.scheffe_test.md).
The display includes:

- Pairwise group comparisons with mean differences

- Confidence intervals for differences (widest among all post-hoc tests)

- Scheffe-adjusted p-values controlling family-wise error rate

- Significance indicators (\* p \< 0.05, \*\* p \< 0.01, \*\*\* p \<
  0.001)

For grouped analyses, results are displayed separately for each group
combination. For weighted analyses, effective sample sizes are used in
calculations.

## Usage

``` r
# S3 method for class 'summary.scheffe_test'
print(x, ...)
```

## Arguments

- x:

  A `summary.scheffe_test` object created by
  [`summary.scheffe_test`](https://YannickDiehl.github.io/mariposa/reference/summary.scheffe_test.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`scheffe_test`](https://YannickDiehl.github.io/mariposa/reference/scheffe_test.md)
for the main analysis,
[`summary.scheffe_test`](https://YannickDiehl.github.io/mariposa/reference/summary.scheffe_test.md)
for summary options.

## Examples

``` r
result <- oneway_anova(survey_data, life_satisfaction,
                       group = education) |> scheffe_test()
summary(result)                       # all sections
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
summary(result, comparisons = FALSE)  # hide comparison tables
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
#> Interpretation:
#> - Positive differences: First group > Second group
#> - Negative differences: First group < Second group
#> - Confidence intervals not containing 0 indicate significant differences
#> - p-values are adjusted for all possible contrasts (most conservative)
#> - Scheffe test has wider CIs than Tukey HSD
```
