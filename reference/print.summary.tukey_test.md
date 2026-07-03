# Print summary of Tukey HSD test results (detailed output)

Displays the detailed output for Tukey HSD post-hoc comparisons, with
sections controlled by the boolean parameters passed to
[`summary.tukey_test`](https://YannickDiehl.github.io/mariposa/reference/summary.tukey_test.md).
The display includes:

- Pairwise group comparisons with mean differences

- Confidence intervals for differences

- Tukey-adjusted p-values controlling family-wise error rate

- Significance indicators (\* p \< 0.05, \*\* p \< 0.01, \*\*\* p \<
  0.001)

For grouped analyses, results are displayed separately for each group
combination. For weighted analyses, effective sample sizes are used in
calculations.

## Usage

``` r
# S3 method for class 'summary.tukey_test'
print(x, ...)
```

## Arguments

- x:

  A `summary.tukey_test` object created by
  [`summary.tukey_test`](https://YannickDiehl.github.io/mariposa/reference/summary.tukey_test.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`tukey_test`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md)
for the main analysis,
[`summary.tukey_test`](https://YannickDiehl.github.io/mariposa/reference/summary.tukey_test.md)
for summary options.

## Examples

``` r
result <- oneway_anova(survey_data, life_satisfaction,
                       group = education) |> tukey_test()
summary(result)                     # all sections
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
summary(result, comparisons = FALSE)  # hide comparison tables
#> Tukey HSD Post-Hoc Test Results
#> -------------------------------
#> 
#> - Dependent variable: life_satisfaction
#> - Grouping variable: education
#> - Confidence level: 95.0%
#>   Family-wise error rate controlled using Tukey HSD
#> 
#> 
#> Interpretation:
#> - Positive differences: First group > Second group
#> - Negative differences: First group < Second group
#> - Confidence intervals not containing 0 indicate significant differences
#> - p-values are adjusted for multiple comparisons (family-wise error control)
```
