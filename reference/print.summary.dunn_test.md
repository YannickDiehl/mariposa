# Print summary of Dunn post-hoc test results (detailed output)

Displays the detailed output for Dunn pairwise comparisons, with
sections controlled by the boolean parameters passed to
[`summary.dunn_test`](https://YannickDiehl.github.io/mariposa/reference/summary.dunn_test.md).
The display includes:

- Pairwise group comparisons with Z-statistics

- Adjusted p-values controlling for multiple comparisons

- Significance indicators (\* p \< 0.05, \*\* p \< 0.01, \*\*\* p \<
  0.001)

For grouped analyses, results are displayed separately for each group
combination.

## Usage

``` r
# S3 method for class 'summary.dunn_test'
print(x, ...)
```

## Arguments

- x:

  A `summary.dunn_test` object created by
  [`summary.dunn_test`](https://YannickDiehl.github.io/mariposa/reference/summary.dunn_test.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`dunn_test`](https://YannickDiehl.github.io/mariposa/reference/dunn_test.md)
for the main analysis,
[`summary.dunn_test`](https://YannickDiehl.github.io/mariposa/reference/summary.dunn_test.md)
for summary options.

## Examples

``` r
result <- kruskal_wallis(survey_data, life_satisfaction,
                         group = education) |> dunn_test()
summary(result)                       # all sections
#> Dunn Post-Hoc Test (Bonferroni) Results
#> ---------------------------------------
#> 
#> - Dependent variable: life_satisfaction
#> - Grouping variable: education
#> - P-value adjustment: Bonferroni
#> 
#> ---------------------------------------------------------------------------- 
#>                 Group 1                Group 2       Z p (unadj) p (adj) Sig 
#>         Basic Secondary Intermediate Secondary  -7.658     <.001   <.001 *** 
#>         Basic Secondary     Academic Secondary  -9.792     <.001   <.001 *** 
#>         Basic Secondary             University -11.545     <.001   <.001 *** 
#>  Intermediate Secondary     Academic Secondary  -2.042     0.041   0.247     
#>  Intermediate Secondary             University  -4.696     <.001   <.001 *** 
#>      Academic Secondary             University  -2.886     0.004   0.023   * 
#> ---------------------------------------------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - Positive Z: First group has higher mean rank
#> - Negative Z: First group has lower mean rank
#> - p-values are adjusted for multiple comparisons
summary(result, comparisons = FALSE)  # hide comparison tables
#> Dunn Post-Hoc Test (Bonferroni) Results
#> ---------------------------------------
#> 
#> - Dependent variable: life_satisfaction
#> - Grouping variable: education
#> - P-value adjustment: Bonferroni
#> 
#> 
#> Interpretation:
#> - Positive Z: First group has higher mean rank
#> - Negative Z: First group has lower mean rank
#> - p-values are adjusted for multiple comparisons
```
