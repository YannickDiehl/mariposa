# Summary method for Dunn post-hoc test results

Creates a summary object that produces detailed output when printed,
including the pairwise comparison tables with Z-statistics, unadjusted
and adjusted p-values, and interpretation.

## Usage

``` r
# S3 method for class 'dunn_test'
summary(object, comparisons = TRUE, interpretation = TRUE, digits = 3, ...)
```

## Arguments

- object:

  A `dunn_test` result object.

- comparisons:

  Logical. Show the pairwise comparison tables? (Default: TRUE)

- interpretation:

  Logical. Show the interpretation section? (Default: TRUE)

- digits:

  Number of decimal places for formatting (Default: 3).

- ...:

  Additional arguments (not used).

## Value

A `summary.dunn_test` object.

## See also

[`dunn_test`](https://YannickDiehl.github.io/mariposa/reference/dunn_test.md)
for the main analysis function.

## Examples

``` r
result <- kruskal_wallis(survey_data, life_satisfaction,
                         group = education) |> dunn_test()
summary(result)
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
summary(result, interpretation = FALSE)
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
```
