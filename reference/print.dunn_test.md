# Print Dunn post-hoc test results (compact)

Compact print method for objects of class `"dunn_test"`. Shows one line
per variable (or group combination) with the number of pairwise
comparisons and how many are significant at the .05 level.

For the full comparison tables (Z-statistics, adjusted p-values), use
[`summary()`](https://rdrr.io/r/base/summary.html).

## Usage

``` r
# S3 method for class 'dunn_test'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An object of class `"dunn_test"` returned by
  [`dunn_test`](https://YannickDiehl.github.io/mariposa/reference/dunn_test.md).

- digits:

  Number of decimal places to display (default: 3)

- ...:

  Additional arguments passed to
  [`print`](https://rdrr.io/r/base/print.html). Currently unused.

## Value

Invisibly returns the input object `x`.

## Examples

``` r
result <- kruskal_wallis(survey_data, life_satisfaction,
                         group = education) |> dunn_test()
result              # compact overview
#> Dunn Post-Hoc Test (Bonferroni) by education
#>   life_satisfaction: 6 comparisons, 5 significant (p < .05)
#> Use summary() for the full comparison table.
summary(result)     # full comparison tables
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
```
