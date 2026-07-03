# Print pairwise Wilcoxon post-hoc test results (compact)

Compact print method for objects of class `"pairwise_wilcoxon"`. Shows
one line per group combination with the number of pairwise comparisons
and how many are significant at the .05 level.

For the full comparison tables (Z-statistics, adjusted p-values), use
[`summary()`](https://rdrr.io/r/base/summary.html).

## Usage

``` r
# S3 method for class 'pairwise_wilcoxon'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An object of class `"pairwise_wilcoxon"` returned by
  [`pairwise_wilcoxon`](https://YannickDiehl.github.io/mariposa/reference/pairwise_wilcoxon.md).

- digits:

  Number of decimal places to display (default: 3)

- ...:

  Additional arguments passed to
  [`print`](https://rdrr.io/r/base/print.html). Currently unused.

## Value

Invisibly returns the input object `x`.

## Examples

``` r
result <- friedman_test(survey_data, trust_government, trust_media,
                        trust_science) |> pairwise_wilcoxon()
result              # compact overview
#> Pairwise Wilcoxon Post-Hoc Test (Bonferroni)
#>   3 comparisons, 3 significant (p < .05)
#> Use summary() for the full comparison table.
summary(result)     # full comparison tables
#> Pairwise Wilcoxon Post-Hoc Test (Bonferroni) Results
#> ----------------------------------------------------
#> 
#> - Variables: trust_government, trust_media, trust_science
#> - P-value adjustment: Bonferroni
#> - Number of comparisons: 3
#> 
#> ------------------------------------------------------------ 
#>             Var 1         Var 2      Z p (unadj) p (adj) Sig 
#>  trust_government   trust_media -5.097     <.001   <.001 *** 
#>  trust_government trust_science 25.945     <.001   <.001 *** 
#>       trust_media trust_science 29.091     <.001   <.001 *** 
#> ------------------------------------------------------------ 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - Positive Z: First variable tends to have higher values
#> - Negative Z: Second variable tends to have higher values
#> - p-values are adjusted for multiple comparisons
```
