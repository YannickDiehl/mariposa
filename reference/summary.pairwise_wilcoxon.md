# Summary method for pairwise Wilcoxon post-hoc test results

Creates a summary object that produces detailed output when printed,
including the pairwise comparison tables with Z-statistics, unadjusted
and adjusted p-values, and interpretation.

## Usage

``` r
# S3 method for class 'pairwise_wilcoxon'
summary(object, comparisons = TRUE, interpretation = TRUE, digits = 3, ...)
```

## Arguments

- object:

  A `pairwise_wilcoxon` result object.

- comparisons:

  Logical. Show the pairwise comparison tables? (Default: TRUE)

- interpretation:

  Logical. Show the interpretation section? (Default: TRUE)

- digits:

  Number of decimal places for formatting (Default: 3).

- ...:

  Additional arguments (not used).

## Value

A `summary.pairwise_wilcoxon` object.

## See also

[`pairwise_wilcoxon`](https://YannickDiehl.github.io/mariposa/reference/pairwise_wilcoxon.md)
for the main analysis function.

## Examples

``` r
result <- friedman_test(survey_data, trust_government, trust_media,
                        trust_science) |> pairwise_wilcoxon()
summary(result)
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
summary(result, interpretation = FALSE)
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
```
