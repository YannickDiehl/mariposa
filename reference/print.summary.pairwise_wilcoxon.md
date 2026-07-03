# Print summary of pairwise Wilcoxon post-hoc test results (detailed output)

Displays the detailed output for pairwise Wilcoxon comparisons, with
sections controlled by the boolean parameters passed to
[`summary.pairwise_wilcoxon`](https://YannickDiehl.github.io/mariposa/reference/summary.pairwise_wilcoxon.md).
The display includes:

- Pairwise measurement comparisons with Z-statistics

- Adjusted p-values controlling for multiple comparisons

- Significance indicators (\* p \< 0.05, \*\* p \< 0.01, \*\*\* p \<
  0.001)

For grouped analyses, results are displayed separately for each group.

## Usage

``` r
# S3 method for class 'summary.pairwise_wilcoxon'
print(x, ...)
```

## Arguments

- x:

  A `summary.pairwise_wilcoxon` object created by
  [`summary.pairwise_wilcoxon`](https://YannickDiehl.github.io/mariposa/reference/summary.pairwise_wilcoxon.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`pairwise_wilcoxon`](https://YannickDiehl.github.io/mariposa/reference/pairwise_wilcoxon.md)
for the main analysis,
[`summary.pairwise_wilcoxon`](https://YannickDiehl.github.io/mariposa/reference/summary.pairwise_wilcoxon.md)
for summary options.

## Examples

``` r
result <- friedman_test(survey_data, trust_government, trust_media,
                        trust_science) |> pairwise_wilcoxon()
summary(result)                       # all sections
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
summary(result, comparisons = FALSE)  # hide comparison tables
#> Pairwise Wilcoxon Post-Hoc Test (Bonferroni) Results
#> ----------------------------------------------------
#> 
#> - Variables: trust_government, trust_media, trust_science
#> - P-value adjustment: Bonferroni
#> - Number of comparisons: 3
#> 
#> 
#> Interpretation:
#> - Positive Z: First variable tends to have higher values
#> - Negative Z: Second variable tends to have higher values
#> - p-values are adjusted for multiple comparisons
```
