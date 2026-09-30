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
#> ---------------------------------------------------------------------------------
#> Var 1             Var 2             N        Z  Based on  p (unadj)  p (adj)     
#> ---------------------------------------------------------------------------------
#> trust_government  trust_media    2227   -5.097  positive      <.001    <.001  ***
#> trust_government  trust_science  2255  -25.945  negative      <.001    <.001  ***
#> trust_media       trust_science  2272  -29.091  negative      <.001    <.001  ***
#> ---------------------------------------------------------------------------------
#> 
#> Note: Each pair uses all cases with values on both variables
#> (pairwise deletion, as SPSS NPAR TESTS /WILCOXON), so N can exceed
#> the Friedman test's N = 2135 (cases complete on all variables).
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - Z is based on second minus first variable and, as in SPSS, on the
#>   smaller rank sum (so it is never positive)
#> - Based on negative ranks: the second variable tends to be higher
#> - Based on positive ranks: the first variable tends to be higher
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
#> - Z is based on second minus first variable and, as in SPSS, on the
#>   smaller rank sum (so it is never positive)
#> - Based on negative ranks: the second variable tends to be higher
#> - Based on positive ranks: the first variable tends to be higher
#> - p-values are adjusted for multiple comparisons
```
