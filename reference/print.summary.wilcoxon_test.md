# Print summary of Wilcoxon signed-rank test results (detailed output)

Displays the detailed SPSS-style output for a Wilcoxon signed-rank test,
with sections controlled by the boolean parameters passed to
[`summary.wilcoxon_test`](https://YannickDiehl.github.io/mariposa/reference/summary.wilcoxon_test.md).
Sections include the rank table and the test statistics table with
effect size interpretation.

## Usage

``` r
# S3 method for class 'summary.wilcoxon_test'
print(x, ...)
```

## Arguments

- x:

  A `summary.wilcoxon_test` object created by
  [`summary.wilcoxon_test`](https://YannickDiehl.github.io/mariposa/reference/summary.wilcoxon_test.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`wilcoxon_test`](https://YannickDiehl.github.io/mariposa/reference/wilcoxon_test.md)
for the main analysis,
[`summary.wilcoxon_test`](https://YannickDiehl.github.io/mariposa/reference/summary.wilcoxon_test.md)
for summary options.

## Examples

``` r
result <- wilcoxon_test(survey_data, x = trust_government, y = trust_media)
summary(result)                # all sections
#> Wilcoxon Signed-Rank Test Results
#> ---------------------------------
#> 
#> - Pair: trust_media vs trust_government
#> 
#> trust_media - trust_government
#> ------------------------------
#>   Ranks:
#>   -------------------------------------------
#>                      N Mean Rank Sum of Ranks
#>    Negative Ranks  955    887.53     847592.5
#>    Positive Ranks  770    832.57     641082.5
#>              Ties  502        NA           NA
#>             Total 2227        NA           NA
#>   -------------------------------------------
#> 
#>   a trust_media < trust_government
#>   b trust_media > trust_government
#>   c trust_media = trust_government
#> 
#>   Test Statistics:
#>   ----------------------------
#>         Z p value Effect r sig
#>    -5.097       0    0.123 ***
#>   ----------------------------
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (r):
#> - Small effect: 0.1 - 0.3
#> - Medium effect: 0.3 - 0.5
#> - Large effect: > 0.5
summary(result, ranks = FALSE) # hide rank table
#> Wilcoxon Signed-Rank Test Results
#> ---------------------------------
#> 
#> - Pair: trust_media vs trust_government
#> 
#> trust_media - trust_government
#> ------------------------------
#>   Test Statistics:
#>   ----------------------------
#>         Z p value Effect r sig
#>    -5.097       0    0.123 ***
#>   ----------------------------
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (r):
#> - Small effect: 0.1 - 0.3
#> - Medium effect: 0.3 - 0.5
#> - Large effect: > 0.5
```
