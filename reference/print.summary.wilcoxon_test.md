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
#>   ---------------------------------------------
#>                      N  Mean Rank  Sum of Ranks
#>   ---------------------------------------------
#>   Negative Ranks   955     887.53     847592.50
#>   Positive Ranks   770     832.57     641082.50
#>   Ties             502                         
#>   Total           2227                         
#>   ---------------------------------------------
#> 
#>   a trust_media < trust_government
#>   b trust_media > trust_government
#>   c trust_media = trust_government
#> 
#>   Test Statistics:
#>   ------------------------------
#>        Z  p value  Effect r     
#>   ------------------------------
#>   -5.097    <.001     0.123  ***
#>   ------------------------------
#>   Z is based on positive ranks (the smaller rank sum), as in SPSS.
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (r):
#> - Negligible: |r| < 0.1
#> - Small: 0.1 <= |r| < 0.3
#> - Medium: 0.3 <= |r| < 0.5
#> - Large: |r| >= 0.5
summary(result, ranks = FALSE) # hide rank table
#> Wilcoxon Signed-Rank Test Results
#> ---------------------------------
#> 
#> - Pair: trust_media vs trust_government
#> 
#> trust_media - trust_government
#> ------------------------------
#>   Test Statistics:
#>   ------------------------------
#>        Z  p value  Effect r     
#>   ------------------------------
#>   -5.097    <.001     0.123  ***
#>   ------------------------------
#>   Z is based on positive ranks (the smaller rank sum), as in SPSS.
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (r):
#> - Negligible: |r| < 0.1
#> - Small: 0.1 <= |r| < 0.3
#> - Medium: 0.3 <= |r| < 0.5
#> - Large: |r| >= 0.5
```
