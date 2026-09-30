# Summary method for Wilcoxon signed-rank test results

Creates a summary object that produces detailed output when printed,
including the rank table (negative/positive ranks, ties) and the test
statistics table with Z, p-value, and effect size r.

## Usage

``` r
# S3 method for class 'wilcoxon_test'
summary(object, ranks = TRUE, results = TRUE, digits = 3, ...)
```

## Arguments

- object:

  A `wilcoxon_test` result object.

- ranks:

  Logical. Show the rank table? (Default: TRUE)

- results:

  Logical. Show test statistics table? (Default: TRUE)

- digits:

  Number of decimal places for formatting (Default: 3).

- ...:

  Additional arguments (not used).

## Value

A `summary.wilcoxon_test` object.

## See also

[`wilcoxon_test`](https://YannickDiehl.github.io/mariposa/reference/wilcoxon_test.md)
for the main analysis function.

## Examples

``` r
result <- wilcoxon_test(survey_data, x = trust_government, y = trust_media)
summary(result)
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
summary(result, ranks = FALSE)
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
