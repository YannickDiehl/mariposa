# Print Wilcoxon signed-rank test results (compact)

Compact print method for objects of class `"wilcoxon_test"`. Shows a
one-line summary per comparison with the Z statistic, p-value, effect
size r, and sample size.

For the full detailed output (rank tables, test statistics), use
[`summary()`](https://rdrr.io/r/base/summary.html).

## Usage

``` r
# S3 method for class 'wilcoxon_test'
print(x, digits = 3, ...)
```

## Arguments

- x:

  A wilcoxon_test object

- digits:

  Number of decimal places to display (default: 3)

- ...:

  Additional arguments (not used)

## Value

Invisibly returns the input object `x`.

## Examples

``` r
result <- wilcoxon_test(survey_data, x = trust_government, y = trust_media)
result              # compact one-line overview
#> Wilcoxon Signed-Rank Test: trust_media - trust_government
#>   Z = -5.097, p < 0.001 ***, r = 0.123 (small), N = 2227
#> Use summary() for detailed output.
summary(result)     # full detailed output
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
```
