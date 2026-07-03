# Print Friedman test results (compact)

Compact print method for objects of class `"friedman_test"`. Shows a
one-line summary with the chi-square statistic, p-value, Kendall's W,
and sample size.

For the full detailed output (rank tables, test statistics), use
[`summary()`](https://rdrr.io/r/base/summary.html).

## Usage

``` r
# S3 method for class 'friedman_test'
print(x, digits = 3, ...)
```

## Arguments

- x:

  A friedman_test object

- digits:

  Number of decimal places to display (default: 3)

- ...:

  Additional arguments (not used)

## Value

Invisibly returns the input object `x`.

## Examples

``` r
result <- friedman_test(survey_data, trust_government, trust_media,
                        trust_science)
result              # compact one-line overview
#> Friedman Test: trust_government, trust_media, trust_science
#>   chi2(2) = 1009.035, p < 0.001 ***, W = 0.236, N = 2135
#> Use summary() for detailed output.
summary(result)     # full detailed output
#> Friedman Test Results
#> ---------------------
#> 
#> - Variables: trust_government, trust_media, trust_science
#> - Number of conditions: 3
#> 
#>   Ranks:
#>   ---------------------------
#>            Variable Mean Rank
#>    trust_government      1.81
#>         trust_media      1.68
#>       trust_science      2.51
#>   ---------------------------
#> 
#>   Test Statistics:
#>   -------------------------------------------
#>       N Chi-Square df p value Kendall's W sig
#>    2135   1009.035  2       0       0.236 ***
#>   -------------------------------------------
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (Kendall's W):
#> - Weak agreement: 0.1 - 0.3
#> - Moderate agreement: 0.3 - 0.5
#> - Strong agreement: > 0.5
```
