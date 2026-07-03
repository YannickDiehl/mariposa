# Print McNemar test results (compact)

Compact print method for objects of class `"mcnemar_test"`. Shows a
one-line summary with the McNemar chi-square statistic, the asymptotic
and exact p-values, and sample size.

For the full detailed output (2x2 contingency table, test results,
discordant pairs), use
[`summary()`](https://rdrr.io/r/base/summary.html).

## Usage

``` r
# S3 method for class 'mcnemar_test'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An object of class `"mcnemar_test"`

- digits:

  Number of decimal places (default: 3)

- ...:

  Additional arguments (currently unused)

## Value

Invisibly returns the input object `x`.

## Examples

``` r
test_data <- transform(survey_data,
  trust_gov_high = as.integer(trust_government >= 4),
  trust_media_high = as.integer(trust_media >= 4))
result <- mcnemar_test(test_data, var1 = trust_gov_high,
                       var2 = trust_media_high)
result              # compact one-line overview
#> McNemar Test: trust_gov_high x trust_media_high
#>   chi2 = 15.116, p < 0.001 (asymp), p < 0.001 (exact) ***, N = 2227
#> Use summary() for detailed output.
summary(result)     # full detailed output
#> McNemar Test Results
#> --------------------
#> 
#> - Variable 1: trust_gov_high
#> - Variable 2: trust_media_high
#> 
#> 2x2 Contingency Table:
#> ---------------------------------------- 
#>    v2
#> v1     0    1
#>   0 1342  338
#>   1  448   99
#> ---------------------------------------- 
#> 
#> Test Results:
#> ----------------------------------------- 
#>  Chi-Sq (cc) p (asymp) p (exact)    N Sig 
#>       15.116     <.001     <.001 2227 *** 
#> ----------------------------------------- 
#> 
#> Discordant pairs: b = 338, c = 448
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
