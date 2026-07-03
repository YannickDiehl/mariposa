# Print chi-square goodness-of-fit test results (compact)

Compact print method for objects of class `"chisq_gof"`. Shows a
one-line summary per variable with the chi-square statistic, degrees of
freedom, p-value, and sample size.

For the full detailed output (frequency tables with observed, expected,
and residual counts), use
[`summary()`](https://rdrr.io/r/base/summary.html).

## Usage

``` r
# S3 method for class 'chisq_gof'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An object of class `"chisq_gof"`

- digits:

  Number of decimal places (default: 3)

- ...:

  Additional arguments (currently unused)

## Value

Invisibly returns the input object `x`.

## Examples

``` r
result <- chisq_gof(survey_data, gender)
result              # compact one-line overview
#> Chi-Square Goodness-of-Fit Test: gender
#>   chi2(1) = 5.018, p = 0.025 *, N = 2500
#> Use summary() for detailed output.
summary(result)     # full detailed output
#> Chi-Square Goodness-of-Fit Test Results
#> ---------------------------------------
#> 
#> - Variables: gender
#> - Expected: Equal proportions
#> 
#>   gender - Frequency Table:
#>   ------------------------------------
#>    category observed expected residual
#>        Male     1194     1250      -56
#>      Female     1306     1250       56
#>   ------------------------------------
#> 
#> ---------------------------------------- 
#>  Variable Chi-Square df p-value    N Sig 
#>    gender      5.018  1   0.025 2500   * 
#> ---------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
