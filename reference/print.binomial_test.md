# Print binomial test results (compact)

Compact print method for objects of class `"binomial_test"`. Shows a
one-line summary per variable with the observed vs. test proportion,
p-value, and sample size.

For the full detailed output (category table, test statistics with
confidence interval), use
[`summary()`](https://rdrr.io/r/base/summary.html).

## Usage

``` r
# S3 method for class 'binomial_test'
print(x, digits = 3, ...)
```

## Arguments

- x:

  A binomial_test object

- digits:

  Number of decimal places to display (default: 3)

- ...:

  Additional arguments (not used)

## Value

Invisibly returns the input object `x`.

## Examples

``` r
result <- binomial_test(survey_data, gender, p = 0.50)
result              # compact one-line overview
#> Binomial Test: gender
#>   Group 1 (Male): prop = 0.478 vs 0.500, p = 0.026 *, N = 2500
#> Use summary() for detailed output.
summary(result)     # full detailed output
#> Binomial Test Results
#> ---------------------
#> 
#> - Test proportion: 0.5
#> - Confidence level: 95.0%
#> 
#> gender
#> ------
#>   Categories:
#>   ------------------------------------
#>                       N Observed Prop.
#>      Group 1: Male 1194          0.478
#>    Group 2: Female 1306          0.522
#>              Total 2500          1.000
#>   ------------------------------------
#> 
#>   Test Statistics:
#>   -----------------------------------------
#>    Test Prop. p value CI lower CI upper sig
#>           0.5   0.026    0.458    0.497   *
#>   -----------------------------------------
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
