# Print summary of binomial test results (detailed output)

Displays the detailed SPSS-style output for a binomial test, with
sections controlled by the boolean parameters passed to
[`summary.binomial_test`](https://YannickDiehl.github.io/mariposa/reference/summary.binomial_test.md).
Sections include the category table and the test statistics table with
confidence interval.

## Usage

``` r
# S3 method for class 'summary.binomial_test'
print(x, ...)
```

## Arguments

- x:

  A `summary.binomial_test` object created by
  [`summary.binomial_test`](https://YannickDiehl.github.io/mariposa/reference/summary.binomial_test.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`binomial_test`](https://YannickDiehl.github.io/mariposa/reference/binomial_test.md)
for the main analysis,
[`summary.binomial_test`](https://YannickDiehl.github.io/mariposa/reference/summary.binomial_test.md)
for summary options.

## Examples

``` r
result <- binomial_test(survey_data, gender, p = 0.50)
summary(result)                     # all sections
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
summary(result, categories = FALSE) # hide category tables
#> Binomial Test Results
#> ---------------------
#> 
#> - Test proportion: 0.5
#> - Confidence level: 95.0%
#> 
#> gender
#> ------
#>   Test Statistics:
#>   -----------------------------------------
#>    Test Prop. p value CI lower CI upper sig
#>           0.5   0.026    0.458    0.497   *
#>   -----------------------------------------
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
