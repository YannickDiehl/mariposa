# Summary method for binomial test results

Creates a summary object that produces detailed output when printed,
including the category table with counts and observed proportions, and
the test statistics table with test proportion, p-value, and confidence
interval.

## Usage

``` r
# S3 method for class 'binomial_test'
summary(object, categories = TRUE, results = TRUE, digits = 3, ...)
```

## Arguments

- object:

  A `binomial_test` result object.

- categories:

  Logical. Show the category table? (Default: TRUE)

- results:

  Logical. Show test statistics table? (Default: TRUE)

- digits:

  Number of decimal places for formatting (Default: 3).

- ...:

  Additional arguments (not used).

## Value

A `summary.binomial_test` object.

## See also

[`binomial_test`](https://YannickDiehl.github.io/mariposa/reference/binomial_test.md)
for the main analysis function.

## Examples

``` r
result <- binomial_test(survey_data, gender, p = 0.50)
summary(result)
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
summary(result, categories = FALSE)
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
