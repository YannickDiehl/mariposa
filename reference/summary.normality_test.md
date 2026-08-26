# Summary method for normality test results

Creates a summary object that produces the detailed SPSS-style "Tests of
Normality" table when printed.

## Usage

``` r
# S3 method for class 'normality_test'
summary(object, tests = TRUE, digits = 3, ...)
```

## Arguments

- object:

  A `normality_test` result object.

- tests:

  Logical. Show the tests-of-normality table? (Default: TRUE)

- digits:

  Number of decimal places for formatting (Default: 3).

- ...:

  Additional arguments (not used).

## Value

A `summary.normality_test` object.

## See also

[`normality_test`](https://YannickDiehl.github.io/mariposa/reference/normality_test.md)
for the main analysis function.

## Examples

``` r
result <- normality_test(survey_data, age, income)
summary(result)
#> 
#> Normality Tests Results
#> -----------------------
#> - Variables: age, income
#> 
#> Tests of Normality
#>   ------------------------------------------ 
#>   Variable     KS    df   KS p      W    W p 
#>   ------------------------------------------ 
#>   age       0.028  2500  <.001  0.990  <.001 
#>   income    0.079  2186  <.001  0.963  <.001 
#>   ------------------------------------------ 
#> 
#> KS = Kolmogorov-Smirnov statistic with Lilliefors significance correction.
#> W = Shapiro-Wilk statistic (computed for 3 <= n <= 5000, as in SPSS).
#> p < 0.05 indicates a significant deviation from normality.
```
