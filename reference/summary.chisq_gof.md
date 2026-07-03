# Summary method for chi-square goodness-of-fit test results

Creates a summary object that produces detailed output when printed,
including the frequency table with observed, expected, and residual
counts, and the test statistics table.

## Usage

``` r
# S3 method for class 'chisq_gof'
summary(object, frequency_table = TRUE, results = TRUE, digits = 3, ...)
```

## Arguments

- object:

  A `chisq_gof` result object.

- frequency_table:

  Logical. Show the frequency table with observed, expected, and
  residual counts? (Default: TRUE)

- results:

  Logical. Show the test statistics table? (Default: TRUE)

- digits:

  Number of decimal places for formatting (Default: 3).

- ...:

  Additional arguments (not used).

## Value

A `summary.chisq_gof` object.

## See also

[`chisq_gof`](https://YannickDiehl.github.io/mariposa/reference/chisq_gof.md)
for the main analysis function.

## Examples

``` r
result <- chisq_gof(survey_data, gender)
summary(result)
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
summary(result, frequency_table = FALSE)
#> Chi-Square Goodness-of-Fit Test Results
#> ---------------------------------------
#> 
#> - Variables: gender
#> - Expected: Equal proportions
#> 
#> ---------------------------------------- 
#>  Variable Chi-Square df p-value    N Sig 
#>    gender      5.018  1   0.025 2500   * 
#> ---------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
