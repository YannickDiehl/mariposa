# Summary method for Friedman test results

Creates a summary object that produces detailed output when printed,
including the mean rank table per measurement and the test statistics
table with chi-square, p-value, and Kendall's W.

## Usage

``` r
# S3 method for class 'friedman_test'
summary(object, ranks = TRUE, results = TRUE, digits = 3, ...)
```

## Arguments

- object:

  A `friedman_test` result object.

- ranks:

  Logical. Show the mean rank table? (Default: TRUE)

- results:

  Logical. Show test statistics table? (Default: TRUE)

- digits:

  Number of decimal places for formatting (Default: 3).

- ...:

  Additional arguments (not used).

## Value

A `summary.friedman_test` object.

## See also

[`friedman_test`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md)
for the main analysis function.

## Examples

``` r
result <- friedman_test(survey_data, trust_government, trust_media,
                        trust_science)
summary(result)
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
summary(result, ranks = FALSE)
#> Friedman Test Results
#> ---------------------
#> 
#> - Variables: trust_government, trust_media, trust_science
#> - Number of conditions: 3
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
