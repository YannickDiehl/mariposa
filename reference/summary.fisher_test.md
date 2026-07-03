# Summary method for Fisher's exact test results

Creates a summary object that produces detailed output when printed,
including the contingency table of observed frequencies and the test
results table with method, exact p-value, and sample size.

## Usage

``` r
# S3 method for class 'fisher_test'
summary(object, contingency_table = TRUE, results = TRUE, digits = 3, ...)
```

## Arguments

- object:

  A `fisher_test` result object.

- contingency_table:

  Logical. Show the contingency table? (Default: TRUE)

- results:

  Logical. Show the test results table? (Default: TRUE)

- digits:

  Number of decimal places for formatting (Default: 3).

- ...:

  Additional arguments (not used).

## Value

A `summary.fisher_test` object.

## See also

[`fisher_test`](https://YannickDiehl.github.io/mariposa/reference/fisher_test.md)
for the main analysis function.

## Examples

``` r
result <- fisher_test(survey_data, row = gender, col = region)
summary(result)
#> Fisher's Exact Test Results
#> ---------------------------
#> 
#> - Row variable: gender
#> - Column variable: region
#> 
#> Contingency Table:
#> ---------------------------------------- 
#>         cc
#> r        East West
#>   Male    238  956
#>   Female  247 1059
#> ---------------------------------------- 
#> 
#> Test Results:
#> ---------------------------------------------------- 
#>                              Method p-value    N Sig 
#>  Fisher's Exact Test for Count Data   0.544 2500     
#> ---------------------------------------------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
summary(result, contingency_table = FALSE)
#> Fisher's Exact Test Results
#> ---------------------------
#> 
#> - Row variable: gender
#> - Column variable: region
#> 
#> Test Results:
#> ---------------------------------------------------- 
#>                              Method p-value    N Sig 
#>  Fisher's Exact Test for Count Data   0.544 2500     
#> ---------------------------------------------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
