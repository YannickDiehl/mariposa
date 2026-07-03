# Print summary of Fisher's exact test results (detailed output)

Displays the detailed output for Fisher's exact test, with sections
controlled by the boolean parameters passed to
[`summary.fisher_test`](https://YannickDiehl.github.io/mariposa/reference/summary.fisher_test.md).
Sections include the contingency table and the test results table.

## Usage

``` r
# S3 method for class 'summary.fisher_test'
print(x, ...)
```

## Arguments

- x:

  A `summary.fisher_test` object created by
  [`summary.fisher_test`](https://YannickDiehl.github.io/mariposa/reference/summary.fisher_test.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`fisher_test`](https://YannickDiehl.github.io/mariposa/reference/fisher_test.md)
for the main analysis,
[`summary.fisher_test`](https://YannickDiehl.github.io/mariposa/reference/summary.fisher_test.md)
for summary options.

## Examples

``` r
result <- fisher_test(survey_data, row = gender, col = region)
summary(result)                            # all sections
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
summary(result, contingency_table = FALSE) # hide contingency table
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
