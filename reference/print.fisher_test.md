# Print Fisher's exact test results (compact)

Compact print method for objects of class `"fisher_test"`. Shows a
one-line summary with the exact p-value, significance stars, and sample
size.

For the full detailed output (contingency table, test results), use
[`summary()`](https://rdrr.io/r/base/summary.html).

## Usage

``` r
# S3 method for class 'fisher_test'
print(x, digits = 4, ...)
```

## Arguments

- x:

  An object of class `"fisher_test"`

- digits:

  Number of decimal places (default: 4)

- ...:

  Additional arguments (currently unused)

## Value

Invisibly returns the input object `x`.

## Examples

``` r
result <- fisher_test(survey_data, row = gender, col = region)
result              # compact one-line overview
#> Fisher's Exact Test: gender x region
#>   p = 0.5435 , N = 2500
#> Use summary() for detailed output.
summary(result)     # full detailed output
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
```
