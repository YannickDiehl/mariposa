# Summary method for McNemar test results

Creates a summary object that produces detailed output when printed,
including the 2x2 contingency table, the test results table with
asymptotic and exact p-values, and the discordant pair counts.

## Usage

``` r
# S3 method for class 'mcnemar_test'
summary(
  object,
  contingency_table = TRUE,
  results = TRUE,
  discordant_pairs = TRUE,
  digits = 3,
  ...
)
```

## Arguments

- object:

  A `mcnemar_test` result object.

- contingency_table:

  Logical. Show the 2x2 contingency table? (Default: TRUE)

- results:

  Logical. Show the test results table? (Default: TRUE)

- discordant_pairs:

  Logical. Show the discordant pair counts? (Default: TRUE)

- digits:

  Number of decimal places for formatting (Default: 3).

- ...:

  Additional arguments (not used).

## Value

A `summary.mcnemar_test` object.

## See also

[`mcnemar_test`](https://YannickDiehl.github.io/mariposa/reference/mcnemar_test.md)
for the main analysis function.

## Examples

``` r
test_data <- transform(survey_data,
  trust_gov_high = as.integer(trust_government >= 4),
  trust_media_high = as.integer(trust_media >= 4))
result <- mcnemar_test(test_data, var1 = trust_gov_high,
                       var2 = trust_media_high)
summary(result)
#> McNemar Test Results
#> --------------------
#> 
#> - Variable 1: trust_gov_high
#> - Variable 2: trust_media_high
#> 
#> 2x2 Contingency Table:
#> ---------------------------------------- 
#>    v2
#> v1     0    1
#>   0 1342  338
#>   1  448   99
#> ---------------------------------------- 
#> 
#> Test Results:
#> ----------------------------------------- 
#>  Chi-Sq (cc) p (asymp) p (exact)    N Sig 
#>       15.116     <.001     <.001 2227 *** 
#> ----------------------------------------- 
#> 
#> Discordant pairs: b = 338, c = 448
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
summary(result, contingency_table = FALSE)
#> McNemar Test Results
#> --------------------
#> 
#> - Variable 1: trust_gov_high
#> - Variable 2: trust_media_high
#> 
#> Test Results:
#> ----------------------------------------- 
#>  Chi-Sq (cc) p (asymp) p (exact)    N Sig 
#>       15.116     <.001     <.001 2227 *** 
#> ----------------------------------------- 
#> 
#> Discordant pairs: b = 338, c = 448
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
