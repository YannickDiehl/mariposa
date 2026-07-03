# Print summary of McNemar test results (detailed output)

Displays the detailed output for a McNemar test, with sections
controlled by the boolean parameters passed to
[`summary.mcnemar_test`](https://YannickDiehl.github.io/mariposa/reference/summary.mcnemar_test.md).
Sections include the 2x2 contingency table, the test results table, and
the discordant pairs.

## Usage

``` r
# S3 method for class 'summary.mcnemar_test'
print(x, ...)
```

## Arguments

- x:

  A `summary.mcnemar_test` object created by
  [`summary.mcnemar_test`](https://YannickDiehl.github.io/mariposa/reference/summary.mcnemar_test.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`mcnemar_test`](https://YannickDiehl.github.io/mariposa/reference/mcnemar_test.md)
for the main analysis,
[`summary.mcnemar_test`](https://YannickDiehl.github.io/mariposa/reference/summary.mcnemar_test.md)
for summary options.

## Examples

``` r
test_data <- transform(survey_data,
  trust_gov_high = as.integer(trust_government >= 4),
  trust_media_high = as.integer(trust_media >= 4))
result <- mcnemar_test(test_data, var1 = trust_gov_high,
                       var2 = trust_media_high)
summary(result)                            # all sections
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
summary(result, discordant_pairs = FALSE)  # hide discordant pairs
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
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
