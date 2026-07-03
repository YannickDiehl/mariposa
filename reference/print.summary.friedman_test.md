# Print summary of Friedman test results (detailed output)

Displays the detailed SPSS-style output for a Friedman test, with
sections controlled by the boolean parameters passed to
[`summary.friedman_test`](https://YannickDiehl.github.io/mariposa/reference/summary.friedman_test.md).
Sections include the mean rank table and the test statistics table with
effect size interpretation.

## Usage

``` r
# S3 method for class 'summary.friedman_test'
print(x, ...)
```

## Arguments

- x:

  A `summary.friedman_test` object created by
  [`summary.friedman_test`](https://YannickDiehl.github.io/mariposa/reference/summary.friedman_test.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`friedman_test`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md)
for the main analysis,
[`summary.friedman_test`](https://YannickDiehl.github.io/mariposa/reference/summary.friedman_test.md)
for summary options.

## Examples

``` r
result <- friedman_test(survey_data, trust_government, trust_media,
                        trust_science)
summary(result)                # all sections
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
summary(result, ranks = FALSE) # hide rank table
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
