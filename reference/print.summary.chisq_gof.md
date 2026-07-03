# Print summary of chi-square goodness-of-fit results (detailed output)

Displays the detailed output for a chi-square goodness-of-fit test, with
sections controlled by the boolean parameters passed to
[`summary.chisq_gof`](https://YannickDiehl.github.io/mariposa/reference/summary.chisq_gof.md).
Sections include per-variable frequency tables and the test statistics
table.

## Usage

``` r
# S3 method for class 'summary.chisq_gof'
print(x, ...)
```

## Arguments

- x:

  A `summary.chisq_gof` object created by
  [`summary.chisq_gof`](https://YannickDiehl.github.io/mariposa/reference/summary.chisq_gof.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`chisq_gof`](https://YannickDiehl.github.io/mariposa/reference/chisq_gof.md)
for the main analysis,
[`summary.chisq_gof`](https://YannickDiehl.github.io/mariposa/reference/summary.chisq_gof.md)
for summary options.

## Examples

``` r
result <- chisq_gof(survey_data, gender)
summary(result)                          # all sections
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
summary(result, frequency_table = FALSE) # hide frequency tables
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
