# Print summary of partial correlation results (detailed output)

Displays the detailed SPSS-style output for a
[`partial_cor`](https://YannickDiehl.github.io/mariposa/reference/partial_cor.md)
result, with sections controlled by the boolean parameters passed to
[`summary.partial_cor`](https://YannickDiehl.github.io/mariposa/reference/summary.partial_cor.md).

## Usage

``` r
# S3 method for class 'summary.partial_cor'
print(x, ...)
```

## Arguments

- x:

  A `summary.partial_cor` object created by
  [`summary.partial_cor`](https://YannickDiehl.github.io/mariposa/reference/summary.partial_cor.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`partial_cor`](https://YannickDiehl.github.io/mariposa/reference/partial_cor.md)
for the main analysis,
[`summary.partial_cor`](https://YannickDiehl.github.io/mariposa/reference/summary.partial_cor.md)
for summary options.

## Examples

``` r
result <- partial_cor(survey_data, life_satisfaction, income, controls = age)
summary(result)
#> 
#> Partial Correlation Results
#> ---------------------------
#> - Variables: life_satisfaction, income
#> - Controlling for: age
#> - Missing: Listwise deletion
#> 
#> Pairwise Results:
#>   -------------------------------------------------------------------------------------- 
#>   Variable 1         Variable 2  Partial r  Zero-order r    df       t      p     n  sig 
#>   -------------------------------------------------------------------------------------- 
#>   life_satisfaction      income      0.448         0.448  2112  23.037  <.001  2115  *** 
#>   -------------------------------------------------------------------------------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
