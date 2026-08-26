# Print partial correlation results (compact)

Compact print method for objects of class `"partial_cor"`: one line per
variable pair with the partial correlation, p-value, and the zero-order
correlation for comparison.

## Usage

``` r
# S3 method for class 'partial_cor'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An object of class `"partial_cor"` returned by
  [`partial_cor`](https://YannickDiehl.github.io/mariposa/reference/partial_cor.md).

- digits:

  Number of decimal places (default: 3).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## Examples

``` r
result <- partial_cor(survey_data, life_satisfaction, income, controls = age)
result              # compact overview
#> Partial Correlation: life_satisfaction, income | controlling for age
#>   life_satisfaction x income: partial r = 0.448, p < 0.001 *** (zero-order r = 0.448), N = 2115
#> Use summary() for detailed output.
summary(result)     # full detailed output
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
