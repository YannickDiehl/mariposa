# Print normality test results (compact)

Compact print method for objects of class `"normality_test"`. Shows one
line per variable with both test results. For grouped analyses, only the
dimensions are shown — use
[`summary()`](https://rdrr.io/r/base/summary.html) for the per-group
tables.

## Usage

``` r
# S3 method for class 'normality_test'
print(x, ...)
```

## Arguments

- x:

  An object of class `"normality_test"` returned by
  [`normality_test`](https://YannickDiehl.github.io/mariposa/reference/normality_test.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## Examples

``` r
result <- normality_test(survey_data, age, income)
result              # compact overview
#> Normality Tests: age, income
#>   age: KS = 0.028, p < 0.001; Shapiro-Wilk W = 0.990, p < 0.001 (n = 2500)
#>   income: KS = 0.079, p < 0.001; Shapiro-Wilk W = 0.963, p < 0.001 (n = 2186)
#> Use summary() for detailed output.
summary(result)     # full detailed output
#> 
#> Normality Tests Results
#> -----------------------
#> - Variables: age, income
#> 
#> Tests of Normality
#>   ------------------------------------------ 
#>   Variable     KS    df   KS p      W    W p 
#>   ------------------------------------------ 
#>   age       0.028  2500  <.001  0.990  <.001 
#>   income    0.079  2186  <.001  0.963  <.001 
#>   ------------------------------------------ 
#> 
#> KS = Kolmogorov-Smirnov statistic with Lilliefors significance correction.
#> W = Shapiro-Wilk statistic (computed for 3 <= n <= 5000, as in SPSS).
#> p < 0.05 indicates a significant deviation from normality.
```
