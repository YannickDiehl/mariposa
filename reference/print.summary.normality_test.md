# Print summary of normality test results (detailed output)

Displays the SPSS-style "Tests of Normality" table (Kolmogorov-Smirnov
with Lilliefors correction, Shapiro-Wilk) for a
[`normality_test`](https://YannickDiehl.github.io/mariposa/reference/normality_test.md)
result, per group for grouped data.

## Usage

``` r
# S3 method for class 'summary.normality_test'
print(x, ...)
```

## Arguments

- x:

  A `summary.normality_test` object created by
  [`summary.normality_test`](https://YannickDiehl.github.io/mariposa/reference/summary.normality_test.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`normality_test`](https://YannickDiehl.github.io/mariposa/reference/normality_test.md)
for the main analysis,
[`summary.normality_test`](https://YannickDiehl.github.io/mariposa/reference/summary.normality_test.md)
for summary options.

## Examples

``` r
result <- normality_test(survey_data, age, income)
summary(result)
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
