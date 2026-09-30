# Print summary of describe results (detailed output)

Displays the descriptive statistics table for a `describe` result,
controlled by the boolean parameter passed to
[`summary.describe`](https://YannickDiehl.github.io/mariposa/reference/summary.describe.md).

## Usage

``` r
# S3 method for class 'summary.describe'
print(x, ...)
```

## Arguments

- x:

  A `summary.describe` object created by
  [`summary.describe`](https://YannickDiehl.github.io/mariposa/reference/summary.describe.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`describe`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
for the main analysis,
[`summary.describe`](https://YannickDiehl.github.io/mariposa/reference/summary.describe.md)
for summary options.

## Examples

``` r
result <- describe(survey_data, age, income)
summary(result)
#> 
#> Descriptive Statistics
#> ----------------------
#> 
#>   ---------------------------------------------------------------------------
#>   Variable     Mean   Median       SD    Range      IQR Skewness    N Missing
#>   ---------------------------------------------------------------------------
#>   age        50.550   50.000   16.976   77.000   24.000    0.172 2500       0
#>   income   3753.934 3500.000 1432.802 7200.000 1900.000    0.730 2186     314
#>   ---------------------------------------------------------------------------
```
