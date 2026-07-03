# Summary method for describe results

Creates a summary object that produces the descriptive statistics table
when printed. Since
[`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
output is already a summary table by nature, the printed content matches
[`print()`](https://rdrr.io/r/base/print.html); the summary layer adds
the `statistics` section toggle and the `digits` formatting option for
consistency with the other analysis functions.

## Usage

``` r
# S3 method for class 'describe'
summary(object, statistics = TRUE, digits = 3, ...)
```

## Arguments

- object:

  A `describe` result object.

- statistics:

  Logical. Show the descriptive statistics table? (Default: TRUE)

- digits:

  Number of decimal places for formatting (Default: 3).

- ...:

  Additional arguments (not used).

## Value

A `summary.describe` object.

## See also

[`describe`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
for the main analysis function.

## Examples

``` r
result <- describe(survey_data, age, income)
summary(result)
#> 
#> Descriptive Statistics
#> ----------------------
#>  Variable     Mean Median       SD Range  IQR Skewness    N Missing
#>       age   50.550     50   16.976    77   24    0.172 2500       0
#>    income 3753.934   3500 1432.802  7200 1900    0.730 2186     314
#> ----------------------------------------
summary(result, digits = 2)
#> 
#> Descriptive Statistics
#> ----------------------
#>  Variable    Mean Median      SD Range  IQR Skewness    N Missing
#>       age   50.55     50   16.98    77   24     0.17 2500       0
#>    income 3753.93   3500 1432.80  7200 1900     0.73 2186     314
#> ----------------------------------------
```
