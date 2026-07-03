# Print method for describe objects

Prints formatted descriptive statistics with professional layout.
Automatically adjusts output based on whether analysis was weighted or
unweighted. The output of
[`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
is a summary table by nature, so
[`print()`](https://rdrr.io/r/base/print.html) and
[`summary()`](https://rdrr.io/r/base/summary.html) display the same
statistics table; [`summary()`](https://rdrr.io/r/base/summary.html)
additionally offers a `statistics` section toggle and a `digits` option.

## Usage

``` r
# S3 method for class 'describe'
print(x, digits = 3, ...)
```

## Arguments

- x:

  A describe object

- digits:

  Number of decimal places to display (default: 3)

- ...:

  Additional arguments (currently unused)

## Value

Invisibly returns the input object `x`.
