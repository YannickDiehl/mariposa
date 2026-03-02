# Print Dunn post-hoc test results

Print method for objects of class `"dunn_test"`. Provides a formatted
display of Dunn pairwise comparison results including Z-statistics and
adjusted p-values.

## Usage

``` r
# S3 method for class 'dunn_test'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An object of class `"dunn_test"` returned by
  [`dunn_test`](https://YannickDiehl.github.io/mariposa/reference/dunn_test.md).

- digits:

  Number of decimal places to display (default: 3)

- ...:

  Additional arguments passed to
  [`print`](https://rdrr.io/r/base/print.html). Currently unused.

## Value

Invisibly returns the input object `x`.

## Details

The print method displays:

- Pairwise group comparisons with Z-statistics

- Adjusted p-values controlling for multiple comparisons

- Significance indicators (\* p \< 0.05, \*\* p \< 0.01, \*\*\* p \<
  0.001)

For grouped analyses, results are displayed separately for each group
combination.
