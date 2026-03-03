# Print pairwise Wilcoxon post-hoc test results

Print method for objects of class `"pairwise_wilcoxon"`. Provides a
formatted display of pairwise Wilcoxon comparison results including
Z-statistics and adjusted p-values.

## Usage

``` r
# S3 method for class 'pairwise_wilcoxon'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An object of class `"pairwise_wilcoxon"` returned by
  [`pairwise_wilcoxon`](https://YannickDiehl.github.io/mariposa/dev/reference/pairwise_wilcoxon.md).

- digits:

  Number of decimal places to display (default: 3)

- ...:

  Additional arguments passed to
  [`print`](https://rdrr.io/r/base/print.html). Currently unused.

## Value

Invisibly returns the input object `x`.

## Details

The print method displays:

- Pairwise measurement comparisons with Z-statistics

- Adjusted p-values controlling for multiple comparisons

- Significance indicators (\* p \< 0.05, \*\* p \< 0.01, \*\*\* p \<
  0.001)

For grouped analyses, results are displayed separately for each group.
