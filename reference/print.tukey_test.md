# Print Tukey HSD test results

Print method for objects of class `"tukey_test"`. Provides a formatted
display of Tukey post-hoc test results including pairwise comparisons,
confidence intervals, and adjusted p-values.

## Usage

``` r
# S3 method for class 'tukey_test'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An object of class `"tukey_test"` returned by
  [`tukey_test`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md).

- digits:

  Integer specifying the number of decimal places to display for numeric
  values. Default is `3`.

- ...:

  Additional arguments passed to
  [`print`](https://rdrr.io/r/base/print.html). Currently unused.

## Value

Invisibly returns the input object `x`.

## Details

The print method displays:

- Pairwise group comparisons with mean differences

- Confidence intervals for differences

- Tukey-adjusted p-values controlling family-wise error rate

- Significance indicators (\* p \< 0.05, \*\* p \< 0.01, \*\*\* p \<
  0.001)

For grouped analyses, results are displayed separately for each group
combination. For weighted analyses, effective sample sizes are used in
calculations.
