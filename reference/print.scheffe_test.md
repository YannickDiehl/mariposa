# Print Scheffe test results

Print method for objects of class `"scheffe_test"`. Provides a formatted
display of Scheffe post-hoc test results including pairwise comparisons,
confidence intervals, and adjusted p-values.

## Usage

``` r
# S3 method for class 'scheffe_test'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An object of class `"scheffe_test"` returned by
  [`scheffe_test`](https://YannickDiehl.github.io/mariposa/reference/scheffe_test.md).

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

- Confidence intervals for differences (widest among all post-hoc tests)

- Scheffe-adjusted p-values controlling family-wise error rate

- Significance indicators (\* p \< 0.05, \*\* p \< 0.01, \*\*\* p \<
  0.001)

For grouped analyses, results are displayed separately for each group
combination. For weighted analyses, effective sample sizes are used in
calculations.
