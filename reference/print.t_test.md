# Print t-test results

Print method for objects of class `"t_test"`. Provides a formatted
display of t-test results including group statistics, test statistics,
p-values, effect sizes, and confidence intervals.

## Usage

``` r
# S3 method for class 't_test'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An object of class `"t_test"` returned by
  [`t_test`](https://YannickDiehl.github.io/mariposa/reference/t_test.md).

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

- Group-specific descriptive statistics (means and sample sizes)

- Test statistics (t-statistic, degrees of freedom, p-value)

- Effect size (Cohen's d) with interpretation

- Confidence intervals for mean differences

- Significance indicators (\* p \< 0.05, \*\* p \< 0.01, \*\*\* p \<
  0.001)

For grouped analyses (when data is grouped with
[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)),
results are displayed separately for each group combination.
