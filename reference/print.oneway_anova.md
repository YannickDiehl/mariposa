# Print ANOVA test results

Print method for objects of class `"oneway_anova"`. Provides a formatted
display of ANOVA results including descriptive statistics, ANOVA tables,
assumption tests, and effect sizes.

## Usage

``` r
# S3 method for class 'oneway_anova'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An object of class `"oneway_anova"` returned by
  [`oneway_anova`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md).

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

- Descriptive statistics by group (means, standard deviations, sample
  sizes)

- Complete ANOVA table (Sum of Squares, degrees of freedom, F-statistic,
  p-value)

- Welch's robust test for unequal variances

- Effect sizes (Eta-squared, Epsilon-squared, Omega-squared) with
  interpretation

- Significance indicators (\* p \< 0.05, \*\* p \< 0.01, \*\*\* p \<
  0.001)

For grouped analyses (when data is grouped with
[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)),
results are displayed separately for each group combination.

For weighted analyses, both actual sample sizes and effective sample
sizes (weighted n) are displayed.
