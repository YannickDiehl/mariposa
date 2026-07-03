# ANOVA for a linear_regression model

For listwise + ungrouped results, dispatches to
[`stats::anova.lm`](https://rdrr.io/r/stats/anova.lm.html) (sequential
Type-I sum of squares per term). For the SPSS-style overall-model ANOVA
table, use `object$anova_table`.

## Usage

``` r
# S3 method for class 'linear_regression'
anova(object, ...)
```

## Arguments

- object:

  A `linear_regression` result.

- ...:

  Passed to [`stats::anova.lm`](https://rdrr.io/r/stats/anova.lm.html).
