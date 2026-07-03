# ANOVA for a logistic_regression model

For ungrouped results, dispatches to
[`stats::anova.glm`](https://rdrr.io/r/stats/anova.glm.html) (sequential
deviance per term). For the SPSS-style omnibus chi-square test, use
`object$omnibus_test`.

## Usage

``` r
# S3 method for class 'logistic_regression'
anova(object, ...)
```

## Arguments

- object:

  A `logistic_regression` result.

- ...:

  Passed to
  [`stats::anova.glm`](https://rdrr.io/r/stats/anova.glm.html).
