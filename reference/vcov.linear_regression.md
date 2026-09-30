# Variance-covariance matrix of a linear_regression model

Unweighted models: [`stats::vcov()`](https://rdrr.io/r/stats/vcov.html)
of the `lm`. Weighted models: the covariance matrix under SPSS frequency
weights (residual variance with `sum(w) - rank` df), whose square-rooted
diagonal equals the Std.Error column of
[`summary()`](https://rdrr.io/r/base/summary.html).

## Usage

``` r
# S3 method for class 'linear_regression'
vcov(object, ...)
```

## Arguments

- object:

  A `linear_regression` result (ungrouped, listwise).

- ...:

  Passed to [`stats::vcov()`](https://rdrr.io/r/stats/vcov.html).

## Value

A square numeric matrix.
