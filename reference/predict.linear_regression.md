# Predict from a linear_regression model

For listwise + ungrouped results, dispatches to
[`stats::predict.lm`](https://rdrr.io/r/stats/predict.lm.html) (the
result inherits from `"lm"`). For grouped or pairwise results, raises an
informative error.

## Usage

``` r
# S3 method for class 'linear_regression'
predict(object, ...)
```

## Arguments

- object:

  A `linear_regression` result.

- ...:

  Passed to
  [`stats::predict.lm`](https://rdrr.io/r/stats/predict.lm.html).

## Value

A numeric vector of predictions (or a matrix/list, depending on the
arguments), as returned by
[`stats::predict.lm`](https://rdrr.io/r/stats/predict.lm.html).
