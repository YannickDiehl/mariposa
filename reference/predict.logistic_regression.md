# Predict from a logistic_regression model

For ungrouped results, dispatches to
[`stats::predict.glm`](https://rdrr.io/r/stats/predict.glm.html) (the
result inherits from `"glm"`). For grouped results, raises an
informative error pointing at `object$groups`.

## Usage

``` r
# S3 method for class 'logistic_regression'
predict(object, ...)
```

## Arguments

- object:

  A `logistic_regression` result.

- ...:

  Passed to
  [`stats::predict.glm`](https://rdrr.io/r/stats/predict.glm.html).
