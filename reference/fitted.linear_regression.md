# Fitted values of a linear_regression model

Dispatches to
[`stats::fitted()`](https://rdrr.io/r/stats/fitted.values.html) for the
fitted `lm`; grouped and pairwise results raise an informative error.

## Usage

``` r
# S3 method for class 'linear_regression'
fitted(object, ...)
```

## Arguments

- object:

  A `linear_regression` result (ungrouped, listwise).

- ...:

  Passed to the `lm` method.

## Value

A numeric vector.
