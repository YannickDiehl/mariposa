# Residuals of a linear_regression model

Dispatches to
[`stats::residuals()`](https://rdrr.io/r/stats/residuals.html) for the
fitted `lm`; grouped and pairwise results raise an informative error.

## Usage

``` r
# S3 method for class 'linear_regression'
residuals(object, ...)
```

## Arguments

- object:

  A `linear_regression` result (ungrouped, listwise).

- ...:

  Passed to the `lm` method.

## Value

A numeric vector.
