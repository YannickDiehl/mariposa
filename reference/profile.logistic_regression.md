# Profile likelihood for a logistic_regression model

Dispatches to the `glm` method
([`stats::profile()`](https://rdrr.io/r/stats/profile.html)); used by
`confint(model, method = "profile")`.

## Usage

``` r
# S3 method for class 'logistic_regression'
profile(fitted, ...)
```

## Arguments

- fitted:

  A `logistic_regression` result (ungrouped).

- ...:

  Passed to [`stats::profile()`](https://rdrr.io/r/stats/profile.html).

## Value

A `"profile"` object, as returned for `glm` fits.
