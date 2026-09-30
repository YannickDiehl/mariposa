# Standard glm accessors for logistic_regression results

[`coef()`](https://rdrr.io/r/stats/coef.html),
[`residuals()`](https://rdrr.io/r/stats/residuals.html),
[`fitted()`](https://rdrr.io/r/stats/fitted.values.html) and
[`vcov()`](https://rdrr.io/r/stats/vcov.html) dispatch to the `glm`
methods for an ungrouped model; a grouped result raises an informative
error (each element of `$groups` is a fitted model).
[`nobs()`](https://rdrr.io/r/stats/nobs.html) returns the number of
cases, or for a weighted model the unrounded sum of the frequency
weights (SPSS N, shown rounded in the summary).

## Usage

``` r
# S3 method for class 'logistic_regression'
coef(object, ...)

# S3 method for class 'logistic_regression'
residuals(object, ...)

# S3 method for class 'logistic_regression'
fitted(object, ...)

# S3 method for class 'logistic_regression'
vcov(object, ...)

# S3 method for class 'logistic_regression'
nobs(object, ...)
```

## Arguments

- object:

  A `logistic_regression` result.

- ...:

  Passed to the `glm` methods.

## Value

As the corresponding `glm` method.
