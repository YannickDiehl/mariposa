# Residual degrees of freedom of a linear_regression model

Weighted models: `sum(w) - rank` (SPSS frequency weights, non-integer);
unweighted: the `lm` value.

## Usage

``` r
# S3 method for class 'linear_regression'
df.residual(object, ...)
```

## Arguments

- object:

  A `linear_regression` result (ungrouped, listwise).

- ...:

  Not used.

## Value

A single number.
