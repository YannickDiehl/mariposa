# Confidence intervals for linear_regression coefficients

t-based intervals for B. For weighted models they use the SPSS
frequency-weight standard errors and df (`sum(w) - rank`) and equal the
CI columns of [`summary()`](https://rdrr.io/r/base/summary.html).

## Usage

``` r
# S3 method for class 'linear_regression'
confint(object, parm, level = 0.95, ...)
```

## Arguments

- object:

  A `linear_regression` result (ungrouped, listwise).

- parm:

  Coefficients to compute intervals for (names or indices; default all).

- level:

  Confidence level (default 0.95).

- ...:

  Not used.

## Value

A matrix with one row per coefficient and columns for the lower and
upper limits.
