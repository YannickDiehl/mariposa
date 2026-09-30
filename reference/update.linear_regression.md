# Update and re-fit a linear_regression model

Re-runs
[`linear_regression`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)
with a modified formula or arguments, e.g.
`update(model, . ~ . + income)`;
[`step()`](https://rdrr.io/r/stats/step.html) works through it. Needs
the data by name: a model fitted inside a `%>%` pipe cannot be updated.

## Usage

``` r
# S3 method for class 'linear_regression'
update(object, formula., ..., evaluate = TRUE)
```

## Arguments

- object:

  A `linear_regression` result (also grouped or pairwise).

- formula.:

  Changes to the formula (see
  [`stats::update()`](https://rdrr.io/r/stats/update.html)).

- ...:

  Further arguments of
  [`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)
  to change.

- evaluate:

  If `FALSE`, return the updated call.

## Value

A new `linear_regression` result (or the call).
