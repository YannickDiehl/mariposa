# Update and re-fit a logistic_regression model

Re-runs
[`logistic_regression`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)
with a modified formula or arguments, e.g.
`update(model, . ~ . + income)`;
[`step()`](https://rdrr.io/r/stats/step.html) works through it. Needs
the data by name: a model fitted inside a `%>%` pipe cannot be updated.

## Usage

``` r
# S3 method for class 'logistic_regression'
update(object, formula., ..., evaluate = TRUE)
```

## Arguments

- object:

  A `logistic_regression` result (also grouped).

- formula.:

  Changes to the formula (see
  [`stats::update()`](https://rdrr.io/r/stats/update.html)).

- ...:

  Further arguments of
  [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)
  to change.

- evaluate:

  If `FALSE`, return the updated call.

## Value

A new `logistic_regression` result (or the call).
