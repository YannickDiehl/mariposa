* `linear_regression()`, `logistic_regression()` and `marginal_effects()`
  print models with long formulas correctly. A formula longer than about
  70 characters (a normal model with five or more predictors) was split
  into two lines: the compact title was printed twice and `summary()`
  aborted with "'length = 2' in coercion to 'logical(1)'". The formula is
  now always shown on one line.
