* `linear_regression()`, `logistic_regression()` and `marginal_effects()`
  print models with long formulas correctly. A formula longer than about
  70 characters (a normal model with five or more predictors) was split
  into two lines: the compact title was printed twice and `summary()`
  aborted with "'length = 2' in coercion to 'logical(1)'". The formula is
  now always shown on one line.
* `confint()` and `profile()` work on `logistic_regression()` results.
  Both failed with "incorrect number of dimensions" because profiling
  called `summary()` and got mariposa's SPSS-style summary instead of the
  glm one. `confint()` now returns Wald intervals by default - the SPSS
  "95% C.I. for EXP(B)" that `summary()` prints (`exp(confint(model))`
  reproduces its Lower/Upper columns); `confint(model, method =
  "profile")` gives glm's profile-likelihood intervals.
  `broom::tidy(model, conf.int = TRUE)` uses the same Wald intervals and
  no longer prints dozens of "non-integer #successes" warnings for
  weighted models.
