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
* `marginal_effects()` computes correct AMEs for transformed predictors.
  It perturbed one column of the model frame, so the transformed column
  kept its fitted values: for `y ~ x + I(x^2)` the AME of `x` was 0.466
  instead of 0.173. Variables that enter only transformed (`log(income)`,
  `poly(x, 2)`, `I(x / 10)`), character and logical predictors were
  dropped without a word. The AMEs are now rebuilt from the original data
  for each variable (all terms using it move together); character and
  logical predictors get discrete-change rows, and a numeric variable
  used only as `factor(x)` is skipped with a warning that says how to get
  its AMEs.
* `marginal_effects()` on a grouped model whose grouping variable is
  haven-labelled no longer aborts with "arguments imply differing number
  of rows: 1, 2".
* Weighted `linear_regression()`: `vcov()`, `confint()`, `nobs()`,
  `df.residual()`, `anova()`, `predict()` (standard errors/intervals),
  `broom::tidy()` and `broom::glance()` now use the SPSS frequency-weight
  convention of `summary()` (N = `sum(w)`, residual df = `sum(w) - rank`).
  They were inherited from `lm()`, which treats weights as analytic
  weights: with `weights = w * 3`, `summary()` showed SE = .0527 but
  `tidy()` .0916, and `nobs()` returned 2115 cases instead of N = 6388.
  Unweighted models are unchanged.
* `logistic_regression()` accepts every outcome with exactly two values,
  as SPSS does: 1/2 codings (lower value = 0, higher = 1), factors with
  an unused level (as `to_label()` leaves them; unused levels are
  dropped, the first remaining level = 0), labelled, character and
  logical outcomes. It used to demand 0/1 or a factor with exactly two
  levels. The output now says which category is modelled: the compact
  print shows `[P(y = category)]`, `summary()` starts with the SPSS
  "Dependent Variable Encoding" table, and the classification table is
  labelled with the categories instead of 0/1. A constant outcome
  (previously "Nagelkerke R2 = -Inf ... Accuracy = 100%") and outcomes
  with more than two values now stop with a clear message.
* Grouped `linear_regression()` and `logistic_regression()` no longer
  abort when one group cannot be fitted (too few cases, a constant
  outcome, ...). Like SPSS SPLIT FILE, the group is skipped with a
  warning naming it and the reason, the other groups are reported, and
  `print()`/`summary()` list the group as "not computed". Before, one
  small group stopped the whole analysis with "Insufficient observations
  for the number of predictors" without saying which group. The message
  itself now names an all-missing variable ("`x` has no non-missing
  values") or gives the case count.
* Model specification in `linear_regression()` / `logistic_regression()`:
  - `dependent =` / `predictors =` work with non-syntactic names such as
    `` `my var` `` or `` `Zufriedenheit (0-10)` `` (they were pasted into
    the formula without backticks: parse error).
  - `linear_regression(data, log(income) ~ age)` fits the transformed
    outcome instead of failing with "Variable(s) not found in data: log.";
    `logistic_regression()` says the outcome must be a single variable.
  - `y ~ .` uses all other columns except the weights and grouping
    variables; `y ~ 1` and an outcome that is also a predictor stop with
    a clear message.
  - A predictor selection that also picks the outcome, the weights or a
    grouping variable (e.g. `predictors = where(is.numeric)`) drops them
    with a message instead of regressing the outcome on itself.
  - Character predictors enter as factors (no more `NA` descriptives and
    base-R warnings).
  - `weights =` accepts an expression such as `sampling_weight * 2`
    (it failed with "Can't convert a call to a string").
  - `anova()` on a weighted logistic model no longer leaks
    "non-integer #successes" warnings.
* `update()` and `step()` work on `linear_regression()` and
  `logistic_regression()` results. The objects stored lm's internal call
  (`data = data_complete, weights = .wt`), so both failed with "object
  'data_complete' not found". The stored call is now the user's own call
  in formula form. A model fitted inside a `%>%` pipe (no data name to
  re-use) gets a clear error instead. `coef()`, `residuals()`,
  `fitted()`, `confint()`, `nobs()` and `vcov()` on grouped results (and
  all but `coef()` on pairwise results) now stop with an informative
  message instead of returning `NULL` or a base-R error; `coef()` of a
  pairwise regression returns its coefficients. `nobs()` of a weighted
  logistic model is the sum of the weights (SPSS N).
* Weighted `linear_regression(use = "pairwise")` uses the unrounded
  smallest pairwise sum of weights in its degrees of freedom, sums of
  squares and standard errors (Validation Charter §5.1); it was rounded
  first. The displayed N stays rounded; results move by a fraction of the
  rounding error.
