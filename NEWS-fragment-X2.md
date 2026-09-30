* `dunn_test()` and `pairwise_wilcoxon()` store their comparison table as
  `$results`, like every other result class (it lived only in
  `$comparisons`, so `x$results` was `NULL`; `$comparisons` is kept). A
  weights-invariance regression test that compared these `NULL`s with
  each other now checks the real comparisons.
* Every analysis result can be turned into a plain table:
  `as.data.frame()` (and `tibble::as_tibble()`) now work for all result
  classes instead of failing with "cannot coerce class ... to a
  data.frame". The table has one row per test/variable/group (per pair
  for correlations and post-hoc tests, per term for ANOVA and regression
  tables, per cell for `crosstab()`, per item for `efa()` loadings); list
  columns are flattened (`t_test()` gets `group1`, `group2`, `mean1`,
  `mean2`, `sd1`, `sd2`) or dropped (the observed/expected tables of
  `chi_square()`), so `write.csv()` works; grouping variables are the
  leading columns, labelled ones as their value labels. With broom
  loaded, `broom::tidy()` now works for all test and descriptive classes
  (it covered only the two regressions), using broom's column names
  (`statistic`, `p.value`, `parameter`/`num.df`/`den.df`, `estimate`,
  `conf.low`, `conf.high`, `adj.p.value`, `method`).
