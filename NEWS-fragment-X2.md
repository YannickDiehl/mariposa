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
* `write_xlsx()` exports analysis results. `crosstab()` results are
  written in the SPSS table layout (Count and the requested "% within"
  / "% of Total" rows per category, Total row and column; one block per
  group), every other result (e.g. `describe()`, `t_test()`,
  `oneway_anova()`, `reliability()`, `linear_regression()`) as its result
  table followed by the secondary tables (group descriptives, mean ranks,
  item statistics, model summary, ...). A named list may now mix data
  frames with any result (`list(Descriptives = describe(...), Data = df)`
  was rejected), and unsupported objects get a clear error instead of
  "no applicable method for 'write_xlsx'".
