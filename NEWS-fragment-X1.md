* Selecting a grouping variable of `group_by()` data (explicitly or through
  a helper such as `where(is.numeric)`) no longer analyses it within its
  own groups. `pearson_cor()`, `spearman_rho()` and `kendall_tau()` warned
  "No variance" in every group and returned `NA` rows, `reliability()`
  used the constant grouping variable as an item, `t_test()` warned
  "constant value" per group. As `dplyr::across()` does, every analysis
  function now leaves grouping variables out of the selection with a
  message (previously only `describe()` and the `w_*` functions did);
  `rec()`, `to_dummy()` and the row operations, which do not compute per
  group, still accept them.
