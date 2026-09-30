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
* A misspelled argument name is now an error that names the intended
  argument: "Unknown argument `weight` of `w_mean()`. Did you mean
  `weights`?". In functions whose `...` selects variables, `weight =`
  (for `weights`), `na_rm =`, `groups =`, `conf =` ... were taken as a
  tidyselect rename: `w_mean(data, age, weight = w)` returned an
  *unweighted* mean plus a bogus "weight" row, `binomial_test()` a garbage
  row, and `describe()`, `frequency()`, `chi_square()`, `kruskal_wallis()`
  failed with unrelated messages. Inside `summarise()` the `w_*` functions
  ignored `...` entirely (`w_mean(age, weight = w)` was silently
  unweighted). Renaming selections (`describe(data, Age = age)`) are refused
  with an explanation. For consistency, functions whose arguments R would
  partially match (`crosstab()`, `fisher_test()`, `mcnemar_test()`,
  `wilcoxon_test()`, `factorial_anova()`, `ancova()`, the regressions,
  `tukey_test()`, `scheffe_test()`, `marginal_effects()`) no longer accept
  abbreviated names such as `weight =` or `percent =`: the same typo now
  gives the same error everywhere. `fisher_test()` and `mcnemar_test()` no
  longer ignore unknown arguments.
