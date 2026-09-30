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
* `weights` accepts the same forms in every function: a bare column name,
  a column name as string (`"sampling_weight"`, `all_of(w)`, `!!w`), an
  expression evaluated in the data (`sampling_weight * 2`,
  `survey_data$sampling_weight`), or a numeric vector with one value per
  row. Outside the regressions only the bare column name worked:
  `weights = survey_data$sampling_weight` and `weights = all_of(w)` failed
  with "Can't convert a call to a string.", `weights = 1` with "Can't
  convert a double vector to a string". An expression is shown by its text
  ("Weights: sampling_weight * 2"). `weights = w` with `w` holding a column
  name now points to `all_of(w)`; a vector of the wrong length or an
  unknown variable inside an expression is named in the error. The `w_*`
  functions no longer accept a factor as weights (its level codes were used
  silently), and in `summarise()` a non-numeric `weights` is an error
  instead of a silent unweighted result. `std()` and `center()` return the
  weights column unchanged (it lost its attributes).
* Labelled data (`haven_labelled`) work when the haven package is not
  loaded, e.g. data restored with `readRDS()` in a fresh session.
  haven is only suggested, and without its namespace the labelled vectors
  have no methods for comparison and arithmetic: `frequency()` failed with
  "Can't convert `x` <haven_labelled> to <character>", `describe()` and
  `w_mean()` with "<haven_labelled_spss> * <double> is not permitted",
  `crosstab()`, `codebook()`, `unlabel()`, `drop_labels()`, `rec()` and
  `write_xlsx()` likewise. The entry helpers now load haven's namespace
  when a selected data set or vector is labelled (and say that haven is
  needed when it is not installed).
