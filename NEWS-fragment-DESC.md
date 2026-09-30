* `describe()` and the `w_*` functions with `na.rm = FALSE`: a variable
  with missing values now gets `NA` for every statistic, as in base R.
  The weighted quantile family returned values computed on shifted
  positions (the weighted median of `income` was 3800 instead of `NA`),
  and the unweighted `describe()` / `w_iqr()` aborted with a base-R
  `quantile()` error.
* `describe(show = "all")` locates the quantile columns by their exact
  names. The print matched them by a regular-expression prefix, so a
  second variable such as `income_Quintile` crashed the print and a name
  with special characters (`Einkommen (EUR)`) silently lost its quantiles.
* `describe()` rejects unknown `show` values with an error listing the
  valid ones (`show = "min"` used to print a table with nothing but N and
  Missing), names quantile columns with at most two decimals
  (`probs = 1/3` gives `Q33.33`, not `Q33.3333333333333`), no longer
  prints Q50 next to the identical Median, and reports undefined
  statistics of a constant variable as `NA` instead of `NaN`.
* `describe()` and the `w_*` functions on grouped data no longer crash when
  the selection contains a grouping variable (explicitly or through a
  helper such as `where(is.numeric)`); errors ranged from "'x' is NULL"
  and "quantile.haven_labelled() not implemented" to a dplyr internal
  error. As in `dplyr::across()`, grouping variables are excluded from the
  analysed variables, with a message.
* Weighted `describe()` prints N and Missing like SPSS FREQUENCIES with
  `WEIGHT BY`: N is the sum of the weights of the valid cases and Missing
  the weighted missing count (income: N 2201, Missing 315). The table used
  to show only Kish's effective sample size (2158.9) under the name
  `Effective_N` and no Missing column. The effective N stays available in
  `$results` (`<variable>_Effective_N`); `<variable>_Missing` is now the
  weighted missing count for weighted analyses.
* `describe()` output: the table is sized to its content (the borders were
  always 40 dashes), grouped output no longer stacks the table rule
  directly under the group underline, every statistic column uses the
  same number of decimals (no `50` next to `50.550`), and a table wider
  than the console is split into column blocks that each repeat the
  Variable column (the continuation block used to lose it).
* `w_mean()` and the other `w_*` functions (including `w_modus()`) print
  every group combination when data are grouped by two or more variables.
  The print iterated over the first grouping variable only: region x
  gender showed two blocks labelled "region = East"/"West" that silently
  contained the Male rows.
* The `w_*` functions print one uniform table: Variable, the statistic,
  N and Missing, one table per group. The prints used to differ across the
  family (raw column names such as `weighted_mean`/`Effective_N` under
  "--- var ---" headers, `w_modus()` as a raw tibble, `w_quantile()`
  repeating the weights name on every row). With weights, N and Missing
  are sums of weights as in SPSS; Kish's effective N (previously the only
  N shown) is displayed by the new `summary()` methods for all eleven
  `w_*` classes. `$results` has the same columns for one or several
  variables (`Variable`, the statistic, `n` or `weighted_n` +
  `effective_n`, `missing`); single-variable results no longer carry the
  duplicated raw columns (`age`, `age_n`, `age_eff_n`).
