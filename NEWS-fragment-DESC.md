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
