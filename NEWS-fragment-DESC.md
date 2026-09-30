* `describe()` and the `w_*` functions with `na.rm = FALSE`: a variable
  with missing values now gets `NA` for every statistic, as in base R.
  The weighted quantile family returned values computed on shifted
  positions (the weighted median of `income` was 3800 instead of `NA`),
  and the unweighted `describe()` / `w_iqr()` aborted with a base-R
  `quantile()` error.
