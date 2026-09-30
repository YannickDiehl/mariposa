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
* `w_*` input checks: `w_mean(1:4, weights = c(1, 3))` says that the
  lengths differ (was a cryptic base-R error), `w_mean(c("a", "b"))` says
  that a numeric vector is needed (was "data must be a data frame"), and
  `w_mean(survey_data, gender)` names the non-numeric variable (was `NA`
  plus a base-R warning).
* `w_modus()`: when several values share the highest frequency, the
  smallest value (first factor level) is returned for weighted and
  unweighted data alike, as in SPSS (the weighted version took the first
  value in data order), `$results$n_modes` counts the tied values, and
  the print flags the result ("Multiple modes exist").
* Weighted `frequency()` of several labelled variables no longer aborts
  with "Can't convert ... due to loss of precision" depending on the order
  of the variables. The weighted branch kept the `haven_labelled` class in
  the value column, so combining variables with different label sets
  failed; values are now bare numbers, as in the unweighted branch.
* `frequency()` no longer lists empty factor levels (e.g. "Student 0" after
  `filter(employment != "Student")`) unless `show_unused = TRUE`, as SPSS
  FREQUENCIES lists observed values only; with `show_unused = TRUE` they
  now also appear in weighted tables.
* `frequency(show_labels = "auto")` also shows the Label column when a
  labelled missing-value code occurs (ALLBUS `age`: -32 "NICHT
  GENERIERBAR"); the automatic mode only looked at the labels of valid
  values, so metric variables printed their missing codes without the
  explaining label.
* `frequency()` tables follow the SPSS FREQUENCIES layout: valid
  categories, "Total valid", the missing categories, "Total missing" (only
  with two or more missing categories) and a grand "Total" row (N and
  100 %). Cells without a value are empty instead of reading "NA"; plain
  numeric, logical and character variables no longer end with two rows
  both called "Total", and tagged missing values no longer end with a
  "NA(total)" row. Further:
  - `show_valid = FALSE` also hides the cumulative percentages (they are
    cumulative *valid* percentages).
  - every column is sized to its content: large weighted counts were cut
    to "2798..." by a fixed 8-character N column.
  - factors, character and logical variables show their categories once
    (Value and Label columns used to repeat each other), and the summary
    line leaves out statistics that do not exist ("mean=NA sd=NA",
    "mean=NaN" for an all-missing variable).
  - labels are left-aligned and never cut (they were cut at 40
    characters); when the table is wider than the console, long labels
    wrap onto extra lines.
  - an all-missing variable prints its missing rows and the Total without
    a "Valid % 100.00" of zero cases or an R warning.
  - `print(x, digits = 0)` rounds the percentages only; the summary line
    keeps two decimals.
* Weighted `crosstab()` reproduces SPSS CROSSTABS' default
  `/COUNT ROUND CELL`: every weighted cell count is rounded first, the
  margins are sums of the rounded cells and percentages, expected counts
  and adjusted residuals come from the rounded table. Cells used to be
  rounded only for display while margins and percentages came from the
  unrounded sums (402 + 447 shown with a margin of 848; ALLBUS DIVERS
  15 | 3 with a row percentage of 81.7). Weighted counts, margins and
  percentages now match the SPSS reference exactly (validated for the
  weighted ungrouped and grouped scenarios).
* `crosstab()`: the Total row shows every requested percentage as SPSS
  CROSSTABS does (row % = column shares, col % = 100 %, total % = column
  shares). With `percentages = "col"` it showed the column shares
  labelled "col %", with `"row"`/`"total"` it had no percentage line and
  with `"all"` only that mislabelled line.
* `crosstab(digits = 2)` is honoured by `print()` and `summary()`; their
  own default of 1 decimal overrode the value stored by `crosstab()`.
* `crosstab()`: `na.rm = FALSE` now keeps cases with a missing value as
  their own "NA" row or column (it had no visible effect),
  `summary(x, percentages = FALSE)` says "Counts only" instead of "Row
  percentages", and `crosstab(data, gender)` without a column variable
  gives a clear error (was the base error 'argument "x" is missing').
* `crosstab()` shows observed categories only, as SPSS CROSSTABS does: an
  empty factor level (e.g. after `filter(employment != "Student")`) used
  to print a "0 0 0" row with a row percentage of "100.0%" of zero cases.
* Weighted `crosstab()` reports missing cases like SPSS's Case Processing
  Summary: as the sum of their weights (it printed an unweighted count
  next to the weighted "N (valid)").
* `crosstab()` output: category labels are shown in full (they were cut
  at 20 characters, so eight ALLBUS ISCO categories all read
  "FUEHRUNGSKRAEFTE,..."), each column is as wide as its own content
  (every column took the width of the widest label: sex x educ was 182
  characters wide), long column headings and row labels wrap when the
  table would be wider than the console, and the title and the column
  spanner show the variable labels, as SPSS does, instead of the
  variable names.
