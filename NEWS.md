# mariposa 0.7.4 (development)

Accumulates all changes since 0.7.3 went live on CRAN (see
`.claude/VERSIONING_POLICY.md` §5, CRAN cadence).

## Breaking changes

Result objects and defaults that change in ways existing code can notice.
All of them move mariposa closer to SPSS or remove silently wrong output;
none has a deprecation bridge because the old values were wrong or
misleading (VERSIONING_POLICY §4.2). By maintainer decision these changes
ship as 0.7.4 rather than a MINOR bump (a one-off exception to
VERSIONING_POLICY §3).

* One-sample `t_test()`: `mean_diff` and its CI are now the difference
  mean - `mu` as in SPSS's One-Sample Test (before: the mean itself, e.g.
  3.628 instead of 0.628 for `mu = 3`).
* `tukey_test()`/`scheffe_test()`: one row per pair in SPSS's
  "(I) - (J)" orientation on every path (before: `"B-A"` unweighted,
  `"A - B"` weighted), so unweighted differences can change sign; new
  `SE` and `t_value` columns.
* Grouped `levene_test()`: results carry the group-key columns instead of
  one `Group` string, the `sig` column is gone, values are no longer
  rounded, and the grouped method no longer has a `variable` argument
  (analysis variables go through `...` as in the data-frame method).
* `w_*()` results use one long format for all eleven functions (columns
  such as `income`, `income_n`, `income_eff_n` are replaced by `Variable`,
  the statistic, `n`, `weighted_n`, `effective_n`, `missing`).
* Weighted `describe()`: the N column is the sum of weights (SPSS
  FREQUENCIES/DESCRIPTIVES), no longer Kish's effective n; `Missing` is
  weighted.
* Weighted `crosstab()`: cell counts follow SPSS's default `/COUNT ROUND
  CELL` (rounded cells, margins and percentages from the rounded cells).
* `chi_square()`: with `correct = TRUE`, Phi and Cramér's V come from the
  Pearson chi-square (SPSS Symmetric Measures); the Goodman-Kruskal gamma
  p-value now uses SPSS's ASE0 (it was wrong before, e.g. .614 instead of
  .460); gamma is only reported for ordinal pairs; `label_maps` is gone
  (the tables carry the labels in their dimnames).
* `binomial_test()`: Group 1 is the category of the first valid case in
  the data (per group), as SPSS NPAR TESTS /BINOMIAL defines it (before:
  the lowest code), and a test proportion other than 0.5 gives SPSS's
  one-tailed exact p-value in the observed direction (new column
  `alternative`). Before, `p = 0.6` could test the other category
  two-tailed: p = 8e-69 instead of SPSS's .011 (reference Test 1d, now
  asserted). The p-values come from binomial tails, so weights summing to
  billions no longer exhaust memory.
* `wilcoxon_test()`/`pairwise_wilcoxon()`: Z follows SPSS (from the smaller
  rank sum, never positive) with the direction in the new `z_based_on`
  column; Phi of a 2x2 table (`chi_square()`, `phi()`) is signed like
  SPSS's. Both were positive where SPSS prints a negative value (see "SPSS
  parity audit").
* Rank tests (`mann_whitney()`, `kruskal_wallis()`, `wilcoxon_test()`,
  `friedman_test()`): nominal factors are an error; ordered factors are
  ranked by level order everywhere.
* `print.fisher_test()` default `digits` 4 -> 3.
* Weighted `linear_regression()`: `vcov()`, `confint()`, `nobs()`,
  `df.residual()`, `tidy()` and `glance()` use SPSS's frequency-weight df
  (N = sum of weights), matching the summary (before: R's analytic-weight
  df, so SEs and CIs disagreed with the summary).
* `logistic_regression()`: `confint()` and `broom::tidy(conf.int = TRUE)`
  return Wald intervals (the SPSS "C.I. for EXP(B)"); profile likelihood
  via `confint(method = "profile")`.
* `pearson_cor(alternative = "less"/"greater")` returns a one-sided CI;
  the pairwise summary table no longer shows r² (still in
  `$correlations`).
* Grouped regressions, tests and scale analyses skip a group that cannot
  be computed (with a warning naming it) instead of aborting the whole
  call; skipped groups are listed in the result.
* `efa()`: components/factors are reflected to SPSS's sign convention
  (positive loading sum), so signs can flip relative to 0.7.3; the ML
  compact line reports the extraction variance.
* `reliability()`: an item with zero variance is removed from the scale
  (with a warning), so alpha changes where such an item was included;
  omega from a Heywood solution is `NA`.
* `ancova()` "Parameter Estimates" use SPSS indicator coding with the last
  category as reference (term names change); `factorial_anova()` and
  `ancova()` stop with an error on a constant dependent variable.
* `rec(rules = "rev")` reverses on the scale range (valid value labels
  plus observed values) instead of the observed range; `rec()` returns a
  `haven_labelled` vector when labels exist.
* `to_label()` on a data frame skips metric variables whose valid values
  carry no labels; duplicate label texts become distinct levels
  ("<label> (<code>)").
* `strip_tags()`/`untag_na()` keep the value labels (labelled result);
  `na_frequencies()` reports numeric codes in code order.
* `std()`, `center()`, `pomps()` and `to_numeric()` return plain `NA` for
  SPSS missing values (SPSS COMPUTE semantics).
* `row_count(count = c(4, 5))` counts cases with any of the values (SPSS
  `COUNT`); it used to recycle the vector.
* `frequency()` hides empty factor levels unless `show_unused = TRUE`.
* Arguments must be spelled out: a named argument inside a variable
  selection (`w_mean(data, age, weight = w)`) or an abbreviation
  (`crosstab(..., percent = "col")`) is an error naming the intended
  argument (before: `weight = w` was silently treated as a second
  variable and the result was unweighted).
* Grouping variables are dropped from variable selections under
  `group_by()` (with a message), as `dplyr::across()` does.
* Weights that contain no positive value are an error (before:
  `describe()` fell back to an unweighted analysis with a warning).
* `efa()` rotations follow SPSS FACTOR's own varimax, direct oblimin and
  promax algorithms (Kaiser normalization, SPSS stopping rules): rotated
  loadings change slightly (varimax up to .004, promax factor correlations
  noticeably, e.g. .055 -> -.002), and oblimin no longer needs
  GPArotation. ML extraction uses SPSS's Heywood bound (.999) and keeps
  the eigenvalue order of the factors.
* `codebook()` returns its result visibly (it prints the overview after
  opening the viewer); `std()`/`center()` no longer add the weights column
  to the returned data; `dunn_test()`/`pairwise_wilcoxon()` also expose
  their table as `$results`.

## Output

* `frequency()`, `crosstab()`, and `multiple_response()` now print
  their full tables directly, matching `describe()`: the result of a
  descriptive table function *is* a table, so `print()` no longer shows
  a compact placeholder with a "Use summary()" hint. `print(x)` is now
  identical to `print(summary(x))` with default toggles; `summary()`
  remains the place for section toggles (`frequency_table=`,
  `residuals=`, ...) and `digits=`. The compact one-line `print()` is
  unchanged for the hypothesis-test and model classes.

## Bug fixes (2026-09 field report)

* `logistic_regression()` with fractional `weights` no longer leaks the
  "non-integer #successes in a binomial glm!" warning under non-English
  locales (e.g. "Nicht-ganzzahlige #Erfolge in einem binomial-GLM"). The
  muffling filter matched the English text only; it now rebuilds the
  message through R's own translation catalog.
* `linear_regression()`: when a predictor is excluded for perfect
  collinearity, Tolerance and VIF of the retained terms are now computed
  without the excluded column. Previously the singular correlation matrix
  yielded either `NA` (so `summary(collinearity = TRUE)` printed no
  table) or rounding artefacts such as VIF = -2.85e13.
* `kendall_tau()` is dramatically faster. The pair counts came from an
  element-wise R loop over all n(n-1)/2 pairs, and on labelled
  (`haven_labelled`) data every element access went through vctrs
  dispatch: 400 labelled cases took 6.5 s, the full ALLBUS sample more than
  20 minutes per pair. The counts now come from a vectorized kernel
  (`.kendall_pair_counts()` in `R/kernels-weighted.R`, exploiting that the
  weighted pair weight sqrt(w_i * w_j) is separable), and the correlation
  engine strips label classes before computing. Full ALLBUS 2023: 6 pairs
  in 0.1 s. Results are unchanged.
* `group_by() %>% t_test()` no longer aborts with "arguments imply
  differing number of rows: 1, 0" when a group lacks one level of
  `group`. The per-group fallback itself built a 0-row data frame and
  swallowed the real reason. The group now gets an all-`NA` row, a warning
  naming the group and the reason, and `print()`/`summary()` say
  "not computed for this group" instead of showing `NA` tables.
* `mann_whitney()` and `kruskal_wallis()`: a variable/group that cannot be
  tested (e.g. a group lacking a level under `group_by()`) no longer
  vanishes silently from `$results`. The error handler assigned its `NA`
  row in its own scope, so the row was discarded. The row is now kept, the
  warning names the group, and the print methods say "not computed for
  this group".
* Grouped output with a missing group key (`NA`, including tagged NAs from
  SPSS files) is printed correctly. All grouped print methods selected a
  group's rows with `==`, which is `NA` for a missing key: every group got
  an extra all-`NA` ghost row, and the `NA` group showed only `NA` rows
  instead of its statistics (`group_by() %>% describe()` was the reported
  case; the pattern sat in ~30 print paths). Matching now goes through one
  NA-safe helper.
* Group headers in grouped output show factor levels and value labels
  instead of codes: the compact `print()` of ~20 classes showed
  `[region = 1]` instead of `[region = East]`, and labelled group
  variables showed their numeric codes everywhere (including the
  `summary()` of `reliability()`/`efa()` and the grouped regression
  output).
* Statistics of an empty (all-missing) variable or group are `NA`
  instead of `NaN`/`-Inf`: `describe()` reported Mean = `NaN` and
  Range = `-Inf` (plus two R warnings from `min()`/`max()`), and the
  unweighted `w_mean()` returned `NaN`. The mean/range kernels and the
  unweighted branch of the `w_*` factory now short-circuit empty input
  the way the weighted branch already did.
* `to_label()` now keeps the original code of every level in a `"codes"`
  attribute, and `to_numeric()` / `to_labelled()` restore those codes.
  Previously the round trip renumbered the values sequentially (6 -> 3,
  42 -> 4, 90 -> 5, ...), and `to_numeric(keep_labels = TRUE)` attached
  labels with the wrong codes. `use_labels = FALSE` still gives the
  sequential 1..k numbering. The attribute survives dplyr verbs; after
  base subsetting (`x[i]`) or renaming levels, the level-based conversion
  applies as before.
* The pipe `%>%` is re-exported, so the documented
  `survey_data %>% describe(age)` works after `library(mariposa)` alone
  (it was only imported, giving 'could not find function "%>%"'). No
  masking message when dplyr is attached as well (identical object).
* `spearman_rho()` no longer presents itself as a weighted correlation.
  Weights only filter cases (SPSS NONPAR CORR convention), but the output
  said "[Weighted]" / "Weighted Spearman's Rank Correlation Analysis", and
  the help page's description and example spoke of weighted correlations.
  The output now names the weights variable as "case filter only".
* The correlation vignette's method comparison showed Pearson's r three
  times: it read a non-existent `$correlation` column for Spearman and
  Kendall (`NULL`), and `data.frame()` recycled the Pearson value. It now
  reads `$rho` and `$tau`.
* `codebook()` on a full SPSS survey file is ~6x faster (ALLBUS 2023,
  5,246 x 579: 20.4 s -> 3.1 s), and `frequency()`, `to_label(drop_na =
  FALSE)`, `unlabel()`, `drop_labels()` and the Stata/SAS readers speed up
  on data with many missing values. Six places read tagged-NA letters with
  `vapply(x, haven::na_tag, ...)`, splitting labelled vectors into one
  vctrs object per element; they now share one vectorized reader.
* Weights read from SPSS files work everywhere. A weight imported by
  `read_spss()` is `haven_labelled_spss` with an `na_range` and format
  attributes; once it contained `NA`, every comparison on it (`w < 0` in
  the weights check, `w > 0` filters, ...) failed inside haven's cast
  with "missing value where TRUE/FALSE needed", so every weighted function
  aborted (reported for `crosstab()`, affected all of them). Weights are
  now converted to plain numbers at the entry point.
* `crosstab()` now validates weights through the package-wide policy:
  negative weights are an error (previously only a warning, unlike every
  other weighted function since 0.6.4).
* `chi_square()` excludes cases with a missing weight. `NA` weights were
  summed into the table and made `chisq.test()` abort ("all entries of
  'x' must be nonnegative and finite").

## Practice-test fixes (2026-09/10)

Found by a seven-agent practice test of the package in everyday analysis
use (`survey_data` and the ALLBUS 2023 SPSS file), each finding reproduced
before it was fixed; SPSS-validated numbers only moved where the SPSS
reference output showed mariposa was wrong.

### Descriptive statistics

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
* `multiple_response()` on data grouped by a labelled variable (e.g.
  ALLBUS `eastwest`) no longer aborts with "arguments imply differing
  number of rows: 1, 2".
* `multiple_response(by = )` with a labelled `by` variable heads the
  crosstab columns with the value labels in code order (it showed the
  codes, e.g. "1"/"2" for ALLBUS `eastwest`).
* `multiple_response()` tables: counts are whole numbers (unweighted
  counts printed as "583.0"; weighted counts had one decimal in the
  frequencies table but none in the crosstab), the frequencies table ends
  with a Total row, and the crosstab has a Total column and a
  "Total (cases)" row as in SPSS MULT RESPONSE (replacing the unwrapped
  "Cases per column" footer). Factor or character indicators whose levels
  do not contain `counted` (e.g. "no"/"yes" with the default
  `counted = 1`) are an error instead of silently counting 0 mentions.
* `codebook()` console output (`print()`/`summary()`) writes counts
  without thousands separators ("2500 observations"), like every other
  table in the package and SPSS's default output.

* `describe()` gains `show = c("min", "max")`, SPSS DESCRIPTIVES'
  Minimum and Maximum (also part of `show = "all"`). Before `show` was
  validated, these names were silently ignored.

### Parametric tests

* `levene_test(center = "median")` on a weighted `factorial_anova()` is an
  error instead of silently returning the unweighted test (the median
  recomputation used the raw, unweighted values). The mean-based test,
  which SPSS UNIANOVA reports for weighted models, is unchanged.
* `t_test()`, `oneway_anova()`, `factorial_anova()`, `ancova()` and the
  post-hoc tests on their results order the groups of a numeric or
  labelled grouping variable by code, as SPSS does, and show value labels
  instead of codes. `t_test()` took the group order from the order of
  appearance in the data, so sorting the data flipped the sign of t and
  of the mean difference; labelled groups printed as "Groups compared:
  1 vs. 2", Tukey/Scheffe rows as "1 - 2", and factorial descriptives and
  ANCOVA marginal means by code. (PAR-02, PAR-10)
* A dependent variable without variance is no longer "tested".
  `oneway_anova()` on a constant variable reported F = 7256 *** (weighted)
  or F = 0.992 computed from floating-point noise in sums of squares that
  are exactly 0, `factorial_anova()` F = 4.011 *, and `tukey_test()`
  adjusted p-values. `oneway_anova()`, `t_test()`, `tukey_test()` and
  `scheffe_test()` now report such a variable (and one without any
  non-missing value) as "not computed (<reason>)" with a warning naming the
  variable; the other variables of the call are still tested.
  `factorial_anova()` and `ancova()` stop with a clear error. `t_test()`
  no longer aborts a multi-variable call with the raw base-R message
  (German locale: "Daten sind praktisch konstant", printed twice as
  "t_test() failed: ... / Caused by: ..."), no longer reports an all-missing
  variable as a grouping problem ("Found 0 levels"), and a grouping
  variable with 3 or more groups gives one error that points to
  `oneway_anova()`. (PAR-01, PAR-24)
* A group with a single case (weighted: a sum of weights <= 1) no longer
  aborts `oneway_anova()` and `t_test()` with the raw base-R error "not
  enough observations" from the Welch part. As in SPSS, the classical
  ANOVA / Student's t-test is computed and Welch's test is marked "not
  computed" with the reason; `t_test(var.equal = FALSE)` then reports
  Student's t with a warning. A group with zero variance no longer
  prints a Welch row "NaN 3 NaN NA <NA>" (weighted) or an infinite Glass'
  Delta, and the Welch block of `summary(oneway_anova())` is titled
  "Robust Tests of Equality of Means" (SPSS) instead of "Assumption
  Tests", with df2 shown with decimals (1229.456, not 1229). (PAR-07,
  PAR-17)
* Grouped `oneway_anova()`, `tukey_test()`, `scheffe_test()` and
  `levene_test()`: a group that cannot be tested (e.g. only one level of
  `group` present in that group) is reported with a warning that names the
  group label and the reason. `oneway_anova()` printed a silent "Results
  not available", the post-hoc tests dropped the group without a word,
  and `levene_test()` warned "in group 1" (the factor code). (PAR-18,
  EDGE-13)
* One-sample `t_test()` follows the SPSS One-Sample Test: `mean_diff` is
  now the mean minus the test value `mu` (was: the mean itself, e.g.
  3.628 instead of 0.628) and `conf_int_lower`/`conf_int_upper` are the
  confidence interval of that difference. The weighted interval now
  follows `alternative` (it was always two-sided). `summary()` shows the
  test value, alternative, confidence level and a One-Sample Statistics
  table (N, Mean, Std. Deviation, Std. Error Mean) and no longer prints an
  effect-size legend for effect sizes a one-sample test does not have.
  (PAR-05, PAR-06)
* `summary(t_test())` prints SPSS-style tables: a Group Statistics table
  (N, Mean, Std. Deviation, Std. Error Mean per group; SD and SE were
  missing) and an Independent Samples Test table with t, df, p, mean
  difference, its standard error and the confidence interval for both
  variance assumptions. Significance stars now come from the exact
  p-value (p = 0.0008 was rounded to 0.001 first and got "**", p = 0.0497
  printed as "0.05" without a star); p-values print as "<.001"/".308"
  instead of a bare "0", df as 2419 / 2384.147, weighted N as whole
  numbers ("1149", not "1149.0"), and `digits` applies to every column.
  Tables with umlaut labels stay aligned. (PAR-15, PAR-21, PAR-26)
* `summary(oneway_anova())` prints an SPSS-style Descriptives table (N,
  Mean, Std. Deviation, Std. Error and the confidence interval of each
  group mean at `conf.level`) and formats the ANOVA, Welch and effect-size
  tables with fixed decimals and `digits`. With `conf.level = 0.99` the
  header said 99% but no interval was printed anywhere; weighted N printed
  as "618.0", Mean Square as "289.79" next to "1.077", and `digits` had no
  effect. `tukey_test()` and `scheffe_test()` keep their own `conf.level`
  (default 95%): as in SPSS, the post-hoc interval follows the post-hoc
  alpha, not the ANOVA's confidence level (reference Tests 6a/6b).
  (PAR-19, PAR-21)
* `group_by() %>% levene_test()` takes its variables through `...` like
  the ungrouped method: several variables, tidyselect helpers
  (`starts_with("trust")`) and `group = "education"` as a string work, and
  `group`/`weights` must be named. The old grouped method had the
  signature `(x, variable, group, weights)`, so a second variable was
  silently used as `weights` ("[Weighted] F(3, 1544896) = 40250"). Grouped
  results now carry the group keys as columns: two `group_by()` variables
  no longer print "region = East, gender = East", and a missing key (`NA`)
  is its own group instead of a false "constant values" warning with
  "F(NA, NA) = ,". An invalid `center` is an error, a variable without
  variance prints "not computed (no variance ...)", and `weights` follow
  the package policy (negative or non-numeric weights are an error; they
  were accepted). (PAR-03, PAR-12, EDGE-04)
* `summary(levene_test())` prints one SPSS-style table per group (Levene
  Statistic, df1, df2, Sig.) with fixed decimals and `digits` (p printed as
  a bare 0, df2 as 474.2032 next to 3), and its recommendation fits the
  design: Welch's ANOVA for three or more groups, Welch's t-test only for
  two groups, a caution for factorial designs. It used to recommend
  "Welch's t-test" after `oneway_anova()` and `factorial_anova()`. The
  compact line no longer ends in "p = 0.125 , variances equal". (PAR-14,
  PAR-21)
* `tukey_test()` and `scheffe_test()` report every comparison in the SPSS
  "(I) - (J)" orientation (I before J in the category order, difference =
  mean(I) - mean(J)) with a spaced separator, identically for the
  unweighted, weighted, Scheffe and `factorial_anova()` paths, and add the
  standard error of the difference. Unweighted Tukey rows came from
  `TukeyHSD()` as "Intermediate Secondary-Basic Secondary 0.497" (later
  minus earlier, unspaced) while weighted Tukey and Scheffe printed
  "Basic Secondary - Intermediate Secondary -0.490". The comparison tables
  stay aligned with umlaut labels (they were padded by bytes). (PAR-11,
  PAR-22)
* `factorial_anova()` and `ancova()` output: the Tests of Between-Subjects
  Effects table no longer wraps at 80 columns (Partial Eta Squared and the
  stars moved into a second block) and shows sums of squares with fixed
  decimals instead of scientific notation (1.754652e+09); the header reads
  "Sum of squares: Type III" instead of "Type III Sum of Squares: Type 3";
  Levene's test prints "p < 0.001 ***" instead of "p = <.001"; the
  compact print shows N once in its title instead of on the last effect
  line only; descriptives, parameter estimates and marginal means honour
  `digits`. The `?factorial_anova` examples no longer call a
  non-existent `summary(marginal_means = FALSE)` toggle. (PAR-20, PAR-26)
* `ancova()` Parameter Estimates follow the SPSS UNIANOVA coding: one row
  per category ("[education=Basic Secondary]"), the last category of each
  factor (and every interaction cell involving it) is the reference and
  shown as a redundant 0, and the intercept is SPSS's. They are now
  validated against the SPSS reference output. R's internal contrasts
  leaked into the table before (education.L/.Q/.C for the ordered
  `education`, gender1, a different intercept). Tiny coefficients print
  in e-notation instead of "0.000 [0.000, 0.000]". With two or more
  factors, `ancova()` also reports the main-effect marginal means (SPSS
  `/EMMEANS=TABLES(factor)`, new element `emm_main_effects`) besides the
  cell means. (PAR-16, EDGE-25)
* `factorial_anova()` and `ancova()` with an empty design cell (e.g. no
  women with a university degree after filtering): `ancova()` no longer
  crashes with "Tibble columns must have compatible sizes", and effects
  whose Type III hypothesis has no degrees of freedom are reported as
  "not computed (not testable: the design has empty cells)" with a
  warning that lists the empty cells, instead of "F(0, 2208) = NaN, p =
  NA" (factorial) or F = -Inf (ANCOVA). The Corrected Model df is the
  rank of the design minus 1, as in SPSS (unchanged for complete designs).
  (PAR-08, PAR-09)
* `factorial_anova()` and `ancova()` honour `group_by()`: one complete
  analysis per group, with the group keys as leading columns of the
  result tables, per-group `print()`/`summary()` output, and grouped
  `tukey_test()`, `scheffe_test()` and `levene_test()` on the result. Both
  ignored the grouping silently and reported one pooled table. A group
  that cannot be analysed (e.g. no variance, a factor with one level) is
  skipped with a warning naming the group. The weighting of each group's
  fit is that of an ungrouped call. (PAR-04, EDGE-02)
* Clear errors for arguments that do not exist: `t_test(paired = TRUE)`
  explains that paired t-tests are not supported and points to
  `wilcoxon_test()` (was: "Can't select columns with TRUE");
  `t_test(x = , y = )` explains that variables come from `data` (was:
  "Variable x is not numeric"); `normality_test(weights = )` says the
  tests are unweighted by design and `normality_test(group = )` points to
  `group_by()` (was: "Variable weights/group is not numeric"). The
  errors of `tukey_test()`, `scheffe_test()` and `levene_test()` for
  unsupported objects now mention `factorial_anova()` results, which work
  as well. (PAR-23, PAR-24)
* The compact `oneway_anova()` line no longer leaves a dangling space
  ("p = 0.396 , eta2 = ...") and prints the total N also when the weights
  sum to more than 2^31 (was "N = NA"). (PAR-21)
* `normality_test()` names the variable (and, for grouped data, the
  group) it cannot test - no variance or fewer than 3 valid values - in a
  warning and prints "not computed (<reason>)" instead of silent "n/a"
  results; the Tests of Normality table stays aligned with umlaut
  variable names. (EDGE-13)
* `summary(t_test())` with weights prints the Student and one-sample df as
  whole numbers like SPSS (2435, not 2434.609) and the Welch df with
  decimals; the unrounded values stay in `$results`. (PAR-21)

### Non-parametric and categorical tests

* Exact tests with expansion (population) weights: the 2x2 Fisher
  p-value and the exact McNemar/binomial p-values come from distribution
  tails instead of enumerating every possible table, with the same result
  as `fisher.test()`/`binom.test()`. Weights summing to billions used to
  take minutes or run out of memory; an r x c `fisher_test()` above 2^31
  weighted cases is now a clear error suggesting to rescale the weights
  (it failed with "cannot allocate memory block of size 134217728 Tb").
* `chisq_gof(expected = )` is applied to every selected variable. With
  several variables it was silently dropped (equal proportions were
  tested while the summary header still showed the custom proportions);
  a variable whose categories do not fit `expected` is now a clear error.
  A named `expected` vector is matched by category name instead of by
  position (`c(Female = .3, Male = .7)` gave Male 30%). As with SPSS
  `/EXPECTED=50 30 20`, counts or other relative frequencies are accepted
  and divided by their sum; proportions that sum to about 1 (e.g. 0.995
  from rounding) are rescaled with a message instead of shrinking every
  expected count, and proportions that clearly do not sum to 1 are an
  error. Categories with an expected count below 5 now trigger a warning,
  as in `chi_square()`, and a group that cannot be tested is reported
  with a warning naming the group instead of an unexplained `NA` row.
* `chi_square()`, `phi()`, `cramers_v()`, `goodman_gamma()` and
  `chisq_gof()` use the categories that actually occur in the data, as
  SPSS does. An empty factor level (typically left over after
  `filter()`) made chi-square, V and gamma `NaN` - also the cause of the
  silent `NA` row of grouped `chi_square()` - and gave `chisq_gof()` a
  phantom category with an extra degree of freedom (chi2 = 1257.5
  instead of 5.0). A constant variable no longer crashes
  `chi_square()` and the effect-size helpers ("replacement has length
  zero"): the result is `NA` with a warning naming the variable (and
  group), and the output says "not computed (x has only one observed
  category)". The `summary()` tables show full value labels (no longer
  cut at 20 characters) and fixed decimals.
* `goodman_gamma()` and the gamma row of `chi_square()` are dramatically
  faster and their p-value now matches SPSS. The concordant/discordant
  counts came from a quadruple R loop over the table (`cramers_v(survey_data,
  age, income)` took ~50 s, because `chi_square()` always computes gamma);
  they now come from 2-D cumulative sums (0.03 s). The loop also counted
  only the pairs below each cell, which mis-stated the null-hypothesis
  standard error (ASE0): the approximate significance of gamma was wrong
  (education x employment: p = .122 where SPSS prints .027). The gamma
  value itself is unchanged.
* `chi_square(correct = TRUE)` computes Phi, Cramer's V and the
  contingency coefficient from the Pearson chi-square, as SPSS does; they
  were computed from the Yates-corrected statistic (gender x region: 0.0119
  instead of 0.0129). The Pearson statistic is kept in
  `pearson_chi_squared`/`pearson_p_value`, `summary()` shows both rows
  ("Pearson Chi-Square" and "Continuity Correction") like SPSS, and the
  compact `print()` marks the statistic as "(continuity-corrected)". The
  compact line now spells out "negligible" (was "neglig.") and ends with
  the "Use summary()" hint.
* `summary()` of `chi_square()` follows the SPSS "Symmetric Measures"
  table: Phi and Cramer's V are shown for every table (Phi was hidden
  outside 2x2 although `phi()` returned it and SPSS prints it), and
  Goodman's gamma - with its verbal label - only when both variables are
  ordinal (ordered factor or numeric). For nominal variables such as
  gender x region its sign depends on the arbitrary category order.
  `goodman_gamma()` still computes gamma on request.
* `chi_square()` and the effect-size helpers warn once about expected
  counts below 5. Base `chisq.test()` added its own "Chi-squared
  approximation may be incorrect" warning (German: "Chi-Quadrat-
  Approximation kann inkorrekt sein") next to mariposa's message; that
  specific warning is now muffled in every locale.
* `mann_whitney(mu = , alternative = )`: U, Z, r and the p-value now
  refer to the same hypothesis. With `mu` the statistics were computed
  for a shift of 0 while the p-value came from `wilcox.test(mu = )`
  ("Z = -0.696, p < 0.001"); group-1 values are now shifted by `mu`
  before ranking, as `wilcox.test()` does. For one-sided tests Z is
  directional (positive when group 1 tends to be larger) so that its sign
  matches the p-value (was Z = -0.226 next to p(less) = .589); the
  two-sided Z keeps the SPSS convention. The weighted (design-based) test
  supports only `mu = 0` and now says so instead of printing
  "Null hypothesis (mu): 500" for an unshifted test.
* The unused confidence interval of `mann_whitney()` is no longer
  computed: `wilcox.test(conf.int = TRUE)` ran for every variable and
  was discarded (the source of German "cannot compute confidence
  interval" warnings for a constant variable), and `summary()` no longer
  advertises a "Confidence level: 95.0%" for which no interval exists.
  `conf.level` of `mann_whitney()`, `kruskal_wallis()`,
  `wilcoxon_test()` and `friedman_test()` is documented as not used.
* `pairwise_wilcoxon()`: the interpretation legend and help page now match
  the direction of the test. The legend said that a positive Z meant the
  *first* variable was higher, while it was the second (score_T1 vs
  score_T2: Z = +5.43 while T2 is higher). Z now follows SPSS (see
  "SPSS parity audit" below) and the new `z_based_on` column carries the
  direction.
* `pairwise_wilcoxon()` reports the number of cases of every pair (new
  `n` column, shown in `summary()`) and explains why it can exceed the
  Friedman N: each pair uses all cases with both values (pairwise
  deletion, exactly like the SPSS `/WILCOXON` tests the results are
  validated against), while `friedman_test()` uses complete cases. The
  comparison table prints p-values in SPSS style (`<.001`, `.123`).
* `fisher_test()` handles larger tables: instead of aborting with the raw
  "FEXACT error 501 ... hash table key cannot be computed" it now falls
  back to a Monte Carlo p-value with a warning (SPSS offers the same
  "Monte Carlo" option next to "Exact"). The new arguments
  `simulate.p.value` and `B` (default 10000 replicates, the SPSS default)
  choose it directly; before, `simulate.p.value = TRUE` was silently
  swallowed by `...`. A group that cannot be tested under `group_by()`
  is reported with a warning instead of a silent `NA` row.
* The rank tests treat ordered factors consistently as ordinal: they are
  ranked by their level order. `mann_whitney()` aborted with "'x' must be
  numeric" and `wilcoxon_test()` with "'-' not meaningful for factors"
  (printing "Z = ,"), while `kruskal_wallis()` and `friedman_test()`
  accepted them. A nominal (unordered) factor or character variable is
  now a clear error in all four tests instead of running silently
  (`kruskal_wallis(gender, group = education)`) or failing with "not
  computed for this group" outside any `group_by()`.
* `group_by() %>% binomial_test()` no longer aborts because the variable
  has only one category in one group: that group gets an `NA` row, a
  warning naming the group and the reason, and the output says "not
  computed (gender has 1 observed category; ...)". An ungrouped single
  variable still stops with a clear error.
* Degenerate cases in the rank tests print a reason instead of broken
  text. `wilcoxon_test()` with identical variables reports Z = 0 and
  p = 1 as SPSS does (was "Z = ," and `NA`); a grouped `friedman_test()`
  whose group cannot be tested no longer prints "chi2(NA) = ,  , W = ,
  N = NA" and its warning names the group; a constant variable in
  `kruskal_wallis()`/`mann_whitney()` is reported ("all values of x are
  identical") instead of "(see warning)" without a warning or German
  "cannot compute confidence interval" warnings; an all-missing variable
  says "x has no valid values" instead of blaming the grouping variable
  ("Found 0 groups ... use a Kruskal-Wallis test"). "not computed for
  this group" appears only under `group_by()`; otherwise the output says
  "not computed (reason)".
* Labelled (SPSS) variables show their value labels instead of codes in
  `kruskal_wallis()` ("Groups: 1, 2, ..., 7"), the pairs of
  `dunn_test()`, `mann_whitney()` ("1 vs. 2"), `binomial_test()`
  ("Group 1 (1)") and the categories of `chisq_gof()`, as `chi_square()`
  already did. Grouping variables are ordered by code, as in SPSS: a
  numeric 0/1 group was ordered by first appearance ("1 vs. 0" while the
  ranks listed 0 first).
* `fisher_test()` output: the contingency table is labelled with the
  variable names and value labels (was "r"/"cc" and codes), empty factor
  levels are dropped, grouped `summary()` shows each group's table (it
  was dropped), the compact line uses 3 decimals like the rest of the
  family (was "p = 0.5435") and reports the odds ratio with its 95% CI for
  2x2 tables (SPSS "Risk Estimate": sample odds ratio, Woolf interval).
* `mcnemar_test()` output: without discordant pairs the output says
  "chi2 not computed (no discordant pairs), p = 1.000 (exact)" instead
  of "chi2 = ,  (asymp)"; tables are labelled with the variable names and
  value labels (was "v1"/"v2" and codes); grouped `summary()` shows each
  group's table and the "(cc)" continuity-correction marker (both were
  dropped); the compact line reports the degrees of freedom
  ("chi2(1) = ..."). Two variables with different category sets (e.g.
  0/1 against 1/2) are now an error instead of being tabulated as if the
  categories matched.
* `mann_whitney()` without `group` stops with "`group` is required" (as
  `kruskal_wallis()` does) instead of the internal "Can't extract column
  with `g_name`".
* Output of the rank and exact tests is formatted consistently.
  `summary()` tables print p-values in SPSS style ("<.001", ".026")
  instead of a bare "p value 0" (Kruskal-Wallis, Wilcoxon, Friedman,
  binomial, Mann-Whitney), leave empty cells blank instead of "NA"
  ("Total 2500 NA", "Ties 502 NA NA"), show counts as integers (Mann-
  Whitney "n = 1149.0") and give every column a fixed number of decimals
  (0.52 next to 0.537, Mean Rank 19 next to 40.19, expected 538.24 next
  to 26.239). Compact lines label Kruskal-Wallis epsilon-squared and
  Kendall's W with an interpretation, `mann_whitney()` ends with the
  "Use summary()" hint, and Mann-Whitney and Wilcoxon share one set of
  r thresholds. The `dunn_test()` comparison table no longer wraps at 80
  characters with long group labels, and a group or variable that
  `dunn_test()` cannot compare is reported with a warning naming the
  group.
* Skipped groups are reported consistently: `mann_whitney()` says how
  many groups with valid values a split has ("gender has 1 group ...")
  instead of suggesting a Kruskal-Wallis test, and `pairwise_wilcoxon()`
  names the group in its warnings; a pair whose values are all tied gets
  Z = 0, p = 1 (as `wilcoxon_test()` and SPSS) instead of a silent `NA`.

### Correlation and regression

* `logistic_regression()` converges with expansion weights. `glm()`'s
  binomial start value sits on 0/1 when the weights are large, so the
  fit diverged (intercept -3.7e15, "algorithm did not converge") for
  weights summing to billions. The IRLS now starts at the (weighted)
  share of 1s; the estimates are unchanged.
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
* Regression output (`linear_regression()`, `logistic_regression()`,
  `marginal_effects()`) is readable for every scale of variable:
  - `summary(digits = )` is honoured in all tables (it was ignored).
  - Estimates that would round to zero are shown in scientific notation
    instead of `0.000` / `-0.000` (e.g. income B = 3.72e-04, as SPSS
    shows 3.72E-4); huge values such as separation estimates no longer
    print as 30-digit numbers. Odds ratios get as many decimals as needed
    to separate their confidence limits (`1.0007 [1.0006, 1.0008]`
    instead of `1.001 [1.001, 1.001]`). Fit statistics never show
    `-0.000`.
  - Term names are printed in full; the column is sized to the longest
    term (dummy names were cut to 20/25 characters, e.g.
    "educationIntermediate S...").
  - Sig. columns use the SPSS style (`<.001`, `.466`) instead of `0.000`.
  - The CI columns name their level ("95% CI Lower").
  - A very large weighted N (sum of weights above 2^31) no longer breaks
    the output with "invalid format '%d'".
* Labelled predictors from SPSS files enter `linear_regression()` and
  `logistic_regression()` with their numeric codes (as in SPSS; `factors
  = "dummy"` applies to factors only). `summary()` now says so and points
  to `to_label()` for dummy coding. The Descriptive Statistics table shows
  one row per dummy (the share of each category) for factor predictors
  instead of the meaningless mean of the level index.
* Correlation output (`pearson_cor()`, `spearman_rho()`, `kendall_tau()`,
  `partial_cor()`):
  - The compact print of a matrix shows the N range over the pairs
    (`N = 2008-2421`); it showed the N of the first pair only (even
    `N = 0`). With more than 15 pairs it lists the strongest significant
    pairs instead of every pair (45 lines for 10 variables).
  - `pearson_cor()` labels the interval by `conf.level` ("90% CI"; every
    interval was called "95% CI", the column `CI_95`). A one-sided test
    (`alternative = "less"`/`"greater"`) is named in print and summary
    and gets the matching one-sided interval, as `cor.test()`.
  - Matrices follow SPSS: blank p-value diagonal (was 0.0000), p in table
    style (`<.001` instead of 0.0000), significance flags on the
    coefficients, `digits` honoured for any number of variables (it
    dropped to 2 decimals above 6 variables), and columns split into
    blocks that fit the console instead of widening the `width` option.
  - A constant variable gives one warning naming it and "not computed (no
    variance)" instead of "r = NA, p = NA ," and one base-R warning per
    pair; `partial_cor()` with a constant control variable no longer
    crashes with "missing value where TRUE/FALSE needed".
  - Aligned pair labels and a pairwise table that no longer wraps; a very
    large weighted N (above 2^31) no longer breaks the output.
* `logistic_regression()` also finds the value labels of an outcome that
  carries them as a plain `labels` attribute (e.g. `rec()` output) for the
  encoding table and the `[P(y = category)]` tag, and both regression
  functions fit labelled (SPSS) variables on their bare numeric codes, so
  a fit no longer depends on haven's arithmetic methods being loaded
  ("<haven_labelled> - <haven_labelled> is not permitted").

### Scale analysis

* `efa()` components and factors now carry SPSS's sign convention. The
  eigenvectors kept the arbitrary sign `eigen()` returned, so three
  positively correlated trust items could load -0.597/-0.475/-0.678 on
  their single component and signs could flip between groups. Every
  extracted column is now reflected to a positive loading sum (as SPSS
  FACTOR does); rotated, pattern and structure matrices and factor
  correlations follow from the reflected solution. This reproduces every
  signed loading SPSS prints in the reference runs, now asserted in the
  SPSS validation tests.
* `efa(extraction = "ml")` no longer reports the PCA share of variance.
  The compact line read "Variance explained: 61.0%" (the eigenvalue share
  of three components) although the three ML factors explain about 24%,
  and it called ML factors "components". The result gains
  `$extraction_variance` (SPSS's "Extraction Sums of Squared Loadings");
  the compact line reports its cumulative percentage and says "factors"
  for ML. `summary()` prints "Total Variance Explained" as one aligned
  SPSS-style table (initial eigenvalues, extraction sums, rotation sums)
  instead of free-text lines whose columns shifted from the 10th
  component on.
* `efa()` no longer crashes with the base error "infinite or missing
  values in 'x'" (German: "unendliche oder fehlende Werte in 'x'") when a
  correlation cannot be computed. A constant item, an item without valid
  values, two items without cases in common, or too few complete cases
  now give an error that names the item(s) and the reason. Under
  `group_by()`, such a group is skipped with a warning naming the group,
  and every other group is still analysed (previously the whole grouped
  result was lost); `print()`/`summary()` show "not computed (...)" for it.
* `efa()` flags singular correlation matrices like SPSS ("not positive
  definite"). A duplicated item used to yield KMO 0.500 (from a
  pseudo-inverse), Bartlett's chi-square `Inf` and "Sig.: 0.000", and
  fewer cases than variables gave KMO `NaN`. Now a warning names the
  perfectly correlated items or the case shortage, KMO and Bartlett's
  test are reported as not computed, and ML extraction stops with a clear
  message instead of "Lapack routine dgesv: system is exactly singular".
  A separate warning appears when there are no more cases than variables.
  Bartlett's and the goodness-of-fit significance use the SPSS style
  ("<.001") instead of "0.000".
* `efa()` shows its sample size. `print()` adds "N = 2168 (smallest
  pairwise)" (or "(listwise)"), `summary()` an N line and SPSS's
  "Descriptive Statistics" table (mean, SD, analysis N, missing N; new
  toggle `descriptives`). With `use = "complete"`, `$item_statistics` now
  describes the complete cases the analysis uses; the analysis N was
  pairwise before.
* `efa()` input and output details: `n_factors = 2.7` is an error instead
  of being truncated to 2; `use = "listwise"` (the SPSS term) is accepted
  as an alias of `"complete"`, and invalid `rotation`/`extraction`/`use`
  values give an English error instead of a translated `match.arg()`
  message. A requested rotation of a single component is no longer
  dropped silently: output says "Only one component was extracted. The
  solution cannot be rotated." as SPSS does. Communalities print with
  fixed decimals ("1.000" instead of "1" next to "0.457"), and the
  summary says "N of Components" for PCA.
* `reliability(na.rm = FALSE)` no longer crashes with "missing value where
  TRUE/FALSE needed" (German: "Fehlender Wert, wo TRUE/FALSE nötig ist")
  as soon as a value is missing. All statistics are `NA`, a warning names
  the items with missing values and points to `na.rm = TRUE` (listwise
  deletion, as SPSS), and `print()`/`summary()` say "not computed (...)"
  instead of "Cronbach's Alpha = NA ()".
* `reliability()` warnings are clearer. An item with zero variance is
  removed from the scale with a warning naming it, as SPSS RELIABILITY
  does; it used to stay in (alpha 0.042 instead of 0.047, standardized
  alpha `NA`) next to the German base warnings "Standardabweichung ist
  Null" and "NaNs wurden erzeugt". When omega cannot be computed (e.g. a
  duplicated item makes the correlation matrix singular), the warning
  says why in English and names the perfectly correlated items instead
  of relaying factanal's translated error. The "omega requires at least
  3 items" warning appears once per call instead of once per group, and
  "Insufficient data (n = 0)" now names the group and the items without
  valid values.
* `reliability()` no longer reports McDonald's omega from a Heywood
  solution. For the trust items in the East region the one-factor model
  put one uniqueness at its lower bound (loading of about 1), and omega
  0.349 was printed next to alpha 0.037 without comment. Omega is now
  `NA` in such cases, with a warning that names the item and the group.
* `reliability()` flags a negative Cronbach's alpha like SPSS: "The value
  is negative due to a negative average covariance among items ... check
  item codings." A warning and the `summary()` footnote name the items
  with a negative corrected item-total correlation (usually items that
  need reverse-coding; new `$negative_items`), and the compact print says
  "negative; check item coding" instead of classifying alpha -0.929 as
  "Poor".
* `reliability()` output is easier to read. The Item-Total Statistics
  table no longer wraps at 80 columns under snake_case headers
  (`scale_mean_deleted`, `corrected_r`, ...); it has SPSS-style two-line
  headers ("Scale Mean / if Deleted", "Alpha if / Deleted", ...). The
  inter-item correlation matrix honours `digits` for more than six items
  (it was forced to 2 decimals) and uses numbered columns so it stays
  narrow. Item statistics print with fixed decimals (no more "1.16" next
  to "2.615"), and a missing omega reads "not computed" with the reason
  instead of "NA".
* `reliability()` and `efa()` show variable labels, as SPSS does.
  `summary()` lists every item with its full label, and the per-item
  tables (item statistics, communalities, loading matrices) add the
  label next to the name, shortened with "..." so that rows fit the
  console width; wide tables keep the names only. Labels with umlauts
  stay aligned. The labels are stored in `$variable_labels`.

### Data import/export, labels and transformation

* `read_sas()` reads `.sas7bdat` files again with the default arguments.
  It passed `catalog_encoding = NULL` explicitly, which haven 2.5 rejects
  ("Expected string vector of length 1"), so every call without an explicit
  catalog encoding failed. The catalog encoding now falls back to the data
  file's encoding, as documented.
* `untag_na()`, `na_frequencies()`, `write_spss()`, `write_xlsx()` and
  `write_xpt()` are much faster on large imported files. The NA tag of
  every missing value was read one element at a time, each through vctrs
  dispatch, and the Excel "Labels" sheet was built from thousands of
  one-row data frames. Full ALLBUS 2023 (579 variables): `untag_na()` over
  all columns 9.8 s -> 0.15 s, `write_spss()` 11.5 s -> 1 s,
  `write_xlsx()` 22 s -> 8 s (the rest is openxlsx2 itself). Output is
  unchanged.
* `write_spss()` no longer crashes ("Failed to insert value ...: The file
  format does not supported character tags for missing values") after
  `rec()`, `std()`, `center()`, `pomps()`, `to_numeric()` or arithmetic on
  variables imported with `read_spss()`. These results kept the tagged-NA
  payloads but lost the code map. Now `rec()` keeps the missing-value
  types, their code map and their labels (so `na_frequencies()`,
  `frequency()` and the SPSS export still show "no answer" etc.), while
  `std()`, `center()`, `pomps()` and `to_numeric()` return plain `NA` by
  design (as an SPSS `COMPUTE` gives system-missing). `write_spss()` writes
  remaining unmapped tags as system missing with one warning naming the
  variables.
* Value labels created by `rec()` (inline `[label]` syntax, `val_labels`,
  mirrored labels of `"rev"`), kept by `strip_tags()` or by
  `to_numeric(keep_labels = TRUE)` now survive `write_spss()` and
  `write_stata()`. They were attached as a bare `labels` attribute without
  the `haven_labelled` class, which haven's writers ignore. The results are
  `haven_labelled` now; `to_labelled()` picks up an existing `labels`
  attribute; the exporters also promote such bare attributes themselves.
* `rec(as_factor = TRUE)` names the levels by the result's value labels
  (e.g. the mirrored labels of `"rev"`, previously ignored: levels "1".."7")
  in code order. Values without a label keep their code as level name
  instead of becoming `NA`, and duplicate label texts are disambiguated by
  their code.
* `rec(rules = "rev")` reverses on the scale range instead of the observed
  range. It computed `max(x) + min(x) - x` over the data, so a 1-5 item
  answered only with 2-5 became 5..2 instead of 4..1, and the value labels
  were mirrored to codes that do not exist. The range now comes from the
  value labels of the valid codes (together with the observed values);
  without labels the observed range is used with a message, and the new
  syntax `rules = "rev(1, 5)"` sets the range explicitly (values outside it
  are reported).
* `rec()` on `haven_labelled_spss` vectors (`haven::read_sav(user_na =
  TRUE)`) treats the user-missing codes as missing: they were reversed or
  recoded like valid values and lost their codes and labels. The input is
  converted to the tagged-NA form of `read_spss()` first. `val_labels` is
  honoured with `rules = "rev"` (it was silently ignored).
* `rec()` syntax is more forgiving and never loses values silently:
  valid values that match no rule still become `NA` but now with a warning
  that lists them and suggests `else=copy` (SPSS's in-place `RECODE`
  keeps them) or `else=NA`; value lists work (`"1,2=1; 3,4:5=2"`, as
  SPSS's `RECODE (1,2=1)`); keywords are case-insensitive (`"REV"`,
  `"Dicho(3)"`); a reversed range such as `"5:1=1"` is an error instead of
  silently matching nothing; inline labels may contain semicolons
  (`"[niedrig; gering]"`); `"dicho(x)"` and a missing `rules` argument give
  clear English errors instead of leaked base-R (German) messages; the
  `" (recoded)"` label suffix is no longer appended again on every call.
* `to_label()` and `to_character()` no longer merge distinct codes that
  share a label text. ALLBUS labels the scale points 2-6 of 88 items "..",
  so e.g. `pt12` collapsed into three levels ("GAR KEIN VERTRAUEN", "..",
  "GROSSES VERTRAUEN") and the `to_numeric()` round trip turned 3-6 into 2.
  Duplicate texts now get their code appended (`".. (2)"`, `".. (3)"`), the
  same rule the test functions use for grouping variables.
* `to_label(data)` and `to_character(data)` without a variable selection
  no longer turn metric variables into factors. ALLBUS `age` and `isei08`
  (whose only labels are missing codes) became all-`NA` factors and the
  weight a factor with one level per value, silently. Now only variables
  whose values are all value-labelled are converted; the others are left
  unchanged with one message listing them. Selected variables are still
  converted, and whenever values without a label become `NA` a warning
  names the variables and points to `add_non_labelled = TRUE`.
* `copy_labels()` no longer forces the source's value labels and class
  onto columns that were converted or summarised: a `to_label()` factor
  became `int+lbl` 1, 2, 3 carrying the labels 1/5/9 of the source codes,
  and group means were labelled as if they were codes. Value labels,
  missing-value metadata and the class are now copied only when the target
  still holds the source's codes; the variable label is always copied.
* `set_na(data, -9, -8, tag = FALSE)` (the documented example) no longer
  strips the labels of every numeric column (`survey_data`: 15 variable
  labels -> 6). Variable labels, the value labels of the remaining codes,
  the `haven_labelled` class and existing missing-value types are kept;
  integer columns stay integer.
* `val_labels()` refuses to set value labels on a factor or character
  column (they were stored but never used) and points to `to_labelled()`;
  `set_na()` warns when a named variable or a vector is a factor instead of
  silently returning it unchanged.
* `to_dummy()` fixes: with `suffix = "label"`, values sharing a label text
  (ALLBUS ".." scale points) all wrote into one column `pt12_` and their
  dummies were lost - every category now gets its own column (value
  appended to duplicate or empty labels); umlauts are transliterated
  (`"männlich"` -> `maennlich`, was `mnnlich`); `ref` works for factors,
  by level name or number (it was compared with the level names only, so
  `ref = 1` silently returned all dummies), and a `ref` that matches no
  category is an error.
* `row_count()` counts value sets and missing values: `count = c(4, 5)`
  was recycled over the cells (wrong counts without a warning), and
  `count = NA` always returned 0. It now counts cells equal to any listed
  value (SPSS `COUNT n = v1 TO v5 (4, 5)`), `NA` counts missing values
  (SPSS `MISSING`), and SPSS missing codes of imported data (e.g. -9, a
  tagged NA after `read_spss()`) are counted when listed - they were
  always 0.
* `row_means()`, `row_sums()` and `row_count()` inside a grouped
  `mutate()` with the `.` placeholder now stop with an explanation and the
  `pick()` form instead of dplyr's bare size-mismatch error; `pick()` is
  the documented, recommended form. Non-numeric columns handed over by
  `pick()` are ignored with a warning naming them (they were dropped
  silently), and a fractional `min_valid` (e.g. 2.5) is rejected.
* `pomps()` warns when values lie outside `scale_min`-`scale_max` (an
  unrecoded "don't know" = 9 on a 1-5 scale silently scored 200), checks
  that `scale_min`/`scale_max` are single finite numbers (a vector gave
  the German base error "Bedingung hat Länge > 1", `NA` a cryptic one),
  and says clearly when an all-`NA` input leaves no range to derive.
* `std()` and `center()` keep the variable label of imported (SPSS)
  variables when overwriting them in place: the label was read after the
  column had already been replaced, so it was lost. Grouped
  standardization/centering returns a plain numeric column instead of
  leaving a `dbl+lbl` vector behind, vector input keeps its label (with
  " (standardized)"/" (centered)"), and the zero-spread warning names the
  variable and the group (e.g. "`x` (g = a): the spread (sd) is zero").
  The weighted mean/SD now come from the shared SPSS kernels (results
  unchanged).
* `write_spss()` and `write_stata()` export a factor created by
  `to_label()` with its original codes and value labels: haven renumbered
  it 1..k (ALLBUS `dm06` codes 100, 120, ... became 1, 2, ...), although
  the factor carries its codes. Other factors are still written as 1..k
  with their levels as labels.
* `read_spss()` -> `write_spss()` round trips the SPSS missing-value
  definitions exactly and quietly. `read_spss()` kept only the codes that
  occur, so an unchanged ALLBUS export produced 133 warnings ("4 discrete
  missing codes exceed SPSS's limit of 3 ... range -42--8") and rewrote
  `LOWEST THRU -1` as `-42 THRU -8`. The original definition is now
  remembered (attribute `spss_missing`) and written back while it fits
  (ALLBUS 2023: all 579 definitions and values identical, no warning).
  Variables with more than 3 codes otherwise use SPSS's "range plus one
  discrete value" form when that avoids valid values (e.g. codes 0, 7, 8,
  9 around valid 1-6, which used to be an error), reported in one message
  with readable ranges ("-11 to -8 and -42").
* `read_spss()`, `read_por()`, `read_stata()`, `read_sas()`, `read_xpt()`
  and `read_xlsx()` recognise a file of the wrong type from its first bytes
  and say what it looks like and which reader to use (e.g. "read_spss()
  cannot read x.dta: it looks like a Stata file (.dta). Use read_stata()
  instead."), instead of readstat's cryptic errors; a missing file is
  reported as such.
* `write_xpt()` no longer truncates variable names silently: the default
  SAS transport version 5 allows 8 characters, and truncation even created
  duplicate names. It now warns (listing old -> new names, suggesting
  `version = 8`) and refuses to write when truncation would produce
  duplicates.
* `write_xlsx()` makes sheet names unique within Excel's 31-character,
  case-insensitive limit (two list names sharing their first 31
  characters, or an element called "Labels", crashed without writing a
  file); renamed sheets are listed in one message. A missing output
  directory is reported clearly, as in `codebook(file = )`.
* `write_xlsx()` of a grouped `frequency()` result writes one block per
  group, headed by the variable and the group ("gender (Gender) - region =
  East") with its own N line. All groups were written as one block under
  the first group's header, without group labels, and the Total row summed
  the groups (Raw % = 200). Weighted N is rounded like the console print
  ("N=2516", was "N=5245.99999999998").
* `find_var()` gains `fixed = TRUE` for literal (case-insensitive) search,
  e.g. label text with parentheses: `"BEFRAGTE(R)"` as a regular expression
  matched "BEFRAGTER" instead. Regular expressions stay the default; a
  message points to `fixed = TRUE` when the literal text would match other
  variables, an invalid regular expression such as `"("` is searched as
  text (the regex engine's warning no longer leaks), and an empty result
  is returned invisibly instead of printing `<0 rows>`.
* `strip_tags()` and `untag_na()` accept a data frame (all numeric
  columns, or the ones selected via `...`) instead of failing with a
  German base-R error, and reject non-numeric vectors with a clear message
  (`strip_tags("a")` returned `NA`). `untag_na()` keeps the value and
  variable labels: the labels of the missing types are attached to their
  restored codes (e.g. -9 = "KEINE ANGABE"), so the result is still
  `haven_labelled`.
* `na_frequencies()` output is reorganised: rows are ordered by code like
  the missing block of `frequency()` (they were sorted by count), columns
  are `code`, `label`, `n`, `prc` (percent of all cases, new) and `tag`
  (the technical tag letter moved last), SPSS codes are numeric (were
  character), the "(System Missing)" row appears only when system-missing
  values occur, and a variable without missing values gives a message
  instead of printing `<0 rows>`. Data frames are accepted
  (`na_frequencies(data, q1, q2)`, as the data-io vignette shows) and
  return one table with a `variable` column.
* New replacement form `var_label(x) <- "Label"` (and
  `var_label(data) <- list(age = "Age", sex = "Sex")`; `NULL` removes a
  label). `drop_labels()` also drops unused factor levels (keeping the
  variable label and the codes of `to_label()` factors); its example
  filtered on a non-existent category and did nothing. The `copy_labels()`
  help no longer claims that `filter()`/`select()`/`mutate()` strip labels
  (they keep them) and names the operations that do.

### Across functions

* A `group_by()` group in which no case has a valid weight has no cases,
  as SPSS excludes cases with a missing weight: `describe()` computed
  that group unweighted (printed under "Weighted Descriptive Statistics"
  with N = 0 and one warning per statistic), `frequency()` failed with
  "Ersetzung hat 1 Zeile, Daten haben 0" and a grouped `crosstab()`
  aborted for all groups. The group now shows N = 0 (`describe()`) or is
  left out with a warning naming it (`frequency()`, `crosstab()`).
* The `weights` argument of every analysis function documents the forms it
  accepts since this release: a column name (unquoted or as a string), an
  expression such as `sampling_weight * 2`, or a numeric vector with one
  weight per row.
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
* Calling a function without a required argument gives a clear error
  naming it ("Argument `covariate` is missing, with no default."), in every
  exported function. Previously base-R errors surfaced from inside, often
  localized and naming internal arguments: 'Argument "data" fehlt (ohne
  Standardwert)', `oneway_anova(data, age)` "Can't extract column with
  `g_name`", `ancova()`/`factorial_anova()` 'Argument "between_expr"
  fehlt', `fisher_test()`/`mcnemar_test()` 'Argument "x" fehlt',
  `crosstab()`/`write_xlsx()` "no applicable method ... class NULL". A bare
  variable name where a value is expected (`linear_regression(data, age)`,
  `find_var(data, age)`, a `path`) is named instead of "object 'age' not
  found"; `set_na(data, age)` explains that it takes values, not variable
  names.
* Weights without any positive value (all zero or missing) are refused
  with one clear error. The analyses failed deep inside with base-R errors
  ("'n' must be a positive integer", "missing value where TRUE/FALSE
  needed", "object 'fit' not found"), and `describe()` silently fell back to
  an unweighted analysis when all weights were missing.
* `group =` given as an expression (`group = region == "East"`) explains
  that the grouping variable must be created first (it failed with "object
  'region' not found"); a `group` selecting no or several columns is an
  error naming the selection. `spearman_rho()` and `kendall_tau()` accept
  ordered factors (ranked by level order) like `mann_whitney()` and
  `kruskal_wallis()`; they rejected e.g. `education` as "not numeric".
* Result tables stay aligned with umlauts and other multi-byte characters
  in variable names, value labels and terms (Tukey/Scheffe, Dunn,
  pairwise correlations, regression coefficients, Kruskal-Wallis ranks,
  goodness-of-fit and many more): the shared table printer padded cells
  with `sprintf("%-20s")`, which counts bytes, so every umlaut shifted the
  rest of its row one column to the left. Cells are now padded by display
  width. Counts and sums of weights of 2^31 or more (expansion weights)
  print as whole numbers instead of `NA` with a coercion warning; leading
  label columns such as "Group 2" are left-aligned like "Group 1"; table
  lines no longer end in a blank.
* Sums of weights of 2^31 or more (expansion weights) no longer print
  as `N = NA` in the compact lines of the rank tests and the
  goodness-of-fit test, as `NA` in the N column of the one-sample t-test,
  and no longer raise integer-coercion warnings in the t-test, one-way
  ANOVA, factorial ANOVA/ANCOVA, Levene and reliability tables: every
  count is formatted as a whole number without integer coercion.
* Grouped output uses one group-header style in every verbose table:
  `describe()`, the `w_*()` functions and the summaries of
  `levene_test()`, `normality_test()`, `marginal_effects()` and the
  post-hoc tests printed "Group: region = East " with a trailing blank and
  no underline, all other summaries an underlined header. Section titles
  without a suffix ("Pearson Correlation", "Chi-Squared Test of
  Independence", "Levene's Test for Homogeneity of Variance") no longer end
  in a blank with an underline one dash too long.
* Counts carry no thousands separators anywhere, as in SPSS tables and
  the console: the HTML `codebook()` header wrote "2,500 observations",
  `mann_whitney()`'s compact line "U = 776,732" (its summary table
  776732). The N/discordant-pair lines of `fisher_test()`,
  `mcnemar_test()`, `chi_square()` and the Friedman note of
  `pairwise_wilcoxon()` use the same whole-number formatter (the latter
  printed "N = 9.36e+09" for large sums of weights).
* Weighted `frequency()` of a factor together with a numeric variable
  (`frequency(survey_data, education, life_satisfaction, weights =
  sampling_weight)`) no longer turns the numeric variable's categories
  into `NA` (listed under "Total missing") with the base-R warning
  "invalid factor level, NA generated": the weighted branch kept the
  factor as the value column when the tables were combined.
* Variable pairs are joined by an ASCII "x" in every output: the titles
  of `chi_square()`, `fisher_test()`, `mcnemar_test()` and `crosstab()`
  and the chi-square "Table size" line used the multiplication sign
  (U+00D7), the correlation, factorial and post-hoc output an "x".
* `print()` of `linear_regression()`, `logistic_regression()` and
  `normality_test()` results accepts `digits` like every other compact
  print (R-squared, statistics and p-values ignored it). The grouped
  `normality_test()` compact print shows the test results under a
  `[group]` line for every group instead of only the number of groups.
* Every compact `print()` of a test or model result now ends with
  "Use summary() for detailed output." (`t_test()`, `oneway_anova()`,
  `factorial_anova()`, `ancova()`, the three correlation functions, both
  regressions, `reliability()` and `efa()` did not show it, the rank tests,
  chi-square family, Levene and normality tests did).
* Grouped compact prints of `linear_regression()` and
  `logistic_regression()` put each group on its own `[region = East]`
  line (they printed "  region = East: R2 = ..."), like every other
  compact print; a skipped group shows "not computed (reason)" under its
  `[group]` line.

* Analysis functions called on a data frame without rows stop with one
  clear message ("`data` has no rows") instead of base-R errors from deep
  inside (`frequency()`, `reliability()`, `efa()`) or seven repeated
  weight warnings (`describe()`). Transformations such as `rec()` still
  pass empty data through.

### Result export and R Markdown

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
  `mean2`, `sd1`, `sd2`), written out as text (the values and value
  labels of a `codebook()`) or dropped (the observed/expected tables of
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
* `codebook()` works in R Markdown and Quarto. It returned invisibly and
  (with `view = interactive()` being `FALSE` while knitting) a chunk
  `codebook(data)` produced nothing. It now returns visibly, and a
  `knit_print()` method embeds the HTML codebook in HTML output (styles
  scoped to the codebook, so the rest of the document keeps its look);
  PDF/Word output shows the console overview. While knitting, the viewer
  is no longer opened by default (`rmarkdown::render()` from an
  interactive session used to open it). In the console, the compact
  overview is now printed after the viewer opens.

### SPSS parity: ANCOVA Levene and factor rotations

* `ancova()`: Levene's test of equality of error variances now matches
  SPSS UNIANOVA. It was computed on the deviations of the dependent
  variable from its raw cell means, ignoring the covariates
  (`life_satisfaction BY gender WITH age`: F = 1.277, p = .258 instead of
  SPSS 1.306, p = .253). SPSS tests the absolute residuals of the full
  model (covariates and factors) across the design cells; mariposa now does
  the same and reproduces all six unweighted Levene tests of the SPSS
  reference run. The weighted path is unchanged until the pending weighted
  reference run. New `levene_test()` method for `ancova()` results
  (`ancova(...) |> levene_test()`, also per group under `group_by()`).
* `efa(rotation = "promax")` now matches SPSS FACTOR `/ROTATION PROMAX`.
  mariposa used `stats::promax()`, which builds the promax target from the
  raw varimax loadings; SPSS first normalizes every row (Kaiser
  normalization). Pattern loadings differed by up to .03 and the component
  correlations were .055 / .155 instead of SPSS -.002 / -.012. The new
  implementation follows the SPSS algorithm and reproduces every pattern,
  structure and correlation matrix of the SPSS reference runs (unweighted,
  weighted, per region), now asserted in the validation suite.
* `efa()` with an oblique rotation (promax, oblimin): the "Rotation Sums of
  Squared Loadings" are now the sums of squares of the structure matrix, as
  SPSS reports them (they were taken from the pattern matrix: 1.604 / 1.065
  / 1.045 instead of SPSS 1.599 / 1.039 / 1.021).
* `efa()` varimax and oblimin rotations now use SPSS FACTOR's own
  algorithms and stopping rules (Kaiser's cyclic pairwise varimax; the
  Jennrich-Sampson direct oblimin, delta = 0; at most 25 iterations).
  `stats::varimax()` stopped earlier and missed SPSS's component
  transformation matrix by up to .004 (2-factor solution: 26.653 % instead
  of SPSS 26.655 % for the first rotated component);
  `GPArotation::oblimin()` iterated past SPSS's stopping point (weighted
  solution: pattern loadings off by up to .002). The rotated factors are
  reflected to a positive sum and ordered by their sums of squares as in
  SPSS. Every varimax, oblimin and promax reference solution (unweighted,
  weighted, per region) now matches SPSS, including SPSS's iteration
  counts, which `summary()` prints as SPSS does ("Rotation converged in 4
  iterations."). Oblimin no longer needs the GPArotation package.
* `efa(extraction = "ml")`: a Heywood variable is now bounded at a
  communality of .999, as SPSS FACTOR does (it was .995, the default bound
  of `stats::factanal()`), and the factors keep SPSS's order (by the
  eigenvalues of the rescaled correlation matrix; `factanal()` re-sorted
  them by sums of squares, so the unrotated factor matrix and the
  extraction sums came in a different order). Where SPSS's ML iteration
  converges (reference runs per region, West), the whole solution now
  matches SPSS: communalities, factor and rotated factor matrices,
  extraction and rotation sums, transformation matrix. The remaining
  reference runs end without convergence in SPSS itself (a model with
  0 degrees of freedom and Heywood cases); see Validation.
* `levene_test()` compact print: a whole-number weighted `df2` above 2^31
  (sums of weights in the billions) printed as `F(1, NA)` with an integer
  overflow warning; it is now printed in full.
* Weighted `ancova()`: Levene's test uses sqrt(w) * |WLS residual| of the
  full model, as SPSS UNIANOVA does with /REGWGT. This reproduces all five
  weighted SPSS references exactly (e.g. 0.902 instead of 0.880) and
  restores the rule that `weights = 1` gives the unweighted result (the
  residual-based unweighted test made the old cell-mean-based weighted
  test disagree with it).

### SPSS parity audit: reference runs no test asserted

A value-by-value comparison of every SPSS reference output with the
current code found values that had been in the reference files all along
but were never asserted. These are now fixed and asserted:

* Weighted `tukey_test()`/`scheffe_test()`: the error mean square and its
  df come from the weighted ANOVA table (df = floor(sum of weights) - k,
  as SPSS ONEWAY and `oneway_anova()` use). The pooled df of the group
  statistics moved SE, confidence limits and Sig. off SPSS (e.g. SE
  62.521 instead of 62.534, Sig. .089 instead of .090); about 550 printed
  values in the reference runs.
* `tukey_test()` confidence limits use the studentized range quantile to
  full precision. `qtukey()` is accurate to about 1e-8, which moved
  4-decimal limits of income differences (214.4432 instead of SPSS's
  214.4433).
* Phi of a 2x2 table (`chi_square()`, `phi()`) carries the sign of the
  association, as SPSS's Symmetric Measures print it: -.082 instead of
  .082 for a negative association (reference Fisher Test 1c). Larger
  tables keep the unsigned sqrt(chi-square / N).
* `wilcoxon_test()` and `pairwise_wilcoxon()` report Z as SPSS does: from
  the smaller rank sum, so it is never positive, with a new `z_based_on`
  column ("negative ranks"/"positive ranks") that names that sum like
  SPSS's footnote; `summary()` prints it. Before, Z was positive whenever
  increases dominated (trust_science - trust_government: +25.945 instead
  of SPSS's -25.945); the tests compared |Z| only. An empty rank
  category has mean rank 0 (SPSS: .00) instead of `NA`.
* Weighted `factorial_anova()`/`ancova()` cell standard deviations match
  SPSS's /REGWGT Descriptive Statistics (weighted sum of squares over
  n - 1): they divided by the sum of weights (1.207 instead of 1.238).
* Printed numbers round halves up as SPSS does: a mean rank of 306/16 =
  19.125 printed as 19.12 (round half to even) where SPSS shows 19.13,
  and a value that rounds to zero no longer prints as "-0.000".

## Validation

* New Tier-3 exceptions for `efa()` ML extraction: EXC-001 (±.002; SPSS stops its
  Newton-Raphson iteration at ECONVERGE(.001), so loadings, sums of
  squares and percentages of variance of converged solutions agree to
  about 1e-3) and EXC-002 (±.05; SPSS reference runs 5a/6a that end
  without convergence, "More than 25 iterations required").
* `efa()` rotations are now validated in full against SPSS: 8 varimax,
  6 oblimin and 6 promax solutions (transformation, pattern, structure and
  correlation matrices, rotation sums, iteration counts); the ML initial
  communalities of all 6 ML reference runs; `ancova()` Levene tests of the
  6 unweighted reference runs.
* The Display-tier p-value tolerance of `assert_spss()` is one unit of the
  last printed decimal. A relative 1% was added on top, which let a p of
  .975 drift by ten units; every reference p matches at the tighter bound.
* `vignette("spss-compatibility")` is regenerated: it lists EXC-001/EXC-002
  with their reason (it still said "No active Tier-3 exceptions") and the
  updated assertion counts of `ancova()`, `crosstab()`, `describe()`,
  `efa()` and `t_test()`.

# mariposa 0.7.3

CRAN resubmission (theme: make the print/cat console contract lexically
visible — the 2026-09 second-round remark on `R/kendall_tau.R`).

## CRAN

* The reviewer-flagged `cat()` in `R/kendall_tau.R` sat in
  `.kendall_tau_spec$pair_extras`, a display callback that only ever ran
  inside the `summary()` print layer — functionally exempt, but
  lexically indistinguishable from computation code. All three
  correlation `pair_extras` callbacks (kendall, pearson, spearman) now
  **return** formatted lines; the single `cat()` lives in the engine's
  `.print_cor_verbose()`. Output is byte-identical.
* The same lexical rule is now enforced package-wide:
  `format_stat_table()` was renamed to `print_stat_table()` (it prints,
  it never returned a formatted string), and `for_each_group()`'s group
  header line moved into a new `print_group_label()` helper. After
  this, every `cat()`/`print()` call in `R/` lives in a function whose
  name starts with `print`/`.print`.
* New static meta-test `test-console-discipline.R` locks the rule in:
  it parses every file in `R/` and fails if a `cat()`, `print()`, or
  `writeLines()` call appears in any top-level object not named
  `print*`/`.print*` (calls inside `capture.output()` are exempt as
  silent). Together with the runtime `test-silent-computation.R` (zero
  stdout from every analysis entry point), this makes the CRAN console
  contract regression-proof.

# mariposa 0.7.2

CRAN resubmission (theme: address all four points of the 2026-09 manual
review).

## CRAN

* DESCRIPTION now cites the published method references in CRAN's
  auto-link form: IBM SPSS Statistics Algorithms, Dallal and Wilkinson
  (1986, doi 10.1080/00031305.1986.10475419) for the Lilliefors
  correction, and Haberman (1973, doi 10.2307/2529686) for adjusted
  standardized residuals.
* **All `\dontrun{}` blocks are gone.** The 15 import/export examples
  are now genuinely executable `\donttest{}` roundtrips through
  `tempfile()` (guarded by `requireNamespace()` for the Suggests
  packages); the two formats R cannot produce (.por, .sas7bdat) run
  behind a `file.exists()` guard. The `unlabel()` example runs
  unconditionally on the bundled data.
* `on.exit()` now registers the restoration *before* the option is
  changed in `ancova()`, `factorial_anova()`, and the correlation-matrix
  print helper, so an interrupt between the two lines cannot leak a
  changed `options()` setting.
* New `test-silent-computation.R` proves the console contract the
  reviewer asked about: every analysis entry point produces zero stdout
  (32 assertions); all `cat()` calls in the package live exclusively in
  the `print()`/`summary()` display layer, and remaining runtime
  messages go through suppressable `message()`-based conditions.

## Bug fixes

Making the former `\dontrun{}` examples actually run uncovered two real
crashes on integer columns (tagged NAs are NaN payloads in doubles;
`haven::na_tag()` errors on integer input):

* **`unlabel()` crashed on any data frame with integer columns**
  ("`x` must be a double vector") — including the bundled
  `survey_data`'s Likert variables. Integer vectors can never carry
  tagged NAs and now pass through directly.
* **`write_xpt()` crashed on integer columns** for the same reason in
  its tag-uppercasing step. Both fixes carry regression tests.

# mariposa 0.7.1

CRAN resubmission (theme: address the incoming-pretest NOTE) plus two
small robustness patches.

## CRAN

* The `kendall_tau()` examples now run on a 300-case subset: Kendall's
  tau is O(n^2) and the full-sample examples exceeded CRAN's 5-second
  limit on the Debian pretest machine (7.5s). A comment in the example
  explains the subsetting.

## Improvements

* The Imports now declare minimum versions matching the APIs actually
  used (`cli >= 3.0.0`, `dplyr >= 1.0.0`, `rlang >= 1.0.0`,
  `tidyselect >= 1.1.0`, `tibble >= 3.0.0`, `htmltools >= 0.5.0`), so
  installations with stale libraries get an automatic update instead of
  runtime errors.
* `set_na()` on a whole data frame without haven installed now aborts
  with the friendly "Package haven is required for tagged NAs" hint
  instead of the raw namespace error (the unnamed-values path reached
  `haven::tagged_na()` without its guard; the vector and named-pairs
  paths were already guarded).

# mariposa 0.7.0

Assumption checks and model interpretation (feature set: three new
functions closing the most common SPSS/Stata gaps for survey
researchers).

## New features

* New function **`normality_test()`** — the SPSS EXAMINE "Tests of
  Normality" table: Kolmogorov-Smirnov with Lilliefors significance
  correction (Dallal-Wilkinson 1986 approximation, as in SPSS) and
  Shapiro-Wilk (computed for 3 <= n <= 5000, matching the SPSS
  convention). Supports tidyselect and `group_by()`
  (SPSS `EXAMINE ... BY factor`). Deliberately takes no `weights`
  argument: neither test has a well-defined fractional-frequency-weight
  form (documented in the help page).
* New function **`partial_cor()`** — SPSS PARTIAL CORR (Stata `pcorr`):
  partial correlations of two or more variables controlling for one or
  more others, with the zero-order correlation alongside for comparison,
  df = n - 2 - k, two-tailed t-test, listwise deletion, weights
  (frequency semantics, unrounded `sum(w)` in df), grouping, and a
  partial-correlation matrix for three or more variables.
* New function **`marginal_effects()`** — average marginal effects for
  `logistic_regression()` models (Stata `margins, dydx(*)`): average
  probability-scale derivatives for continuous predictors, average
  discrete changes vs. the reference level for factors, delta-method
  standard errors with analytic gradients, weights and `group_by()`
  support. Calling it on a `linear_regression()` explains that B already
  is the marginal effect there.
* New function **`multiple_response()`** — SPSS MULT RESPONSE for
  "check all that apply" questions (dichotomy sets): the frequencies
  table with both percentage bases (*percent of responses*, summing to
  100%, and *percent of cases*, summing above it), and via `by =` the
  set-by-demographic crosstab with case-based column percentages.
  SPSS case rule (valid = at least one non-missing indicator), variable
  labels as option labels, frequency weights with unrounded sums,
  `group_by()` support.

All four ship with the full three-layer print/summary output.

## Bug fixes

* **pkgdown: formulas on the reference pages render again.** The
  `w_*` help pages embed LaTeX via `\eqn{}`, but the site configuration
  loaded no math engine, so pages showed raw LaTeX source
  (`\frac{...}`). `_pkgdown.yml` now sets
  `template: math-rendering: katex`.

## Validation

* All three functions carry property-based validation (Charter Tier 4
  where an SPSS reference run is still pending): `normality_test()`
  against `stats::ks.test()` and the independent
  `nortest::lillie.test()` implementation (new Suggests dependency,
  test-only); `partial_cor()` against the residual-of-regressions
  characterization via `lm()`; `multiple_response()` against direct
  hand-computation from the indicator matrix; `marginal_effects()`
  against the analytic logit-AME formula and an independent
  finite-difference delta-method recomputation (SPSS has no AME
  procedure — permanently Tier 4). Weighted paths have `w == 1`
  invariance blocks; grouped paths are pinned to per-subset
  recomputation. SPSS v29 reference syntax for all pending runs lives
  in `.claude/spss-syntax-0.7.0-references.sps`.

# mariposa 0.6.17

Crosstab cell diagnostics (theme: after the chi-square test, show *which*
cells drive the association).

## New features

* `crosstab()` now computes **expected cell counts and adjusted
  standardized residuals** (SPSS `CROSSTABS /CELLS=EXPECTED ASRESID`,
  Haberman 1973). Display them with `summary(result, residuals = TRUE)`
  — a sub-row per cell, 1 decimal as in SPSS, with a footnote explaining
  the |adj.res.| > 2 rule of thumb. The raw matrices are available as
  `$expected` and `$adj_residuals`. Weighted tables use the unrounded
  weighted cell counts (Charter §5.1) and reduce exactly to the
  unweighted result at `weights == 1` (invariance suite extended).

## Validation

* The residuals are verified against the independent Haberman-formula
  implementation in `chisq.test()$stdres` (unweighted) and a hand-derived
  weighted recomputation; the SPSS v29 `/CELLS=ASRESID` reference run is
  pending and the compatibility vignette discloses them as Tier 4 until
  it lands.

# mariposa 0.6.16

CRAN readiness (theme: the package passes CRAN's submission conventions,
not just `R CMD check`). No statistical behavior changes.

## CRAN conventions

* DESCRIPTION: software names are single-quoted per CRAN policy
  ('SPSS', 'Stata', 'SAS', 'Excel', 'tidyverse'); the promotional
  "Professional" opener is gone.
* Help pages no longer contain Unicode characters that break the CRAN
  PDF manual build (`>=`, `<=`, `->` replace their typographic
  variants) — local and CI checks had always run `--no-manual`, so
  this was never exercised.
* `\value{}` sections added to the five documented S3 method pages that
  lacked them (`predict`/`anova` for both regressions,
  `print.w_quantile`).
* The `set_na()` example is now runnable (haven-guarded) instead of
  `\dontrun`; the `efa()` oblimin example guards its GPArotation
  dependency so `--run-donttest` passes without Suggests installed.
* `?mariposa` now resolves: the package help topic is generated from
  DESCRIPTION (previously suppressed by `@noRd`).

## Test infrastructure

* SPSS-validation assertions (`assert_spss()` and everything built on
  it) skip on CRAN. They are golden-number comparisons against SPSS v29
  references and remain the release gate in CI; skipping them on CRAN
  removes the false-positive risk from BLAS/platform numeric variation.
  Unit, property, invariance, and print/summary tests still run on CRAN.

# mariposa 0.6.15

Regression correctness (theme: the two regression functions compute what
they claim under weights and degenerate inputs, and say what they show).

## Bug fixes

* **Weighted `logistic_regression()`: -2LL, omnibus chi-square, and
  Cox & Snell / Nagelkerke R-squared were wrong under fractional
  weights.** The -2 log-likelihood came from `stats::logLik()`, whose
  binomial method *rounds* prior weights internally
  (`dbinom(round(m*y), round(m), mu)`). All likelihood-based statistics
  now derive from the residual/null deviance, which equals -2LL exactly
  for a 0/1 response and honors fractional frequency weights unrounded
  (Charter §5.1). Unweighted and integer-weighted results are unchanged.
  A weighted validation test with a hand-summed log-likelihood oracle
  pins the fix.
* **`linear_regression(use = "pairwise")` no longer silently drops
  interactions and transformed terms.** The pairwise path rebuilds the
  model from the raw correlation matrix, so `y ~ a * b` was silently
  fitted as `y ~ a + b`. Formulas with non-additive terms now abort with
  a pointer to `use = "listwise"`.
* **Rank-deficient fits no longer crash the unweighted
  `linear_regression()` path** ("Tibble columns must have compatible
  sizes"): perfectly collinear terms are excluded from the coefficient
  table (matching SPSS's excluded-variables handling) with a one-line
  message, and the ANOVA df now count estimated terms (`model$rank`)
  instead of raw coefficients — the same rule the weighted path has used
  since 0.6.4.

## New features

* The `linear_regression()` coefficients table now **prints the
  confidence intervals for B** (SPSS `/STATISTICS CI`) that it always
  computed; the level is the `conf.level` argument. A new
  `summary(..., conf_int = FALSE)` toggle hides the two columns.

## Documentation

* `factors = "dummy"` docs (both regressions) now state that *ordered*
  factors get R's default polynomial contrasts (`.L`/`.Q`/`.C` terms),
  not treatment dummies, and how to get dummy coding.
* The SPSS-compatibility vignette now discloses `logistic_regression()`
  as Tier 4 (textbook-formula oracle; no SPSS v29 reference run yet)
  instead of implying SPSS-validated status.
* The logistic classification-table header reads the stored cutoff
  instead of hardcoding "0.50".

# mariposa 0.6.14

Codebook robustness (theme: `codebook()` survives real-world data and says
what it shows). A stress test of the codebook stack (metadata extraction,
console print/summary, HTML builder, xlsx export) surfaced a batch of
crashes, silent data errors, and display leaks; this release fixes all of
them and adds a `view` argument for side-effect control.

## Bug fixes

* Variables with value labels but **no variable label** no longer show a
  fake label like "1 | 2 | 3": `attr(x, "label")` partially matched the
  `labels` attribute; all label reads now use `exact = TRUE`. The
  "Variables with labels" count excludes such variables accordingly.
* `codebook()` no longer errors on **inline data expressions**
  (e.g. `codebook(data.frame(...))` spanning multiple deparse lines);
  long expressions collapse to the generic dataset name "data".
* **List columns** no longer crash the frequency computation: they are
  skipped with a warning ("list column ... skipped - not supported in
  codebooks") and a data frame consisting only of list columns aborts
  with a clear error.
* The **tagged-NA breakdown is now consistent across all three layers**:
  - the HTML builder appends the missing-value rows (codes, labels,
    counts) below range-displayed variables too — previously an
    all-user-missing or high-cardinality variable silently lost them
    (the xlsx export already did this correctly);
  - NA frequencies are computed even for high-cardinality/range-displayed
    variables;
  - `print(summary(cb))` gains a "Missing values:" section (codes,
    labels, counts) — `show_na` was a no-op on the console layer before.

## Improvements

* **Central display formatting for numeric values**: empirical values and
  ranges no longer leak 15-digit doubles or scientific notation into the
  console/HTML/xlsx output. Fractional values show 4 significant digits
  ("0.3333"), whole numbers keep their integer look ("1"), tiny values
  are expanded ("0.00000001", never "1e-08"). Frequency matching is
  unaffected: display strings and raw matching keys are carried
  separately (`empirical_values` vs. new `empirical_keys`).
* The percentage and effective-n columns (`prc`, `valid_prc`, `cum_prc`,
  `n_eff`) on `write_xlsx(cb, frequencies = TRUE)` sheets are rounded to
  2 decimals.
* `max_values` (single integer >= 1) and `max_len` (single integer >= 4)
  are validated up front with a clear error.
* Factor levels now respect `max_values` and truncate with the same
  "... (N more)" note used for character values.
* Range displays show the cardinality: "18 - 95 (78 distinct)" instead
  of just "18 - 95".
* `file =` into a nonexistent directory aborts early, naming the missing
  directory.
* Polish: "1 variable" / "1 observation" pluralization in the console
  and HTML subtitle; very long character values are truncated to
  `max_len` with "..." (raw values still drive frequency matching);
  zero-row data frames say "(no observations)" instead of
  "(all missing)".

## New features

* New `view` argument for `codebook()` (default: `interactive()`):
  controls whether the HTML codebook opens in the RStudio Viewer.
  `view = FALSE` suppresses the Viewer side effect entirely; writing via
  `file =` is unaffected. The compact `print()` only advertises the
  Viewer when it was actually opened (result gains a `viewed` flag).


# mariposa 0.6.13

McDonald's omega (theme: reliability() learns a second reliability
coefficient). `reliability()` now reports McDonald's omega alongside
Cronbach's alpha — a new statistic within an existing function, hence a
PATCH per the clarified versioning policy.

## New features

* `reliability()` computes **McDonald's omega** from a one-factor
  maximum-likelihood model (`stats::factanal` on the same (weighted)
  correlation matrix already used for standardized alpha):
  - `omega` — raw/total omega in the covariance metric (analogous to
    raw alpha), reported as "McDonald's Omega";
  - `omega_std` — standardized omega in the correlation metric
    (analogous to standardized alpha);
  - `omega_if_deleted` — a new column in `item_total`, refitting the
    one-factor model per deleted item (NA when the reduced scale has
    fewer than 3 items, where the model is unidentified).
  Scales with fewer than 3 items get NA omega fields plus a warning
  (alpha is unaffected); non-convergent factor fits degrade to NA with
  the factanal message. The compact `print()` shows omega next to
  alpha, and `summary()` adds omega rows to the Reliability Statistics
  block and an omega column to the Item-Total table.

## Validation

* McDonald's omega is **Tier 4 (Internal, R-only)** for now: SPSS v27+
  offers omega in `RELIABILITY`, but IBM's algorithm documentation is
  not publicly retrievable and no SPSS v29 reference run exists yet.
  The pending reference run is prepared in
  `.claude/spss-syntax-omega-references.sps` (expected values included);
  until it lands, omega is guarded by a parameter-recovery test on
  simulated congeneric data, exact cross-checks against a manual
  factanal computation, cross-checks against `psych::omega()` and a
  lavaan/semTools one-factor CFA (`tests/testthat/test-reliability-omega.R`),
  and a `w == 1` block in the weights-invariance suite. The help page
  carries the Tier-4 disclosure; the compatibility vignette flags
  omega as Internal (Tier 4).
* `psych`, `lavaan`, and `semTools` added to Suggests (cross-check
  tests only; all gated by `skip_if_not_installed()`).


# mariposa 0.6.12

Weighted-rank correctness and accurate claims (theme: the weighted rank
family says exactly what it is). Two formula errors in weighted rank
statistics are fixed and a package-wide invariance suite now guards
every weighted entry point; alongside, the user-facing claim surface
(README, DESCRIPTION, help pages, compatibility vignette) is realigned
with what the validation suite actually covers.

## Bug fixes

* Weighted `kendall_tau()`: the tau-b denominator omitted double-tied
  pairs (`ties_both`) from the two tie-correction factors, deflating
  |tau| on tied data. The weighted denominator now mirrors the
  unweighted `(n0 - Tx - Txy)(n0 - Ty - Txy)` structure.
* Weighted `kruskal_wallis()`: the grand mean rank was still the
  hard-coded `N/2` of the pre-0.6.4 rank convention instead of
  `(N+1)/2`, inflating H. It is now derived from the weighted mid-ranks
  themselves.
* Both bugs violated the invariant that weights of exactly 1 must
  reproduce the unweighted result. A new package-wide invariance suite
  (`tests/testthat/test-weights-invariance.R`) enforces this w == 1
  reduction for every weighted entry point; intentionally approximate
  reductions (design-based `mann_whitney`, weighted Kendall z/p) are
  documented exceptions with bounded assertions.

## Accurate claims

* README and DESCRIPTION no longer overclaim: the paired t-test mode
  (not yet implemented) is no longer advertised, and "every function is
  validated ... your results will match" is replaced by the
  Charter-compliant wording — validated against SPSS v29 within
  documented per-tier tolerances, with
  `vignette("spss-compatibility")` for per-function status.
* The weighted variants of the rank-based family — `mann_whitney()`,
  `kruskal_wallis()`, `wilcoxon_test()`, `friedman_test()`,
  `binomial_test()`, `dunn_test()`, `pairwise_wilcoxon()`, and
  `kendall_tau()` — are now disclosed as R-only (Tier 4) in a
  "Weighted variants" note on each help page: SPSS `NPAR TESTS` /
  `NONPAR CORR` ignore `WEIGHT BY`, so no SPSS reference exists for
  these weighted paths. `mann_whitney()`'s note also states that its
  design-based U/W may differ from SPSS's expanded-data U (Z and p are
  the validated quantities); `oneway_anova()` now documents that
  omega-/epsilon-squared are truncated at 0 (negative raw estimates
  occur when F < 1).

## Validation

* The SPSS-compatibility vignette is regenerated (was frozen at
  2026-05-19) and now carries an "Internal (Tier 4)" marker for the
  weighted rank variants. Generator fixes: the `w_*` family and
  `scheffe_test` are correctly matched to their shared test files
  (previously shown as "not validated" despite existing tests),
  zero-match tier counts no longer report as 1, and
  `assert_spss_count()` call sites are tallied as Spec.
* `test-t-test-spss-validation.R`: the header tier table claimed the
  t-statistic at Spec (±1e-5) while the assertions use Display(3); the
  header now matches the assertions.
* The last `expect_no_error()` in a validation file
  (`test-linear-regression-spss-validation.R`) is replaced by real
  assertions on the per-group predictions; the validation-discipline
  meta-test now passes with `MARIPOSA_VALIDATION_STRICT=TRUE`.


# mariposa 0.6.11

Deprecation cleanup (theme: the due bridges come out). Two batches of
deprecations reached their removal release together: the 0.6.9 argument
bridges (originally slated for 0.6.10) and the 0.6.10 duplicate result
columns. Removing both here keeps the run-up to the 1.0 API freeze tidy.

## Breaking changes

* The 0.6.9 dot-case argument bridges are removed as announced. The old
  names no longer warn-and-work; they now error (falling through to
  tidyselect or SET-mode validation):
  - `codebook()`: `show.id`, `show.type`, `show.labels`, `show.values`,
    `show.freq`, `show.na`, `show.unused`, `max.values`, `max.len`,
    `sort.by.name` (use `show_id`, `show_type`, `show_labels`,
    `show_values`, `show_freq`, `show_na`, `show_unused`, `max_values`,
    `max_len`, `sort_by_name`)
  - `val_labels()`: `drop.na` (use `drop_na`)
  - `drop_labels()`: `drop.na` (use `drop_na`)
  These bridges were originally slated for removal in 0.6.10 and are
  batched into this release. The `frequency()`/`rec()`/`to_label()`
  family of removed-argument errors introduced in 0.6.9 remain in place
  as permanent guidance (their `...` consumes tidyselect, so a clear
  error beats a silent misinterpretation).

* The deprecated duplicate result columns kept for one release in 0.6.10
  are removed; only the canonical column remains:
  - `chisq_gof()`, `friedman_test()`: `chi_sq` removed (use `chi_squared`)
  - `mcnemar_test()`: `statistic` removed (use `chi_squared`)
  - `mann_whitney()`: `effect_size_r` removed (use `r_effect`)
  - `oneway_anova()`: `F_stat` removed (use `F_statistic`)
  The statistical values are unchanged; only the redundant column names
  go away.


# mariposa 0.6.10

Result-column harmonization (theme: one statistic, one column name).
A style audit found the same statistic carrying different result-column
names across sibling functions; the drifted names now converge on the
canonical spelling, with the old columns kept as duplicates for one
release.

## Improvements

* The `$results` columns for shared statistics are harmonized on the
  canonical names already used elsewhere in the package:
  - Chi-square statistic: `chi_squared` (as in `chi_square()`) - now
    also in `chisq_gof()`, `friedman_test()`, and `mcnemar_test()`
  - Effect size r: `r_effect` (as in `wilcoxon_test()`) - now also in
    `mann_whitney()`
  - F statistic: `F_statistic` (as in `levene_test()`) - now also in
    `oneway_anova()`
  Print and summary methods read the canonical columns; the statistical
  values are unchanged.

## Deprecations

* The old result-column names remain available as duplicated columns
  (positioned right after their canonical counterpart) for one release
  and will be removed in 0.6.11:
  - `chisq_gof()`, `friedman_test()`: `chi_sq` (use `chi_squared`)
  - `mcnemar_test()`: `statistic` (use `chi_squared`)
  - `mann_whitney()`: `effect_size_r` (use `r_effect`)
  - `oneway_anova()`: `F_stat` (use `F_statistic`)


# mariposa 0.6.9

API-cleanup completion (theme: the 0.6.8 bridges come out, the last
dot-case stragglers get theirs). One step closer to the 1.0 API freeze.

## Breaking changes

* The 0.6.8 deprecation bridges are removed as announced. The dot-case
  argument names now error instead of warning:
  - `frequency()`/`fre()`: `sort.frq`, `show.na`, `show.prc`,
    `show.valid`, `show.sum`, `show.labels`, `show.unused`
  - `rec()`: `as.factor`, `var.label`, `val.labels`
  - `to_label()`/`to_character()`/`to_numeric()`: `drop.na`,
    `drop.unused`, `add.non.labelled`, `use.labels`, `start.at`,
    `keep.labels`
  - `read_spss()`/`read_por()`/`read_stata()`/`read_sas()`/`read_xpt()`:
    `tag.na`
  Before: `frequency(data, x, sort.frq = "desc")` warned and worked.
  After: it errors with a pointer to `sort_frq`. In the functions whose
  `...` selects variables, the old names raise a clear "removed in
  0.6.9" error instead of being silently swallowed by tidyselect; in
  the readers they fail as unused arguments.

## Deprecations

* The remaining dot-case arguments are renamed to snake_case with the
  usual one-release bridge (old names warn once per session; removal
  planned for 0.6.10):
  - `codebook()`: `show_id`, `show_type`, `show_labels`, `show_values`,
    `show_freq`, `show_na`, `show_unused`, `max_values`, `max_len`,
    `sort_by_name`
  - `val_labels()`: `drop_na`
  - `drop_labels()`: `drop_na`
  The display options stored on codebook results (`result$options`) use
  the snake_case keys as well.


# mariposa 0.6.8

API-unification release (theme: snake_case arguments). One release-long
deprecation bridge per the versioning policy - old names keep working
and warn once per session; they will be removed in 0.6.9.

## Breaking changes (with bridge)

* Dot-case arguments renamed to snake_case:
  - `frequency()`/`fre()`: `sort_frq`, `show_na`, `show_prc`,
    `show_valid`, `show_sum`, `show_labels`, `show_unused`
  - `rec()`: `as_factor`, `var_label`, `val_labels`
  - `to_label()`/`to_character()`/`to_numeric()`: `drop_na`,
    `drop_unused`, `add_non_labelled`, `use_labels`, `start_at`,
    `keep_labels`
  - `read_spss()`/`read_por()`/`read_stata()`/`read_sas()`/`read_xpt()`:
    `tag_na`
  Base-R-universal names (`na.rm`, `conf.level`, `var.equal`) are kept.

## Breaking changes (no bridge)

* `t_test()` results no longer carry the duplicated `CI_lower`/`CI_upper`
  alias columns; `conf_int_lower`/`conf_int_upper` are the contract.

## Improvements

* `sort_frq` is validated (`"none"/"asc"/"desc"`) - typos used to
  silently produce an unsorted table; `show_labels` validates its
  `TRUE`/`FALSE`/`"auto"` values with a clear error.
* Vignettes showcase the new argument names.


# mariposa 0.6.7

Output-layer release (theme: uniform three-layer output). Statistical
results are unchanged; what changed is how results present themselves.

## Uniform three-layer output (visible change)

Every analysis class now follows the documented pattern that t_test and
chi_square pioneered: `result` prints a compact overview (headline
statistic, p-value, significance stars, one line per test), and
`summary(result)` carries the full detailed output behind boolean
section toggles. Newly migrated: kruskal_wallis, wilcoxon_test,
friedman_test, binomial_test, fisher_test, chisq_gof, mcnemar_test,
levene_test, tukey_test, scheffe_test, dunn_test, pairwise_wilcoxon,
frequency, crosstab (describe was already compact and gained the
summary layer for uniformity). Nothing was removed - everything the
old print() showed is in summary(), verified line-by-line.

## Internal architecture

* One shared engine for the three correlation functions
  (pearson/spearman/kendall results verified byte-identical across 21
  scenarios; ~500 lines removed).
* The w_* factory now supports multi-value and non-numeric statistics;
  w_quantile() and w_modus() are ordinary plugins instead of pipeline
  reimplementations (~420 lines removed, results identical across 31
  scenarios).


# mariposa 0.6.6

Internal-architecture release (theme: shared cores and formatting
utilities). No statistical results change; table rendering in the
Tukey/Scheffe output is now aligned and uses SPSS-style p display.

* One home for every weighted formula: the new weighted-statistics
  kernel file backs describe(), frequency(), the w_* functions and the
  rank tests; the weighted variance formula previously existed in six
  files.
* New internal output utilities (bordered table renderer, grouped-results
  iterator, unified number/p formatting) - the building blocks the print
  style guide documented; adoption started with the post-hoc tests.
* Tukey and Scheffe now share one engine and one print implementation
  (results verified byte-identical); t_test() and oneway_anova() were
  restructured from 500-line nested-closure bodies into short
  orchestrators with file-level helpers (byte-identical results).
* Weights in summarise() context are captured as quosures
  (enquo/eval_tidy) instead of frame-walking; shared validators report
  errors at the user-facing call site.
* Documentation internals standardized on @noRd (man/ shrinks by ~120
  internal pages).


# mariposa 0.6.5

Housekeeping release (theme: package hygiene). No statistical results
change.

* Slimmer dependencies: removed the unused tidyr import and pruned unused
  `importFrom` entries.
* Error chains: failures inside grouped analyses are re-thrown with
  `cli_abort(parent = ...)` so the original condition is preserved;
  the haven requirement is enforced by one central guard that reports
  the calling function instead of an internal helper.
* Import internals: the native-missing-value detection shared by
  `read_stata()`, `read_sas()` and `read_xpt()` now lives in one helper
  instead of three copies.
* Mechanical polish: remaining `sapply()` calls in the oldest files
  converted to type-stable `vapply()`; pkgdown reference now lists
  `phi()`, `cramers_v()`, `goodman_gamma()`.
* Test suite: removed a legacy tolerance registry that contradicted the
  Validation Charter and was no longer used by any test.


# mariposa 0.6.4

A quality release. Following an in-depth internal review of the entire
statistical codebase, this version sharpens the accuracy of several
statistics, makes the package behave more consistently across functions,
and adds a dedicated regression-test suite
(`tests/testthat/test-audit-regressions.R`) so these guarantees hold in
future releases. Some outputs change slightly as a result - in every case
toward the standard reference implementations.

## More accurate statistics

* Kendall's tau-b significance test now agrees with `stats::cor.test()`
  (and the SPSS formula) to machine precision, which is most noticeable
  for heavily tied data such as binary variables.
* The weighted Wilcoxon signed-rank test (and `pairwise_wilcoxon()`) now
  uses frequency-expansion mid-ranks: with integer weights the statistic
  equals the expanded-data Wilcoxon exactly, and `weights = 1` reproduces
  the unweighted test. Displayed rank means in the weighted
  Kruskal-Wallis and Dunn tests follow the same convention.
* The Mann-Whitney asymptotic p-value now matches its reported Z
  (both follow the SPSS convention without continuity correction).
* Regression degrees of freedom are now derived from the fitted model
  terms, improving results for models with dummy-coded factors or
  interaction terms (weighted linear regression and the logistic
  omnibus test).
* The Kruskal-Wallis effect size is now correctly labelled: the returned
  field is `epsilon_squared` (previously named `eta_squared`).

## New and refined API

* `linear_regression()` gains SPSS-style collinearity diagnostics
  (Tolerance and VIF per model term), including a `collinearity` toggle
  in `summary()`.
* `phi()`, `cramers_v()`, and `goodman_gamma()` now return the requested
  effect size directly as a numeric value - the convenient behavior
  their names suggest. For the full test output, use `chi_square()`.
* The weighted two-sample `t_test()` now honors `var.equal` for its
  primary result. `oneway_anova()` always reports both the classical and
  Welch results (like SPSS ONEWAY), so its `var.equal` argument is
  deprecated; `ss_type` in `factorial_anova()`/`ancova()` is likewise
  deprecated in favor of the SPSS-standard Type III.

## More consistent behavior

* One package-wide weights policy: invalid (negative) weights are now
  rejected with a clear message at every entry point, instead of being
  handled differently depending on the function.
* The weighted median now always equals the weighted 50th percentile,
  and unweighted quantiles follow the SPSS convention (Type 6/HAVERAGE)
  throughout.
* `frequency()` header statistics use the same formulas as `describe()`
  and the `w_*` functions.
* Significance stars follow a single boundary convention everywhere,
  matching the printed legend.

## More robust in edge cases

* Correlation functions handle constant variables gracefully (NA instead
  of an error), `frequency(show.unused = TRUE)` works on variables
  tagged via `set_na()`/`read_spss()`, and `frequency(sort.frq =)` now
  sorts by frequency with a monotone cumulative-percent column.
* `write_spss()` protects valid values when many missing-value codes
  must be consolidated into a range, and explains what it is doing.
* `logistic_regression()` surfaces separation and convergence warnings
  again; post-hoc tests report when a computation could not be carried
  out instead of skipping it silently.

## Housekeeping

* Internal code paths were consolidated (shared helpers for weighted
  mid-ranks and the Wilcoxon core) and a substantial amount of unused
  code was removed, making the codebase easier to maintain.
* Grouped single-variable `w_*` results print their statistics again.


# mariposa 0.6.3.2

## `rec()` reliably matches decimal single values

Single-value recode rules now match decimal codes (e.g. `"3.6=2"`) even
when the stored value carries floating-point representation error. The
single-value comparison was changed from exact numeric equality
(`x == value`) to a string comparison (`as.character(x) == as.character(value)`),
which rounds to 15 significant digits and thereby absorbs the error.

Reason: a value such as `0.1 + 0.2` is stored as `0.30000000000000004`,
so the previous exact `==` test silently failed to match a rule `"0.3=..."`.
This mirrors the behaviour of `sjmisc::rec()`, on which `rec()`'s string
syntax is modelled. Range rules were already robust (they use `>=`/`<=`)
and are unchanged.


# mariposa 0.6.3.1

## broom tidiers now work natively

Adds explicit `tidy()`, `glance()`, and `augment()` methods for both
`linear_regression` and `logistic_regression` results, registered via
the standard `s3_register()` pattern (broom in Suggests, no hard dep).

Reason: with `class(r) = c("linear_regression", "lm")`, `broom::tidy.lm()`
and `broom::glance.lm()` dispatched as expected, but internally called
`summary(x)` — which (because of our specialised
`summary.linear_regression()` overriding `summary.lm`) returned the
mariposa SPSS-style summary instead of the lm summary broom needs.
The visible failures:

* `broom::glance(r)` raised `object 'r.squared' not found` because
  mariposa's summary stores it as `R_squared`.
* `broom::tidy(r, conf.int = TRUE)` returned only 4 columns
  (`term`, `estimate`, `conf.low`, `conf.high`) instead of the expected
  6+ (`term`, `estimate`, `std.error`, `statistic`, `p.value`,
  `conf.low`, `conf.high`).

The new methods strip our `linear_regression` / `logistic_regression`
class before delegating to `broom::tidy.lm` / `tidy.glm` etc., so the
inner `summary()` call dispatches to `summary.lm` / `summary.glm` and
broom receives its expected shape. The user-facing `summary(r)` still
returns mariposa's SPSS-style output (more specific method wins).

Edge cases stay consistent with the rest of the lm-generic surface:
`broom::tidy()` / `glance()` / `augment()` on a grouped or pairwise
result raise an actionable error pointing at `lapply(r$groups, ...)`
or `use = "listwise"`.

New tests in `test-broom-methods.R` cover all three tidiers for both
regression types, plus the grouped/pairwise error paths.

# mariposa 0.6.3

## Behavior Change — regression results inherit from `lm` / `glm`

`linear_regression()` and `logistic_regression()` results now ARE the
fitted `lm` / `glm` object (with mariposa-specific tables attached as
additional slots), instead of wrapping it in `$model`. All base-R and
`broom` generics dispatch natively:

```r
r <- linear_regression(survey_data, life_satisfaction ~ age + income)
coef(r)                                 # named numeric vector
predict(r, newdata = head(survey_data)) # works directly
anova(r)                                # sequential SS table
vcov(r); confint(r); residuals(r); fitted(r)
broom::tidy(r); broom::glance(r); broom::augment(r)
```

Class hierarchy is `c("linear_regression", "lm")` for linear and
`c("logistic_regression", "glm", "lm")` for logistic. `summary(r)`
still returns the SPSS-style mariposa summary (more specific method
wins); for the raw `lm`/`glm` summary call `stats::summary.lm(r)` /
`stats::summary.glm(r)`.

### Slot renames (breaking)

Two slots collided with `lm`/`glm` conventions and were renamed:

| Before        | After          |
|:------------- |:-------------- |
| `$coefficients` (tibble) | `$coef_table` (tibble)  |
| `$anova` (tibble)        | `$anova_table` (tibble) |
| `$model` (lm/glm)        | the object IS the model — use `r` directly |

Migration:

* `r$coefficients` → `r$coef_table` (SPSS-style tibble) or `coef(r)`
  (named numeric vector).
* `r$anova` → `r$anova_table` (SPSS-style overall-model ANOVA tibble)
  or `anova(r)` (R's per-term sequential SS table).
* `r$model |> predict(...)` → `predict(r, ...)` directly.
* `r$model |> broom::tidy()` → `broom::tidy(r)` directly.

### Edge cases

* `use = "pairwise"`: no single fitted lm is available, so the result
  is a custom list with class `"linear_regression"` only.
  `predict()`/`anova()` etc. raise an informative error pointing at
  `use = "listwise"`.
* Grouped results (top-level): no single model. `predict()`/`anova()`
  raise an informative error pointing at `lapply(r$groups, predict,
  ...)`. Each `r$groups[[i]]` is itself an lm-inheriting object, so
  per-group generics work directly.

## Test Suite

* New test block in `test-linear-regression-spss-validation.R` verifies
  that `coef()`, `predict()`, `anova()`, `vcov()`, `confint()`,
  `residuals()`, `fitted()`, `formula()`, `nobs()`, `model.matrix()`
  all dispatch natively, plus the grouped/pairwise error paths.
* 1184/1184 tests pass; R CMD check on built tarball: Status OK.

# mariposa 0.6.2

## Behavior Change

### `linear_regression()` and `logistic_regression()`: factor predictor handling

Both regression functions now expose a `factors` argument controlling how
factor predictors enter the model. The new default `factors = "dummy"`
matches base R `lm()` / `glm()`: a factor with `L` levels expands into
`L - 1` dummy contrasts via `stats::model.matrix()`. Previous versions
silently coerced factor levels to integer codes (SPSS ordinal-as-scale
default) with no warning, which surprised users who relied on standard R
semantics.

To restore the previous SPSS-style behavior, pass `factors = "numeric"`
explicitly. That mode emits a one-line `cli::cli_inform()` listing the
coerced variables for transparency. The "numeric" mode is required to
reproduce SPSS `REGRESSION` / `LOGISTIC REGRESSION` output when factor
predictors carry ordered meaning (e.g., a 4-level education variable
treated as 1–4 ordinal scale).

Behavioral consequences:

* A model with a 3-level factor predictor that previously returned one
  coefficient row now returns two dummy-contrast rows under the new default.
* For pairwise missing handling (`use = "pairwise"`), factor predictors are
  not supported with `factors = "dummy"`; the function now errors with an
  actionable message pointing to either `factors = "numeric"` or
  `use = "listwise"`.

Migration: scripts that depend on the old SPSS-style coercion should set
`factors = "numeric"` at the call site. The `cli_inform()` message can be
silenced with `suppressMessages()` if desired.

## Source-Code Fixes (weighted regression)

Two more functions joined the Charter §5.1 audit list (the "unrounded
`sum(w)`" weighted-statistics convention previously applied to `t_test`,
`oneway_anova`, and `levene_test`):

* `linear_regression()`: weighted variance, SE, df, F, R², and adjusted-R²
  now use the unrounded `sum(weights)` throughout. Earlier versions used
  `n_effective <- round(sum(w))` in df and MS calculations, producing
  systematic drift from SPSS REGRESSION (off by ~0.001 on F, ~0.01 on adj-R²
  for typical weights). The displayed N is still `round(sum(w))`.
* `logistic_regression()`: pseudo-R² formulas (Cox & Snell, Nagelkerke,
  McFadden) now use the unrounded `sum(weights)` in the exponential
  denominator. The displayed N and rounded classification counts remain
  integers.

These are bug fixes; weighted results may shift slightly toward closer
agreement with SPSS v29.

## Other Fixes

* `summary.linear_regression(descriptives = TRUE)` now actually prints the
  Descriptive Statistics table (Variable, Mean, SD, N). Previously the
  parameter was accepted but documented as "Reserved for future use" and
  produced no output.
* The compact `print.linear_regression()` no longer crashes on weighted
  models with non-integer df: the F-statistic line now rounds df for
  display before formatting with `%d`.
* Roxygen examples for `summary.linear_regression` (`collinearity = FALSE`)
  and `summary.logistic_regression` (`classification_table = FALSE`)
  referenced parameters that do not exist; corrected to `descriptives =
  FALSE` and `classification = FALSE` respectively.

## Test Suite

The `linear_regression` SPSS validation test suite expanded from 1
scenario (unweighted bivariate) to 6 scenarios covering all four Charter §8
quadrants — Tests 1a, 1c, 2a, 2c, 3a, and 4a from
`tests/spss_reference/outputs/linear_regression_output.txt`. The weighted
scenarios (2a, 2c, 4a) verify the Charter §5.1 fix above. New behavioral
tests cover the `factors` argument (dummy expansion, numeric coercion,
pairwise + dummy + factor error path). 222/222 assertions pass.

# mariposa 0.6.1

## Validation

Substantial hardening of the SPSS-compatibility test suite. All 29 SPSS-
validation test files were rewritten under a new Validation Charter (see
`vignette("spss-compatibility")`) that defines tolerance tiers
(Spec / Display / Exception / Internal),
forbids inline tolerance literals, NA placeholders, and `expect_true(TRUE)`
reporting blocks, and requires citation comments linking every reference
value to its source line in `tests/spss_reference/outputs/`.

* 1832+ passing assertions across all 29 validation files, 0 failures.
* New `tests/testthat/helper-validation-tolerances.R` provides
  `assert_spss()` and `tol()` helpers with explicit tier semantics.
* New `tests/testthat/test-validation-discipline.R` meta-test lints
  validation files for Charter-forbidden patterns.
* New `vignettes/spss-compatibility.Rmd` reports per-function validation
  status, auto-generated from the test suite.
* New CI workflow `.github/workflows/strict-validation.yaml` runs the
  full suite in strict-discipline mode on release tags and weekly.

## Source-Code Fixes (weighted statistics)

Three weighted statistical functions were corrected to use unrounded
`sum(w)` per SPSS frequency-weights convention. Earlier versions rounded
too early and produced systematic drift from SPSS in weighted scenarios.

* `t_test()`: weighted variance, SE, and df calculations now use
  unrounded `sum(w)` (one-sample and two-sample paths). Welch-
  Satterthwaite df now derived from unrounded per-group weighted N.
* `oneway_anova()`: weighted variance divisor is now `(sum(w) - 1)`
  (sample formula, not population). Weighted SE uses `sqrt(sum(w))`,
  not `sqrt(physical n)`. Weighted CI t-critical-value uses
  `df = sum(w) - 1`, not Kish design-effective N. `df_within` now uses
  `floor(sum(w)) - k` (SPSS ONEWAY-specific convention).
* `levene_test()`: weighted Levene df now uses unrounded `sum(w) - k`
  (SPSS T-TEST family convention).

These changes are bug fixes and may slightly shift weighted-scenario
results in user code. Differences are small (typically < 0.01 on F or t)
and bring mariposa into closer agreement with SPSS v29.

## SPSS-Compatibility Vignette

`vignette("spss-compatibility")` documents the per-function validation
status, the four tolerance tiers, and the SPSS-procedure-specific
WEIGHT BY conventions discovered during the migration:

* T-TEST family: unrounded `sum(w)`
* ONEWAY: `floor(sum(w))`
* UNIANOVA: Type III SS
* NPAR TESTS: WEIGHT BY effectively ignored
* NONPAR CORR (Spearman, Kendall): WEIGHT BY effectively ignored
* CORRELATIONS (Pearson): WEIGHT BY honored
* CROSSTABS, FREQUENCIES, RELIABILITY, FACTOR, REGRESSION: WEIGHT BY honored

## DESCRIPTION

* `Title` shortened to "SPSS-Compatible Statistical Tools for Survey Data"
  (CRAN soft-limit compliance).
* Suggests cleanup: removed `PMCMRplus` and `survey` (no longer needed).

## Audit-Driven Math Fixes (post-Phase-1)

A second audit pass identified additional math defects and test fudges,
all corrected in this release:

* `dunn_test()`: SE now includes the Dunn (1964) / Conover (1999) tie
  correction. Previous versions systematically under-estimated `|Z|`
  on tied data (e.g., Likert scales). Baselines regenerated from
  `PMCMRplus::kwAllPairsDunnTest` (exact match to 4 decimals).
* `friedman_test()`: weighted branch now applies the tie correction
  consistently with `stats::friedman.test` (unweighted branch). The
  inconsistency caused weighted chi-squared values to be too low for
  tied data.
* `describe()`: weighted skewness and kurtosis now delegate to
  `.calc_skewness()` / `.calc_kurtosis()` in `helpers.R` (Joanes-Gill
  Type-2 with `Σw` substitution), matching `w_skew()` / `w_kurtosis()`
  and SPSS FREQUENCIES exactly. The previous duplicate implementation
  used a simple weighted moment without bias correction.
* `.w_quantile()`: weighted quantiles now use Type-6 (HAVERAGE) linear
  interpolation between cumulative-weight crossings — matches SPSS
  FREQUENCIES /PERCENTILES. Unweighted quantiles also switched from
  R default `type = 7` to SPSS-compatible `type = 6`.

## Documentation Honesty

Several SPSS-compatibility claims were narrowed to reflect what the
code actually does:

* Source comments and test-file headers for the weighted paths of
  `kruskal_wallis()`, `wilcoxon_test()`, and `friedman_test()` corrected
  from "design-based" / "Lumley-Scott" to "frequency-weighted
  approximation". Only `mann_whitney()` is a genuine Lumley & Scott (2013)
  implementation; the others substitute `sum(w)` for `n` in the standard
  variance formula.
* `mann_whitney()` test now includes a permanent cross-check against
  `survey::svyranktest()` (skipped when survey is not installed).
* `spearman_rho()`: `weights` parameter docstring rewritten to disclose
  that weights are used only for case filtering (per SPSS NONPAR CORR
  convention), not in the rank correlation itself.
* `pearson_cor()`: docstring now warns that the weighted-df convention
  (`n = sum(w)`) gives spuriously narrow CIs for raw expansion weights;
  users with such weights should normalize first.
* `logistic_regression()`: test file replaced with property-based
  assertions (Wald formula, Sig from chi-sq, exp(B) vs independent
  2x2 odds ratio, Cox & Snell/Nagelkerke/McFadden from textbook
  formulas, Omnibus from likelihood ratio). No longer a tautological
  glm-vs-glm self-comparison.

## Code Smell Cleanup

* `oneway_anova()`: removed dead-code overwrite of `grand_mean_welch`
  in the weighted Welch path.
* `levene_test()`: stale comment claiming `df2 = floor(sum(w)) - k`
  corrected — the code uses unrounded `sum(w) - k` (T-TEST family
  convention).

# mariposa 0.6.0

## New Functions — Label Management

This release adds 10 label management functions for working with labelled
survey data (inspired by `sjlabelled`, consolidated into a clean, consistent
API), plus data transformation, row operations, and data exploration functions.

### Variable & Value Labels

* New `var_label()`: dual-mode function for getting and setting variable labels.
  `var_label(data)` returns all variable labels as a named character vector;
  `var_label(data, x = "Age", y = "Gender")` sets labels for specific columns.
  Supports tidyselect for column selection when getting labels.

* New `val_labels()`: dual-mode function for getting and setting value labels.
  `val_labels(data)` returns all value labels as a named list;
  `val_labels(data, x = c("Low" = 1, "High" = 2))` sets labels.
  Use `.add = TRUE` to extend existing labels without replacing them.

* New `copy_labels()`: copies all label attributes (variable labels, value labels,
  class, tagged NA metadata) from a source data frame to matching columns in the
  target. Essential for preserving labels after `dplyr` operations that strip
  attributes.

* New `drop_labels()`: removes value labels for values that do not actually occur
  in the data. Use `drop.na = TRUE` to also remove labels for tagged NA values.

### Type Conversions

* New `to_label()`: converts `haven_labelled` vectors to factors, using value
  labels as factor levels. Supports `ordered`, `drop.na`, `drop.unused`, and
  `add.non.labelled` options. Factor levels are ordered by their original numeric
  codes (not alphabetically).

* New `to_character()`: converts `haven_labelled` vectors to character, replacing
  numeric codes with their label text.

* New `to_numeric()`: converts factors or labelled vectors to numeric. When
  `use.labels = TRUE`, uses value labels if they are numeric; otherwise assigns
  sequential integers (controlled by `start.at`).

* New `to_labelled()`: converts factors, character, or numeric vectors to
  `haven_labelled` with proper value labels. Factor levels become value labels
  automatically.

### Missing Value Management

* New `set_na()`: declares specific numeric values as missing (NA or tagged NA).
  Supports unnamed values (applied to all numeric columns) and named pairs for
  per-variable control (e.g., `set_na(data, income = c(-9, -8))`). With
  `tag = TRUE` (default), creates tagged NAs that integrate with `na_frequencies()`,
  `frequency()`, and `codebook()`. Can be called incrementally to add new missing
  value codes.

* New `unlabel()`: strips all label metadata from variables, converting
  `haven_labelled` vectors to plain base R types. Removes variable labels, value
  labels, tagged NA metadata, and format attributes. Tagged NAs become regular NA.
  Supports tidyselect for selective column unlabelling.

## New Functions — Data Transformation

* New `rec()`: flexible recoding with string syntax (e.g.,
  `rec(data, x, rec = "1:2=1 [Low]; 3:5=2 [High]")`). Supports value ranges,
  `min`/`max` keywords, `copy` for unchanged values, and automatic value label
  generation from bracket syntax. Works with numeric, character, and labelled
  vectors.

* New `to_dummy()`: creates dummy (indicator) variables from categorical or
  labelled vectors. Generates one 0/1 column per unique value with informative
  column names. Supports tidyselect for multi-variable dummy coding and
  `suffix = "label"` to use value labels in column names.

* New `std()`: z-standardization with four methods (`"sd"`, `"2sd"`, `"mad"`,
  `"gmd"`). Supports survey weights, grouped standardization via
  `dplyr::group_by()`, and `robust = TRUE` for median/MAD-based standardization.

* New `center()`: mean-centering (grand-mean or group-mean). Supports survey
  weights and `dplyr::group_by()` for group-mean centering. Returns centered
  values with the centering value stored as an attribute.

## New Functions — Row Operations

* New `row_means()`: computes row-wise means across selected columns, with
  `min_valid` parameter matching SPSS `MEAN.x()` syntax. Designed for use inside
  `dplyr::mutate()`. Replaces the deprecated `scale_index()`.

* New `row_sums()`: computes row-wise sums across selected columns, with
  `min_valid` parameter for minimum valid (non-NA) values.

* New `row_count()`: counts occurrences of specific values per row. Useful for
  counting endorsements in multi-item scales (e.g., how many items a respondent
  agreed with).

## New Functions — Data Exploration

* New `find_var()`: searches variables by name or label using regular expressions.
  Returns matching variable names with their labels. Useful for exploring large
  survey datasets with many variables.

## Breaking Changes

* `scale_index()` has been removed and replaced by `row_means()`, which provides
  the same functionality with a clearer name. Update existing code:
  `scale_index(data, x, y, z)` → `row_means(data, x, y, z)`.

# mariposa 0.5.6

## New Functions

* New `write_spss()` function: exports data frames to SPSS `.sav` format with
  full tagged NA roundtripping. Tagged NAs are converted back to SPSS user-defined
  missing values, enabling lossless roundtrips via `read_spss()` -> processing ->
  `write_spss()`. Supports byte, none, and zsav compression.

* New `write_stata()` function: exports data frames to Stata `.dta` format.
  Tagged NAs from any source format are written as Stata extended missing values
  (`.a` through `.z`). Supports Stata versions 8-15.

* New `write_xpt()` function: exports data frames to SAS transport `.xpt` format.
  Tagged NAs are written as SAS special missing values (`.A` through `.Z`, `._`).
  Supports transport versions 5 and 8.

## Enhancements

* mariposa now provides a unified data import/export platform:
  - **Import**: `read_spss()`, `read_por()`, `read_stata()`, `read_sas()`,
    `read_xpt()`, `read_xlsx()`
  - **Export**: `write_spss()`, `write_stata()`, `write_xpt()`, `write_xlsx()`
* Cross-format export is supported: data imported from one format can be exported
  to another (e.g., SPSS to Stata) with automatic missing value type conversion.

# mariposa 0.5.5

## New Functions

* New `read_xlsx()` function: reads Excel (`.xlsx`) files with automatic label
  reconstruction. When reading back files created by `write_xlsx()`, variable
  labels, value labels, and tagged NA metadata are fully restored -- enabling
  lossless roundtripping of labelled survey data through Excel.
  - Auto-detects mariposa export format (data frame, list, codebook)
  - Reconstructs `haven_labelled` columns, factor levels, and variable labels
  - Restores tagged NAs with `na_tag_map` from missing codes in the data
  - Works as a plain Excel reader for non-mariposa files

* New `write_xlsx()` generic: exports data frames, codebooks, and named lists
  to Excel (`.xlsx`) with full support for variable labels, value labels, and
  tagged NA metadata. Uses `openxlsx2` as an optional dependency.
  - `write_xlsx(data, "file.xlsx")` -- data + "Labels" reference sheet with
    variable labels, value labels, and missing value codes
  - `codebook(data) |> write_xlsx("codebook.xlsx")` -- structured codebook
    workbook with Overview, Codebook, and optional per-variable frequency
    sheets (`frequencies = TRUE`)
  - `write_xlsx(list(a = df1, b = df2), "multi.xlsx")` -- multi-sheet export
    where each named list element becomes a sheet

## Enhancements

* `write_xlsx()` now preserves tagged NA codes (-9, -11, etc.) as visible
  values in the data sheet instead of empty cells, enabling perfect
  roundtripping with `read_xlsx()`. System NAs remain as empty cells.
* The "Labels" sheet now includes a `Column_Type` column (`haven_labelled` or
  `factor`) so `read_xlsx()` can deterministically reconstruct column types.

## Dependencies

* Added `openxlsx2` as a suggested dependency for Excel import/export.

# mariposa 0.5.4

## New Functions

* New `read_stata()` function: reads Stata `.dta` files and annotates native
  extended missing values (`.a` through `.z`) for use with mariposa's tagged NA
  system. Stata tagged NAs are preserved automatically by haven; `read_stata()`
  adds the `na_tag_map` attribute for seamless integration with `na_frequencies()`,
  `frequency()`, and `codebook()`.

* New `read_sas()` function: reads SAS `.sas7bdat` files with optional catalog
  file (`.sas7bcat`) for value labels. Annotates SAS special missing values
  (`.A` through `.Z` and `._`) for tagged NA integration.

* New `read_xpt()` function: reads SAS transport files (`.xpt`) with tagged
  missing value support. Transport files are the FDA-approved, platform-independent
  SAS data format.

* New `read_por()` function: reads SPSS portable `.por` files with the same
  tagged NA support as `read_spss()`. Shares the SPSS missing value conversion
  logic internally.

## Breaking Changes

* `na_frequencies()` column `spss_code` has been renamed to `code` to reflect
  multi-format support. The column now contains character values: numeric SPSS
  codes (e.g., `"-9"`) or native format codes (e.g., `".a"` for Stata,
  `".A"` for SAS).

## Improvements

* `na_frequencies()`, `untag_na()`, and `strip_tags()` now work universally
  with data from all supported formats (SPSS, Stata, SAS).

* `untag_na()` is now format-aware: for Stata and SAS data (where tagged NAs
  are the native representation with no numeric codes to recover), it warns
  and falls back to `strip_tags()` behavior.

* `frequency()` and `codebook()` automatically display format-appropriate
  missing value codes (e.g., `-9` for SPSS, `.a` for Stata, `.A` for SAS).

# mariposa 0.5.3

## New Functions

* New `read_spss()` function: reads SPSS `.sav` files and preserves user-defined
  missing values as tagged NAs instead of converting them to regular `NA`. This
  allows distinguishing between different types of missing data (e.g., "no answer",
  "not applicable", "refused") while still treating them as `NA` in standard R
  operations. Fixes the `sjlabelled::read_spss(tag.na=TRUE)` crash on large
  datasets (e.g., ALLBUS) caused by out-of-bounds `letters[]` indexing.

* New `na_frequencies()` function: shows a breakdown of the different types of
  missing values in a tagged NA variable, with counts, original SPSS codes, and
  value labels.

* New `untag_na()` function: converts tagged NAs back to their original SPSS
  missing value codes (e.g., -9, -8, -42).

* New `strip_tags()` function: converts all tagged NAs to regular (untagged) `NA`
  values, producing the same result as reading with `haven::read_sav()` directly.

## Improvements

* `frequency()` now displays tagged NAs individually when data was imported with
  `read_spss()`. Each missing value type is shown as a separate row with its
  original SPSS code and label, followed by a "Total Valid" and "Total Missing"
  summary row.

* `frequency()` with `show.unused = TRUE` correctly handles tagged NA labels
  (no longer shows them as unused with freq=0).

# mariposa 0.5.2.2

## Convenience

* New `fre()` shorthand alias for `frequency()`. Both functions are identical;
  `fre()` simply provides a quicker way to call frequency analysis.
  `?fre` shows the same help page as `?frequency`.

# mariposa 0.5.2

## New Features

* New `codebook()` function: generates an interactive HTML data dictionary
  displayed in the RStudio Viewer pane. Shows variable ID, name, type, label,
  empirical values, value labels, and frequencies in a clean, scrollable table.
  Inspired by sjPlot's `view_df()` but built natively with `htmltools`.

* HTML codebook features a subtle-accent design: dark header, alternating row
  stripes, monospace type badges, and per-value frequency counts displayed as
  vertical lists aligned across columns.

* Console `print()` shows a minimal metadata overview (variable count,
  observations, types). Full details are reserved for the HTML viewer.

* `summary()` method provides a detailed text-based fallback with toggleable
  sections (`overview`, `variable_details`, `value_labels`).

* Supports tidyselect variable selection, optional survey weights for weighted
  frequencies, and `sort.by.name` ordering.

## Dependencies

* Added `htmltools` as an imported dependency for HTML codebook generation.

# mariposa 0.5.1

## Three-Layer Output System

* All 13 analysis functions now support `summary()` for detailed SPSS-style
  output with toggleable sections. The three-layer pattern works as follows:
  - `print()` — compact one-line overview (default when typing the object name)
  - `summary()` — builds a detailed summary object with boolean section toggles
  - `print.summary()` — renders the full verbose output with all requested sections

* Supported functions: `t_test()`, `oneway_anova()`, `factorial_anova()`,
  `ancova()`, `chi_square()`, `mann_whitney()`, `pearson_cor()`, `spearman_rho()`,
  `kendall_tau()`, `reliability()`, `efa()`, `linear_regression()`,
  `logistic_regression()`.

* Each `summary()` method accepts boolean parameters to control which output
  sections are displayed (e.g., `summary(result, effect_sizes = FALSE)` or
  `summary(result, descriptives = FALSE)`).

## Internal Helpers

* Added `build_summary_object()` and `format_p_compact()` in
  `R/summary_helpers.R` as shared infrastructure for all summary methods.

## Documentation

* Complete Roxygen2 documentation for all 39 S3 methods (13 print + 13 summary
  + 13 print.summary), each with `@description`, `@param`, `@return`,
  `@examples`, and `@seealso`.

* All 13 main function `@examples` now demonstrate the three-layer output
  pattern (`result`, `summary(result)`, `summary(result, toggle = FALSE)`).

* Added `print.reliability()` and `print.efa()` documentation (previously
  undocumented).

## Bug Fixes

* Fixed `print.chi_square()` Roxygen2 tag (`@keywords internal` replaced with
  correct `@method print chi_square`).

* Fixed example syntax errors in `ancova()` and `factorial_anova()` (formula
  syntax replaced with correct `dv`/`between` interface).

* Fixed incorrect variable name `education_level` in examples (corrected to
  `education`).

## Tests

* Added `test-summary-methods.R` with tests for all 13 summary methods.

* Updated `test-print-methods.R` to reflect the new three-layer structure.

---

# mariposa 0.5.0

## New Functions

* Added `factorial_anova()` for multi-factor between-subjects ANOVA (up to 3
  factors) with Type III Sum of Squares matching SPSS UNIANOVA. Includes main
  effects, all interaction terms, partial eta squared, R-squared, and Levene's
  test for homogeneity of variance. Full survey weight support via WLS
  (matching SPSS /REGWGT). Integrates with existing `tukey_test()`,
  `scheffe_test()`, and `levene_test()` S3 generics.

* Added `ancova()` for Analysis of Covariance — tests group differences after
  controlling for continuous covariates. Matches SPSS UNIANOVA with the WITH
  keyword. Provides ANOVA table, parameter estimates (B, SE, t, p, partial eta
  squared), estimated marginal means (adjusted for covariates), and Levene's
  test. Supports up to 3 factors and multiple covariates with full survey
  weight support.

## SPSS Validation

* Added 612 SPSS validation tests for `factorial_anova()` across 9 scenarios:
  unweighted (2-factor, 3-factor, 2-factor with missing data), weighted
  (2-factor, 3-factor), grouped (2-factor, 3-factor), and weighted+grouped
  (2-factor, 3-factor).

* Added 579 SPSS validation tests for `ancova()` across 11 scenarios:
  one-way ANCOVA, two-way ANCOVA, weighted, grouped, weighted+grouped,
  multiple covariates, and single factor with single covariate.

* Total test suite: 4,986 tests passing (0 failures).

## Technical Details

* Type III Sum of Squares computed via `contr.sum` contrasts and `stats::drop1()`
  — no dependency on the `car` package.

* Weighted analyses use WLS (`stats::lm()` with weights), matching SPSS's
  /REGWGT subcommand behavior exactly.

* Weighted Levene's test uses the SPSS /REGWGT algorithm:
  `z_i = sqrt(w_i) * |y_i - weighted_cell_mean_i|` followed by unweighted ANOVA.

* Corrected Model SS computed as `Corrected Total - Error` (not sum of Type III
  SS) to correctly handle unbalanced designs.

---

# mariposa 0.4.0

## New Functions

* Added `fisher_test()` for Fisher's exact test of independence in contingency
  tables. Recommended when sample sizes are small or expected cell frequencies
  fall below 5 (where chi-square approximation becomes unreliable). Supports
  survey weights, multi-variable analysis, and `group_by()`.

* Added `chisq_gof()` for chi-square goodness-of-fit testing. Tests whether
  the observed frequency distribution of a categorical variable matches an
  expected distribution (default: equal proportions). Supports custom expected
  proportions, residual analysis, survey weights, and multi-variable analysis.

* Added `mcnemar_test()` for testing changes in paired proportions between two
  dichotomous measurements (e.g., before/after designs). Provides both
  asymptotic and exact binomial p-values, 2×2 contingency tables, and
  continuity correction. Supports survey weights.

* Added `dunn_test()` as an S3 generic for Dunn's post-hoc pairwise
  comparisons following a significant Kruskal-Wallis test. Identifies which
  specific group pairs differ using rank-based Z-statistics with adjustable
  p-value correction (Bonferroni, Holm, BH, etc.). Dispatches on
  `kruskal_wallis` result objects.

* Added `pairwise_wilcoxon()` as an S3 generic for pairwise Wilcoxon
  signed-rank post-hoc comparisons following a significant Friedman test.
  Identifies which measurement pairs differ with adjustable p-value correction.
  Dispatches on `friedman_test` result objects.

## SPSS Validation

* Added SPSS validation tests for all 5 new functions across
  weighted/unweighted and grouped/ungrouped scenarios.

## Improvements

* Extended post-hoc analysis framework: `dunn_test()` and `pairwise_wilcoxon()`
  join `tukey_test()`, `scheffe_test()`, and `levene_test()` as S3 generics
  that dispatch on their parent test result objects.

---

# mariposa 0.3.1

## Enhancements

* `efa()` now supports Maximum Likelihood (ML) extraction via `extraction = "ml"`.
  ML extraction provides a goodness-of-fit chi-square test, initial communalities
  as SMC (squared multiple correlations), and uniquenesses. Uses `stats::factanal()`
  with correlation matrix input for seamless survey weight support.

* `efa()` now supports Promax rotation via `rotation = "promax"`. Like Oblimin,
  Promax is an oblique rotation that produces Pattern Matrix, Structure Matrix,
  and Factor Correlation Matrix. Uses `stats::promax()` (base R, no new dependency).

* Internal refactoring of `efa()`: extraction logic separated into
  `.efa_extract_pca()` and `.efa_extract_ml()` for cleaner architecture and
  easier extension with future extraction methods (PAF planned).

---

# mariposa 0.3.0

## New Functions

* Added `kruskal_wallis()` for comparing 3+ independent groups on ordinal data
  (non-parametric alternative to one-way ANOVA). Supports survey weights,
  `group_by()`, and multi-variable analysis. Effect size: Eta-squared.

* Added `wilcoxon_test()` for comparing two paired measurements without assuming
  normality (Wilcoxon signed-rank test). Includes rank categories (negative,
  positive, ties) and effect size r.

* Added `friedman_test()` for comparing 3+ related measurements on ordinal data
  (non-parametric alternative to repeated-measures ANOVA). Effect size:
  Kendall's W.

* Added `binomial_test()` for testing whether an observed proportion matches an
  expected value (exact binomial test). Supports multiple binary variables and
  custom test proportions.

## SPSS Validation

* Added 294 new SPSS validation tests across all 4 non-parametric functions,
  covering weighted/unweighted and grouped/ungrouped scenarios.

* Total test suite: 2,227 tests passing (0 failures, 0 skips).

---

# mariposa 0.2.0

## New Functions

* Added `reliability()` for Cronbach's Alpha with item statistics, including
  corrected item-total correlations, alpha-if-item-deleted, and inter-item
  correlation matrix. Genuine implementation with full survey weight support.

* Added `efa()` for Exploratory Factor Analysis with PCA extraction. Supports
  Varimax rotation (Base R) and Oblimin rotation (via optional `GPArotation`
  package). Includes KMO measure, Bartlett's test, communalities, and sorted
  factor loading matrix with configurable blank threshold.

* Added `scale_index()` for creating mean indices across survey items, with
  `min_valid` parameter matching SPSS `MEAN.x()` syntax. Designed for use
  inside `dplyr::mutate()`.

* Added `pomps()` for Percent of Maximum Possible Scores transformation,
  rescaling values to a 0-100 range for cross-scale comparability.

* Added `linear_regression()` as a wrapper around `stats::lm()` with
  SPSS-compatible output: coefficients table (B, SE, Beta, t, p), ANOVA table,
  model summary (R, R-squared, adjusted R-squared), and standardized
  coefficients. Supports both formula and SPSS-style (dependent/predictors)
  interfaces.

* Added `logistic_regression()` as a wrapper around `stats::glm()` with odds
  ratios, Wald statistics, pseudo-R-squared measures (Nagelkerke, Cox-Snell,
  McFadden), and classification table.

## Dependencies

* Added `GPArotation` as suggested dependency for Oblimin rotation in `efa()`.

* Added `MASS` as suggested dependency for enhanced regression diagnostics.

## Improvements

* All 6 new functions support survey weights and grouped analysis via
  `dplyr::group_by()`.

* All functions include comprehensive roxygen2 documentation with practical
  examples, "When to Use" guidance, and "Understanding the Output" sections.

---

# mariposa 0.1.0

## Breaking Changes

* `gamma()` has been renamed to `goodman_gamma()` to avoid shadowing `base::gamma()`.
  The function remains an alias for `chi_square()` and works identically.

* S3 class names unified: removed `_results` suffix from all result classes
  (e.g., `chi_square_results` -> `chi_square`, `t_test_results` -> `t_test`).
  Class names now match the function name that created them.

## Bug Fixes

* Fixed namespace collisions from triple-defined internal helper functions
  (`.process_variables()`, `.process_weights()`, `.effective_n()`). These are now
  defined once in `helpers.R` and shared across all functions.

* Fixed weighted variance/SD formula inconsistency. All weighted calculations now
  use the SPSS frequency weights formula: `sum(w * (x - w_mean)^2) / (V1 - 1)`.

* Fixed Gamma ASE (asymptotic standard error) calculation. Replaced empirical
  magic-number formula with the correct ASE0 formula from Agresti (2002).

* Fixed weighted Cohen's d calculation in `t_test()`. Previously multiplied
  values by weights (`x * w`); now uses proper weighted means and pooled
  weighted standard deviation.

* Fixed weighted kurtosis formula. Changed from population excess kurtosis
  (`m4/m2^2 - 3`) to SPSS Type 2 sample-corrected formula
  (`G2 = ((n+1)*g2 + 6) * (n-1) / ((n-2)*(n-3))`), matching SPSS output.

## Improvements

* Refactored 9 of 11 `w_*` functions to use a shared factory pattern
  (`R/w_factory.R`). Eliminated ~2,460 lines of duplicated boilerplate
  (4,204 → 1,740 lines, -58.6%). `w_modus` and `w_quantile` remain
  standalone due to their fundamentally different interfaces.

* Added 83 SPSS validation tests for all `w_*` functions across 4 scenarios
  (weighted/unweighted × grouped/ungrouped) in
  `test-weighted-statistics-spss-validation.R`.

* Reduced memory usage: result objects now store only the columns needed for
  post-hoc tests instead of the full input data frame.

* Unified print helper system: all output formatting now uses `print_helpers.R`.
  Removed deprecated `.print_header()`, `.print_border()`, `.get_border()`, and
  `.print_group_header()` from `helpers.R`.

* Deduplicated `t_test.R` print methods: `print.t_test_results` and
  `print.t_test_result` now share a common implementation (~360 fewer lines).

* Added input validation:
  - `t_test()`: validates `conf.level` is between 0 and 1
  - `t_test()`, `oneway_anova()`: validate that selected variables are numeric
  - `chi_square()`: warns when expected cell counts < 5

* Migrated error handling from `stop()`/`warning()` to `cli_abort()`/`cli_warn()`
  with structured messages, `{.arg}` and `{.var}` markup, and pluralization support.

* Added `cli` as dependency for professional user-facing messages and print output.

* Added `@family` tags to all 24 exported functions for cross-referencing in
  documentation (families: descriptive, hypothesis_tests, correlation, posthoc,
  weighted_statistics).

* Added `tests/testthat/helper-mariposa.R` with shared test utilities and
  centralized SPSS validation tolerances.

* Migrated `print_helpers.R` infrastructure to `cli` (`cli_rule()`,
  `cli_bullets()`, `cli_h2()`).

* Extended `globals.R` with missing NSE variable declarations.

* Fixed "SURVEYSTAT" reference in `imports.R`.
