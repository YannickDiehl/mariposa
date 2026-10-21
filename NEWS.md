# mariposa 0.7.4

This release brings many more results in line with IBM SPSS Statistics 29,
makes every analysis result exportable, and improves output and error
messages throughout the package. Some results and result objects change;
see "Breaking changes".

## Breaking changes

* Results that now follow SPSS:
  * Weighted `describe()` reports N as the sum of the weights and a
    weighted missing count, as SPSS FREQUENCIES and DESCRIPTIVES do;
    Kish's effective N remains available in `$results`.
  * Weighted `crosstab()` rounds each weighted cell count before margins
    and percentages are computed (SPSS `/COUNT ROUND CELL`).
  * `binomial_test()` uses the category of the first valid case as
    Group 1 and, for a test proportion other than 0.5, reports the
    one-tailed exact p-value (new column `alternative`).
  * One-sample `t_test()`: `mean_diff` and its confidence interval refer to
    the difference between the mean and `mu`.
  * `tukey_test()` and `scheffe_test()` report one row per pair in the
    "(I) - (J)" orientation, with the new columns `SE` and `t_value`;
    weighted analyses use the error mean square and degrees of freedom of
    the weighted ANOVA.
  * `wilcoxon_test()` and `pairwise_wilcoxon()` compute Z from the smaller
    rank sum, so Z is never positive; the new column `z_based_on` gives the
    direction. The weighted two-sided `mann_whitney()` follows the same
    convention.
  * `chi_square()` and `phi()`: Phi of a 2x2 table carries the sign of the
    association; with `correct = TRUE`, Phi and Cramér's V are based on the
    Pearson chi-square; Goodman's gamma is reported for ordinal variables
    only.
  * `efa()` uses the SPSS algorithms for varimax, oblimin and promax
    rotation and reflects factors to positive loading sums.
  * `ancova()` parameter estimates use indicator coding with the last
    category as reference.
  * `rec(rules = "rev")` reverses on the scale range defined by the value
    labels instead of the observed range.
* Result objects:
  * The `w_*()` functions return one long table (`Variable`, the
    statistic, `n` or `weighted_n`, `effective_n`, `missing`).
  * Grouped `levene_test()` results contain the grouping variables as
    columns; the grouped method takes its variables through `...`.
  * `chi_square()` no longer returns `label_maps` (the tables carry the
    labels); `dunn_test()` and `pairwise_wilcoxon()` also return their
    comparisons as `$results`.
  * `std()`, `center()`, `pomps()` and `to_numeric()` return `NA` for SPSS
    missing values; `std()` and `center()` no longer add the weights column
    to the result.
  * `codebook()` returns its result visibly.
* Input handling:
  * Argument names must be written out in full. A misspelled or
    abbreviated name such as `weight =` or `percent =` is an error that
    names the intended argument.
  * Grouping variables of `group_by()` data are excluded from variable
    selections, with a message.
  * Weights without any positive value are an error.
  * The rank tests reject unordered factors and rank ordered factors by
    their level order.
  * `to_label()` on a data frame converts only fully labelled variables.
  * `frequency()` omits empty factor levels unless `show_unused = TRUE`.
  * `row_count(count = c(4, 5))` counts cells equal to any of the values.
* `reliability()` removes items without variance (with a warning) and
  reports omega as `NA` for a Heywood solution.

## New features

* Every analysis result can be exported: `as.data.frame()` and
  `tibble::as_tibble()` work for all result classes, `broom::tidy()` for
  all tests and descriptive results, and `write_xlsx()` writes analysis
  results (cross-tabulations in the SPSS layout) as well as lists that
  combine results and data frames.
* `weights` accepts a column name (unquoted or as a string), `all_of()`,
  an expression such as `sampling_weight * 2`, or a numeric vector, in
  every function.
* `codebook()` embeds the HTML codebook in R Markdown and Quarto
  documents.
* `factorial_anova()` and `ancova()` support `group_by()`; `ancova()`
  reports the marginal means of the main effects and has a `levene_test()`
  method.
* `describe()` gains `show = "min"` and `show = "max"`.
* `fisher_test()` reports the odds ratio with its 95% confidence interval
  for 2x2 tables and offers a Monte Carlo p-value for large tables
  (arguments `simulate.p.value` and `B`).
* `efa()` reports the extraction sums of squared loadings, the sample
  size, descriptive statistics and the number of rotation iterations;
  oblimin rotation no longer requires the GPArotation package.
* `logistic_regression()` accepts any outcome with two values (for example
  1/2 codes, labelled, character or logical) and shows the dependent
  variable encoding.
* `confint()`, `profile()`, `update()` and `step()` work on regression
  results; weighted `linear_regression()` methods use the same degrees of
  freedom as `summary()`.
* `rec()` accepts value lists (`"1,2=1"`) and an explicit scale range
  (`"rev(1, 5)"`).
* `var_label(x) <- "label"` and `var_label(data) <- list(...)` set
  variable labels.
* `find_var(fixed = TRUE)` searches for literal text; `na_frequencies()`,
  `strip_tags()` and `untag_na()` accept data frames.
* `pairwise_wilcoxon()` reports the number of cases of each pair.
* `chisq_gof(expected = )` applies to every selected variable, matches
  named proportions by category and accepts counts, as SPSS `/EXPECTED`
  does.

## SPSS compatibility

* The following results now match SPSS Statistics 29: the p-value of
  Goodman's gamma; Levene's test of `ancova()`; standard errors,
  confidence intervals and significance of weighted `tukey_test()` and
  `scheffe_test()`; weighted cell standard deviations of
  `factorial_anova()` and `ancova()`; the expected counts and residuals of
  `chisq_gof()`; all rotations and the ML extraction of `efa()`.
* Tests with labelled or numeric grouping variables order the groups by
  code and show value labels, as SPSS does.
* Printed values are rounded half up, as in SPSS.
* The validation suite compares about 14,600 values from SPSS reference
  output; see `vignette("spss-compatibility")`.

## Improvements

* Output follows the SPSS table layouts: Group Statistics and Independent
  Samples Test (`t_test()`), Descriptives and Robust Tests of Equality of
  Means (`oneway_anova()`), valid and missing totals (`frequency()`),
  Item-Total Statistics (`reliability()`), Total Variance Explained
  (`efa()`).
* Group headers, test groups and categories show value labels; titles show
  variable labels; labels are never truncated.
* Consistent number formatting: fixed decimals per column, p-values as
  `<.001`, whole-number counts without thousands separators, aligned
  columns for labels with non-ASCII characters.
* Tables are printed as one block wherever they can be displayed; in the
  console, a table is split into column blocks only when it is wider than
  the console.
* `frequency()`, `crosstab()` and `multiple_response()` print their full
  tables.
* Results that cannot be computed print "not computed" with the reason,
  and warnings name the variable and group.
* Grouped analyses skip a group that cannot be computed, with a warning
  naming it, instead of stopping.
* `reliability()` explains a negative Cronbach's alpha and names the items
  to check; `efa()` reports a singular correlation matrix like SPSS.
* Calling a function without a required argument, or on a data frame
  without rows, gives a clear error message.
* `kendall_tau()`, `goodman_gamma()`, `codebook()`, `untag_na()`,
  `write_spss()` and `write_xlsx()` are considerably faster on large data.
* Exact tests (`fisher_test()`, `mcnemar_test()`, `binomial_test()`) and
  `logistic_regression()` work with very large (expansion) weights.
* `citation("mariposa")` gives the DOI of the CRAN package.
* Compatible with the upcoming haven release, which stores the tags of
  tagged missing values in lower case: SAS special missing values are
  still shown as `.A`, `.B`, ..., and `write_xpt()` shortens over-long
  variable names itself (@hadley, #22).

## Bug fixes

* Weights imported with `read_spss()`, and labelled data used without
  loading the haven package, work in all functions.
* `marginal_effects()` computes correct effects for transformed
  predictors such as `I(x^2)` or `log(x)`.
* `read_sas()` works with its default arguments.
* `read_spss()` and `write_spss()` preserve SPSS missing-value
  definitions; `write_spss()` and `write_stata()` keep value labels
  created by `rec()` and the original codes of `to_label()` factors.
* The import functions detect files of another format and name the right
  reader.
* `to_label()` and `to_numeric()` keep the original codes and keep codes
  that share a label text apart.
* `rec()` keeps SPSS missing-value types and warns about values that match
  no rule.
* `write_xpt()` warns about shortened variable names; `write_xlsx()`
  creates unique sheet names.
* `%>%` is available after `library(mariposa)`.

# mariposa 0.7.3

Maintenance release: internal changes for CRAN policy compliance; no
user-visible changes.

# mariposa 0.7.2

Maintenance release: internal changes for CRAN policy compliance, plus the
user-visible changes below.

## Improvements

* The examples of the import and export functions now run as complete
  write-and-read roundtrips through temporary files.
* `ancova()`, `factorial_anova()` and the printing of correlation matrices
  restore changed `options()` settings reliably, even when interrupted.
* The package description cites the method references: IBM SPSS Statistics
  Algorithms, Dallal and Wilkinson (1986) for the Lilliefors correction,
  and Haberman (1973) for adjusted standardized residuals.

## Bug fixes

* `unlabel()` no longer fails on data frames with integer columns, such as
  the Likert items in `survey_data`.
* `write_xpt()` no longer fails on integer columns.

# mariposa 0.7.1

Maintenance release: internal changes for CRAN policy compliance, plus two
small robustness improvements.

## Improvements

* mariposa declares minimum versions of its dependencies (cli >= 3.0.0,
  dplyr >= 1.0.0, rlang >= 1.0.0, tidyselect >= 1.1.0, tibble >= 3.0.0,
  htmltools >= 0.5.0), so outdated installations are updated automatically
  instead of failing at run time.
* `set_na()` applied to a whole data frame reports clearly that the haven
  package is required when it is not installed.

# mariposa 0.7.0

Adds normality tests, partial correlations, multiple-response analysis and
average marginal effects.

## New features

* New `normality_test()` produces the SPSS EXAMINE "Tests of Normality":
  Kolmogorov-Smirnov with Lilliefors significance correction and
  Shapiro-Wilk (for 3 to 5,000 cases), computed as in SPSS. It supports
  tidyselect and `group_by()`; it has no `weights` argument because neither
  test has a well-defined form for fractional frequency weights.
* New `partial_cor()` computes partial correlations controlling for one or
  more variables (SPSS PARTIAL CORR, Stata `pcorr`), with zero-order
  correlations for comparison, two-tailed tests, listwise deletion,
  weights, `group_by()` support and a correlation matrix for three or more
  variables.
* New `marginal_effects()` computes average marginal effects for
  `logistic_regression()` models (Stata `margins, dydx(*)`): average
  derivatives on the probability scale for continuous predictors, average
  discrete changes relative to the reference level for factors, and
  delta-method standard errors, with weights and `group_by()` support. For
  `linear_regression()` models it explains that B already is the marginal
  effect.
* New `multiple_response()` analyzes "check all that apply" questions
  (SPSS MULT RESPONSE dichotomy sets): frequencies as percent of responses
  and percent of cases, and with `by =` a crosstab with case-based column
  percentages. It supports weights and `group_by()`.
* All four functions provide a compact `print()` and a detailed
  `summary()`.
* `normality_test()`, `partial_cor()` and `multiple_response()` are not
  yet compared with SPSS output; SPSS has no procedure for average marginal
  effects.

## Bug fixes

* Formulas on the reference pages of the package website render correctly
  again.

# mariposa 0.6.17

## New features

* `crosstab()` computes expected cell counts and adjusted standardized
  residuals (SPSS `CROSSTABS /CELLS=EXPECTED ASRESID`).
  `summary(result, residuals = TRUE)` shows them below each cell, with a
  note on the |adjusted residual| > 2 rule of thumb; the matrices are
  available as `$expected` and `$adj_residuals`.
* Weighted tables use the unrounded weighted cell counts for these
  statistics.
* The adjusted residuals agree with those of `chisq.test()`; they are not
  yet compared with SPSS output.

# mariposa 0.6.16

Maintenance release: internal changes for CRAN policy compliance;
statistical results are unchanged.

## Improvements

* `?mariposa` opens the package help page.
* The help pages of the `predict()` and `anova()` methods of both
  regression functions, and of the `print()` method for `w_quantile()`
  results, describe their return values.
* The `set_na()` example runs, and the `efa()` oblimin example runs only
  when the optional GPArotation package is installed.

# mariposa 0.6.15

Correctness improvements for the regression functions.

## New features

* The coefficients table of `linear_regression()` shows confidence
  intervals for B (SPSS `/STATISTICS CI`) at the level set by
  `conf.level`; `summary(..., conf_int = FALSE)` hides them.

## Improvements

* The documentation of `factors = "dummy"` explains that ordered factors
  receive polynomial contrasts rather than dummy coding, and how to obtain
  dummy coding.
* The SPSS compatibility vignette states that `logistic_regression()` has
  not yet been compared with SPSS output.

## Bug fixes

* Weighted `logistic_regression()` computes the -2 log-likelihood, the
  omnibus chi-square and the Cox & Snell and Nagelkerke R-squared correctly
  with fractional weights; unweighted and integer-weighted results are
  unchanged.
* `linear_regression(use = "pairwise")` no longer silently drops
  interactions and transformed terms; such formulas raise an error that
  suggests `use = "listwise"`.
* `linear_regression()` no longer fails on perfectly collinear predictors:
  these are excluded from the coefficient table with a message, as in SPSS,
  and the ANOVA degrees of freedom count only estimated terms.
* The classification table of `logistic_regression()` shows the cutoff
  actually used instead of always 0.50.

# mariposa 0.6.14

Robustness and display improvements for `codebook()`.

## New features

* New `view` argument of `codebook()` (default `interactive()`) controls
  whether the HTML codebook opens in the RStudio Viewer; writing with
  `file =` is unaffected. `print()` mentions the Viewer only when it was
  opened.

## Improvements

* Numeric values and ranges are formatted consistently in console, HTML
  and Excel output: fractional values with 4 significant digits, whole
  numbers as integers, and no scientific notation.
* Percentages and effective n in the frequency sheets of
  `write_xlsx(cb, frequencies = TRUE)` are rounded to 2 decimals.
* Range displays include the number of distinct values (e.g.
  "18 - 95 (78 distinct)").
* Factor levels respect `max_values` and are truncated with an
  "... (N more)" note; long character values are truncated to `max_len`.
* Invalid `max_values` or `max_len` values, and a `file =` path in a
  nonexistent directory, raise a clear error before any work is done.
* Output uses singular forms where appropriate ("1 variable") and shows
  "(no observations)" for data frames without rows.

## Bug fixes

* Variables with value labels but no variable label no longer show a
  spurious variable label and are no longer counted as labelled.
* `codebook()` no longer fails on inline data expressions such as
  `codebook(data.frame(...))`.
* List columns are skipped with a warning instead of causing an error.
* Missing-value codes, labels and counts appear consistently: in the HTML
  codebook also for variables displayed as a range, and in `summary()` in
  a new "Missing values" section, so `show_na` now takes effect on the
  console.

# mariposa 0.6.13

Adds McDonald's omega to `reliability()`.

## New features

* `reliability()` reports McDonald's omega, estimated from a one-factor
  maximum-likelihood model, also for weighted data: `omega` (raw,
  analogous to raw alpha) and `omega_std` (standardized, analogous to
  standardized alpha).
* The item-total table gains an `omega_if_deleted` column (NA when fewer
  than 3 items remain).
* `print()` shows omega next to alpha; `summary()` adds omega to the
  Reliability Statistics block and the Item-Total table.
* For scales with fewer than 3 items (with a warning), or when the factor
  model does not converge, the omega fields are NA; alpha is unaffected.
* Omega has not yet been compared with SPSS output; it is cross-checked
  against `psych::omega()` and a one-factor confirmatory factor analysis.
  The help page notes this.

# mariposa 0.6.12

Corrections to weighted rank statistics and clearer documentation of which
results are compared with SPSS.

## Improvements

* The README, the package description and
  `vignette("spss-compatibility")` describe which results are compared
  with SPSS output.
* The help pages of `mann_whitney()`, `kruskal_wallis()`, `wilcoxon_test()`,
  `friedman_test()`, `binomial_test()`, `dunn_test()`,
  `pairwise_wilcoxon()` and `kendall_tau()` note that their weighted
  results are not compared with SPSS output.
* The `mann_whitney()` help page notes that its weighted U and W are
  design-based quantities; Z and p are the test result.
* The `oneway_anova()` help page documents that omega-squared and
  epsilon-squared are truncated at 0.
* The SPSS compatibility vignette lists the `w_*` functions and
  `scheffe_test()` as validated.

## Bug fixes

* Weighted `kendall_tau()` computes the tau-b denominator correctly for
  data with pairs tied on both variables.
* Weighted `kruskal_wallis()` uses the correct grand mean rank, so H is no
  longer too large.

# mariposa 0.6.11

Removes the argument names and result columns deprecated in 0.6.9 and
0.6.10.

## Breaking changes

* The dot-case argument names of `codebook()` (`show.id`, `show.type`,
  `show.labels`, `show.values`, `show.freq`, `show.na`, `show.unused`,
  `max.values`, `max.len`, `sort.by.name`) and of `val_labels()` and
  `drop_labels()` (`drop.na`) raise an error; use the snake_case names
  (e.g. `show_id`, `drop_na`).
* The duplicate result columns are removed; use the canonical names:
  `chi_squared` instead of `chi_sq` (`chisq_gof()`, `friedman_test()`) and
  instead of `statistic` (`mcnemar_test()`), `r_effect` instead of
  `effect_size_r` (`mann_whitney()`), and `F_statistic` instead of
  `F_stat` (`oneway_anova()`). Statistical values are unchanged.

# mariposa 0.6.10

Harmonizes result column names across functions.

## Breaking changes

* The old column names `chi_sq` (`chisq_gof()`, `friedman_test()`),
  `statistic` (`mcnemar_test()`), `effect_size_r` (`mann_whitney()`) and
  `F_stat` (`oneway_anova()`) are deprecated. They remain available as
  duplicate columns in this release and will be removed in 0.6.11.

## Improvements

* Shared statistics use the same result column names in all functions:
  `chi_squared` in `chisq_gof()`, `friedman_test()` and `mcnemar_test()`
  (as in `chi_square()`), `r_effect` in `mann_whitney()` (as in
  `wilcoxon_test()`), and `F_statistic` in `oneway_anova()` (as in
  `levene_test()`). Statistical values are unchanged.

# mariposa 0.6.9

Completes the switch to snake_case argument names.

## Breaking changes

* The dot-case argument names deprecated in 0.6.8 raise an error in
  `frequency()`, `fre()`, `rec()`, `to_label()`, `to_character()`,
  `to_numeric()`, `read_spss()`, `read_por()`, `read_stata()`, `read_sas()`
  and `read_xpt()`; use the snake_case names (e.g. `sort_frq` instead of
  `sort.frq`).
* In functions that select variables through `...`, an old argument name
  raises an informative error instead of being taken as a variable
  selection.
* The arguments of `codebook()` are renamed to snake_case (`show_id`,
  `show_type`, `show_labels`, `show_values`, `show_freq`, `show_na`,
  `show_unused`, `max_values`, `max_len`, `sort_by_name`), as is `drop_na`
  in `val_labels()` and `drop_labels()`. The old names still work in this
  release with a warning (once per session).
* The display options stored in `codebook()` results (`result$options`)
  use snake_case keys.

# mariposa 0.6.8

Argument names switch to snake_case.

## Breaking changes

* Arguments with dots in their names are renamed to snake_case; the old
  names still work in this release with a warning (once per session).
  The base R conventions `na.rm`, `conf.level` and `var.equal` are
  unchanged. Renamed arguments:
  * `frequency()` and `fre()`: `sort_frq`, `show_na`, `show_prc`,
    `show_valid`, `show_sum`, `show_labels`, `show_unused`
  * `rec()`: `as_factor`, `var_label`, `val_labels`
  * `to_label()`, `to_character()`, `to_numeric()`: `drop_na`,
    `drop_unused`, `add_non_labelled`, `use_labels`, `start_at`,
    `keep_labels`
  * `read_spss()`, `read_por()`, `read_stata()`, `read_sas()`,
    `read_xpt()`: `tag_na`
* `t_test()` results no longer contain the duplicate columns `CI_lower`
  and `CI_upper`; use `conf_int_lower` and `conf_int_upper`.

## Improvements

* `frequency()` validates `sort_frq` (`"none"`, `"asc"`, `"desc"`) and
  `show_labels` (`TRUE`, `FALSE`, `"auto"`) with a clear error; a
  misspelled `sort_frq` previously produced an unsorted table silently.
* The vignettes use the new argument names.

# mariposa 0.6.7

Uniform output for all analysis functions; statistical results are
unchanged.

## Improvements

* Printing a result of `kruskal_wallis()`, `wilcoxon_test()`,
  `friedman_test()`, `binomial_test()`, `fisher_test()`, `chisq_gof()`,
  `mcnemar_test()`, `levene_test()`, `tukey_test()`, `scheffe_test()`,
  `dunn_test()`, `pairwise_wilcoxon()`, `frequency()` or `crosstab()`
  shows a compact overview (headline statistic, p-value, significance
  stars, one line per test). Everything previously shown by `print()` is
  available through `summary()`, which can show or hide each section.
* `describe()` gains a `summary()` method, consistent with the other
  functions.

# mariposa 0.6.6

Maintenance release: internal restructuring; statistical results are
unchanged.

## Improvements

* `tukey_test()` and `scheffe_test()` print aligned tables and show
  p-values in SPSS style.
* Input validation errors name the function called by the user rather than
  an internal helper.
* Internal functions no longer have help pages.

# mariposa 0.6.5

Maintenance release; statistical results are unchanged.

## Improvements

* tidyr is no longer a dependency.
* Errors inside grouped analyses keep the original error message.
* The error for a missing haven installation names the function that was
  called.
* The function reference on the package website lists `phi()`,
  `cramers_v()` and `goodman_gamma()`.

# mariposa 0.6.4

Improves the accuracy of several statistics and makes functions behave
more consistently. Some results change slightly, in each case toward the
standard reference implementations.

## Breaking changes

* `kruskal_wallis()` reports its effect size as `epsilon_squared`
  (previously `eta_squared`).
* `phi()`, `cramers_v()` and `goodman_gamma()` return the effect size as a
  single number; use `chi_square()` for the full test output.
* Negative weights are rejected with a clear error in all functions.
* `oneway_anova()` always reports both the classical and the Welch results
  (as SPSS ONEWAY does), so its `var.equal` argument is deprecated;
  `ss_type` in `factorial_anova()` and `ancova()` is deprecated in favor of
  the SPSS-standard Type III sums of squares.

## New features

* `linear_regression()` reports collinearity diagnostics (Tolerance and
  VIF per model term); `summary(..., collinearity = FALSE)` hides them.

## Improvements

* The weighted Wilcoxon signed-rank test (`wilcoxon_test()`,
  `pairwise_wilcoxon()`) uses frequency-expansion mid-ranks: integer
  weights give exactly the expanded-data result, and weights of 1
  reproduce the unweighted test. Mean ranks in weighted `kruskal_wallis()`
  and `dunn_test()` follow the same convention.
* The weighted median equals the weighted 50th percentile, and unweighted
  quantiles follow the SPSS definition (HAVERAGE, type 6) throughout.
* `frequency()` header statistics use the same formulas as `describe()`
  and the `w_*` functions.
* `write_spss()` protects valid values when many missing-value codes must
  be combined into a range, and explains what it does.
* Post-hoc tests report when a computation cannot be carried out instead
  of skipping it silently.

## Bug fixes

* The significance test of Kendall's tau-b agrees with `stats::cor.test()`
  and the SPSS formula, notably for heavily tied data such as binary
  variables.
* The asymptotic p-value of `mann_whitney()` matches its reported Z (SPSS
  convention, no continuity correction).
* Degrees of freedom in weighted `linear_regression()` and in the omnibus
  test of `logistic_regression()` are derived from the fitted model terms,
  improving results for models with dummy-coded factors or interactions.
* The weighted two-sample `t_test()` respects `var.equal` for its primary
  result.
* Correlation functions return NA for constant variables instead of an
  error.
* `frequency(show.unused = TRUE)` works on variables tagged with `set_na()`
  or `read_spss()`, and `sort.frq` sorts by frequency with a monotone
  cumulative percentage.
* Significance stars use one boundary convention everywhere, matching the
  printed legend.
* `logistic_regression()` shows separation and convergence warnings again.
* Grouped single-variable `w_*` results print their statistics again.

# mariposa 0.6.3.2

## Bug fixes

* `rec()` matches single decimal values (e.g. `"3.6=2"`) reliably, even
  when the stored value carries floating-point representation error.

# mariposa 0.6.3.1

## Bug fixes

* `broom::tidy()`, `broom::glance()` and `broom::augment()` work correctly
  on `linear_regression()` and `logistic_regression()` results; `summary()`
  still returns the SPSS-style output.
* On grouped results or results with `use = "pairwise"`, these functions
  raise an informative error that points to per-group use
  (`lapply(r$groups, ...)`) or `use = "listwise"`.

# mariposa 0.6.3

Regression results are now standard R model objects.

## Breaking changes

* `linear_regression()` and `logistic_regression()` results are the fitted
  `lm` or `glm` objects themselves (classes `c("linear_regression", "lm")`
  and `c("logistic_regression", "glm", "lm")`), with the mariposa tables
  attached. The `$model` element is gone; use the result directly (e.g.
  `predict(r, ...)`).
* The `$coefficients` table is renamed to `$coef_table` (`coef(r)` returns
  the coefficient vector), and the `$anova` table to `$anova_table`
  (`anova(r)` returns R's sequential ANOVA table).

## New features

* Standard model functions such as `coef()`, `predict()`, `anova()`,
  `vcov()`, `confint()`, `residuals()` and `fitted()` work directly on
  regression results. `summary()` still returns the SPSS-style summary;
  `stats::summary.lm()` and `stats::summary.glm()` give R's standard
  summaries.
* In grouped results, each element of `r$groups` is a model object, so
  these functions can be applied per group; on the grouped result itself
  and on `use = "pairwise"` results they raise an informative error.

# mariposa 0.6.2

## Breaking changes

* `linear_regression()` and `logistic_regression()` gain a `factors`
  argument. The new default `factors = "dummy"` expands a factor with L
  levels into L - 1 dummy variables, as `lm()` and `glm()` do; previously,
  factor levels were silently converted to integer codes.
  `factors = "numeric"` restores this SPSS-style treatment and lists the
  converted variables in a message.
* With `use = "pairwise"`, factor predictors require `factors = "numeric"`
  or `use = "listwise"`; otherwise an informative error is raised.

## Bug fixes

* Weighted `linear_regression()` and `logistic_regression()` use the
  unrounded sum of weights in degrees of freedom, F, R-squared and
  pseudo R-squared, bringing results closer to SPSS; the displayed N stays
  rounded.
* `summary(descriptives = TRUE)` for `linear_regression()` prints the
  Descriptive Statistics table.
* Printing weighted `linear_regression()` results with non-integer degrees
  of freedom no longer fails.
* The examples of the regression `summary()` methods use the correct
  argument names.

# mariposa 0.6.1

Brings weighted results closer to SPSS (weighted results may shift
slightly) and documents the validation status of each function.

## Improvements

* New `vignette("spss-compatibility")` shows the validation status of each
  function.
* The `spearman_rho()` documentation states that weights are used only to
  select cases, not in the rank correlation itself.
* The `pearson_cor()` documentation explains that raw expansion weights
  produce too narrow confidence intervals and should be normalized first.
* PMCMRplus and survey are no longer suggested dependencies.

## Bug fixes

* Weighted `t_test()` uses the unrounded sum of weights for variances,
  standard errors and degrees of freedom, including the Welch correction.
* Weighted `oneway_anova()` computes variances, standard errors,
  confidence intervals and within-groups degrees of freedom as SPSS ONEWAY
  does.
* Weighted `levene_test()` uses the unrounded sum of weights in its
  degrees of freedom.
* `dunn_test()` applies the tie correction to its standard errors, so |Z|
  is no longer underestimated for tied data such as Likert scales.
* Weighted `friedman_test()` applies the tie correction like the
  unweighted test.
* Weighted skewness and kurtosis in `describe()` use the bias-corrected
  formulas of `w_skew()`, `w_kurtosis()` and SPSS FREQUENCIES.
* Weighted and unweighted quantiles follow SPSS FREQUENCIES (HAVERAGE,
  type 6) instead of R's default type 7.

# mariposa 0.6.0

Adds functions for label management, data transformation, row operations
and variable search.

## Breaking changes

* `scale_index()` is removed; use `row_means()`, which provides the same
  functionality.

## New features

* New `var_label()` gets and sets variable labels.
* New `val_labels()` gets and sets value labels; `.add = TRUE` extends
  existing labels.
* New `copy_labels()` copies all label attributes to matching columns of
  another data frame, e.g. to restore labels lost in dplyr operations.
* New `drop_labels()` removes value labels for values that do not occur in
  the data.
* New `to_label()` and `to_character()` convert labelled vectors to
  factors (levels in code order) or character; new `to_numeric()` and
  `to_labelled()` convert factors and other vectors to numeric or labelled
  vectors.
* New `set_na()` declares values as missing, by default as tagged NAs that
  `na_frequencies()`, `frequency()` and `codebook()` recognize; named
  arguments set codes per variable.
* New `unlabel()` removes all label metadata and returns plain R vectors.
* New `rec()` recodes values with a compact string syntax (e.g.
  `"1:2=1 [Low]; 3:5=2 [High]"`), including ranges, `min`/`max`, `copy`
  and value labels.
* New `to_dummy()` creates 0/1 indicator variables, optionally named after
  the value labels.
* New `std()` standardizes variables with four methods (`"sd"`, `"2sd"`,
  `"mad"`, `"gmd"`), with weights, `group_by()` and a robust option.
* New `center()` centers variables at the grand mean or group mean, with
  weights.
* New `row_means()` and `row_sums()` compute row-wise means and sums with
  a minimum number of valid values (`min_valid`, like SPSS `MEAN.x()`);
  new `row_count()` counts specific values per row.
* New `find_var()` searches variables by name or label.

# mariposa 0.5.6

Adds export to SPSS, Stata and SAS transport files.

## New features

* New `write_spss()` writes SPSS `.sav` files and converts tagged NAs back
  to user-defined missing values, for lossless roundtrips with
  `read_spss()`; it supports byte, none and zsav compression.
* New `write_stata()` writes Stata `.dta` files (versions 8 to 15), with
  tagged NAs as extended missing values (`.a` to `.z`).
* New `write_xpt()` writes SAS transport `.xpt` files (versions 5 and 8),
  with tagged NAs as special missing values (`.A` to `.Z`, `._`).
* Data imported from one format can be exported to another (e.g. SPSS to
  Stata), with missing-value types converted automatically.

# mariposa 0.5.5

Adds Excel import and export with label preservation.

## New features

* New `read_xlsx()` reads Excel files and restores variable labels, value
  labels and tagged NAs from files written by `write_xlsx()`; other Excel
  files are read as plain data.
* New `write_xlsx()` exports data frames (with a "Labels" sheet for
  variable labels, value labels and missing-value codes), codebooks (with
  optional frequency sheets, `frequencies = TRUE`) and named lists (one
  sheet per element) to Excel. Missing-value codes stay visible in the
  data sheet.
* Excel import and export require the suggested package openxlsx2.

# mariposa 0.5.4

Adds import of Stata, SAS and SPSS portable files.

## Breaking changes

* The `spss_code` column of `na_frequencies()` is renamed to `code` and
  contains character values, such as `"-9"` for SPSS or `".a"` for Stata.

## New features

* New `read_stata()` reads Stata `.dta` files and integrates extended
  missing values (`.a` to `.z`) with `na_frequencies()`, `frequency()` and
  `codebook()`.
* New `read_sas()` reads SAS `.sas7bdat` files, optionally with a catalog
  file (`.sas7bcat`) for value labels, and integrates special missing
  values (`.A` to `.Z`, `._`).
* New `read_xpt()` reads SAS transport `.xpt` files with support for
  special missing values.
* New `read_por()` reads SPSS portable `.por` files with the same tagged NA
  support as `read_spss()`.

## Improvements

* `na_frequencies()`, `untag_na()` and `strip_tags()` work with data from
  all supported formats.
* For Stata and SAS data, which have no numeric missing-value codes to
  recover, `untag_na()` warns and behaves like `strip_tags()`.
* `frequency()` and `codebook()` show missing-value codes in the notation
  of the source format (e.g. `-9`, `.a`, `.A`).

# mariposa 0.5.3

Adds SPSS import with tagged missing values.

## New features

* New `read_spss()` reads SPSS `.sav` files and keeps user-defined missing
  values as tagged NAs, so different types of missing data (e.g. "no
  answer", "not applicable") remain distinguishable while behaving as `NA`
  in R. It also handles large survey files.
* New `na_frequencies()` shows the types of missing values in a variable
  with counts, original codes and labels.
* New `untag_na()` converts tagged NAs back to their original SPSS
  missing-value codes.
* New `strip_tags()` converts tagged NAs to regular `NA`.

## Improvements

* `frequency()` lists each type of tagged NA in its own row with code and
  label, followed by "Total Valid" and "Total Missing" rows.

## Bug fixes

* `frequency(show.unused = TRUE)` no longer lists labels of tagged NAs as
  unused categories.

# mariposa 0.5.2.2

## New features

* New `fre()` is a shorthand for `frequency()`.

# mariposa 0.5.2

## New features

* New `codebook()` creates an interactive HTML data dictionary in the
  RStudio Viewer, showing each variable's position, name, type, label,
  values, value labels and frequencies. It is built with htmltools, a new
  dependency.
* Printing a codebook gives a short overview; `summary()` gives a text
  version with selectable sections (`overview`, `variable_details`,
  `value_labels`).
* `codebook()` supports tidyselect, survey weights for weighted
  frequencies, and sorting by variable name.

# mariposa 0.5.1

## New features

* `t_test()`, `oneway_anova()`, `factorial_anova()`, `ancova()`,
  `chi_square()`, `mann_whitney()`, `pearson_cor()`, `spearman_rho()`,
  `kendall_tau()`, `reliability()`, `efa()`, `linear_regression()` and
  `logistic_regression()` gain `summary()` methods: printing a result gives
  a compact overview, and `summary()` gives the detailed SPSS-style output.
* Arguments of `summary()` show or hide individual sections (e.g.
  `summary(result, effect_sizes = FALSE)`).

## Improvements

* All print and summary methods are documented, and the examples of the
  analysis functions demonstrate both output levels.

## Bug fixes

* The examples of `ancova()` and `factorial_anova()` use the correct
  `dv`/`between` interface, and examples refer to the `education` variable
  correctly.

# mariposa 0.5.0

Adds factorial ANOVA and ANCOVA.

## New features

* New `factorial_anova()` performs between-subjects ANOVA with up to three
  factors and Type III sums of squares, as SPSS UNIANOVA does: main
  effects, all interactions, partial eta squared, R-squared and Levene's
  test. It works with `tukey_test()`, `scheffe_test()` and
  `levene_test()`.
* New `ancova()` performs analysis of covariance (SPSS UNIANOVA with WITH):
  ANOVA table, parameter estimates, estimated marginal means adjusted for
  the covariates, and Levene's test; it supports up to three factors and
  multiple covariates.
* Both functions support survey weights through weighted least squares, as
  SPSS /REGWGT does (including Levene's test), handle unbalanced designs
  correctly, and do not require the car package.

# mariposa 0.4.0

Adds exact and goodness-of-fit tests and non-parametric post-hoc tests.

## New features

* New `fisher_test()` performs Fisher's exact test of independence,
  recommended for small samples or expected cell counts below 5; it
  supports weights, multiple variables and `group_by()`.
* New `chisq_gof()` tests whether a categorical variable follows an
  expected distribution (equal proportions by default, or custom
  proportions), with residuals, weights and multiple variables.
* New `mcnemar_test()` tests changes in paired proportions of two
  dichotomous measurements (e.g. before/after), with asymptotic and exact
  p-values, the 2x2 table, continuity correction and weights.
* New `dunn_test()` performs Dunn's pairwise post-hoc comparisons on
  `kruskal_wallis()` results, with adjustable p-value correction
  (Bonferroni, Holm, BH and others).
* New `pairwise_wilcoxon()` performs pairwise Wilcoxon signed-rank
  post-hoc comparisons on `friedman_test()` results, with adjustable
  p-value correction.

# mariposa 0.3.1

## New features

* `efa()` supports maximum likelihood extraction (`extraction = "ml"`) with
  a goodness-of-fit chi-square test, initial communalities (squared
  multiple correlations) and uniquenesses; survey weights are supported.
* `efa()` supports Promax rotation (`rotation = "promax"`), an oblique
  rotation that reports the pattern matrix, structure matrix and factor
  correlation matrix.

# mariposa 0.3.0

Adds non-parametric tests.

## New features

* New `kruskal_wallis()` compares three or more independent groups on
  ordinal data (non-parametric alternative to one-way ANOVA), with an
  effect size, weights, `group_by()` and multiple variables.
* New `wilcoxon_test()` compares two paired measurements (Wilcoxon
  signed-rank test), with rank categories (negative, positive, ties) and
  effect size r.
* New `friedman_test()` compares three or more related measurements
  (non-parametric alternative to repeated-measures ANOVA), with Kendall's
  W as effect size.
* New `binomial_test()` tests whether an observed proportion matches an
  expected value (exact binomial test), for one or more binary variables.

# mariposa 0.2.0

Adds scale analysis and regression.

## New features

* New `reliability()` computes Cronbach's alpha with item statistics:
  corrected item-total correlations, alpha if item deleted, and the
  inter-item correlation matrix.
* New `efa()` performs exploratory factor analysis with principal
  component extraction and Varimax or Oblimin rotation (Oblimin requires
  the suggested package GPArotation), including KMO, Bartlett's test,
  communalities and a sorted loading matrix with a configurable blank
  threshold.
* New `scale_index()` computes mean indices across items, with
  `min_valid` like SPSS `MEAN.x()`.
* New `pomps()` rescales values to percent of maximum possible scores
  (0-100).
* New `linear_regression()` fits linear models with SPSS-style output
  (coefficients with B, SE, Beta, t and p; ANOVA table; R, R-squared and
  adjusted R-squared), using a formula or the SPSS-style
  dependent/predictors interface.
* New `logistic_regression()` fits logistic models with odds ratios, Wald
  statistics, pseudo R-squared (Nagelkerke, Cox & Snell, McFadden) and a
  classification table.
* All new functions support survey weights and `group_by()`.

# mariposa 0.1.0

## Breaking changes

* `gamma()` is renamed to `goodman_gamma()` so that it no longer masks
  `base::gamma()`.
* Result classes no longer carry the `_results` suffix and match the
  function name (e.g. `t_test` instead of `t_test_results`).

## Improvements

* Result objects store only the columns needed for post-hoc tests, which
  reduces memory use.
* `t_test()` checks that `conf.level` is between 0 and 1; `t_test()` and
  `oneway_anova()` check that the selected variables are numeric;
  `chi_square()` warns when expected cell counts are below 5.
* Error and warning messages and printed output are formatted with the cli
  package.
* Help pages link related functions by family (descriptive statistics,
  hypothesis tests, correlation, post-hoc tests, weighted statistics).

## Bug fixes

* Weighted variance and standard deviation use the SPSS frequency-weight
  formula consistently.
* The asymptotic standard error of Goodman and Kruskal's gamma uses the
  ASE0 formula (Agresti, 2002).
* Weighted Cohen's d in `t_test()` uses weighted means and the pooled
  weighted standard deviation.
* Weighted kurtosis uses the SPSS sample-corrected (Type 2) formula.
