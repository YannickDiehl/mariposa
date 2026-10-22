# Changelog

## mariposa 0.7.4

This release brings many more results in line with IBM SPSS Statistics
29, makes every analysis result exportable, and improves output and
error messages throughout the package. Some results and result objects
change; see “Breaking changes”.

### Breaking changes

- Results that now follow SPSS:
  - Weighted
    [`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
    reports N as the sum of the weights and a weighted missing count, as
    SPSS FREQUENCIES and DESCRIPTIVES do; Kish’s effective N remains
    available in `$results`.
  - Weighted
    [`crosstab()`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md)
    rounds each weighted cell count before margins and percentages are
    computed (SPSS `/COUNT ROUND CELL`).
  - [`binomial_test()`](https://YannickDiehl.github.io/mariposa/reference/binomial_test.md)
    uses the category of the first valid case as Group 1 and, for a test
    proportion other than 0.5, reports the one-tailed exact p-value (new
    column `alternative`).
  - One-sample
    [`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md):
    `mean_diff` and its confidence interval refer to the difference
    between the mean and `mu`.
  - [`tukey_test()`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md)
    and
    [`scheffe_test()`](https://YannickDiehl.github.io/mariposa/reference/scheffe_test.md)
    report one row per pair in the “(I) - (J)” orientation, with the new
    columns `SE` and `t_value`; weighted analyses use the error mean
    square and degrees of freedom of the weighted ANOVA.
  - [`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/reference/wilcoxon_test.md)
    and
    [`pairwise_wilcoxon()`](https://YannickDiehl.github.io/mariposa/reference/pairwise_wilcoxon.md)
    compute Z from the smaller rank sum, so Z is never positive; the new
    column `z_based_on` gives the direction. The weighted two-sided
    [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md)
    follows the same convention.
  - [`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
    and
    [`phi()`](https://YannickDiehl.github.io/mariposa/reference/phi.md):
    Phi of a 2x2 table carries the sign of the association; with
    `correct = TRUE`, Phi and Cramér’s V are based on the Pearson
    chi-square; Goodman’s gamma is reported for ordinal variables only.
  - [`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md)
    uses the SPSS algorithms for varimax, oblimin and promax rotation
    and reflects factors to positive loading sums.
  - [`ancova()`](https://YannickDiehl.github.io/mariposa/reference/ancova.md)
    parameter estimates use indicator coding with the last category as
    reference.
  - `rec(rules = "rev")` reverses on the scale range defined by the
    value labels instead of the observed range.
- Result objects:
  - The `w_*()` functions return one long table (`Variable`, the
    statistic, `n` or `weighted_n`, `effective_n`, `missing`).
  - Grouped
    [`levene_test()`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md)
    results contain the grouping variables as columns; the grouped
    method takes its variables through `...`.
  - [`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
    no longer returns `label_maps` (the tables carry the labels);
    [`dunn_test()`](https://YannickDiehl.github.io/mariposa/reference/dunn_test.md)
    and
    [`pairwise_wilcoxon()`](https://YannickDiehl.github.io/mariposa/reference/pairwise_wilcoxon.md)
    also return their comparisons as `$results`.
  - [`std()`](https://YannickDiehl.github.io/mariposa/reference/std.md),
    [`center()`](https://YannickDiehl.github.io/mariposa/reference/center.md),
    [`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md)
    and
    [`to_numeric()`](https://YannickDiehl.github.io/mariposa/reference/to_numeric.md)
    return `NA` for SPSS missing values;
    [`std()`](https://YannickDiehl.github.io/mariposa/reference/std.md)
    and
    [`center()`](https://YannickDiehl.github.io/mariposa/reference/center.md)
    no longer add the weights column to the result.
  - [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)
    returns its result visibly.
- Input handling:
  - Argument names must be written out in full. A misspelled or
    abbreviated name such as `weight =` or `percent =` is an error that
    names the intended argument.
  - Grouping variables of
    [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
    data are excluded from variable selections, with a message.
  - Weights without any positive value are an error.
  - The rank tests reject unordered factors and rank ordered factors by
    their level order.
  - [`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md)
    on a data frame converts only fully labelled variables.
  - [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
    omits empty factor levels unless `show_unused = TRUE`.
  - `row_count(count = c(4, 5))` counts cells equal to any of the
    values.
- [`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md)
  removes items without variance (with a warning) and reports omega as
  `NA` for a Heywood solution.

### New features

- Every analysis result can be exported:
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) and
  [`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  work for all result classes,
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) for
  all tests and descriptive results, and
  [`write_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/write_xlsx.md)
  writes analysis results (cross-tabulations in the SPSS layout) as well
  as lists that combine results and data frames.
- `weights` accepts a column name (unquoted or as a string),
  [`all_of()`](https://tidyselect.r-lib.org/reference/all_of.html), an
  expression such as `sampling_weight * 2`, or a numeric vector, in
  every function.
- [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)
  embeds the HTML codebook in R Markdown and Quarto documents.
- [`factorial_anova()`](https://YannickDiehl.github.io/mariposa/reference/factorial_anova.md)
  and
  [`ancova()`](https://YannickDiehl.github.io/mariposa/reference/ancova.md)
  support
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html);
  [`ancova()`](https://YannickDiehl.github.io/mariposa/reference/ancova.md)
  reports the marginal means of the main effects and has a
  [`levene_test()`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md)
  method.
- [`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
  gains `show = "min"` and `show = "max"`.
- [`fisher_test()`](https://YannickDiehl.github.io/mariposa/reference/fisher_test.md)
  reports the odds ratio with its 95% confidence interval for 2x2 tables
  and offers a Monte Carlo p-value for large tables (arguments
  `simulate.p.value` and `B`).
- [`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md)
  reports the extraction sums of squared loadings, the sample size,
  descriptive statistics and the number of rotation iterations; oblimin
  rotation no longer requires the GPArotation package.
- [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)
  accepts any outcome with two values (for example 1/2 codes, labelled,
  character or logical) and shows the dependent variable encoding.
- [`confint()`](https://rdrr.io/r/stats/confint.html),
  [`profile()`](https://rdrr.io/r/stats/profile.html),
  [`update()`](https://rdrr.io/r/stats/update.html) and
  [`step()`](https://rdrr.io/r/stats/step.html) work on regression
  results; weighted
  [`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)
  methods use the same degrees of freedom as
  [`summary()`](https://rdrr.io/r/base/summary.html).
- [`rec()`](https://YannickDiehl.github.io/mariposa/reference/rec.md)
  accepts value lists (`"1,2=1"`) and an explicit scale range
  (`"rev(1, 5)"`).
- `var_label(x) <- "label"` and `var_label(data) <- list(...)` set
  variable labels.
- `find_var(fixed = TRUE)` searches for literal text;
  [`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md),
  [`strip_tags()`](https://YannickDiehl.github.io/mariposa/reference/strip_tags.md)
  and
  [`untag_na()`](https://YannickDiehl.github.io/mariposa/reference/untag_na.md)
  accept data frames.
- [`pairwise_wilcoxon()`](https://YannickDiehl.github.io/mariposa/reference/pairwise_wilcoxon.md)
  reports the number of cases of each pair.
- `chisq_gof(expected = )` applies to every selected variable, matches
  named proportions by category and accepts counts, as SPSS `/EXPECTED`
  does.

### SPSS compatibility

- The following results now match SPSS Statistics 29: the p-value of
  Goodman’s gamma; Levene’s test of
  [`ancova()`](https://YannickDiehl.github.io/mariposa/reference/ancova.md);
  standard errors, confidence intervals and significance of weighted
  [`tukey_test()`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md)
  and
  [`scheffe_test()`](https://YannickDiehl.github.io/mariposa/reference/scheffe_test.md);
  weighted cell standard deviations of
  [`factorial_anova()`](https://YannickDiehl.github.io/mariposa/reference/factorial_anova.md)
  and
  [`ancova()`](https://YannickDiehl.github.io/mariposa/reference/ancova.md);
  the expected counts and residuals of
  [`chisq_gof()`](https://YannickDiehl.github.io/mariposa/reference/chisq_gof.md);
  all rotations and the ML extraction of
  [`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md).
- Tests with labelled or numeric grouping variables order the groups by
  code and show value labels, as SPSS does.
- Printed values are rounded half up, as in SPSS.
- The validation suite compares about 14,600 values from SPSS reference
  output; see
  [`vignette("spss-compatibility")`](https://YannickDiehl.github.io/mariposa/articles/spss-compatibility.md).

### Improvements

- Output follows the SPSS table layouts: Group Statistics and
  Independent Samples Test
  ([`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)),
  Descriptives and Robust Tests of Equality of Means
  ([`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)),
  valid and missing totals
  ([`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)),
  Item-Total Statistics
  ([`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md)),
  Total Variance Explained
  ([`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md)).
- Group headers, test groups and categories show value labels; titles
  show variable labels; labels are never truncated.
- Consistent number formatting: fixed decimals per column, p-values as
  `<.001`, whole-number counts without thousands separators, aligned
  columns for labels with non-ASCII characters.
- Tables are printed as one block wherever they can be displayed; in the
  console, a table is split into column blocks only when it is wider
  than the console.
- [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md),
  [`crosstab()`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md)
  and
  [`multiple_response()`](https://YannickDiehl.github.io/mariposa/reference/multiple_response.md)
  print their full tables.
- Results that cannot be computed print “not computed” with the reason,
  and warnings name the variable and group.
- Grouped analyses skip a group that cannot be computed, with a warning
  naming it, instead of stopping.
- [`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md)
  explains a negative Cronbach’s alpha and names the items to check;
  [`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md)
  reports a singular correlation matrix like SPSS.
- Calling a function without a required argument, or on a data frame
  without rows, gives a clear error message.
- [`kendall_tau()`](https://YannickDiehl.github.io/mariposa/reference/kendall_tau.md),
  [`goodman_gamma()`](https://YannickDiehl.github.io/mariposa/reference/phi.md),
  [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md),
  [`untag_na()`](https://YannickDiehl.github.io/mariposa/reference/untag_na.md),
  [`write_spss()`](https://YannickDiehl.github.io/mariposa/reference/write_spss.md)
  and
  [`write_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/write_xlsx.md)
  are considerably faster on large data.
- Exact tests
  ([`fisher_test()`](https://YannickDiehl.github.io/mariposa/reference/fisher_test.md),
  [`mcnemar_test()`](https://YannickDiehl.github.io/mariposa/reference/mcnemar_test.md),
  [`binomial_test()`](https://YannickDiehl.github.io/mariposa/reference/binomial_test.md))
  and
  [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)
  work with very large (expansion) weights.
- `citation("mariposa")` gives the DOI of the CRAN package.
- Compatible with the upcoming haven release, which stores the tags of
  tagged missing values in lower case: SAS special missing values are
  still shown as `.A`, `.B`, …, and
  [`write_xpt()`](https://YannickDiehl.github.io/mariposa/reference/write_xpt.md)
  shortens over-long variable names itself
  ([@hadley](https://github.com/hadley),
  [\#22](https://github.com/YannickDiehl/mariposa/issues/22)).

### Bug fixes

- Weights imported with
  [`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md),
  and labelled data used without loading the haven package, work in all
  functions.
- [`marginal_effects()`](https://YannickDiehl.github.io/mariposa/reference/marginal_effects.md)
  computes correct effects for transformed predictors such as `I(x^2)`
  or `log(x)`.
- [`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md)
  works with its default arguments.
- [`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md)
  and
  [`write_spss()`](https://YannickDiehl.github.io/mariposa/reference/write_spss.md)
  preserve SPSS missing-value definitions;
  [`write_spss()`](https://YannickDiehl.github.io/mariposa/reference/write_spss.md)
  and
  [`write_stata()`](https://YannickDiehl.github.io/mariposa/reference/write_stata.md)
  keep value labels created by
  [`rec()`](https://YannickDiehl.github.io/mariposa/reference/rec.md)
  and the original codes of
  [`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md)
  factors.
- The import functions detect files of another format and name the right
  reader.
- [`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md)
  and
  [`to_numeric()`](https://YannickDiehl.github.io/mariposa/reference/to_numeric.md)
  keep the original codes and keep codes that share a label text apart.
- [`rec()`](https://YannickDiehl.github.io/mariposa/reference/rec.md)
  keeps SPSS missing-value types and warns about values that match no
  rule.
- [`write_xpt()`](https://YannickDiehl.github.io/mariposa/reference/write_xpt.md)
  warns about shortened variable names;
  [`write_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/write_xlsx.md)
  creates unique sheet names.
- `%>%` is available after
  [`library(mariposa)`](https://YannickDiehl.github.io/mariposa/).
- [`set_na()`](https://YannickDiehl.github.io/mariposa/reference/set_na.md),
  [`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md)
  and
  [`read_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/read_xlsx.md)
  keep more than 26 missing-value codes of a variable apart with the
  upcoming haven release, which no longer distinguishes upper- and
  lower-case tags.

## mariposa 0.7.3

CRAN release: 2026-09-28

Maintenance release: internal changes for CRAN policy compliance; no
user-visible changes.

## mariposa 0.7.2

Maintenance release: internal changes for CRAN policy compliance, plus
the user-visible changes below.

### Improvements

- The examples of the import and export functions now run as complete
  write-and-read roundtrips through temporary files.
- [`ancova()`](https://YannickDiehl.github.io/mariposa/reference/ancova.md),
  [`factorial_anova()`](https://YannickDiehl.github.io/mariposa/reference/factorial_anova.md)
  and the printing of correlation matrices restore changed
  [`options()`](https://rdrr.io/r/base/options.html) settings reliably,
  even when interrupted.
- The package description cites the method references: IBM SPSS
  Statistics Algorithms, Dallal and Wilkinson (1986) for the Lilliefors
  correction, and Haberman (1973) for adjusted standardized residuals.

### Bug fixes

- [`unlabel()`](https://YannickDiehl.github.io/mariposa/reference/unlabel.md)
  no longer fails on data frames with integer columns, such as the
  Likert items in `survey_data`.
- [`write_xpt()`](https://YannickDiehl.github.io/mariposa/reference/write_xpt.md)
  no longer fails on integer columns.

## mariposa 0.7.1

Maintenance release: internal changes for CRAN policy compliance, plus
two small robustness improvements.

### Improvements

- mariposa declares minimum versions of its dependencies (cli \>= 3.0.0,
  dplyr \>= 1.0.0, rlang \>= 1.0.0, tidyselect \>= 1.1.0, tibble \>=
  3.0.0, htmltools \>= 0.5.0), so outdated installations are updated
  automatically instead of failing at run time.
- [`set_na()`](https://YannickDiehl.github.io/mariposa/reference/set_na.md)
  applied to a whole data frame reports clearly that the haven package
  is required when it is not installed.

## mariposa 0.7.0

Adds normality tests, partial correlations, multiple-response analysis
and average marginal effects.

### New features

- New
  [`normality_test()`](https://YannickDiehl.github.io/mariposa/reference/normality_test.md)
  produces the SPSS EXAMINE “Tests of Normality”: Kolmogorov-Smirnov
  with Lilliefors significance correction and Shapiro-Wilk (for 3 to
  5,000 cases), computed as in SPSS. It supports tidyselect and
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html);
  it has no `weights` argument because neither test has a well-defined
  form for fractional frequency weights.
- New
  [`partial_cor()`](https://YannickDiehl.github.io/mariposa/reference/partial_cor.md)
  computes partial correlations controlling for one or more variables
  (SPSS PARTIAL CORR, Stata `pcorr`), with zero-order correlations for
  comparison, two-tailed tests, listwise deletion, weights,
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  support and a correlation matrix for three or more variables.
- New
  [`marginal_effects()`](https://YannickDiehl.github.io/mariposa/reference/marginal_effects.md)
  computes average marginal effects for
  [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)
  models (Stata `margins, dydx(*)`): average derivatives on the
  probability scale for continuous predictors, average discrete changes
  relative to the reference level for factors, and delta-method standard
  errors, with weights and
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  support. For
  [`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)
  models it explains that B already is the marginal effect.
- New
  [`multiple_response()`](https://YannickDiehl.github.io/mariposa/reference/multiple_response.md)
  analyzes “check all that apply” questions (SPSS MULT RESPONSE
  dichotomy sets): frequencies as percent of responses and percent of
  cases, and with `by =` a crosstab with case-based column percentages.
  It supports weights and
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
- All four functions provide a compact
  [`print()`](https://rdrr.io/r/base/print.html) and a detailed
  [`summary()`](https://rdrr.io/r/base/summary.html).
- [`normality_test()`](https://YannickDiehl.github.io/mariposa/reference/normality_test.md),
  [`partial_cor()`](https://YannickDiehl.github.io/mariposa/reference/partial_cor.md)
  and
  [`multiple_response()`](https://YannickDiehl.github.io/mariposa/reference/multiple_response.md)
  are not yet compared with SPSS output; SPSS has no procedure for
  average marginal effects.

### Bug fixes

- Formulas on the reference pages of the package website render
  correctly again.

## mariposa 0.6.17

### New features

- [`crosstab()`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md)
  computes expected cell counts and adjusted standardized residuals
  (SPSS `CROSSTABS /CELLS=EXPECTED ASRESID`).
  `summary(result, residuals = TRUE)` shows them below each cell, with a
  note on the \|adjusted residual\| \> 2 rule of thumb; the matrices are
  available as `$expected` and `$adj_residuals`.
- Weighted tables use the unrounded weighted cell counts for these
  statistics.
- The adjusted residuals agree with those of
  [`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html); they are
  not yet compared with SPSS output.

## mariposa 0.6.16

Maintenance release: internal changes for CRAN policy compliance;
statistical results are unchanged.

### Improvements

- [`?mariposa`](https://YannickDiehl.github.io/mariposa/reference/mariposa-package.md)
  opens the package help page.
- The help pages of the
  [`predict()`](https://rdrr.io/r/stats/predict.html) and
  [`anova()`](https://rdrr.io/r/stats/anova.html) methods of both
  regression functions, and of the
  [`print()`](https://rdrr.io/r/base/print.html) method for
  [`w_quantile()`](https://YannickDiehl.github.io/mariposa/reference/w_quantile.md)
  results, describe their return values.
- The
  [`set_na()`](https://YannickDiehl.github.io/mariposa/reference/set_na.md)
  example runs, and the
  [`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md)
  oblimin example runs only when the optional GPArotation package is
  installed.

## mariposa 0.6.15

Correctness improvements for the regression functions.

### New features

- The coefficients table of
  [`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)
  shows confidence intervals for B (SPSS `/STATISTICS CI`) at the level
  set by `conf.level`; `summary(..., conf_int = FALSE)` hides them.

### Improvements

- The documentation of `factors = "dummy"` explains that ordered factors
  receive polynomial contrasts rather than dummy coding, and how to
  obtain dummy coding.
- The SPSS compatibility vignette states that
  [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)
  has not yet been compared with SPSS output.

### Bug fixes

- Weighted
  [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)
  computes the -2 log-likelihood, the omnibus chi-square and the Cox &
  Snell and Nagelkerke R-squared correctly with fractional weights;
  unweighted and integer-weighted results are unchanged.
- `linear_regression(use = "pairwise")` no longer silently drops
  interactions and transformed terms; such formulas raise an error that
  suggests `use = "listwise"`.
- [`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)
  no longer fails on perfectly collinear predictors: these are excluded
  from the coefficient table with a message, as in SPSS, and the ANOVA
  degrees of freedom count only estimated terms.
- The classification table of
  [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)
  shows the cutoff actually used instead of always 0.50.

## mariposa 0.6.14

Robustness and display improvements for
[`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md).

### New features

- New `view` argument of
  [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)
  (default [`interactive()`](https://rdrr.io/r/base/interactive.html))
  controls whether the HTML codebook opens in the RStudio Viewer;
  writing with `file =` is unaffected.
  [`print()`](https://rdrr.io/r/base/print.html) mentions the Viewer
  only when it was opened.

### Improvements

- Numeric values and ranges are formatted consistently in console, HTML
  and Excel output: fractional values with 4 significant digits, whole
  numbers as integers, and no scientific notation.
- Percentages and effective n in the frequency sheets of
  `write_xlsx(cb, frequencies = TRUE)` are rounded to 2 decimals.
- Range displays include the number of distinct values (e.g. “18 - 95
  (78 distinct)”).
- Factor levels respect `max_values` and are truncated with an “… (N
  more)” note; long character values are truncated to `max_len`.
- Invalid `max_values` or `max_len` values, and a `file =` path in a
  nonexistent directory, raise a clear error before any work is done.
- Output uses singular forms where appropriate (“1 variable”) and shows
  “(no observations)” for data frames without rows.

### Bug fixes

- Variables with value labels but no variable label no longer show a
  spurious variable label and are no longer counted as labelled.
- [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)
  no longer fails on inline data expressions such as
  `codebook(data.frame(...))`.
- List columns are skipped with a warning instead of causing an error.
- Missing-value codes, labels and counts appear consistently: in the
  HTML codebook also for variables displayed as a range, and in
  [`summary()`](https://rdrr.io/r/base/summary.html) in a new “Missing
  values” section, so `show_na` now takes effect on the console.

## mariposa 0.6.13

Adds McDonald’s omega to
[`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md).

### New features

- [`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md)
  reports McDonald’s omega, estimated from a one-factor
  maximum-likelihood model, also for weighted data: `omega` (raw,
  analogous to raw alpha) and `omega_std` (standardized, analogous to
  standardized alpha).
- The item-total table gains an `omega_if_deleted` column (NA when fewer
  than 3 items remain).
- [`print()`](https://rdrr.io/r/base/print.html) shows omega next to
  alpha; [`summary()`](https://rdrr.io/r/base/summary.html) adds omega
  to the Reliability Statistics block and the Item-Total table.
- For scales with fewer than 3 items (with a warning), or when the
  factor model does not converge, the omega fields are NA; alpha is
  unaffected.
- Omega has not yet been compared with SPSS output; it is cross-checked
  against [`psych::omega()`](https://rdrr.io/pkg/psych/man/omega.html)
  and a one-factor confirmatory factor analysis. The help page notes
  this.

## mariposa 0.6.12

Corrections to weighted rank statistics and clearer documentation of
which results are compared with SPSS.

### Improvements

- The README, the package description and
  [`vignette("spss-compatibility")`](https://YannickDiehl.github.io/mariposa/articles/spss-compatibility.md)
  describe which results are compared with SPSS output.
- The help pages of
  [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md),
  [`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md),
  [`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/reference/wilcoxon_test.md),
  [`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md),
  [`binomial_test()`](https://YannickDiehl.github.io/mariposa/reference/binomial_test.md),
  [`dunn_test()`](https://YannickDiehl.github.io/mariposa/reference/dunn_test.md),
  [`pairwise_wilcoxon()`](https://YannickDiehl.github.io/mariposa/reference/pairwise_wilcoxon.md)
  and
  [`kendall_tau()`](https://YannickDiehl.github.io/mariposa/reference/kendall_tau.md)
  note that their weighted results are not compared with SPSS output.
- The
  [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md)
  help page notes that its weighted U and W are design-based quantities;
  Z and p are the test result.
- The
  [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)
  help page documents that omega-squared and epsilon-squared are
  truncated at 0.
- The SPSS compatibility vignette lists the `w_*` functions and
  [`scheffe_test()`](https://YannickDiehl.github.io/mariposa/reference/scheffe_test.md)
  as validated.

### Bug fixes

- Weighted
  [`kendall_tau()`](https://YannickDiehl.github.io/mariposa/reference/kendall_tau.md)
  computes the tau-b denominator correctly for data with pairs tied on
  both variables.
- Weighted
  [`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md)
  uses the correct grand mean rank, so H is no longer too large.

## mariposa 0.6.11

Removes the argument names and result columns deprecated in 0.6.9 and
0.6.10.

### Breaking changes

- The dot-case argument names of
  [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)
  (`show.id`, `show.type`, `show.labels`, `show.values`, `show.freq`,
  `show.na`, `show.unused`, `max.values`, `max.len`, `sort.by.name`) and
  of
  [`val_labels()`](https://YannickDiehl.github.io/mariposa/reference/val_labels.md)
  and
  [`drop_labels()`](https://YannickDiehl.github.io/mariposa/reference/drop_labels.md)
  (`drop.na`) raise an error; use the snake_case names (e.g. `show_id`,
  `drop_na`).
- The duplicate result columns are removed; use the canonical names:
  `chi_squared` instead of `chi_sq`
  ([`chisq_gof()`](https://YannickDiehl.github.io/mariposa/reference/chisq_gof.md),
  [`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md))
  and instead of `statistic`
  ([`mcnemar_test()`](https://YannickDiehl.github.io/mariposa/reference/mcnemar_test.md)),
  `r_effect` instead of `effect_size_r`
  ([`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md)),
  and `F_statistic` instead of `F_stat`
  ([`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)).
  Statistical values are unchanged.

## mariposa 0.6.10

Harmonizes result column names across functions.

### Breaking changes

- The old column names `chi_sq`
  ([`chisq_gof()`](https://YannickDiehl.github.io/mariposa/reference/chisq_gof.md),
  [`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md)),
  `statistic`
  ([`mcnemar_test()`](https://YannickDiehl.github.io/mariposa/reference/mcnemar_test.md)),
  `effect_size_r`
  ([`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md))
  and `F_stat`
  ([`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md))
  are deprecated. They remain available as duplicate columns in this
  release and will be removed in 0.6.11.

### Improvements

- Shared statistics use the same result column names in all functions:
  `chi_squared` in
  [`chisq_gof()`](https://YannickDiehl.github.io/mariposa/reference/chisq_gof.md),
  [`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md)
  and
  [`mcnemar_test()`](https://YannickDiehl.github.io/mariposa/reference/mcnemar_test.md)
  (as in
  [`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)),
  `r_effect` in
  [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md)
  (as in
  [`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/reference/wilcoxon_test.md)),
  and `F_statistic` in
  [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)
  (as in
  [`levene_test()`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md)).
  Statistical values are unchanged.

## mariposa 0.6.9

Completes the switch to snake_case argument names.

### Breaking changes

- The dot-case argument names deprecated in 0.6.8 raise an error in
  [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md),
  [`fre()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md),
  [`rec()`](https://YannickDiehl.github.io/mariposa/reference/rec.md),
  [`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md),
  [`to_character()`](https://YannickDiehl.github.io/mariposa/reference/to_character.md),
  [`to_numeric()`](https://YannickDiehl.github.io/mariposa/reference/to_numeric.md),
  [`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md),
  [`read_por()`](https://YannickDiehl.github.io/mariposa/reference/read_por.md),
  [`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md),
  [`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md)
  and
  [`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md);
  use the snake_case names (e.g. `sort_frq` instead of `sort.frq`).
- In functions that select variables through `...`, an old argument name
  raises an informative error instead of being taken as a variable
  selection.
- The arguments of
  [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)
  are renamed to snake_case (`show_id`, `show_type`, `show_labels`,
  `show_values`, `show_freq`, `show_na`, `show_unused`, `max_values`,
  `max_len`, `sort_by_name`), as is `drop_na` in
  [`val_labels()`](https://YannickDiehl.github.io/mariposa/reference/val_labels.md)
  and
  [`drop_labels()`](https://YannickDiehl.github.io/mariposa/reference/drop_labels.md).
  The old names still work in this release with a warning (once per
  session).
- The display options stored in
  [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)
  results (`result$options`) use snake_case keys.

## mariposa 0.6.8

Argument names switch to snake_case.

### Breaking changes

- Arguments with dots in their names are renamed to snake_case; the old
  names still work in this release with a warning (once per session).
  The base R conventions `na.rm`, `conf.level` and `var.equal` are
  unchanged. Renamed arguments:
  - [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
    and
    [`fre()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md):
    `sort_frq`, `show_na`, `show_prc`, `show_valid`, `show_sum`,
    `show_labels`, `show_unused`
  - [`rec()`](https://YannickDiehl.github.io/mariposa/reference/rec.md):
    `as_factor`, `var_label`, `val_labels`
  - [`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md),
    [`to_character()`](https://YannickDiehl.github.io/mariposa/reference/to_character.md),
    [`to_numeric()`](https://YannickDiehl.github.io/mariposa/reference/to_numeric.md):
    `drop_na`, `drop_unused`, `add_non_labelled`, `use_labels`,
    `start_at`, `keep_labels`
  - [`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md),
    [`read_por()`](https://YannickDiehl.github.io/mariposa/reference/read_por.md),
    [`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md),
    [`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md),
    [`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md):
    `tag_na`
- [`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)
  results no longer contain the duplicate columns `CI_lower` and
  `CI_upper`; use `conf_int_lower` and `conf_int_upper`.

### Improvements

- [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
  validates `sort_frq` (`"none"`, `"asc"`, `"desc"`) and `show_labels`
  (`TRUE`, `FALSE`, `"auto"`) with a clear error; a misspelled
  `sort_frq` previously produced an unsorted table silently.
- The vignettes use the new argument names.

## mariposa 0.6.7

Uniform output for all analysis functions; statistical results are
unchanged.

### Improvements

- Printing a result of
  [`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md),
  [`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/reference/wilcoxon_test.md),
  [`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md),
  [`binomial_test()`](https://YannickDiehl.github.io/mariposa/reference/binomial_test.md),
  [`fisher_test()`](https://YannickDiehl.github.io/mariposa/reference/fisher_test.md),
  [`chisq_gof()`](https://YannickDiehl.github.io/mariposa/reference/chisq_gof.md),
  [`mcnemar_test()`](https://YannickDiehl.github.io/mariposa/reference/mcnemar_test.md),
  [`levene_test()`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md),
  [`tukey_test()`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md),
  [`scheffe_test()`](https://YannickDiehl.github.io/mariposa/reference/scheffe_test.md),
  [`dunn_test()`](https://YannickDiehl.github.io/mariposa/reference/dunn_test.md),
  [`pairwise_wilcoxon()`](https://YannickDiehl.github.io/mariposa/reference/pairwise_wilcoxon.md),
  [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
  or
  [`crosstab()`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md)
  shows a compact overview (headline statistic, p-value, significance
  stars, one line per test). Everything previously shown by
  [`print()`](https://rdrr.io/r/base/print.html) is available through
  [`summary()`](https://rdrr.io/r/base/summary.html), which can show or
  hide each section.
- [`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
  gains a [`summary()`](https://rdrr.io/r/base/summary.html) method,
  consistent with the other functions.

## mariposa 0.6.6

Maintenance release: internal restructuring; statistical results are
unchanged.

### Improvements

- [`tukey_test()`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md)
  and
  [`scheffe_test()`](https://YannickDiehl.github.io/mariposa/reference/scheffe_test.md)
  print aligned tables and show p-values in SPSS style.
- Input validation errors name the function called by the user rather
  than an internal helper.
- Internal functions no longer have help pages.

## mariposa 0.6.5

Maintenance release; statistical results are unchanged.

### Improvements

- tidyr is no longer a dependency.
- Errors inside grouped analyses keep the original error message.
- The error for a missing haven installation names the function that was
  called.
- The function reference on the package website lists
  [`phi()`](https://YannickDiehl.github.io/mariposa/reference/phi.md),
  [`cramers_v()`](https://YannickDiehl.github.io/mariposa/reference/phi.md)
  and
  [`goodman_gamma()`](https://YannickDiehl.github.io/mariposa/reference/phi.md).

## mariposa 0.6.4

Improves the accuracy of several statistics and makes functions behave
more consistently. Some results change slightly, in each case toward the
standard reference implementations.

### Breaking changes

- [`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md)
  reports its effect size as `epsilon_squared` (previously
  `eta_squared`).
- [`phi()`](https://YannickDiehl.github.io/mariposa/reference/phi.md),
  [`cramers_v()`](https://YannickDiehl.github.io/mariposa/reference/phi.md)
  and
  [`goodman_gamma()`](https://YannickDiehl.github.io/mariposa/reference/phi.md)
  return the effect size as a single number; use
  [`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
  for the full test output.
- Negative weights are rejected with a clear error in all functions.
- [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)
  always reports both the classical and the Welch results (as SPSS
  ONEWAY does), so its `var.equal` argument is deprecated; `ss_type` in
  [`factorial_anova()`](https://YannickDiehl.github.io/mariposa/reference/factorial_anova.md)
  and
  [`ancova()`](https://YannickDiehl.github.io/mariposa/reference/ancova.md)
  is deprecated in favor of the SPSS-standard Type III sums of squares.

### New features

- [`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)
  reports collinearity diagnostics (Tolerance and VIF per model term);
  `summary(..., collinearity = FALSE)` hides them.

### Improvements

- The weighted Wilcoxon signed-rank test
  ([`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/reference/wilcoxon_test.md),
  [`pairwise_wilcoxon()`](https://YannickDiehl.github.io/mariposa/reference/pairwise_wilcoxon.md))
  uses frequency-expansion mid-ranks: integer weights give exactly the
  expanded-data result, and weights of 1 reproduce the unweighted test.
  Mean ranks in weighted
  [`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md)
  and
  [`dunn_test()`](https://YannickDiehl.github.io/mariposa/reference/dunn_test.md)
  follow the same convention.
- The weighted median equals the weighted 50th percentile, and
  unweighted quantiles follow the SPSS definition (HAVERAGE, type 6)
  throughout.
- [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
  header statistics use the same formulas as
  [`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
  and the `w_*` functions.
- [`write_spss()`](https://YannickDiehl.github.io/mariposa/reference/write_spss.md)
  protects valid values when many missing-value codes must be combined
  into a range, and explains what it does.
- Post-hoc tests report when a computation cannot be carried out instead
  of skipping it silently.

### Bug fixes

- The significance test of Kendall’s tau-b agrees with
  [`stats::cor.test()`](https://rdrr.io/r/stats/cor.test.html) and the
  SPSS formula, notably for heavily tied data such as binary variables.
- The asymptotic p-value of
  [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md)
  matches its reported Z (SPSS convention, no continuity correction).
- Degrees of freedom in weighted
  [`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)
  and in the omnibus test of
  [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)
  are derived from the fitted model terms, improving results for models
  with dummy-coded factors or interactions.
- The weighted two-sample
  [`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)
  respects `var.equal` for its primary result.
- Correlation functions return NA for constant variables instead of an
  error.
- `frequency(show.unused = TRUE)` works on variables tagged with
  [`set_na()`](https://YannickDiehl.github.io/mariposa/reference/set_na.md)
  or
  [`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md),
  and `sort.frq` sorts by frequency with a monotone cumulative
  percentage.
- Significance stars use one boundary convention everywhere, matching
  the printed legend.
- [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)
  shows separation and convergence warnings again.
- Grouped single-variable `w_*` results print their statistics again.

## mariposa 0.6.3.2

### Bug fixes

- [`rec()`](https://YannickDiehl.github.io/mariposa/reference/rec.md)
  matches single decimal values (e.g. `"3.6=2"`) reliably, even when the
  stored value carries floating-point representation error.

## mariposa 0.6.3.1

### Bug fixes

- [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html),
  [`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
  and
  [`broom::augment()`](https://generics.r-lib.org/reference/augment.html)
  work correctly on
  [`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)
  and
  [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)
  results; [`summary()`](https://rdrr.io/r/base/summary.html) still
  returns the SPSS-style output.
- On grouped results or results with `use = "pairwise"`, these functions
  raise an informative error that points to per-group use
  (`lapply(r$groups, ...)`) or `use = "listwise"`.

## mariposa 0.6.3

Regression results are now standard R model objects.

### Breaking changes

- [`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)
  and
  [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)
  results are the fitted `lm` or `glm` objects themselves (classes
  `c("linear_regression", "lm")` and
  `c("logistic_regression", "glm", "lm")`), with the mariposa tables
  attached. The `$model` element is gone; use the result directly (e.g.
  `predict(r, ...)`).
- The `$coefficients` table is renamed to `$coef_table` (`coef(r)`
  returns the coefficient vector), and the `$anova` table to
  `$anova_table` (`anova(r)` returns R’s sequential ANOVA table).

### New features

- Standard model functions such as
  [`coef()`](https://rdrr.io/r/stats/coef.html),
  [`predict()`](https://rdrr.io/r/stats/predict.html),
  [`anova()`](https://rdrr.io/r/stats/anova.html),
  [`vcov()`](https://rdrr.io/r/stats/vcov.html),
  [`confint()`](https://rdrr.io/r/stats/confint.html),
  [`residuals()`](https://rdrr.io/r/stats/residuals.html) and
  [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) work directly
  on regression results.
  [`summary()`](https://rdrr.io/r/base/summary.html) still returns the
  SPSS-style summary;
  [`stats::summary.lm()`](https://rdrr.io/r/stats/summary.lm.html) and
  [`stats::summary.glm()`](https://rdrr.io/r/stats/summary.glm.html)
  give R’s standard summaries.
- In grouped results, each element of `r$groups` is a model object, so
  these functions can be applied per group; on the grouped result itself
  and on `use = "pairwise"` results they raise an informative error.

## mariposa 0.6.2

### Breaking changes

- [`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)
  and
  [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)
  gain a `factors` argument. The new default `factors = "dummy"` expands
  a factor with L levels into L - 1 dummy variables, as
  [`lm()`](https://rdrr.io/r/stats/lm.html) and
  [`glm()`](https://rdrr.io/r/stats/glm.html) do; previously, factor
  levels were silently converted to integer codes. `factors = "numeric"`
  restores this SPSS-style treatment and lists the converted variables
  in a message.
- With `use = "pairwise"`, factor predictors require
  `factors = "numeric"` or `use = "listwise"`; otherwise an informative
  error is raised.

### Bug fixes

- Weighted
  [`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)
  and
  [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)
  use the unrounded sum of weights in degrees of freedom, F, R-squared
  and pseudo R-squared, bringing results closer to SPSS; the displayed N
  stays rounded.
- `summary(descriptives = TRUE)` for
  [`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)
  prints the Descriptive Statistics table.
- Printing weighted
  [`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)
  results with non-integer degrees of freedom no longer fails.
- The examples of the regression
  [`summary()`](https://rdrr.io/r/base/summary.html) methods use the
  correct argument names.

## mariposa 0.6.1

Brings weighted results closer to SPSS (weighted results may shift
slightly) and documents the validation status of each function.

### Improvements

- New
  [`vignette("spss-compatibility")`](https://YannickDiehl.github.io/mariposa/articles/spss-compatibility.md)
  shows the validation status of each function.
- The
  [`spearman_rho()`](https://YannickDiehl.github.io/mariposa/reference/spearman_rho.md)
  documentation states that weights are used only to select cases, not
  in the rank correlation itself.
- The
  [`pearson_cor()`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md)
  documentation explains that raw expansion weights produce too narrow
  confidence intervals and should be normalized first.
- PMCMRplus and survey are no longer suggested dependencies.

### Bug fixes

- Weighted
  [`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)
  uses the unrounded sum of weights for variances, standard errors and
  degrees of freedom, including the Welch correction.
- Weighted
  [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)
  computes variances, standard errors, confidence intervals and
  within-groups degrees of freedom as SPSS ONEWAY does.
- Weighted
  [`levene_test()`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md)
  uses the unrounded sum of weights in its degrees of freedom.
- [`dunn_test()`](https://YannickDiehl.github.io/mariposa/reference/dunn_test.md)
  applies the tie correction to its standard errors, so \|Z\| is no
  longer underestimated for tied data such as Likert scales.
- Weighted
  [`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md)
  applies the tie correction like the unweighted test.
- Weighted skewness and kurtosis in
  [`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
  use the bias-corrected formulas of
  [`w_skew()`](https://YannickDiehl.github.io/mariposa/reference/w_skew.md),
  [`w_kurtosis()`](https://YannickDiehl.github.io/mariposa/reference/w_kurtosis.md)
  and SPSS FREQUENCIES.
- Weighted and unweighted quantiles follow SPSS FREQUENCIES (HAVERAGE,
  type 6) instead of R’s default type 7.

## mariposa 0.6.0

Adds functions for label management, data transformation, row operations
and variable search.

### Breaking changes

- `scale_index()` is removed; use
  [`row_means()`](https://YannickDiehl.github.io/mariposa/reference/row_means.md),
  which provides the same functionality.

### New features

- New
  [`var_label()`](https://YannickDiehl.github.io/mariposa/reference/var_label.md)
  gets and sets variable labels.
- New
  [`val_labels()`](https://YannickDiehl.github.io/mariposa/reference/val_labels.md)
  gets and sets value labels; `.add = TRUE` extends existing labels.
- New
  [`copy_labels()`](https://YannickDiehl.github.io/mariposa/reference/copy_labels.md)
  copies all label attributes to matching columns of another data frame,
  e.g. to restore labels lost in dplyr operations.
- New
  [`drop_labels()`](https://YannickDiehl.github.io/mariposa/reference/drop_labels.md)
  removes value labels for values that do not occur in the data.
- New
  [`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md)
  and
  [`to_character()`](https://YannickDiehl.github.io/mariposa/reference/to_character.md)
  convert labelled vectors to factors (levels in code order) or
  character; new
  [`to_numeric()`](https://YannickDiehl.github.io/mariposa/reference/to_numeric.md)
  and
  [`to_labelled()`](https://YannickDiehl.github.io/mariposa/reference/to_labelled.md)
  convert factors and other vectors to numeric or labelled vectors.
- New
  [`set_na()`](https://YannickDiehl.github.io/mariposa/reference/set_na.md)
  declares values as missing, by default as tagged NAs that
  [`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md),
  [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
  and
  [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)
  recognize; named arguments set codes per variable.
- New
  [`unlabel()`](https://YannickDiehl.github.io/mariposa/reference/unlabel.md)
  removes all label metadata and returns plain R vectors.
- New
  [`rec()`](https://YannickDiehl.github.io/mariposa/reference/rec.md)
  recodes values with a compact string syntax (e.g.
  `"1:2=1 [Low]; 3:5=2 [High]"`), including ranges, `min`/`max`, `copy`
  and value labels.
- New
  [`to_dummy()`](https://YannickDiehl.github.io/mariposa/reference/to_dummy.md)
  creates 0/1 indicator variables, optionally named after the value
  labels.
- New
  [`std()`](https://YannickDiehl.github.io/mariposa/reference/std.md)
  standardizes variables with four methods (`"sd"`, `"2sd"`, `"mad"`,
  `"gmd"`), with weights,
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  and a robust option.
- New
  [`center()`](https://YannickDiehl.github.io/mariposa/reference/center.md)
  centers variables at the grand mean or group mean, with weights.
- New
  [`row_means()`](https://YannickDiehl.github.io/mariposa/reference/row_means.md)
  and
  [`row_sums()`](https://YannickDiehl.github.io/mariposa/reference/row_sums.md)
  compute row-wise means and sums with a minimum number of valid values
  (`min_valid`, like SPSS `MEAN.x()`); new
  [`row_count()`](https://YannickDiehl.github.io/mariposa/reference/row_count.md)
  counts specific values per row.
- New
  [`find_var()`](https://YannickDiehl.github.io/mariposa/reference/find_var.md)
  searches variables by name or label.

## mariposa 0.5.6

Adds export to SPSS, Stata and SAS transport files.

### New features

- New
  [`write_spss()`](https://YannickDiehl.github.io/mariposa/reference/write_spss.md)
  writes SPSS `.sav` files and converts tagged NAs back to user-defined
  missing values, for lossless roundtrips with
  [`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md);
  it supports byte, none and zsav compression.
- New
  [`write_stata()`](https://YannickDiehl.github.io/mariposa/reference/write_stata.md)
  writes Stata `.dta` files (versions 8 to 15), with tagged NAs as
  extended missing values (`.a` to `.z`).
- New
  [`write_xpt()`](https://YannickDiehl.github.io/mariposa/reference/write_xpt.md)
  writes SAS transport `.xpt` files (versions 5 and 8), with tagged NAs
  as special missing values (`.A` to `.Z`, `._`).
- Data imported from one format can be exported to another (e.g. SPSS to
  Stata), with missing-value types converted automatically.

## mariposa 0.5.5

Adds Excel import and export with label preservation.

### New features

- New
  [`read_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/read_xlsx.md)
  reads Excel files and restores variable labels, value labels and
  tagged NAs from files written by
  [`write_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/write_xlsx.md);
  other Excel files are read as plain data.
- New
  [`write_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/write_xlsx.md)
  exports data frames (with a “Labels” sheet for variable labels, value
  labels and missing-value codes), codebooks (with optional frequency
  sheets, `frequencies = TRUE`) and named lists (one sheet per element)
  to Excel. Missing-value codes stay visible in the data sheet.
- Excel import and export require the suggested package openxlsx2.

## mariposa 0.5.4

Adds import of Stata, SAS and SPSS portable files.

### Breaking changes

- The `spss_code` column of
  [`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md)
  is renamed to `code` and contains character values, such as `"-9"` for
  SPSS or `".a"` for Stata.

### New features

- New
  [`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md)
  reads Stata `.dta` files and integrates extended missing values (`.a`
  to `.z`) with
  [`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md),
  [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
  and
  [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md).
- New
  [`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md)
  reads SAS `.sas7bdat` files, optionally with a catalog file
  (`.sas7bcat`) for value labels, and integrates special missing values
  (`.A` to `.Z`, `._`).
- New
  [`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md)
  reads SAS transport `.xpt` files with support for special missing
  values.
- New
  [`read_por()`](https://YannickDiehl.github.io/mariposa/reference/read_por.md)
  reads SPSS portable `.por` files with the same tagged NA support as
  [`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md).

### Improvements

- [`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md),
  [`untag_na()`](https://YannickDiehl.github.io/mariposa/reference/untag_na.md)
  and
  [`strip_tags()`](https://YannickDiehl.github.io/mariposa/reference/strip_tags.md)
  work with data from all supported formats.
- For Stata and SAS data, which have no numeric missing-value codes to
  recover,
  [`untag_na()`](https://YannickDiehl.github.io/mariposa/reference/untag_na.md)
  warns and behaves like
  [`strip_tags()`](https://YannickDiehl.github.io/mariposa/reference/strip_tags.md).
- [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
  and
  [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)
  show missing-value codes in the notation of the source format
  (e.g. `-9`, `.a`, `.A`).

## mariposa 0.5.3

Adds SPSS import with tagged missing values.

### New features

- New
  [`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md)
  reads SPSS `.sav` files and keeps user-defined missing values as
  tagged NAs, so different types of missing data (e.g. “no answer”, “not
  applicable”) remain distinguishable while behaving as `NA` in R. It
  also handles large survey files.
- New
  [`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md)
  shows the types of missing values in a variable with counts, original
  codes and labels.
- New
  [`untag_na()`](https://YannickDiehl.github.io/mariposa/reference/untag_na.md)
  converts tagged NAs back to their original SPSS missing-value codes.
- New
  [`strip_tags()`](https://YannickDiehl.github.io/mariposa/reference/strip_tags.md)
  converts tagged NAs to regular `NA`.

### Improvements

- [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
  lists each type of tagged NA in its own row with code and label,
  followed by “Total Valid” and “Total Missing” rows.

### Bug fixes

- `frequency(show.unused = TRUE)` no longer lists labels of tagged NAs
  as unused categories.

## mariposa 0.5.2.2

### New features

- New
  [`fre()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
  is a shorthand for
  [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md).

## mariposa 0.5.2

### New features

- New
  [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)
  creates an interactive HTML data dictionary in the RStudio Viewer,
  showing each variable’s position, name, type, label, values, value
  labels and frequencies. It is built with htmltools, a new dependency.
- Printing a codebook gives a short overview;
  [`summary()`](https://rdrr.io/r/base/summary.html) gives a text
  version with selectable sections (`overview`, `variable_details`,
  `value_labels`).
- [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)
  supports tidyselect, survey weights for weighted frequencies, and
  sorting by variable name.

## mariposa 0.5.1

### New features

- [`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md),
  [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md),
  [`factorial_anova()`](https://YannickDiehl.github.io/mariposa/reference/factorial_anova.md),
  [`ancova()`](https://YannickDiehl.github.io/mariposa/reference/ancova.md),
  [`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md),
  [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md),
  [`pearson_cor()`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md),
  [`spearman_rho()`](https://YannickDiehl.github.io/mariposa/reference/spearman_rho.md),
  [`kendall_tau()`](https://YannickDiehl.github.io/mariposa/reference/kendall_tau.md),
  [`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md),
  [`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md),
  [`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)
  and
  [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)
  gain [`summary()`](https://rdrr.io/r/base/summary.html) methods:
  printing a result gives a compact overview, and
  [`summary()`](https://rdrr.io/r/base/summary.html) gives the detailed
  SPSS-style output.
- Arguments of [`summary()`](https://rdrr.io/r/base/summary.html) show
  or hide individual sections (e.g.
  `summary(result, effect_sizes = FALSE)`).

### Improvements

- All print and summary methods are documented, and the examples of the
  analysis functions demonstrate both output levels.

### Bug fixes

- The examples of
  [`ancova()`](https://YannickDiehl.github.io/mariposa/reference/ancova.md)
  and
  [`factorial_anova()`](https://YannickDiehl.github.io/mariposa/reference/factorial_anova.md)
  use the correct `dv`/`between` interface, and examples refer to the
  `education` variable correctly.

## mariposa 0.5.0

Adds factorial ANOVA and ANCOVA.

### New features

- New
  [`factorial_anova()`](https://YannickDiehl.github.io/mariposa/reference/factorial_anova.md)
  performs between-subjects ANOVA with up to three factors and Type III
  sums of squares, as SPSS UNIANOVA does: main effects, all
  interactions, partial eta squared, R-squared and Levene’s test. It
  works with
  [`tukey_test()`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md),
  [`scheffe_test()`](https://YannickDiehl.github.io/mariposa/reference/scheffe_test.md)
  and
  [`levene_test()`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md).
- New
  [`ancova()`](https://YannickDiehl.github.io/mariposa/reference/ancova.md)
  performs analysis of covariance (SPSS UNIANOVA with WITH): ANOVA
  table, parameter estimates, estimated marginal means adjusted for the
  covariates, and Levene’s test; it supports up to three factors and
  multiple covariates.
- Both functions support survey weights through weighted least squares,
  as SPSS /REGWGT does (including Levene’s test), handle unbalanced
  designs correctly, and do not require the car package.

## mariposa 0.4.0

Adds exact and goodness-of-fit tests and non-parametric post-hoc tests.

### New features

- New
  [`fisher_test()`](https://YannickDiehl.github.io/mariposa/reference/fisher_test.md)
  performs Fisher’s exact test of independence, recommended for small
  samples or expected cell counts below 5; it supports weights, multiple
  variables and
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
- New
  [`chisq_gof()`](https://YannickDiehl.github.io/mariposa/reference/chisq_gof.md)
  tests whether a categorical variable follows an expected distribution
  (equal proportions by default, or custom proportions), with residuals,
  weights and multiple variables.
- New
  [`mcnemar_test()`](https://YannickDiehl.github.io/mariposa/reference/mcnemar_test.md)
  tests changes in paired proportions of two dichotomous measurements
  (e.g. before/after), with asymptotic and exact p-values, the 2x2
  table, continuity correction and weights.
- New
  [`dunn_test()`](https://YannickDiehl.github.io/mariposa/reference/dunn_test.md)
  performs Dunn’s pairwise post-hoc comparisons on
  [`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md)
  results, with adjustable p-value correction (Bonferroni, Holm, BH and
  others).
- New
  [`pairwise_wilcoxon()`](https://YannickDiehl.github.io/mariposa/reference/pairwise_wilcoxon.md)
  performs pairwise Wilcoxon signed-rank post-hoc comparisons on
  [`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md)
  results, with adjustable p-value correction.

## mariposa 0.3.1

### New features

- [`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md)
  supports maximum likelihood extraction (`extraction = "ml"`) with a
  goodness-of-fit chi-square test, initial communalities (squared
  multiple correlations) and uniquenesses; survey weights are supported.
- [`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md)
  supports Promax rotation (`rotation = "promax"`), an oblique rotation
  that reports the pattern matrix, structure matrix and factor
  correlation matrix.

## mariposa 0.3.0

Adds non-parametric tests.

### New features

- New
  [`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md)
  compares three or more independent groups on ordinal data
  (non-parametric alternative to one-way ANOVA), with an effect size,
  weights,
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  and multiple variables.
- New
  [`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/reference/wilcoxon_test.md)
  compares two paired measurements (Wilcoxon signed-rank test), with
  rank categories (negative, positive, ties) and effect size r.
- New
  [`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md)
  compares three or more related measurements (non-parametric
  alternative to repeated-measures ANOVA), with Kendall’s W as effect
  size.
- New
  [`binomial_test()`](https://YannickDiehl.github.io/mariposa/reference/binomial_test.md)
  tests whether an observed proportion matches an expected value (exact
  binomial test), for one or more binary variables.

## mariposa 0.2.0

Adds scale analysis and regression.

### New features

- New
  [`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md)
  computes Cronbach’s alpha with item statistics: corrected item-total
  correlations, alpha if item deleted, and the inter-item correlation
  matrix.
- New
  [`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md)
  performs exploratory factor analysis with principal component
  extraction and Varimax or Oblimin rotation (Oblimin requires the
  suggested package GPArotation), including KMO, Bartlett’s test,
  communalities and a sorted loading matrix with a configurable blank
  threshold.
- New `scale_index()` computes mean indices across items, with
  `min_valid` like SPSS `MEAN.x()`.
- New
  [`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md)
  rescales values to percent of maximum possible scores (0-100).
- New
  [`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)
  fits linear models with SPSS-style output (coefficients with B, SE,
  Beta, t and p; ANOVA table; R, R-squared and adjusted R-squared),
  using a formula or the SPSS-style dependent/predictors interface.
- New
  [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)
  fits logistic models with odds ratios, Wald statistics, pseudo
  R-squared (Nagelkerke, Cox & Snell, McFadden) and a classification
  table.
- All new functions support survey weights and
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).

## mariposa 0.1.0

### Breaking changes

- [`gamma()`](https://rdrr.io/r/base/Special.html) is renamed to
  [`goodman_gamma()`](https://YannickDiehl.github.io/mariposa/reference/phi.md)
  so that it no longer masks
  [`base::gamma()`](https://rdrr.io/r/base/Special.html).
- Result classes no longer carry the `_results` suffix and match the
  function name (e.g. `t_test` instead of `t_test_results`).

### Improvements

- Result objects store only the columns needed for post-hoc tests, which
  reduces memory use.
- [`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)
  checks that `conf.level` is between 0 and 1;
  [`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)
  and
  [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)
  check that the selected variables are numeric;
  [`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
  warns when expected cell counts are below 5.
- Error and warning messages and printed output are formatted with the
  cli package.
- Help pages link related functions by family (descriptive statistics,
  hypothesis tests, correlation, post-hoc tests, weighted statistics).

### Bug fixes

- Weighted variance and standard deviation use the SPSS frequency-weight
  formula consistently.
- The asymptotic standard error of Goodman and Kruskal’s gamma uses the
  ASE0 formula (Agresti, 2002).
- Weighted Cohen’s d in
  [`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)
  uses weighted means and the pooled weighted standard deviation.
- Weighted kurtosis uses the SPSS sample-corrected (Type 2) formula.
