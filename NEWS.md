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
