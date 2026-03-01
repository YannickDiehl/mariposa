# Changelog

## mariposa 0.3.0

### New Functions

- Added
  [`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md)
  for comparing 3+ independent groups on ordinal data (non-parametric
  alternative to one-way ANOVA). Supports survey weights,
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),
  and multi-variable analysis. Effect size: Eta-squared.

- Added
  [`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/reference/wilcoxon_test.md)
  for comparing two paired measurements without assuming normality
  (Wilcoxon signed-rank test). Includes rank categories (negative,
  positive, ties) and effect size r.

- Added
  [`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md)
  for comparing 3+ related measurements on ordinal data (non-parametric
  alternative to repeated-measures ANOVA). Effect size: Kendall’s W.

- Added
  [`binomial_test()`](https://YannickDiehl.github.io/mariposa/reference/binomial_test.md)
  for testing whether an observed proportion matches an expected value
  (exact binomial test). Supports multiple binary variables and custom
  test proportions.

### SPSS Validation

- Added 294 new SPSS validation tests across all 4 non-parametric
  functions, covering weighted/unweighted and grouped/ungrouped
  scenarios.

- Total test suite: 2,227 tests passing (0 failures, 0 skips).

------------------------------------------------------------------------

## mariposa 0.2.0

### New Functions

- Added
  [`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md)
  for Cronbach’s Alpha with item statistics, including corrected
  item-total correlations, alpha-if-item-deleted, and inter-item
  correlation matrix. Genuine implementation with full survey weight
  support.

- Added
  [`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md)
  for Exploratory Factor Analysis with PCA extraction. Supports Varimax
  rotation (Base R) and Oblimin rotation (via optional `GPArotation`
  package). Includes KMO measure, Bartlett’s test, communalities, and
  sorted factor loading matrix with configurable blank threshold.

- Added
  [`scale_index()`](https://YannickDiehl.github.io/mariposa/reference/scale_index.md)
  for creating mean indices across survey items, with `min_valid`
  parameter matching SPSS `MEAN.x()` syntax. Designed for use inside
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).

- Added
  [`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md)
  for Percent of Maximum Possible Scores transformation, rescaling
  values to a 0-100 range for cross-scale comparability.

- Added
  [`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)
  as a wrapper around [`stats::lm()`](https://rdrr.io/r/stats/lm.html)
  with SPSS-compatible output: coefficients table (B, SE, Beta, t, p),
  ANOVA table, model summary (R, R-squared, adjusted R-squared), and
  standardized coefficients. Supports both formula and SPSS-style
  (dependent/predictors) interfaces.

- Added
  [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)
  as a wrapper around [`stats::glm()`](https://rdrr.io/r/stats/glm.html)
  with odds ratios, Wald statistics, pseudo-R-squared measures
  (Nagelkerke, Cox-Snell, McFadden), and classification table.

### Dependencies

- Added `GPArotation` as suggested dependency for Oblimin rotation in
  [`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md).

- Added `MASS` as suggested dependency for enhanced regression
  diagnostics.

### Improvements

- All 6 new functions support survey weights and grouped analysis via
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).

- All functions include comprehensive roxygen2 documentation with
  practical examples, “When to Use” guidance, and “Understanding the
  Output” sections.

------------------------------------------------------------------------

## mariposa 0.1.0

### Breaking Changes

- [`gamma()`](https://rdrr.io/r/base/Special.html) has been renamed to
  [`goodman_gamma()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
  to avoid shadowing
  [`base::gamma()`](https://rdrr.io/r/base/Special.html). The function
  remains an alias for
  [`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
  and works identically.

- S3 class names unified: removed `_results` suffix from all result
  classes (e.g., `chi_square_results` -\> `chi_square`, `t_test_results`
  -\> `t_test`). Class names now match the function name that created
  them.

### Bug Fixes

- Fixed namespace collisions from triple-defined internal helper
  functions
  ([`.process_variables()`](https://YannickDiehl.github.io/mariposa/reference/dot-process_variables.md),
  [`.process_weights()`](https://YannickDiehl.github.io/mariposa/reference/dot-process_weights.md),
  [`.effective_n()`](https://YannickDiehl.github.io/mariposa/reference/dot-effective_n.md)).
  These are now defined once in `helpers.R` and shared across all
  functions.

- Fixed weighted variance/SD formula inconsistency. All weighted
  calculations now use the SPSS frequency weights formula:
  `sum(w * (x - w_mean)^2) / (V1 - 1)`.

- Fixed Gamma ASE (asymptotic standard error) calculation. Replaced
  empirical magic-number formula with the correct ASE0 formula from
  Agresti (2002).

- Fixed weighted Cohen’s d calculation in
  [`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md).
  Previously multiplied values by weights (`x * w`); now uses proper
  weighted means and pooled weighted standard deviation.

- Fixed weighted kurtosis formula. Changed from population excess
  kurtosis (`m4/m2^2 - 3`) to SPSS Type 2 sample-corrected formula
  (`G2 = ((n+1)*g2 + 6) * (n-1) / ((n-2)*(n-3))`), matching SPSS output.

### Improvements

- Refactored 9 of 11 `w_*` functions to use a shared factory pattern
  (`R/w_factory.R`). Eliminated ~2,460 lines of duplicated boilerplate
  (4,204 → 1,740 lines, -58.6%). `w_modus` and `w_quantile` remain
  standalone due to their fundamentally different interfaces.

- Added 83 SPSS validation tests for all `w_*` functions across 4
  scenarios (weighted/unweighted × grouped/ungrouped) in
  `test-weighted-statistics-spss-validation.R`.

- Reduced memory usage: result objects now store only the columns needed
  for post-hoc tests instead of the full input data frame.

- Unified print helper system: all output formatting now uses
  `print_helpers.R`. Removed deprecated `.print_header()`,
  `.print_border()`, `.get_border()`, and `.print_group_header()` from
  `helpers.R`.

- Deduplicated `t_test.R` print methods: `print.t_test_results` and
  `print.t_test_result` now share a common implementation (~360 fewer
  lines).

- Added input validation:

  - [`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md):
    validates `conf.level` is between 0 and 1
  - [`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md),
    [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md):
    validate that selected variables are numeric
  - [`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md):
    warns when expected cell counts \< 5

- Migrated error handling from
  [`stop()`](https://rdrr.io/r/base/stop.html)/[`warning()`](https://rdrr.io/r/base/warning.html)
  to `cli_abort()`/`cli_warn()` with structured messages, `{.arg}` and
  `{.var}` markup, and pluralization support.

- Added `cli` as dependency for professional user-facing messages and
  print output.

- Added `@family` tags to all 24 exported functions for
  cross-referencing in documentation (families: descriptive,
  hypothesis_tests, correlation, posthoc, weighted_statistics).

- Added `tests/testthat/helper-mariposa.R` with shared test utilities
  and centralized SPSS validation tolerances.

- Migrated `print_helpers.R` infrastructure to `cli` (`cli_rule()`,
  `cli_bullets()`, `cli_h2()`).

- Extended `globals.R` with missing NSE variable declarations.

- Fixed “SURVEYSTAT” reference in `imports.R`.
