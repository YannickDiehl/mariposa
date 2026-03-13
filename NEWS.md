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
