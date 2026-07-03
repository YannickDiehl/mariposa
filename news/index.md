# Changelog

## mariposa 0.6.14

Codebook robustness (theme:
[`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)
survives real-world data and says what it shows). A stress test of the
codebook stack (metadata extraction, console print/summary, HTML
builder, xlsx export) surfaced a batch of crashes, silent data errors,
and display leaks; this release fixes all of them and adds a `view`
argument for side-effect control.

### Bug fixes

- Variables with value labels but **no variable label** no longer show a
  fake label like “1 \| 2 \| 3”: `attr(x, "label")` partially matched
  the `labels` attribute; all label reads now use `exact = TRUE`. The
  “Variables with labels” count excludes such variables accordingly.
- [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)
  no longer errors on **inline data expressions**
  (e.g. `codebook(data.frame(...))` spanning multiple deparse lines);
  long expressions collapse to the generic dataset name “data”.
- **List columns** no longer crash the frequency computation: they are
  skipped with a warning (“list column … skipped - not supported in
  codebooks”) and a data frame consisting only of list columns aborts
  with a clear error.
- The **tagged-NA breakdown is now consistent across all three layers**:
  - the HTML builder appends the missing-value rows (codes, labels,
    counts) below range-displayed variables too — previously an
    all-user-missing or high-cardinality variable silently lost them
    (the xlsx export already did this correctly);
  - NA frequencies are computed even for
    high-cardinality/range-displayed variables;
  - `print(summary(cb))` gains a “Missing values:” section (codes,
    labels, counts) — `show_na` was a no-op on the console layer before.

### Improvements

- **Central display formatting for numeric values**: empirical values
  and ranges no longer leak 15-digit doubles or scientific notation into
  the console/HTML/xlsx output. Fractional values show 4 significant
  digits (“0.3333”), whole numbers keep their integer look (“1”), tiny
  values are expanded (“0.00000001”, never “1e-08”). Frequency matching
  is unaffected: display strings and raw matching keys are carried
  separately (`empirical_values` vs. new `empirical_keys`).
- The percentage and effective-n columns (`prc`, `valid_prc`, `cum_prc`,
  `n_eff`) on `write_xlsx(cb, frequencies = TRUE)` sheets are rounded to
  2 decimals.
- `max_values` (single integer \>= 1) and `max_len` (single integer
  \>= 4) are validated up front with a clear error.
- Factor levels now respect `max_values` and truncate with the same “…
  (N more)” note used for character values.
- Range displays show the cardinality: “18 - 95 (78 distinct)” instead
  of just “18 - 95”.
- `file =` into a nonexistent directory aborts early, naming the missing
  directory.
- Polish: “1 variable” / “1 observation” pluralization in the console
  and HTML subtitle; very long character values are truncated to
  `max_len` with “…” (raw values still drive frequency matching);
  zero-row data frames say “(no observations)” instead of “(all
  missing)”.

### New features

- New `view` argument for
  [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)
  (default: [`interactive()`](https://rdrr.io/r/base/interactive.html)):
  controls whether the HTML codebook opens in the RStudio Viewer.
  `view = FALSE` suppresses the Viewer side effect entirely; writing via
  `file =` is unaffected. The compact
  [`print()`](https://rdrr.io/r/base/print.html) only advertises the
  Viewer when it was actually opened (result gains a `viewed` flag).

## mariposa 0.6.13

McDonald’s omega (theme: reliability() learns a second reliability
coefficient).
[`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md)
now reports McDonald’s omega alongside Cronbach’s alpha — a new
statistic within an existing function, hence a PATCH per the clarified
versioning policy.

### New features

- [`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md)
  computes **McDonald’s omega** from a one-factor maximum-likelihood
  model ([`stats::factanal`](https://rdrr.io/r/stats/factanal.html) on
  the same (weighted) correlation matrix already used for standardized
  alpha):
  - `omega` — raw/total omega in the covariance metric (analogous to raw
    alpha), reported as “McDonald’s Omega”;
  - `omega_std` — standardized omega in the correlation metric
    (analogous to standardized alpha);
  - `omega_if_deleted` — a new column in `item_total`, refitting the
    one-factor model per deleted item (NA when the reduced scale has
    fewer than 3 items, where the model is unidentified). Scales with
    fewer than 3 items get NA omega fields plus a warning (alpha is
    unaffected); non-convergent factor fits degrade to NA with the
    factanal message. The compact
    [`print()`](https://rdrr.io/r/base/print.html) shows omega next to
    alpha, and [`summary()`](https://rdrr.io/r/base/summary.html) adds
    omega rows to the Reliability Statistics block and an omega column
    to the Item-Total table.

### Validation

- McDonald’s omega is **Tier 4 (Internal, R-only)** for now: SPSS v27+
  offers omega in `RELIABILITY`, but IBM’s algorithm documentation is
  not publicly retrievable and no SPSS v29 reference run exists yet. The
  pending reference run is prepared in
  `.claude/spss-syntax-omega-references.sps` (expected values included);
  until it lands, omega is guarded by a parameter-recovery test on
  simulated congeneric data, exact cross-checks against a manual
  factanal computation, cross-checks against
  [`psych::omega()`](https://rdrr.io/pkg/psych/man/omega.html) and a
  lavaan/semTools one-factor CFA
  (`tests/testthat/test-reliability-omega.R`), and a `w == 1` block in
  the weights-invariance suite. The help page carries the Tier-4
  disclosure; the compatibility vignette flags omega as Internal (Tier
  4).
- `psych`, `lavaan`, and `semTools` added to Suggests (cross-check tests
  only; all gated by `skip_if_not_installed()`).

## mariposa 0.6.12

Weighted-rank correctness and accurate claims (theme: the weighted rank
family says exactly what it is). Two formula errors in weighted rank
statistics are fixed and a package-wide invariance suite now guards
every weighted entry point; alongside, the user-facing claim surface
(README, DESCRIPTION, help pages, compatibility vignette) is realigned
with what the validation suite actually covers.

### Bug fixes

- Weighted
  [`kendall_tau()`](https://YannickDiehl.github.io/mariposa/reference/kendall_tau.md):
  the tau-b denominator omitted double-tied pairs (`ties_both`) from the
  two tie-correction factors, deflating \|tau\| on tied data. The
  weighted denominator now mirrors the unweighted
  `(n0 - Tx - Txy)(n0 - Ty - Txy)` structure.
- Weighted
  [`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md):
  the grand mean rank was still the hard-coded `N/2` of the pre-0.6.4
  rank convention instead of `(N+1)/2`, inflating H. It is now derived
  from the weighted mid-ranks themselves.
- Both bugs violated the invariant that weights of exactly 1 must
  reproduce the unweighted result. A new package-wide invariance suite
  (`tests/testthat/test-weights-invariance.R`) enforces this w == 1
  reduction for every weighted entry point; intentionally approximate
  reductions (design-based `mann_whitney`, weighted Kendall z/p) are
  documented exceptions with bounded assertions.

### Accurate claims

- README and DESCRIPTION no longer overclaim: the paired t-test mode
  (not yet implemented) is no longer advertised, and “every function is
  validated … your results will match” is replaced by the
  Charter-compliant wording — validated against SPSS v29 within
  documented per-tier tolerances, with
  [`vignette("spss-compatibility")`](https://YannickDiehl.github.io/mariposa/articles/spss-compatibility.md)
  for per-function status.
- The weighted variants of the rank-based family —
  [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md),
  [`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md),
  [`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/reference/wilcoxon_test.md),
  [`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md),
  [`binomial_test()`](https://YannickDiehl.github.io/mariposa/reference/binomial_test.md),
  [`dunn_test()`](https://YannickDiehl.github.io/mariposa/reference/dunn_test.md),
  [`pairwise_wilcoxon()`](https://YannickDiehl.github.io/mariposa/reference/pairwise_wilcoxon.md),
  and
  [`kendall_tau()`](https://YannickDiehl.github.io/mariposa/reference/kendall_tau.md)
  — are now disclosed as R-only (Tier 4) in a “Weighted variants” note
  on each help page: SPSS `NPAR TESTS` / `NONPAR CORR` ignore
  `WEIGHT BY`, so no SPSS reference exists for these weighted paths.
  [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md)’s
  note also states that its design-based U/W may differ from SPSS’s
  expanded-data U (Z and p are the validated quantities);
  [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)
  now documents that omega-/epsilon-squared are truncated at 0 (negative
  raw estimates occur when F \< 1).

### Validation

- The SPSS-compatibility vignette is regenerated (was frozen at
  2026-05-19) and now carries an “Internal (Tier 4)” marker for the
  weighted rank variants. Generator fixes: the `w_*` family and
  `scheffe_test` are correctly matched to their shared test files
  (previously shown as “not validated” despite existing tests),
  zero-match tier counts no longer report as 1, and
  `assert_spss_count()` call sites are tallied as Spec.
- `test-t-test-spss-validation.R`: the header tier table claimed the
  t-statistic at Spec (±1e-5) while the assertions use Display(3); the
  header now matches the assertions.
- The last `expect_no_error()` in a validation file
  (`test-linear-regression-spss-validation.R`) is replaced by real
  assertions on the per-group predictions; the validation-discipline
  meta-test now passes with `MARIPOSA_VALIDATION_STRICT=TRUE`.

## mariposa 0.6.11

Deprecation cleanup (theme: the due bridges come out). Two batches of
deprecations reached their removal release together: the 0.6.9 argument
bridges (originally slated for 0.6.10) and the 0.6.10 duplicate result
columns. Removing both here keeps the run-up to the 1.0 API freeze tidy.

### Breaking changes

- The 0.6.9 dot-case argument bridges are removed as announced. The old
  names no longer warn-and-work; they now error (falling through to
  tidyselect or SET-mode validation):
  - [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md):
    `show.id`, `show.type`, `show.labels`, `show.values`, `show.freq`,
    `show.na`, `show.unused`, `max.values`, `max.len`, `sort.by.name`
    (use `show_id`, `show_type`, `show_labels`, `show_values`,
    `show_freq`, `show_na`, `show_unused`, `max_values`, `max_len`,
    `sort_by_name`)
  - [`val_labels()`](https://YannickDiehl.github.io/mariposa/reference/val_labels.md):
    `drop.na` (use `drop_na`)
  - [`drop_labels()`](https://YannickDiehl.github.io/mariposa/reference/drop_labels.md):
    `drop.na` (use `drop_na`) These bridges were originally slated for
    removal in 0.6.10 and are batched into this release. The
    [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)/[`rec()`](https://YannickDiehl.github.io/mariposa/reference/rec.md)/[`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md)
    family of removed-argument errors introduced in 0.6.9 remain in
    place as permanent guidance (their `...` consumes tidyselect, so a
    clear error beats a silent misinterpretation).
- The deprecated duplicate result columns kept for one release in 0.6.10
  are removed; only the canonical column remains:
  - [`chisq_gof()`](https://YannickDiehl.github.io/mariposa/reference/chisq_gof.md),
    [`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md):
    `chi_sq` removed (use `chi_squared`)
  - [`mcnemar_test()`](https://YannickDiehl.github.io/mariposa/reference/mcnemar_test.md):
    `statistic` removed (use `chi_squared`)
  - [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md):
    `effect_size_r` removed (use `r_effect`)
  - [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md):
    `F_stat` removed (use `F_statistic`) The statistical values are
    unchanged; only the redundant column names go away.

## mariposa 0.6.10

Result-column harmonization (theme: one statistic, one column name). A
style audit found the same statistic carrying different result-column
names across sibling functions; the drifted names now converge on the
canonical spelling, with the old columns kept as duplicates for one
release.

### Improvements

- The `$results` columns for shared statistics are harmonized on the
  canonical names already used elsewhere in the package:
  - Chi-square statistic: `chi_squared` (as in
    [`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)) -
    now also in
    [`chisq_gof()`](https://YannickDiehl.github.io/mariposa/reference/chisq_gof.md),
    [`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md),
    and
    [`mcnemar_test()`](https://YannickDiehl.github.io/mariposa/reference/mcnemar_test.md)
  - Effect size r: `r_effect` (as in
    [`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/reference/wilcoxon_test.md)) -
    now also in
    [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md)
  - F statistic: `F_statistic` (as in
    [`levene_test()`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md)) -
    now also in
    [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)
    Print and summary methods read the canonical columns; the
    statistical values are unchanged.

### Deprecations

- The old result-column names remain available as duplicated columns
  (positioned right after their canonical counterpart) for one release
  and will be removed in 0.6.11:
  - [`chisq_gof()`](https://YannickDiehl.github.io/mariposa/reference/chisq_gof.md),
    [`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md):
    `chi_sq` (use `chi_squared`)
  - [`mcnemar_test()`](https://YannickDiehl.github.io/mariposa/reference/mcnemar_test.md):
    `statistic` (use `chi_squared`)
  - [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md):
    `effect_size_r` (use `r_effect`)
  - [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md):
    `F_stat` (use `F_statistic`)

## mariposa 0.6.9

API-cleanup completion (theme: the 0.6.8 bridges come out, the last
dot-case stragglers get theirs). One step closer to the 1.0 API freeze.

### Breaking changes

- The 0.6.8 deprecation bridges are removed as announced. The dot-case
  argument names now error instead of warning:
  - [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)/[`fre()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md):
    `sort.frq`, `show.na`, `show.prc`, `show.valid`, `show.sum`,
    `show.labels`, `show.unused`
  - [`rec()`](https://YannickDiehl.github.io/mariposa/reference/rec.md):
    `as.factor`, `var.label`, `val.labels`
  - [`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md)/[`to_character()`](https://YannickDiehl.github.io/mariposa/reference/to_character.md)/[`to_numeric()`](https://YannickDiehl.github.io/mariposa/reference/to_numeric.md):
    `drop.na`, `drop.unused`, `add.non.labelled`, `use.labels`,
    `start.at`, `keep.labels`
  - [`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md)/[`read_por()`](https://YannickDiehl.github.io/mariposa/reference/read_por.md)/[`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md)/[`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md)/[`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md):
    `tag.na` Before: `frequency(data, x, sort.frq = "desc")` warned and
    worked. After: it errors with a pointer to `sort_frq`. In the
    functions whose `...` selects variables, the old names raise a clear
    “removed in 0.6.9” error instead of being silently swallowed by
    tidyselect; in the readers they fail as unused arguments.

### Deprecations

- The remaining dot-case arguments are renamed to snake_case with the
  usual one-release bridge (old names warn once per session; removal
  planned for 0.6.10):
  - [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md):
    `show_id`, `show_type`, `show_labels`, `show_values`, `show_freq`,
    `show_na`, `show_unused`, `max_values`, `max_len`, `sort_by_name`
  - [`val_labels()`](https://YannickDiehl.github.io/mariposa/reference/val_labels.md):
    `drop_na`
  - [`drop_labels()`](https://YannickDiehl.github.io/mariposa/reference/drop_labels.md):
    `drop_na` The display options stored on codebook results
    (`result$options`) use the snake_case keys as well.

## mariposa 0.6.8

API-unification release (theme: snake_case arguments). One release-long
deprecation bridge per the versioning policy - old names keep working
and warn once per session; they will be removed in 0.6.9.

### Breaking changes (with bridge)

- Dot-case arguments renamed to snake_case:
  - [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)/[`fre()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md):
    `sort_frq`, `show_na`, `show_prc`, `show_valid`, `show_sum`,
    `show_labels`, `show_unused`
  - [`rec()`](https://YannickDiehl.github.io/mariposa/reference/rec.md):
    `as_factor`, `var_label`, `val_labels`
  - [`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md)/[`to_character()`](https://YannickDiehl.github.io/mariposa/reference/to_character.md)/[`to_numeric()`](https://YannickDiehl.github.io/mariposa/reference/to_numeric.md):
    `drop_na`, `drop_unused`, `add_non_labelled`, `use_labels`,
    `start_at`, `keep_labels`
  - [`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md)/[`read_por()`](https://YannickDiehl.github.io/mariposa/reference/read_por.md)/[`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md)/[`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md)/[`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md):
    `tag_na` Base-R-universal names (`na.rm`, `conf.level`, `var.equal`)
    are kept.

### Breaking changes (no bridge)

- [`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)
  results no longer carry the duplicated `CI_lower`/`CI_upper` alias
  columns; `conf_int_lower`/`conf_int_upper` are the contract.

### Improvements

- `sort_frq` is validated (`"none"/"asc"/"desc"`) - typos used to
  silently produce an unsorted table; `show_labels` validates its
  `TRUE`/`FALSE`/`"auto"` values with a clear error.
- Vignettes showcase the new argument names.

## mariposa 0.6.7

Output-layer release (theme: uniform three-layer output). Statistical
results are unchanged; what changed is how results present themselves.

### Uniform three-layer output (visible change)

Every analysis class now follows the documented pattern that t_test and
chi_square pioneered: `result` prints a compact overview (headline
statistic, p-value, significance stars, one line per test), and
`summary(result)` carries the full detailed output behind boolean
section toggles. Newly migrated: kruskal_wallis, wilcoxon_test,
friedman_test, binomial_test, fisher_test, chisq_gof, mcnemar_test,
levene_test, tukey_test, scheffe_test, dunn_test, pairwise_wilcoxon,
frequency, crosstab (describe was already compact and gained the summary
layer for uniformity). Nothing was removed - everything the old print()
showed is in summary(), verified line-by-line.

### Internal architecture

- One shared engine for the three correlation functions
  (pearson/spearman/kendall results verified byte-identical across 21
  scenarios; ~500 lines removed).
- The w\_\* factory now supports multi-value and non-numeric statistics;
  w_quantile() and w_modus() are ordinary plugins instead of pipeline
  reimplementations (~420 lines removed, results identical across 31
  scenarios).

## mariposa 0.6.6

Internal-architecture release (theme: shared cores and formatting
utilities). No statistical results change; table rendering in the
Tukey/Scheffe output is now aligned and uses SPSS-style p display.

- One home for every weighted formula: the new weighted-statistics
  kernel file backs describe(), frequency(), the w\_\* functions and the
  rank tests; the weighted variance formula previously existed in six
  files.
- New internal output utilities (bordered table renderer,
  grouped-results iterator, unified number/p formatting) - the building
  blocks the print style guide documented; adoption started with the
  post-hoc tests.
- Tukey and Scheffe now share one engine and one print implementation
  (results verified byte-identical); t_test() and oneway_anova() were
  restructured from 500-line nested-closure bodies into short
  orchestrators with file-level helpers (byte-identical results).
- Weights in summarise() context are captured as quosures
  (enquo/eval_tidy) instead of frame-walking; shared validators report
  errors at the user-facing call site.
- Documentation internals standardized on
  [@noRd](https://github.com/noRd) (man/ shrinks by ~120 internal
  pages).

## mariposa 0.6.5

Housekeeping release (theme: package hygiene). No statistical results
change.

- Slimmer dependencies: removed the unused tidyr import and pruned
  unused `importFrom` entries.
- Error chains: failures inside grouped analyses are re-thrown with
  `cli_abort(parent = ...)` so the original condition is preserved; the
  haven requirement is enforced by one central guard that reports the
  calling function instead of an internal helper.
- Import internals: the native-missing-value detection shared by
  [`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md),
  [`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md)
  and
  [`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md)
  now lives in one helper instead of three copies.
- Mechanical polish: remaining
  [`sapply()`](https://rdrr.io/r/base/lapply.html) calls in the oldest
  files converted to type-stable
  [`vapply()`](https://rdrr.io/r/base/lapply.html); pkgdown reference
  now lists
  [`phi()`](https://YannickDiehl.github.io/mariposa/reference/phi.md),
  [`cramers_v()`](https://YannickDiehl.github.io/mariposa/reference/phi.md),
  [`goodman_gamma()`](https://YannickDiehl.github.io/mariposa/reference/phi.md).
- Test suite: removed a legacy tolerance registry that contradicted the
  Validation Charter and was no longer used by any test.

## mariposa 0.6.4

A quality release. Following an in-depth internal review of the entire
statistical codebase, this version sharpens the accuracy of several
statistics, makes the package behave more consistently across functions,
and adds a dedicated regression-test suite
(`tests/testthat/test-audit-regressions.R`) so these guarantees hold in
future releases. Some outputs change slightly as a result - in every
case toward the standard reference implementations.

### More accurate statistics

- Kendall’s tau-b significance test now agrees with
  [`stats::cor.test()`](https://rdrr.io/r/stats/cor.test.html) (and the
  SPSS formula) to machine precision, which is most noticeable for
  heavily tied data such as binary variables.
- The weighted Wilcoxon signed-rank test (and
  [`pairwise_wilcoxon()`](https://YannickDiehl.github.io/mariposa/reference/pairwise_wilcoxon.md))
  now uses frequency-expansion mid-ranks: with integer weights the
  statistic equals the expanded-data Wilcoxon exactly, and `weights = 1`
  reproduces the unweighted test. Displayed rank means in the weighted
  Kruskal-Wallis and Dunn tests follow the same convention.
- The Mann-Whitney asymptotic p-value now matches its reported Z (both
  follow the SPSS convention without continuity correction).
- Regression degrees of freedom are now derived from the fitted model
  terms, improving results for models with dummy-coded factors or
  interaction terms (weighted linear regression and the logistic omnibus
  test).
- The Kruskal-Wallis effect size is now correctly labelled: the returned
  field is `epsilon_squared` (previously named `eta_squared`).

### New and refined API

- [`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)
  gains SPSS-style collinearity diagnostics (Tolerance and VIF per model
  term), including a `collinearity` toggle in
  [`summary()`](https://rdrr.io/r/base/summary.html).
- [`phi()`](https://YannickDiehl.github.io/mariposa/reference/phi.md),
  [`cramers_v()`](https://YannickDiehl.github.io/mariposa/reference/phi.md),
  and
  [`goodman_gamma()`](https://YannickDiehl.github.io/mariposa/reference/phi.md)
  now return the requested effect size directly as a numeric value - the
  convenient behavior their names suggest. For the full test output, use
  [`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md).
- The weighted two-sample
  [`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)
  now honors `var.equal` for its primary result.
  [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)
  always reports both the classical and Welch results (like SPSS
  ONEWAY), so its `var.equal` argument is deprecated; `ss_type` in
  [`factorial_anova()`](https://YannickDiehl.github.io/mariposa/reference/factorial_anova.md)/[`ancova()`](https://YannickDiehl.github.io/mariposa/reference/ancova.md)
  is likewise deprecated in favor of the SPSS-standard Type III.

### More consistent behavior

- One package-wide weights policy: invalid (negative) weights are now
  rejected with a clear message at every entry point, instead of being
  handled differently depending on the function.
- The weighted median now always equals the weighted 50th percentile,
  and unweighted quantiles follow the SPSS convention (Type 6/HAVERAGE)
  throughout.
- [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
  header statistics use the same formulas as
  [`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
  and the `w_*` functions.
- Significance stars follow a single boundary convention everywhere,
  matching the printed legend.

### More robust in edge cases

- Correlation functions handle constant variables gracefully (NA instead
  of an error), `frequency(show.unused = TRUE)` works on variables
  tagged via
  [`set_na()`](https://YannickDiehl.github.io/mariposa/reference/set_na.md)/[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md),
  and `frequency(sort.frq =)` now sorts by frequency with a monotone
  cumulative-percent column.
- [`write_spss()`](https://YannickDiehl.github.io/mariposa/reference/write_spss.md)
  protects valid values when many missing-value codes must be
  consolidated into a range, and explains what it is doing.
- [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)
  surfaces separation and convergence warnings again; post-hoc tests
  report when a computation could not be carried out instead of skipping
  it silently.

### Housekeeping

- Internal code paths were consolidated (shared helpers for weighted
  mid-ranks and the Wilcoxon core) and a substantial amount of unused
  code was removed, making the codebase easier to maintain.
- Grouped single-variable `w_*` results print their statistics again.

## mariposa 0.6.3.2

### `rec()` reliably matches decimal single values

Single-value recode rules now match decimal codes (e.g. `"3.6=2"`) even
when the stored value carries floating-point representation error. The
single-value comparison was changed from exact numeric equality
(`x == value`) to a string comparison
(`as.character(x) == as.character(value)`), which rounds to 15
significant digits and thereby absorbs the error.

Reason: a value such as `0.1 + 0.2` is stored as `0.30000000000000004`,
so the previous exact `==` test silently failed to match a rule
`"0.3=..."`. This mirrors the behaviour of `sjmisc::rec()`, on which
[`rec()`](https://YannickDiehl.github.io/mariposa/reference/rec.md)’s
string syntax is modelled. Range rules were already robust (they use
`>=`/`<=`) and are unchanged.

## mariposa 0.6.3.1

### broom tidiers now work natively

Adds explicit `tidy()`, `glance()`, and `augment()` methods for both
`linear_regression` and `logistic_regression` results, registered via
the standard `s3_register()` pattern (broom in Suggests, no hard dep).

Reason: with `class(r) = c("linear_regression", "lm")`,
[`broom::tidy.lm()`](https://broom.tidymodels.org/reference/tidy.lm.html)
and
[`broom::glance.lm()`](https://broom.tidymodels.org/reference/glance.lm.html)
dispatched as expected, but internally called `summary(x)` — which
(because of our specialised
[`summary.linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/summary.linear_regression.md)
overriding `summary.lm`) returned the mariposa SPSS-style summary
instead of the lm summary broom needs. The visible failures:

- `broom::glance(r)` raised `object 'r.squared' not found` because
  mariposa’s summary stores it as `R_squared`.
- `broom::tidy(r, conf.int = TRUE)` returned only 4 columns (`term`,
  `estimate`, `conf.low`, `conf.high`) instead of the expected 6+
  (`term`, `estimate`, `std.error`, `statistic`, `p.value`, `conf.low`,
  `conf.high`).

The new methods strip our `linear_regression` / `logistic_regression`
class before delegating to
[`broom::tidy.lm`](https://broom.tidymodels.org/reference/tidy.lm.html)
/ `tidy.glm` etc., so the inner
[`summary()`](https://rdrr.io/r/base/summary.html) call dispatches to
`summary.lm` / `summary.glm` and broom receives its expected shape. The
user-facing `summary(r)` still returns mariposa’s SPSS-style output
(more specific method wins).

Edge cases stay consistent with the rest of the lm-generic surface:
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) /
`glance()` / `augment()` on a grouped or pairwise result raise an
actionable error pointing at `lapply(r$groups, ...)` or
`use = "listwise"`.

New tests in `test-broom-methods.R` cover all three tidiers for both
regression types, plus the grouped/pairwise error paths.

## mariposa 0.6.3

### Behavior Change — regression results inherit from `lm` / `glm`

[`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)
and
[`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)
results now ARE the fitted `lm` / `glm` object (with mariposa-specific
tables attached as additional slots), instead of wrapping it in
`$model`. All base-R and `broom` generics dispatch natively:

``` r

r <- linear_regression(survey_data, life_satisfaction ~ age + income)
coef(r)                                 # named numeric vector
predict(r, newdata = head(survey_data)) # works directly
anova(r)                                # sequential SS table
vcov(r); confint(r); residuals(r); fitted(r)
broom::tidy(r); broom::glance(r); broom::augment(r)
```

Class hierarchy is `c("linear_regression", "lm")` for linear and
`c("logistic_regression", "glm", "lm")` for logistic. `summary(r)` still
returns the SPSS-style mariposa summary (more specific method wins); for
the raw `lm`/`glm` summary call `stats::summary.lm(r)` /
`stats::summary.glm(r)`.

#### Slot renames (breaking)

Two slots collided with `lm`/`glm` conventions and were renamed:

| Before                   | After                                      |
|:-------------------------|:-------------------------------------------|
| `$coefficients` (tibble) | `$coef_table` (tibble)                     |
| `$anova` (tibble)        | `$anova_table` (tibble)                    |
| `$model` (lm/glm)        | the object IS the model — use `r` directly |

Migration:

- `r$coefficients` → `r$coef_table` (SPSS-style tibble) or `coef(r)`
  (named numeric vector).
- `r$anova` → `r$anova_table` (SPSS-style overall-model ANOVA tibble) or
  `anova(r)` (R’s per-term sequential SS table).
- `r$model |> predict(...)` → `predict(r, ...)` directly.
- `r$model |> broom::tidy()` → `broom::tidy(r)` directly.

#### Edge cases

- `use = "pairwise"`: no single fitted lm is available, so the result is
  a custom list with class `"linear_regression"` only.
  [`predict()`](https://rdrr.io/r/stats/predict.html)/[`anova()`](https://rdrr.io/r/stats/anova.html)
  etc. raise an informative error pointing at `use = "listwise"`.
- Grouped results (top-level): no single model.
  [`predict()`](https://rdrr.io/r/stats/predict.html)/[`anova()`](https://rdrr.io/r/stats/anova.html)
  raise an informative error pointing at
  `lapply(r$groups, predict, ...)`. Each `r$groups[[i]]` is itself an
  lm-inheriting object, so per-group generics work directly.

### Test Suite

- New test block in `test-linear-regression-spss-validation.R` verifies
  that [`coef()`](https://rdrr.io/r/stats/coef.html),
  [`predict()`](https://rdrr.io/r/stats/predict.html),
  [`anova()`](https://rdrr.io/r/stats/anova.html),
  [`vcov()`](https://rdrr.io/r/stats/vcov.html),
  [`confint()`](https://rdrr.io/r/stats/confint.html),
  [`residuals()`](https://rdrr.io/r/stats/residuals.html),
  [`fitted()`](https://rdrr.io/r/stats/fitted.values.html),
  [`formula()`](https://rdrr.io/r/stats/formula.html),
  [`nobs()`](https://rdrr.io/r/stats/nobs.html),
  [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) all
  dispatch natively, plus the grouped/pairwise error paths.
- 1184/1184 tests pass; R CMD check on built tarball: Status OK.

## mariposa 0.6.2

### Behavior Change

#### `linear_regression()` and `logistic_regression()`: factor predictor handling

Both regression functions now expose a `factors` argument controlling
how factor predictors enter the model. The new default
`factors = "dummy"` matches base R
[`lm()`](https://rdrr.io/r/stats/lm.html) /
[`glm()`](https://rdrr.io/r/stats/glm.html): a factor with `L` levels
expands into `L - 1` dummy contrasts via
[`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html).
Previous versions silently coerced factor levels to integer codes (SPSS
ordinal-as-scale default) with no warning, which surprised users who
relied on standard R semantics.

To restore the previous SPSS-style behavior, pass `factors = "numeric"`
explicitly. That mode emits a one-line
[`cli::cli_inform()`](https://cli.r-lib.org/reference/cli_abort.html)
listing the coerced variables for transparency. The “numeric” mode is
required to reproduce SPSS `REGRESSION` / `LOGISTIC REGRESSION` output
when factor predictors carry ordered meaning (e.g., a 4-level education
variable treated as 1–4 ordinal scale).

Behavioral consequences:

- A model with a 3-level factor predictor that previously returned one
  coefficient row now returns two dummy-contrast rows under the new
  default.
- For pairwise missing handling (`use = "pairwise"`), factor predictors
  are not supported with `factors = "dummy"`; the function now errors
  with an actionable message pointing to either `factors = "numeric"` or
  `use = "listwise"`.

Migration: scripts that depend on the old SPSS-style coercion should set
`factors = "numeric"` at the call site. The `cli_inform()` message can
be silenced with
[`suppressMessages()`](https://rdrr.io/r/base/message.html) if desired.

### Source-Code Fixes (weighted regression)

Two more functions joined the Charter §5.1 audit list (the “unrounded
`sum(w)`” weighted-statistics convention previously applied to `t_test`,
`oneway_anova`, and `levene_test`):

- [`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md):
  weighted variance, SE, df, F, R², and adjusted-R² now use the
  unrounded `sum(weights)` throughout. Earlier versions used
  `n_effective <- round(sum(w))` in df and MS calculations, producing
  systematic drift from SPSS REGRESSION (off by ~0.001 on F, ~0.01 on
  adj-R² for typical weights). The displayed N is still `round(sum(w))`.
- [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md):
  pseudo-R² formulas (Cox & Snell, Nagelkerke, McFadden) now use the
  unrounded `sum(weights)` in the exponential denominator. The displayed
  N and rounded classification counts remain integers.

These are bug fixes; weighted results may shift slightly toward closer
agreement with SPSS v29.

### Other Fixes

- `summary.linear_regression(descriptives = TRUE)` now actually prints
  the Descriptive Statistics table (Variable, Mean, SD, N). Previously
  the parameter was accepted but documented as “Reserved for future use”
  and produced no output.
- The compact
  [`print.linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/print.linear_regression.md)
  no longer crashes on weighted models with non-integer df: the
  F-statistic line now rounds df for display before formatting with
  `%d`.
- Roxygen examples for `summary.linear_regression`
  (`collinearity = FALSE`) and `summary.logistic_regression`
  (`classification_table = FALSE`) referenced parameters that do not
  exist; corrected to `descriptives = FALSE` and
  `classification = FALSE` respectively.

### Test Suite

The `linear_regression` SPSS validation test suite expanded from 1
scenario (unweighted bivariate) to 6 scenarios covering all four Charter
§8 quadrants — Tests 1a, 1c, 2a, 2c, 3a, and 4a from
`tests/spss_reference/outputs/linear_regression_output.txt`. The
weighted scenarios (2a, 2c, 4a) verify the Charter §5.1 fix above. New
behavioral tests cover the `factors` argument (dummy expansion, numeric
coercion, pairwise + dummy + factor error path). 222/222 assertions
pass.

## mariposa 0.6.1

### Validation

Substantial hardening of the SPSS-compatibility test suite. All 29 SPSS-
validation test files were rewritten under a new Validation Charter (see
[`vignette("spss-compatibility")`](https://YannickDiehl.github.io/mariposa/articles/spss-compatibility.md))
that defines tolerance tiers (Spec / Display / Exception / Internal),
forbids inline tolerance literals, NA placeholders, and
`expect_true(TRUE)` reporting blocks, and requires citation comments
linking every reference value to its source line in
`tests/spss_reference/outputs/`.

- 1832+ passing assertions across all 29 validation files, 0 failures.
- New `tests/testthat/helper-validation-tolerances.R` provides
  `assert_spss()` and `tol()` helpers with explicit tier semantics.
- New `tests/testthat/test-validation-discipline.R` meta-test lints
  validation files for Charter-forbidden patterns.
- New `vignettes/spss-compatibility.Rmd` reports per-function validation
  status, auto-generated from the test suite.
- New CI workflow `.github/workflows/strict-validation.yaml` runs the
  full suite in strict-discipline mode on release tags and weekly.

### Source-Code Fixes (weighted statistics)

Three weighted statistical functions were corrected to use unrounded
`sum(w)` per SPSS frequency-weights convention. Earlier versions rounded
too early and produced systematic drift from SPSS in weighted scenarios.

- [`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md):
  weighted variance, SE, and df calculations now use unrounded `sum(w)`
  (one-sample and two-sample paths). Welch- Satterthwaite df now derived
  from unrounded per-group weighted N.
- [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md):
  weighted variance divisor is now `(sum(w) - 1)` (sample formula, not
  population). Weighted SE uses `sqrt(sum(w))`, not `sqrt(physical n)`.
  Weighted CI t-critical-value uses `df = sum(w) - 1`, not Kish
  design-effective N. `df_within` now uses `floor(sum(w)) - k` (SPSS
  ONEWAY-specific convention).
- [`levene_test()`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md):
  weighted Levene df now uses unrounded `sum(w) - k` (SPSS T-TEST family
  convention).

These changes are bug fixes and may slightly shift weighted-scenario
results in user code. Differences are small (typically \< 0.01 on F or
t) and bring mariposa into closer agreement with SPSS v29.

### SPSS-Compatibility Vignette

[`vignette("spss-compatibility")`](https://YannickDiehl.github.io/mariposa/articles/spss-compatibility.md)
documents the per-function validation status, the four tolerance tiers,
and the SPSS-procedure-specific WEIGHT BY conventions discovered during
the migration:

- T-TEST family: unrounded `sum(w)`
- ONEWAY: `floor(sum(w))`
- UNIANOVA: Type III SS
- NPAR TESTS: WEIGHT BY effectively ignored
- NONPAR CORR (Spearman, Kendall): WEIGHT BY effectively ignored
- CORRELATIONS (Pearson): WEIGHT BY honored
- CROSSTABS, FREQUENCIES, RELIABILITY, FACTOR, REGRESSION: WEIGHT BY
  honored

### DESCRIPTION

- `Title` shortened to “SPSS-Compatible Statistical Tools for Survey
  Data” (CRAN soft-limit compliance).
- Suggests cleanup: removed `PMCMRplus` and `survey` (no longer needed).

### Audit-Driven Math Fixes (post-Phase-1)

A second audit pass identified additional math defects and test fudges,
all corrected in this release:

- [`dunn_test()`](https://YannickDiehl.github.io/mariposa/reference/dunn_test.md):
  SE now includes the Dunn (1964) / Conover (1999) tie correction.
  Previous versions systematically under-estimated `|Z|` on tied data
  (e.g., Likert scales). Baselines regenerated from
  `PMCMRplus::kwAllPairsDunnTest` (exact match to 4 decimals).
- [`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md):
  weighted branch now applies the tie correction consistently with
  [`stats::friedman.test`](https://rdrr.io/r/stats/friedman.test.html)
  (unweighted branch). The inconsistency caused weighted chi-squared
  values to be too low for tied data.
- [`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md):
  weighted skewness and kurtosis now delegate to `.calc_skewness()` /
  `.calc_kurtosis()` in `helpers.R` (Joanes-Gill Type-2 with `Σw`
  substitution), matching
  [`w_skew()`](https://YannickDiehl.github.io/mariposa/reference/w_skew.md)
  /
  [`w_kurtosis()`](https://YannickDiehl.github.io/mariposa/reference/w_kurtosis.md)
  and SPSS FREQUENCIES exactly. The previous duplicate implementation
  used a simple weighted moment without bias correction.
- `.w_quantile()`: weighted quantiles now use Type-6 (HAVERAGE) linear
  interpolation between cumulative-weight crossings — matches SPSS
  FREQUENCIES /PERCENTILES. Unweighted quantiles also switched from R
  default `type = 7` to SPSS-compatible `type = 6`.

### Documentation Honesty

Several SPSS-compatibility claims were narrowed to reflect what the code
actually does:

- Source comments and test-file headers for the weighted paths of
  [`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md),
  [`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/reference/wilcoxon_test.md),
  and
  [`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md)
  corrected from “design-based” / “Lumley-Scott” to “frequency-weighted
  approximation”. Only
  [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md)
  is a genuine Lumley & Scott (2013) implementation; the others
  substitute `sum(w)` for `n` in the standard variance formula.
- [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md)
  test now includes a permanent cross-check against
  [`survey::svyranktest()`](https://rdrr.io/pkg/survey/man/svyranktest.html)
  (skipped when survey is not installed).
- [`spearman_rho()`](https://YannickDiehl.github.io/mariposa/reference/spearman_rho.md):
  `weights` parameter docstring rewritten to disclose that weights are
  used only for case filtering (per SPSS NONPAR CORR convention), not in
  the rank correlation itself.
- [`pearson_cor()`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md):
  docstring now warns that the weighted-df convention (`n = sum(w)`)
  gives spuriously narrow CIs for raw expansion weights; users with such
  weights should normalize first.
- [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md):
  test file replaced with property-based assertions (Wald formula, Sig
  from chi-sq, exp(B) vs independent 2x2 odds ratio, Cox &
  Snell/Nagelkerke/McFadden from textbook formulas, Omnibus from
  likelihood ratio). No longer a tautological glm-vs-glm
  self-comparison.

### Code Smell Cleanup

- [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md):
  removed dead-code overwrite of `grand_mean_welch` in the weighted
  Welch path.
- [`levene_test()`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md):
  stale comment claiming `df2 = floor(sum(w)) - k` corrected — the code
  uses unrounded `sum(w) - k` (T-TEST family convention).

## mariposa 0.6.0

### New Functions — Label Management

This release adds 10 label management functions for working with
labelled survey data (inspired by `sjlabelled`, consolidated into a
clean, consistent API), plus data transformation, row operations, and
data exploration functions.

#### Variable & Value Labels

- New
  [`var_label()`](https://YannickDiehl.github.io/mariposa/reference/var_label.md):
  dual-mode function for getting and setting variable labels.
  `var_label(data)` returns all variable labels as a named character
  vector; `var_label(data, x = "Age", y = "Gender")` sets labels for
  specific columns. Supports tidyselect for column selection when
  getting labels.

- New
  [`val_labels()`](https://YannickDiehl.github.io/mariposa/reference/val_labels.md):
  dual-mode function for getting and setting value labels.
  `val_labels(data)` returns all value labels as a named list;
  `val_labels(data, x = c("Low" = 1, "High" = 2))` sets labels. Use
  `.add = TRUE` to extend existing labels without replacing them.

- New
  [`copy_labels()`](https://YannickDiehl.github.io/mariposa/reference/copy_labels.md):
  copies all label attributes (variable labels, value labels, class,
  tagged NA metadata) from a source data frame to matching columns in
  the target. Essential for preserving labels after `dplyr` operations
  that strip attributes.

- New
  [`drop_labels()`](https://YannickDiehl.github.io/mariposa/reference/drop_labels.md):
  removes value labels for values that do not actually occur in the
  data. Use `drop.na = TRUE` to also remove labels for tagged NA values.

#### Type Conversions

- New
  [`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md):
  converts `haven_labelled` vectors to factors, using value labels as
  factor levels. Supports `ordered`, `drop.na`, `drop.unused`, and
  `add.non.labelled` options. Factor levels are ordered by their
  original numeric codes (not alphabetically).

- New
  [`to_character()`](https://YannickDiehl.github.io/mariposa/reference/to_character.md):
  converts `haven_labelled` vectors to character, replacing numeric
  codes with their label text.

- New
  [`to_numeric()`](https://YannickDiehl.github.io/mariposa/reference/to_numeric.md):
  converts factors or labelled vectors to numeric. When
  `use.labels = TRUE`, uses value labels if they are numeric; otherwise
  assigns sequential integers (controlled by `start.at`).

- New
  [`to_labelled()`](https://YannickDiehl.github.io/mariposa/reference/to_labelled.md):
  converts factors, character, or numeric vectors to `haven_labelled`
  with proper value labels. Factor levels become value labels
  automatically.

#### Missing Value Management

- New
  [`set_na()`](https://YannickDiehl.github.io/mariposa/reference/set_na.md):
  declares specific numeric values as missing (NA or tagged NA).
  Supports unnamed values (applied to all numeric columns) and named
  pairs for per-variable control (e.g.,
  `set_na(data, income = c(-9, -8))`). With `tag = TRUE` (default),
  creates tagged NAs that integrate with
  [`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md),
  [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md),
  and
  [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md).
  Can be called incrementally to add new missing value codes.

- New
  [`unlabel()`](https://YannickDiehl.github.io/mariposa/reference/unlabel.md):
  strips all label metadata from variables, converting `haven_labelled`
  vectors to plain base R types. Removes variable labels, value labels,
  tagged NA metadata, and format attributes. Tagged NAs become regular
  NA. Supports tidyselect for selective column unlabelling.

### New Functions — Data Transformation

- New
  [`rec()`](https://YannickDiehl.github.io/mariposa/reference/rec.md):
  flexible recoding with string syntax (e.g.,
  `rec(data, x, rec = "1:2=1 [Low]; 3:5=2 [High]")`). Supports value
  ranges, `min`/`max` keywords, `copy` for unchanged values, and
  automatic value label generation from bracket syntax. Works with
  numeric, character, and labelled vectors.

- New
  [`to_dummy()`](https://YannickDiehl.github.io/mariposa/reference/to_dummy.md):
  creates dummy (indicator) variables from categorical or labelled
  vectors. Generates one 0/1 column per unique value with informative
  column names. Supports tidyselect for multi-variable dummy coding and
  `suffix = "label"` to use value labels in column names.

- New
  [`std()`](https://YannickDiehl.github.io/mariposa/reference/std.md):
  z-standardization with four methods (`"sd"`, `"2sd"`, `"mad"`,
  `"gmd"`). Supports survey weights, grouped standardization via
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),
  and `robust = TRUE` for median/MAD-based standardization.

- New
  [`center()`](https://YannickDiehl.github.io/mariposa/reference/center.md):
  mean-centering (grand-mean or group-mean). Supports survey weights and
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  for group-mean centering. Returns centered values with the centering
  value stored as an attribute.

### New Functions — Row Operations

- New
  [`row_means()`](https://YannickDiehl.github.io/mariposa/reference/row_means.md):
  computes row-wise means across selected columns, with `min_valid`
  parameter matching SPSS `MEAN.x()` syntax. Designed for use inside
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).
  Replaces the deprecated `scale_index()`.

- New
  [`row_sums()`](https://YannickDiehl.github.io/mariposa/reference/row_sums.md):
  computes row-wise sums across selected columns, with `min_valid`
  parameter for minimum valid (non-NA) values.

- New
  [`row_count()`](https://YannickDiehl.github.io/mariposa/reference/row_count.md):
  counts occurrences of specific values per row. Useful for counting
  endorsements in multi-item scales (e.g., how many items a respondent
  agreed with).

### New Functions — Data Exploration

- New
  [`find_var()`](https://YannickDiehl.github.io/mariposa/reference/find_var.md):
  searches variables by name or label using regular expressions. Returns
  matching variable names with their labels. Useful for exploring large
  survey datasets with many variables.

### Breaking Changes

- `scale_index()` has been removed and replaced by
  [`row_means()`](https://YannickDiehl.github.io/mariposa/reference/row_means.md),
  which provides the same functionality with a clearer name. Update
  existing code: `scale_index(data, x, y, z)` →
  `row_means(data, x, y, z)`.

## mariposa 0.5.6

### New Functions

- New
  [`write_spss()`](https://YannickDiehl.github.io/mariposa/reference/write_spss.md)
  function: exports data frames to SPSS `.sav` format with full tagged
  NA roundtripping. Tagged NAs are converted back to SPSS user-defined
  missing values, enabling lossless roundtrips via
  [`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md)
  -\> processing -\>
  [`write_spss()`](https://YannickDiehl.github.io/mariposa/reference/write_spss.md).
  Supports byte, none, and zsav compression.

- New
  [`write_stata()`](https://YannickDiehl.github.io/mariposa/reference/write_stata.md)
  function: exports data frames to Stata `.dta` format. Tagged NAs from
  any source format are written as Stata extended missing values (`.a`
  through `.z`). Supports Stata versions 8-15.

- New
  [`write_xpt()`](https://YannickDiehl.github.io/mariposa/reference/write_xpt.md)
  function: exports data frames to SAS transport `.xpt` format. Tagged
  NAs are written as SAS special missing values (`.A` through `.Z`,
  `._`). Supports transport versions 5 and 8.

### Enhancements

- mariposa now provides a unified data import/export platform:
  - **Import**:
    [`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md),
    [`read_por()`](https://YannickDiehl.github.io/mariposa/reference/read_por.md),
    [`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md),
    [`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md),
    [`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md),
    [`read_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/read_xlsx.md)
  - **Export**:
    [`write_spss()`](https://YannickDiehl.github.io/mariposa/reference/write_spss.md),
    [`write_stata()`](https://YannickDiehl.github.io/mariposa/reference/write_stata.md),
    [`write_xpt()`](https://YannickDiehl.github.io/mariposa/reference/write_xpt.md),
    [`write_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/write_xlsx.md)
- Cross-format export is supported: data imported from one format can be
  exported to another (e.g., SPSS to Stata) with automatic missing value
  type conversion.

## mariposa 0.5.5

### New Functions

- New
  [`read_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/read_xlsx.md)
  function: reads Excel (`.xlsx`) files with automatic label
  reconstruction. When reading back files created by
  [`write_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/write_xlsx.md),
  variable labels, value labels, and tagged NA metadata are fully
  restored – enabling lossless roundtripping of labelled survey data
  through Excel.
  - Auto-detects mariposa export format (data frame, list, codebook)
  - Reconstructs `haven_labelled` columns, factor levels, and variable
    labels
  - Restores tagged NAs with `na_tag_map` from missing codes in the data
  - Works as a plain Excel reader for non-mariposa files
- New
  [`write_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/write_xlsx.md)
  generic: exports data frames, codebooks, and named lists to Excel
  (`.xlsx`) with full support for variable labels, value labels, and
  tagged NA metadata. Uses `openxlsx2` as an optional dependency.
  - `write_xlsx(data, "file.xlsx")` – data + “Labels” reference sheet
    with variable labels, value labels, and missing value codes
  - `codebook(data) |> write_xlsx("codebook.xlsx")` – structured
    codebook workbook with Overview, Codebook, and optional per-variable
    frequency sheets (`frequencies = TRUE`)
  - `write_xlsx(list(a = df1, b = df2), "multi.xlsx")` – multi-sheet
    export where each named list element becomes a sheet

### Enhancements

- [`write_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/write_xlsx.md)
  now preserves tagged NA codes (-9, -11, etc.) as visible values in the
  data sheet instead of empty cells, enabling perfect roundtripping with
  [`read_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/read_xlsx.md).
  System NAs remain as empty cells.
- The “Labels” sheet now includes a `Column_Type` column
  (`haven_labelled` or `factor`) so
  [`read_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/read_xlsx.md)
  can deterministically reconstruct column types.

### Dependencies

- Added `openxlsx2` as a suggested dependency for Excel import/export.

## mariposa 0.5.4

### New Functions

- New
  [`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md)
  function: reads Stata `.dta` files and annotates native extended
  missing values (`.a` through `.z`) for use with mariposa’s tagged NA
  system. Stata tagged NAs are preserved automatically by haven;
  [`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md)
  adds the `na_tag_map` attribute for seamless integration with
  [`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md),
  [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md),
  and
  [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md).

- New
  [`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md)
  function: reads SAS `.sas7bdat` files with optional catalog file
  (`.sas7bcat`) for value labels. Annotates SAS special missing values
  (`.A` through `.Z` and `._`) for tagged NA integration.

- New
  [`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md)
  function: reads SAS transport files (`.xpt`) with tagged missing value
  support. Transport files are the FDA-approved, platform-independent
  SAS data format.

- New
  [`read_por()`](https://YannickDiehl.github.io/mariposa/reference/read_por.md)
  function: reads SPSS portable `.por` files with the same tagged NA
  support as
  [`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md).
  Shares the SPSS missing value conversion logic internally.

### Breaking Changes

- [`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md)
  column `spss_code` has been renamed to `code` to reflect multi-format
  support. The column now contains character values: numeric SPSS codes
  (e.g., `"-9"`) or native format codes (e.g., `".a"` for Stata, `".A"`
  for SAS).

### Improvements

- [`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md),
  [`untag_na()`](https://YannickDiehl.github.io/mariposa/reference/untag_na.md),
  and
  [`strip_tags()`](https://YannickDiehl.github.io/mariposa/reference/strip_tags.md)
  now work universally with data from all supported formats (SPSS,
  Stata, SAS).

- [`untag_na()`](https://YannickDiehl.github.io/mariposa/reference/untag_na.md)
  is now format-aware: for Stata and SAS data (where tagged NAs are the
  native representation with no numeric codes to recover), it warns and
  falls back to
  [`strip_tags()`](https://YannickDiehl.github.io/mariposa/reference/strip_tags.md)
  behavior.

- [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
  and
  [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)
  automatically display format-appropriate missing value codes (e.g.,
  `-9` for SPSS, `.a` for Stata, `.A` for SAS).

## mariposa 0.5.3

### New Functions

- New
  [`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md)
  function: reads SPSS `.sav` files and preserves user-defined missing
  values as tagged NAs instead of converting them to regular `NA`. This
  allows distinguishing between different types of missing data (e.g.,
  “no answer”, “not applicable”, “refused”) while still treating them as
  `NA` in standard R operations. Fixes the
  `sjlabelled::read_spss(tag.na=TRUE)` crash on large datasets (e.g.,
  ALLBUS) caused by out-of-bounds `letters[]` indexing.

- New
  [`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md)
  function: shows a breakdown of the different types of missing values
  in a tagged NA variable, with counts, original SPSS codes, and value
  labels.

- New
  [`untag_na()`](https://YannickDiehl.github.io/mariposa/reference/untag_na.md)
  function: converts tagged NAs back to their original SPSS missing
  value codes (e.g., -9, -8, -42).

- New
  [`strip_tags()`](https://YannickDiehl.github.io/mariposa/reference/strip_tags.md)
  function: converts all tagged NAs to regular (untagged) `NA` values,
  producing the same result as reading with
  [`haven::read_sav()`](https://haven.tidyverse.org/reference/read_spss.html)
  directly.

### Improvements

- [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
  now displays tagged NAs individually when data was imported with
  [`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md).
  Each missing value type is shown as a separate row with its original
  SPSS code and label, followed by a “Total Valid” and “Total Missing”
  summary row.

- [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
  with `show.unused = TRUE` correctly handles tagged NA labels (no
  longer shows them as unused with freq=0).

## mariposa 0.5.2.2

### Convenience

- New
  [`fre()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
  shorthand alias for
  [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md).
  Both functions are identical;
  [`fre()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
  simply provides a quicker way to call frequency analysis.
  [`?fre`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
  shows the same help page as
  [`?frequency`](https://YannickDiehl.github.io/mariposa/reference/frequency.md).

## mariposa 0.5.2

### New Features

- New
  [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)
  function: generates an interactive HTML data dictionary displayed in
  the RStudio Viewer pane. Shows variable ID, name, type, label,
  empirical values, value labels, and frequencies in a clean, scrollable
  table. Inspired by sjPlot’s `view_df()` but built natively with
  `htmltools`.

- HTML codebook features a subtle-accent design: dark header,
  alternating row stripes, monospace type badges, and per-value
  frequency counts displayed as vertical lists aligned across columns.

- Console [`print()`](https://rdrr.io/r/base/print.html) shows a minimal
  metadata overview (variable count, observations, types). Full details
  are reserved for the HTML viewer.

- [`summary()`](https://rdrr.io/r/base/summary.html) method provides a
  detailed text-based fallback with toggleable sections (`overview`,
  `variable_details`, `value_labels`).

- Supports tidyselect variable selection, optional survey weights for
  weighted frequencies, and `sort.by.name` ordering.

### Dependencies

- Added `htmltools` as an imported dependency for HTML codebook
  generation.

## mariposa 0.5.1

### Three-Layer Output System

- All 13 analysis functions now support
  [`summary()`](https://rdrr.io/r/base/summary.html) for detailed
  SPSS-style output with toggleable sections. The three-layer pattern
  works as follows:

  - [`print()`](https://rdrr.io/r/base/print.html) — compact one-line
    overview (default when typing the object name)
  - [`summary()`](https://rdrr.io/r/base/summary.html) — builds a
    detailed summary object with boolean section toggles
  - `print.summary()` — renders the full verbose output with all
    requested sections

- Supported functions:
  [`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md),
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
  [`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md),
  [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md).

- Each [`summary()`](https://rdrr.io/r/base/summary.html) method accepts
  boolean parameters to control which output sections are displayed
  (e.g., `summary(result, effect_sizes = FALSE)` or
  `summary(result, descriptives = FALSE)`).

### Internal Helpers

- Added `build_summary_object()` and `format_p_compact()` in
  `R/summary_helpers.R` as shared infrastructure for all summary
  methods.

### Documentation

- Complete Roxygen2 documentation for all 39 S3 methods (13 print + 13
  summary

  - 13 print.summary), each with `@description`, `@param`, `@return`,
    `@examples`, and `@seealso`.

- All 13 main function `@examples` now demonstrate the three-layer
  output pattern (`result`, `summary(result)`,
  `summary(result, toggle = FALSE)`).

- Added
  [`print.reliability()`](https://YannickDiehl.github.io/mariposa/reference/print.reliability.md)
  and
  [`print.efa()`](https://YannickDiehl.github.io/mariposa/reference/print.efa.md)
  documentation (previously undocumented).

### Bug Fixes

- Fixed
  [`print.chi_square()`](https://YannickDiehl.github.io/mariposa/reference/print.chi_square.md)
  Roxygen2 tag (`@keywords internal` replaced with correct
  `@method print chi_square`).

- Fixed example syntax errors in
  [`ancova()`](https://YannickDiehl.github.io/mariposa/reference/ancova.md)
  and
  [`factorial_anova()`](https://YannickDiehl.github.io/mariposa/reference/factorial_anova.md)
  (formula syntax replaced with correct `dv`/`between` interface).

- Fixed incorrect variable name `education_level` in examples (corrected
  to `education`).

### Tests

- Added `test-summary-methods.R` with tests for all 13 summary methods.

- Updated `test-print-methods.R` to reflect the new three-layer
  structure.

------------------------------------------------------------------------

## mariposa 0.5.0

### New Functions

- Added
  [`factorial_anova()`](https://YannickDiehl.github.io/mariposa/reference/factorial_anova.md)
  for multi-factor between-subjects ANOVA (up to 3 factors) with Type
  III Sum of Squares matching SPSS UNIANOVA. Includes main effects, all
  interaction terms, partial eta squared, R-squared, and Levene’s test
  for homogeneity of variance. Full survey weight support via WLS
  (matching SPSS /REGWGT). Integrates with existing
  [`tukey_test()`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md),
  [`scheffe_test()`](https://YannickDiehl.github.io/mariposa/reference/scheffe_test.md),
  and
  [`levene_test()`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md)
  S3 generics.

- Added
  [`ancova()`](https://YannickDiehl.github.io/mariposa/reference/ancova.md)
  for Analysis of Covariance — tests group differences after controlling
  for continuous covariates. Matches SPSS UNIANOVA with the WITH
  keyword. Provides ANOVA table, parameter estimates (B, SE, t, p,
  partial eta squared), estimated marginal means (adjusted for
  covariates), and Levene’s test. Supports up to 3 factors and multiple
  covariates with full survey weight support.

### SPSS Validation

- Added 612 SPSS validation tests for
  [`factorial_anova()`](https://YannickDiehl.github.io/mariposa/reference/factorial_anova.md)
  across 9 scenarios: unweighted (2-factor, 3-factor, 2-factor with
  missing data), weighted (2-factor, 3-factor), grouped (2-factor,
  3-factor), and weighted+grouped (2-factor, 3-factor).

- Added 579 SPSS validation tests for
  [`ancova()`](https://YannickDiehl.github.io/mariposa/reference/ancova.md)
  across 11 scenarios: one-way ANCOVA, two-way ANCOVA, weighted,
  grouped, weighted+grouped, multiple covariates, and single factor with
  single covariate.

- Total test suite: 4,986 tests passing (0 failures).

### Technical Details

- Type III Sum of Squares computed via `contr.sum` contrasts and
  [`stats::drop1()`](https://rdrr.io/r/stats/add1.html) — no dependency
  on the `car` package.

- Weighted analyses use WLS
  ([`stats::lm()`](https://rdrr.io/r/stats/lm.html) with weights),
  matching SPSS’s /REGWGT subcommand behavior exactly.

- Weighted Levene’s test uses the SPSS /REGWGT algorithm:
  `z_i = sqrt(w_i) * |y_i - weighted_cell_mean_i|` followed by
  unweighted ANOVA.

- Corrected Model SS computed as `Corrected Total - Error` (not sum of
  Type III SS) to correctly handle unbalanced designs.

------------------------------------------------------------------------

## mariposa 0.4.0

### New Functions

- Added
  [`fisher_test()`](https://YannickDiehl.github.io/mariposa/reference/fisher_test.md)
  for Fisher’s exact test of independence in contingency tables.
  Recommended when sample sizes are small or expected cell frequencies
  fall below 5 (where chi-square approximation becomes unreliable).
  Supports survey weights, multi-variable analysis, and
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).

- Added
  [`chisq_gof()`](https://YannickDiehl.github.io/mariposa/reference/chisq_gof.md)
  for chi-square goodness-of-fit testing. Tests whether the observed
  frequency distribution of a categorical variable matches an expected
  distribution (default: equal proportions). Supports custom expected
  proportions, residual analysis, survey weights, and multi-variable
  analysis.

- Added
  [`mcnemar_test()`](https://YannickDiehl.github.io/mariposa/reference/mcnemar_test.md)
  for testing changes in paired proportions between two dichotomous
  measurements (e.g., before/after designs). Provides both asymptotic
  and exact binomial p-values, 2×2 contingency tables, and continuity
  correction. Supports survey weights.

- Added
  [`dunn_test()`](https://YannickDiehl.github.io/mariposa/reference/dunn_test.md)
  as an S3 generic for Dunn’s post-hoc pairwise comparisons following a
  significant Kruskal-Wallis test. Identifies which specific group pairs
  differ using rank-based Z-statistics with adjustable p-value
  correction (Bonferroni, Holm, BH, etc.). Dispatches on
  `kruskal_wallis` result objects.

- Added
  [`pairwise_wilcoxon()`](https://YannickDiehl.github.io/mariposa/reference/pairwise_wilcoxon.md)
  as an S3 generic for pairwise Wilcoxon signed-rank post-hoc
  comparisons following a significant Friedman test. Identifies which
  measurement pairs differ with adjustable p-value correction.
  Dispatches on `friedman_test` result objects.

### SPSS Validation

- Added SPSS validation tests for all 5 new functions across
  weighted/unweighted and grouped/ungrouped scenarios.

### Improvements

- Extended post-hoc analysis framework:
  [`dunn_test()`](https://YannickDiehl.github.io/mariposa/reference/dunn_test.md)
  and
  [`pairwise_wilcoxon()`](https://YannickDiehl.github.io/mariposa/reference/pairwise_wilcoxon.md)
  join
  [`tukey_test()`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md),
  [`scheffe_test()`](https://YannickDiehl.github.io/mariposa/reference/scheffe_test.md),
  and
  [`levene_test()`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md)
  as S3 generics that dispatch on their parent test result objects.

------------------------------------------------------------------------

## mariposa 0.3.1

### Enhancements

- [`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md)
  now supports Maximum Likelihood (ML) extraction via
  `extraction = "ml"`. ML extraction provides a goodness-of-fit
  chi-square test, initial communalities as SMC (squared multiple
  correlations), and uniquenesses. Uses
  [`stats::factanal()`](https://rdrr.io/r/stats/factanal.html) with
  correlation matrix input for seamless survey weight support.

- [`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md)
  now supports Promax rotation via `rotation = "promax"`. Like Oblimin,
  Promax is an oblique rotation that produces Pattern Matrix, Structure
  Matrix, and Factor Correlation Matrix. Uses
  [`stats::promax()`](https://rdrr.io/r/stats/varimax.html) (base R, no
  new dependency).

- Internal refactoring of
  [`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md):
  extraction logic separated into `.efa_extract_pca()` and
  `.efa_extract_ml()` for cleaner architecture and easier extension with
  future extraction methods (PAF planned).

------------------------------------------------------------------------

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

- Added `scale_index()` for creating mean indices across survey items,
  with `min_valid` parameter matching SPSS `MEAN.x()` syntax. Designed
  for use inside
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
  [`goodman_gamma()`](https://YannickDiehl.github.io/mariposa/reference/phi.md)
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
  functions (`.process_variables()`, `.process_weights()`,
  `.effective_n()`). These are now defined once in `helpers.R` and
  shared across all functions.

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
