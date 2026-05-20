# SPSS Compatibility Status

This vignette reports the SPSS-compatibility status of every statistical
function in **mariposa**. It is auto-generated from the test suite and
the validation exception registry.

**Generated:** 2026-05-19

## Summary

- Functions in SPSS-validation scope: **50**
- With validation tests in place: **39**
- Validation gaps (no test file yet): **11**
- Active Tier-3 algorithmic exceptions: **0**

## Tier Definitions

Every numerical comparison between an R-side value and an SPSS reference
falls into one of four tiers (Charter §4):

- **Spec** — same closed-form formula; tolerance `±1e-5` (statistics) or
  `±1e-4` (p-values)
- **Display** — SPSS rounds for print; tolerance is half a unit of the
  last printed decimal
- **Exception** — documented algorithmic difference; see
  `VALIDATION_EXCEPTIONS.md`
- **Internal** — statistic has no SPSS equivalent; verified via snapshot
  tests only

## Per-Function Status

The columns show how many `assert_spss()` calls per tier the validation
file contains. “Total” is the total number of charter-compliant
assertions.

| Function              | Status        | Spec | Display | Exception | Total |
|-----------------------|---------------|-----:|--------:|----------:|------:|
| `ancova`              | compliant     |    1 |       5 |         1 |     5 |
| `binomial_test`       | compliant     |    1 |       3 |         1 |     3 |
| `center`              | not validated |    — |       — |         — |     — |
| `chi_square`          | compliant     |    1 |       6 |         1 |     6 |
| `chisq_gof`           | compliant     |    1 |       2 |         1 |     2 |
| `codebook`            | not validated |    — |       — |         — |     — |
| `cramers_v`           | not validated |    — |       — |         — |     — |
| `crosstab`            | compliant     |    1 |       2 |         1 |     2 |
| `describe`            | compliant     |    1 |       3 |         1 |     3 |
| `dunn_test`           | compliant     |    1 |       1 |         1 |     1 |
| `efa`                 | compliant     |    1 |       7 |         1 |     7 |
| `factorial_anova`     | compliant     |    1 |       5 |         1 |     5 |
| `fisher_test`         | compliant     |    1 |       3 |         1 |     3 |
| `frequency`           | compliant     |    1 |       5 |         1 |     5 |
| `friedman_test`       | compliant     |    1 |       3 |         1 |     3 |
| `goodman_gamma`       | not validated |    — |       — |         — |     — |
| `kendall_tau`         | compliant     |    1 |       6 |         1 |     6 |
| `kruskal_wallis`      | compliant     |    1 |       3 |         1 |     3 |
| `levene_test`         | compliant     |    1 |       2 |         1 |     2 |
| `linear_regression`   | compliant     |    1 |      15 |         1 |    15 |
| `logistic_regression` | compliant     |    1 |       4 |         1 |     4 |
| `mann_whitney`        | compliant     |    1 |       6 |         1 |     6 |
| `mcnemar_test`        | compliant     |    1 |       1 |         1 |     1 |
| `oneway_anova`        | compliant     |    1 |      16 |         1 |    16 |
| `pairwise_wilcoxon`   | compliant     |    1 |       1 |         1 |     1 |
| `pearson_cor`         | compliant     |    1 |       5 |         1 |     5 |
| `phi`                 | not validated |    — |       — |         — |     — |
| `pomps`               | not validated |    — |       — |         — |     — |
| `rec`                 | not validated |    — |       — |         — |     — |
| `reliability`         | compliant     |    1 |       5 |         1 |     5 |
| `row_count`           | not validated |    — |       — |         — |     — |
| `row_means`           | not validated |    — |       — |         — |     — |
| `row_sums`            | not validated |    — |       — |         — |     — |
| `scheffe_test`        | not validated |    — |       — |         — |     — |
| `spearman_rho`        | compliant     |    1 |       2 |         1 |     2 |
| `std`                 | not validated |    — |       — |         — |     — |
| `t_test`              | compliant     |    1 |      30 |         1 |    30 |
| `tukey_test`          | compliant     |    1 |       4 |         1 |     4 |
| `w_iqr`               | not validated |    — |       — |         — |     — |
| `w_kurtosis`          | not validated |    — |       — |         — |     — |
| `w_mean`              | not validated |    — |       — |         — |     — |
| `w_median`            | not validated |    — |       — |         — |     — |
| `w_modus`             | not validated |    — |       — |         — |     — |
| `w_quantile`          | not validated |    — |       — |         — |     — |
| `w_range`             | not validated |    — |       — |         — |     — |
| `w_sd`                | not validated |    — |       — |         — |     — |
| `w_se`                | not validated |    — |       — |         — |     — |
| `w_skew`              | not validated |    — |       — |         — |     — |
| `w_var`               | not validated |    — |       — |         — |     — |
| `wilcoxon_test`       | compliant     |    1 |       6 |         1 |     6 |

## Active Exceptions

No active Tier-3 exceptions. All validated statistics agree with SPSS
within Spec or Display tolerances.

## Validation Gaps

The following functions are in SPSS-validation scope (per Charter §9)
but do not yet have a `test-<fn>-spss-validation.R` file:

- `center`
- `codebook`
- `cramers_v`
- `goodman_gamma`
- `phi`
- `pomps`
- `rec`
- `row_count`
- `row_means`
- `row_sums`
- `std`

## How to Read “SPSS-Compatible”

A function is **SPSS-compatible** when:

1.  It has a validation test file (column “Status: compliant”)
2.  Its Legacy column is zero
3.  Every value it asserts falls into Spec, Display, or a registered
    Exception
4.  It carries no `NA` placeholders in its `spss_values` block

Functions with non-zero Legacy counts are mid-migration and not yet
Charter-compliant; their assertions may pass but do not yet enforce the
documented tolerance policy.

## Methodology

All validation runs SPSS v29 syntax scripts in
`tests/spss_reference/syntax/` against `tests/spss_reference/data/`,
saves the output to `tests/spss_reference/outputs/`, and asserts the
R-side computation matches via `assert_spss()` from the validation
helpers in `tests/testthat/`. Inline cached values in test files cite
their source line via trailing `# <output_file>:<line>` comments; the
dev-only script `tests/spss_reference/verify_inline.R` audits this
citation chain before every release.
