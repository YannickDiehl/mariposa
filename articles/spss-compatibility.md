# SPSS Compatibility Status

This vignette reports the SPSS-compatibility status of every statistical
function in **mariposa**. It is auto-generated from the test suite and
the validation exception registry.

**Generated:** 2026-09-30

## Summary

- Functions in SPSS-validation scope: **53**
- With validation tests in place: **42**
- Validation gaps (no test file yet): **11**
- Active Tier-3 algorithmic exceptions: **2**

## Tier Definitions

Every numerical comparison between an R-side value and an SPSS reference
falls into one of four tiers (Charter §4):

- **Spec** — exact integers and SPSS-truncated sentinels (exact match)
- **Display** — SPSS rounds for print; tolerance is half a unit of the
  last printed decimal
- **Exception** — documented algorithmic difference; see
  `VALIDATION_EXCEPTIONS.md`
- **Internal** — statistic has no SPSS equivalent; verified via snapshot
  tests only

## Per-Function Status

The columns show how many SPSS reference values per tier the validation
file checks when it runs. “Total” is the total number of
charter-compliant comparisons. The `w_*` functions share one validation
file, whose total each of them shows.

The “Internal (Tier 4)” column flags statistics that have no SPSS
reference and are therefore R-only: for the rank-based family the
*weighted variant* (SPSS `NPAR TESTS` and `NONPAR CORR` ignore
`WEIGHT BY`); for `reliability`, McDonald’s omega in all paths (IBM does
not publicly document the SPSS omega algorithm; an SPSS v29 reference
run is pending). Tier-4 statistics are covered by internal regression
and cross-check tests plus a weights-equal-1 invariance suite instead of
SPSS references. (`mann_whitney`’s weighted variant is a design-based
rank test additionally validated against
[`survey::svyranktest()`](https://rdrr.io/pkg/survey/man/svyranktest.html).)
All other statistics of these functions are SPSS-validated as shown in
the tier columns.

| Function | Status | Spec | Display | Exception | Total | Internal (Tier 4) |
|----|----|---:|---:|---:|---:|----|
| `ancova` | compliant | 147 | 653 | 0 | 800 | — |
| `binomial_test` | compliant | 24 | 34 | 0 | 58 | weighted variant |
| `center` | not validated | — | — | — | — | — |
| `chi_square` | compliant | 53 | 182 | 0 | 235 | — |
| `chisq_gof` | compliant | 51 | 82 | 0 | 133 | — |
| `codebook` | not validated | — | — | — | — | — |
| `cramers_v` | not validated | — | — | — | — | — |
| `crosstab` | compliant | 1419 | 1950 | 0 | 3369 | adjusted standardized residuals (Haberman-formula oracle; SPSS /CELLS=ASRESID reference run pending) |
| `describe` | compliant | 18 | 270 | 0 | 288 | — |
| `dunn_test` | compliant | 0 | 6 | 0 | 6 | weighted variant |
| `efa` | compliant | 21 | 473 | 84 | 578 | — |
| `factorial_anova` | compliant | 200 | 409 | 0 | 609 | — |
| `fisher_test` | compliant | 100 | 312 | 0 | 412 | — |
| `frequency` | compliant | 24 | 126 | 0 | 150 | — |
| `friedman_test` | compliant | 18 | 38 | 0 | 56 | weighted variant |
| `goodman_gamma` | not validated | — | — | — | — | — |
| `kendall_tau` | compliant | 100 | 102 | 0 | 202 | weighted variant |
| `kruskal_wallis` | compliant | 52 | 45 | 0 | 97 | weighted variant |
| `levene_test` | compliant | 0 | 144 | 0 | 144 | — |
| `linear_regression` | compliant | 35 | 567 | 0 | 602 | — |
| `logistic_regression` | compliant | 12 | 17 | 0 | 29 | all statistics (textbook-formula oracle; no SPSS v29 reference run yet) |
| `mann_whitney` | compliant | 19 | 74 | 0 | 93 | weighted variant |
| `mcnemar_test` | compliant | 84 | 59 | 0 | 143 | — |
| `multiple_response` | compliant | 19 | 33 | 0 | 52 | all statistics (hand-computation oracle; SPSS MULT RESPONSE reference run pending) |
| `normality_test` | compliant | 6 | 15 | 0 | 21 | all statistics (independent-implementation oracles; SPSS EXAMINE reference run pending) |
| `oneway_anova` | compliant | 234 | 1209 | 0 | 1443 | — |
| `pairwise_wilcoxon` | compliant | 106 | 136 | 0 | 242 | weighted variant |
| `partial_cor` | compliant | 4 | 16 | 0 | 20 | all statistics (residual-regression oracle; SPSS PARTIAL CORR reference run pending) |
| `pearson_cor` | compliant | 18 | 55 | 0 | 73 | — |
| `phi` | not validated | — | — | — | — | — |
| `pomps` | not validated | — | — | — | — | — |
| `rec` | not validated | — | — | — | — | — |
| `reliability` | compliant | 45 | 413 | 0 | 458 | McDonald’s omega (all paths) |
| `row_count` | not validated | — | — | — | — | — |
| `row_means` | not validated | — | — | — | — | — |
| `row_sums` | not validated | — | — | — | — | — |
| `scheffe_test` | compliant | 0 | 1422 | 0 | 1422 | — |
| `spearman_rho` | compliant | 118 | 120 | 0 | 238 | — |
| `std` | not validated | — | — | — | — | — |
| `t_test` | compliant | 9 | 664 | 0 | 673 | — |
| `tukey_test` | compliant | 0 | 1418 | 0 | 1418 | — |
| `w_iqr` | compliant | 132 | 312 | 0 | 444 | — |
| `w_kurtosis` | compliant | 132 | 312 | 0 | 444 | — |
| `w_mean` | compliant | 132 | 312 | 0 | 444 | — |
| `w_median` | compliant | 132 | 312 | 0 | 444 | — |
| `w_modus` | compliant | 132 | 312 | 0 | 444 | — |
| `w_quantile` | compliant | 132 | 312 | 0 | 444 | — |
| `w_range` | compliant | 132 | 312 | 0 | 444 | — |
| `w_sd` | compliant | 132 | 312 | 0 | 444 | — |
| `w_se` | compliant | 132 | 312 | 0 | 444 | — |
| `w_skew` | compliant | 132 | 312 | 0 | 444 | — |
| `w_var` | compliant | 132 | 312 | 0 | 444 | — |
| `wilcoxon_test` | compliant | 48 | 66 | 0 | 114 | weighted variant |

## Active Exceptions

| ID | Function | Statistic | Tolerance | Reason |
|----|----|----|---:|----|
| EXC-001 | `efa` | ML loadings, sums of squares, % of variance | 0.002 | SPSS stops the ML iteration at its convergence criterion (.001); mariposa iterates to the optimum. |
| EXC-002 | `efa` | ML communalities / SS where SPSS did not converge | 0.05 | The SPSS reference runs end without convergence (3 factors from 6 items, df = 0, Heywood cases). |

An exception covers an identified algorithmic difference to SPSS, not a
bug: its tolerance is what the two algorithms agree to. Statistics
outside an exception are asserted at Spec or Display tier, or are listed
as Internal (Tier 4) in the table above.

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
