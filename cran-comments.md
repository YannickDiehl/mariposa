## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Test environments

* local macOS Tahoe, R 4.6.0 (via `devtools::check()`: 0/0/0)
* local macOS Tahoe, R 4.6.0 (via `R CMD check --as-cran --no-manual`)
* GitHub Actions: ubuntu-latest (release, devel, oldrel-1),
  macOS-latest (release), windows-latest (release) — all `--as-cran`

## Notes

* checking CRAN incoming feasibility ... NOTE — "New submission". Expected.

## Package summary

* 76 exported functions across 15 categories: data I/O (SPSS, Stata,
  SAS, Excel), label management, data transformation, descriptive
  statistics, parametric and non-parametric tests, correlations,
  post-hoc tests, factor analysis, regression, and survey-weighted
  versions of common summaries.
* 4200 passing test assertions across the full testthat suite
  (29 SPSS-validation test files contribute ~1900 of these,
  validated against SPSS v29 reference outputs under a published
  Validation Charter — see `vignette("spss-compatibility")`).
* All Suggests packages properly gated with `requireNamespace()`
  or `skip_if_not_installed()`.
* No reverse dependencies (new package).
* Strict CRAN-style checks pass on all platforms via the
  R-CMD-check workflow with `--as-cran`.
