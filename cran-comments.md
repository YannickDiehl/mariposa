## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new submission.

## Test environments

* local macOS (Apple Silicon), R 4.6.0 — `devtools::check()`: 0 errors,
  0 warnings, 0 notes (no local LaTeX; the PDF manual is checked via
  win-builder, see below)
* GitHub Actions (`R CMD check --as-cran`): ubuntu-latest (release,
  devel, oldrel-1), macOS-latest (release), windows-latest (release)
<!-- TODO before submission: run devtools::check_win_devel() and paste
     the result here — it is the only environment that builds the PDF
     manual. Remove this comment afterwards. -->

## CRAN-specific test behavior

* The SPSS-validation test layer (golden-number comparisons against
  IBM SPSS Statistics v29 reference output, 29 test files) is skipped
  on CRAN via `skip_on_cran()`: tight numeric tolerances across CRAN's
  BLAS/platform mix would risk false-positive failures. This layer is
  the release gate and runs in CI on every commit. The remaining unit,
  property-based, weights-invariance, and output tests all run on CRAN;
  the full suite completes in under a minute.
* All Suggests packages are guarded with `requireNamespace()` in code
  and examples and `skip_if_not_installed()` in tests; examples write
  only to `tempdir()`.

## Reverse dependencies

This is a new package; there are no reverse dependencies.
