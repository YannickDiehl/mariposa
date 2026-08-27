## Resubmission

This is a resubmission of the 0.7.0 submission (2026-08-26), addressing
the Debian incoming-pretest NOTE:

* "Examples with CPU (user + system) or elapsed time > 5s:
  kendall_tau 7.543s" — Kendall's tau is O(n^2); the examples now run
  on a 300-case subset of the bundled data (locally: 0.1s, previously
  4.5s). No other example exceeds 1s.

Additionally, version floors were added to the Imports and one
missing `requireNamespace()` guard was fixed; no other changes.

## R CMD check results

0 errors | 0 warnings | 1 note

The only NOTE is "checking CRAN incoming feasibility":

* "New submission" — this is a new package.
* "Possibly misspelled words in DESCRIPTION": ANCOVA, codebook,
  roundtripping, toggleable — all are established statistical/technical
  terms (ANCOVA is the standard acronym for analysis of covariance;
  "codebook" is the standard survey-research term for a data
  dictionary).

## Test environments

* win-builder, R Under development (2026-08-25 r90448 ucrt),
  mariposa 0.7.1 — 1 NOTE (see above); examples in 24s total, PDF and
  HTML manuals build cleanly
* local macOS (Apple Silicon), R 4.6.0 — `devtools::check()`: 0 errors,
  0 warnings, 0 notes (`--no-manual`; the manual is covered by
  win-builder above)
* GitHub Actions (`R CMD check --as-cran`): ubuntu-latest (release,
  devel, oldrel-1), macOS-latest (release), windows-latest (release)

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
