## Resubmission

This resubmission addresses all four points of the manual review of
0.7.1 (Leonore Hochhauser, 2026-09-09). Thank you for the review!

1. **References in DESCRIPTION**: added — Dallal and Wilkinson (1986)
   <doi:10.1080/00031305.1986.10475419> for the Lilliefors significance
   correction and Haberman (1973) <doi:10.2307/2529686> for adjusted
   standardized residuals, in the requested auto-link form. The primary
   reference, IBM Corp. (2023, "IBM SPSS Statistics Algorithms"), has
   no DOI or ISBN, and IBM's documentation server answers automated
   URL checks inconsistently (intermittent 403), so it is cited in
   plain text without an angle-bracket URL. Both DOIs are verified
   against Crossref; should the Dallal-Wilkinson DOI show as
   "(possibly) invalid", that is Taylor & Francis intermittently
   answering automated requests with 403 — it resolves correctly in a
   browser.

2. **\dontrun{}**: removed from the entire package. The import/export
   examples are now genuinely executable \donttest{} roundtrips through
   tempfile() (wrapped in requireNamespace() guards for the Suggests
   packages 'haven'/'openxlsx2'); the two file formats R cannot produce
   (.por, .sas7bdat) run behind a file.exists() guard so the examples
   never error. The unlabel() example now runs unconditionally on the
   bundled dataset.

3. **print()/cat() to the console (R/kendall_tau.R,
   R/print_helpers.R)**: the flagged cat() calls live exclusively in
   the print()/summary() display layer — R/print_helpers.R holds the
   shared display helpers called only from print methods, and the
   kendall_tau.R occurrence is a display closure invoked only by
   print(summary(x)). No computation writes to the console: runtime
   information goes through cli's message-based conditions
   (suppressable via suppressMessages()). This contract is now proven
   by a dedicated test file (tests/testthat/test-silent-computation.R):
   every analysis entry point is asserted to produce zero stdout.

4. **options() restoration**: on.exit() now registers the restoration
   immediately after saving and *before* changing the option in
   ancova(), factorial_anova(), and the correlation-matrix print
   helper, exactly per the recommended pattern.

## R CMD check results

0 errors | 0 warnings | 1 note

The only NOTE is "checking CRAN incoming feasibility":

* "New submission" — this is a new package.
* "Possibly misspelled words in DESCRIPTION": ANCOVA, codebook,
  roundtripping, toggleable, plus the cited author names — all are
  established statistical/technical terms or surnames.

## Test environments

* win-builder, R Under development, mariposa 0.7.2 — 1 NOTE (see
  above); PDF and HTML manuals build cleanly
* local macOS (Apple Silicon), R 4.6.0 — `devtools::check()`
  (including `--run-donttest`): 0 errors, 0 warnings, 0 notes
* GitHub Actions (`R CMD check --as-cran`): ubuntu-latest (release,
  devel, oldrel-1), macOS-latest (release), windows-latest (release)

## CRAN-specific test behavior

* The SPSS-validation test layer (golden-number comparisons against
  IBM SPSS Statistics v29 reference output) is skipped on CRAN via
  `skip_on_cran()`: tight numeric tolerances across CRAN's
  BLAS/platform mix would risk false-positive failures. This layer is
  the release gate and runs in CI on every commit. The remaining unit,
  property-based, weights-invariance, and output tests all run on CRAN;
  the full suite completes in under a minute.
* All Suggests packages are guarded with `requireNamespace()` in code
  and examples and `skip_if_not_installed()` in tests; examples write
  only to `tempfile()`.

## Reverse dependencies

This is a new package; there are no reverse dependencies.
