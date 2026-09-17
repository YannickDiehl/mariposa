## Resubmission

This resubmission addresses the remaining point of the manual review of
0.7.2 (Leonore Hochhauser, 2026-09-16): information messages written
with print()/cat() in R/kendall_tau.R.

The flagged cat() call sat inside `.kendall_tau_spec$pair_extras`, a
display callback stored in a top-level list. It was only ever executed
by `print(summary(x))` — i.e. inside the print/summary layer the
policy exempts — but from the source alone that was impossible to see,
which we take to be the point of the remark. Rather than argue the
exemption, we restructured the code so the exemption is visible:

1. **The callback no longer writes to the console.** All three
   correlation display callbacks (kendall_tau, pearson_cor,
   spearman_rho — the latter two had the identical pattern) now
   *return* formatted lines; the single cat() call lives in the
   shared print helper `.print_cor_verbose()`. Output is
   byte-identical.

2. **The rule is now lexically true package-wide**: every cat()/print()
   call in R/ lives inside a function whose name starts with `print`/
   `.print` and that is called only from print()/summary() methods.
   Two display helpers were renamed/refactored to make that hold
   everywhere (`format_stat_table()` → `print_stat_table()`; the group
   header line of `for_each_group()` moved into `print_group_label()`).

3. **Both sides of the contract are enforced by tests**:
   `test-silent-computation.R` (runs on CRAN) asserts that every
   analysis entry point produces zero stdout at runtime, and the new
   `test-console-discipline.R` (runs in development and CI, where the
   R/ sources are present) statically parses all of R/ and fails if a
   cat()/print()/writeLines() call ever appears outside a print-layer
   function again.

No computation writes to the console; runtime information goes through
suppressable message()-based conditions.

## R CMD check results

0 errors | 0 warnings | 1 note

The only NOTE is "checking CRAN incoming feasibility":

* "New submission" — this is a new package.
* "Possibly misspelled words in DESCRIPTION": ANCOVA, codebook,
  roundtripping, toggleable, plus the cited author names — all are
  established statistical/technical terms or surnames.

## Test environments

* win-builder, R Under development, mariposa 0.7.3 — 1 NOTE (see
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
