## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Test environments

* local macOS, R 4.6.0
* GitHub Actions: ubuntu-latest (release, devel, oldrel-1),
  macOS-latest (release), windows-latest (release)

## Notes

* checking CRAN incoming feasibility ... NOTE — "New submission". Expected.

## Package summary

* 76 exported functions across 15 categories
* 1832 passing assertions across 29 SPSS-validation test files,
  validated against SPSS v29 under a published Validation Charter
  (see `vignette("spss-compatibility")` and
  `.claude/VALIDATION_CHARTER.md` in the repo).
* All Suggests packages properly gated with `requireNamespace()`
* No reverse dependencies (new package)
* Strict CRAN-style checks pass on all platforms via R-CMD-check
  workflow with `--as-cran`
