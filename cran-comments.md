## Update submission

This is an update of mariposa from 0.7.3 to 0.7.4. It brings more results
in line with IBM SPSS Statistics, adds `as.data.frame()`, `broom::tidy()`
and `write_xlsx()` support for all result objects, and improves output and
error messages. Details are in NEWS.md.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local macOS (Apple Silicon), R 4.5.3: `R CMD check --as-cran`, including
  the PDF manual and the URL checks
* win-builder, R-devel
* GitHub Actions (`R CMD check --as-cran`): ubuntu-latest (R release,
  devel and oldrel-1), macOS-latest (R release), windows-latest (R release)

## Tests on CRAN

The comparisons with IBM SPSS Statistics reference output are skipped on
CRAN with `skip_on_cran()`, because their tight numeric tolerances could
fail on other BLAS implementations; they run in continuous integration on
every change. All other tests run on CRAN and take about one minute.
Suggested packages are used conditionally in code, examples and tests;
examples write only to `tempfile()`.

## Reverse dependencies

There are no reverse dependencies.
