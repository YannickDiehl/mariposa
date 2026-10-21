## Resubmission

This is a resubmission. In this version I have:

* replaced the relative link to CONTRIBUTING.md in README.md, a file that is
  not part of the package, with its URL;
* corrected the package version shown by `citation("mariposa")` and added
  the DOI of the CRAN package;
* made the package compatible with the upcoming haven release, which
  stores the tags of tagged missing values in lower case (contributed by
  the haven maintainers); the tests pass with the CRAN and the development
  version of haven.

## Update submission

This is an update of mariposa from 0.7.3 to 0.7.4. It brings more results
in line with IBM SPSS Statistics, adds `as.data.frame()`, `broom::tidy()`
and `write_xlsx()` support for all result objects, and improves output and
error messages. Details are in NEWS.md.

The update follows 0.7.3 after three weeks because it corrects results of
several functions, for example the marginal effects of transformed
predictors in `marginal_effects()` and the standard errors of weighted
post-hoc tests, and a failing default call of `read_sas()`.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local macOS (Apple Silicon), R 4.5.3: `R CMD check --as-cran`, including
  the PDF manual and the URL checks, with the CRAN and the development
  version of haven
* win-builder, R-devel
* GitHub Actions (`R CMD check --as-cran`): ubuntu-latest (R release,
  devel and oldrel-1), macOS-latest (R release), windows-latest (R release)

## Tests on CRAN

The comparisons with IBM SPSS Statistics reference output are skipped on
CRAN with `skip_on_cran()`, because their tight numeric tolerances could
fail on other BLAS implementations; they run in continuous integration on
every change. All other tests run on CRAN and take about one minute
locally (about four minutes on win-builder).
Suggested packages are used conditionally in code, examples and tests;
examples write only to `tempfile()`.

## Reverse dependencies

There are no reverse dependencies.
