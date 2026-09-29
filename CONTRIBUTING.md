# Contributing to mariposa

Thank you for your interest in contributing to mariposa! This document
explains how to report issues, seek support, and contribute code or
documentation.

## Reporting Bugs

Please report bugs via the [GitHub issue
tracker](https://github.com/YannickDiehl/mariposa/issues). A good bug
report includes:

- A minimal reproducible example (ideally using the bundled
  `survey_data` dataset)
- The output you observed and the output you expected
- If the report concerns a statistical result: the SPSS version and
  syntax you compared against, if applicable
- Your R version and mariposa version (`packageVersion("mariposa")`)

## Seeking Support

- **Usage questions**: Open a [GitHub
  issue](https://github.com/YannickDiehl/mariposa/issues) with the
  `question` label. Please check the [documentation
  site](https://YannickDiehl.github.io/mariposa/) and the vignettes
  first — most workflows are covered there.
- **Feature requests**: Also via GitHub issues. Note that some features
  are deliberately deferred; check existing issues before filing.

mariposa is maintained by a single developer. Issues are typically
triaged within a week; there is no guaranteed response time.

## Contributing Code

Pull requests are welcome. For anything larger than a typo fix, please
open an issue first to discuss the change.

### Development Setup

``` r

# Clone, then from the package root:
devtools::install_dev_deps()
devtools::load_all()
devtools::test()
```

### Project Conventions

- **Code style**: Follow the existing style (tidyverse-flavored,
  snake_case arguments). Match the conventions of neighboring code.
- **Documentation**: All exported functions use roxygen2 with practical
  examples based on `survey_data`. Run `devtools::document()` after
  changes.
- **Output**: Analysis functions follow a three-layer output pattern
  ([`print()`](https://rdrr.io/r/base/print.html) compact,
  [`summary()`](https://rdrr.io/r/base/summary.html) builder,
  `print.summary.*()` verbose).

### Statistical Correctness Rules (load-bearing)

mariposa’s core claim is SPSS-compatible, validated results.
Contributions touching statistical code must follow these rules:

1.  **SPSS validation**: New statistics need reference values from SPSS
    v29 and a validation test asserting them via `assert_spss()`.
    Tolerances come exclusively from the tier system in
    `tests/testthat/helper-validation-tolerances.R` — inline numeric
    tolerances are rejected by an automated discipline test.
2.  **Weighted formulas live in one place**: Any weighted formula
    belongs in `R/kernels-weighted.R`, never inline in a function file.
3.  **Weights invariance**: Every weighted entry point must reproduce
    the unweighted result at `weights == 1`. New weighted statistics
    must add a block to `tests/testthat/test-weights-invariance.R`.

See `.claude/VALIDATION_CHARTER.md` in the repository for the full
policy.

### Before Submitting a Pull Request

`devtools::test()` passes

`devtools::check()` is clean (no errors, warnings, or new notes)

New functions are documented and, if user-facing, mentioned in the
appropriate vignette

`NEWS.md` has an entry describing the change

## Code of Conduct

Please be respectful and constructive in all project spaces. Harassment
or abusive behavior is not tolerated.
