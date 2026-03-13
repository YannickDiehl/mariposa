# Frequency Table of Missing Value Types

Shows a breakdown of the different types of missing values in a variable
that was read with
[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md),
[`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md),
[`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md),
or
[`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md)
and contains tagged NAs.

## Usage

``` r
na_frequencies(x)
```

## Arguments

- x:

  A numeric vector with tagged NAs.

## Value

A data frame with columns:

- tag:

  The tag character (e.g., a-z for Stata, A-Z for SAS, a-z/A-Z/0-9 for
  SPSS)

- n:

  Number of cases with this missing type

- code:

  The original missing value code: numeric SPSS codes (e.g., -9, -8) or
  native format codes (e.g., ".a" for Stata, ".A" for SAS)

- label:

  The value label for this missing type (if available)

## See also

[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md),
[`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md),
[`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md),
[`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md),
[`untag_na()`](https://YannickDiehl.github.io/mariposa/reference/untag_na.md),
[`strip_tags()`](https://YannickDiehl.github.io/mariposa/reference/strip_tags.md)

Other data-import:
[`read_por()`](https://YannickDiehl.github.io/mariposa/reference/read_por.md),
[`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md),
[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md),
[`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md),
[`read_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/read_xlsx.md),
[`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md),
[`strip_tags()`](https://YannickDiehl.github.io/mariposa/reference/strip_tags.md),
[`untag_na()`](https://YannickDiehl.github.io/mariposa/reference/untag_na.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# SPSS data
data <- read_spss("survey.sav")
na_frequencies(data$satisfaction)
#   tag    n  code            label
# 1   b 1774   -11       TNZ: SPLIT
# 2   c   63    -9     KEINE ANGABE
# 3   d   11    -8    WEISS NICHT
# 4   a    6   -42 DATENFEHLER: MFN

# Stata data
data <- read_stata("survey.dta")
na_frequencies(data$income)
#   tag  n code           label
# 1   a 42   .a     Not applicable
# 2   b 15   .b     Refused
} # }
```
