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
na_frequencies(x, ...)
```

## Arguments

- x:

  A numeric vector with tagged NAs, or a data frame.

- ...:

  For a data frame: the variables to tabulate (tidyselect). If empty,
  every numeric variable with missing values is tabulated.

## Value

A data frame with one row per missing type, ordered by code like the
missing block of
[`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
(system missing last, listed only when it occurs):

- code:

  The original missing value code: numeric for SPSS codes (e.g., -9,
  -8), character for native format codes (e.g., ".a" for Stata, ".A" for
  SAS)

- label:

  The value label for this missing type (if available)

- n:

  Number of cases with this missing type

- prc:

  Percent of all cases

- tag:

  The internal tag character of the tagged NA

For a data frame, a first column `variable` names the variable. Without
any missing values the (empty) result is returned invisibly with a
message.

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
# \donttest{
if (requireNamespace("haven", quietly = TRUE)) {
  # Declare -9/-8 as distinct tagged missing types, then inspect them
  x <- set_na(c(1, 2, -9, 3, -8, -9), -9, -8)
  na_frequencies(x)
  #   code label n      prc tag
  # 1   -9  <NA> 2 33.33333   a
  # 2   -8  <NA> 1 16.66667   b
}
#>   code label n      prc tag
#> 1   -9  <NA> 2 33.33333   a
#> 2   -8  <NA> 1 16.66667   b
# }
```
