# Convert Tagged NAs Back to Original Codes

Replaces tagged NAs with their original missing value codes. Works with
data imported via
[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md)
(with `tag.na = TRUE`) or any reader that used the `tag.na` parameter
([`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md),
[`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md),
[`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md)).
For native Stata/SAS tagged NAs (e.g., `.a`, `.A`) that have no numeric
codes to recover, use
[`strip_tags()`](https://YannickDiehl.github.io/mariposa/reference/strip_tags.md)
instead.

## Usage

``` r
untag_na(x)
```

## Arguments

- x:

  A numeric vector with tagged NAs.

## Value

A numeric vector where tagged NAs with numeric codes have been replaced
with their original values (e.g., -9, -8, -42). System NAs (untagged)
remain as `NA`. For native Stata/SAS tagged NAs (no numeric codes),
falls back to
[`strip_tags()`](https://YannickDiehl.github.io/mariposa/reference/strip_tags.md)
behavior with a warning.

## See also

[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md),
[`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md),
[`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md),
[`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md),
[`strip_tags()`](https://YannickDiehl.github.io/mariposa/reference/strip_tags.md)

Other data-import:
[`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md),
[`read_por()`](https://YannickDiehl.github.io/mariposa/reference/read_por.md),
[`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md),
[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md),
[`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md),
[`read_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/read_xlsx.md),
[`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md),
[`strip_tags()`](https://YannickDiehl.github.io/mariposa/reference/strip_tags.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# SPSS data
data <- read_spss("survey.sav")
original <- untag_na(data$satisfaction)

# Stata data with tag.na
data <- read_stata("survey.dta", tag.na = c(-9, -8, -42))
original <- untag_na(data$income)
} # }
```
