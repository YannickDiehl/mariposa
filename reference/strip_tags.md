# Strip Tags from Tagged NAs

Converts all tagged NAs to regular (untagged) `NA` values, effectively
removing the missing value type information. Works with data from any
format:
[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md),
[`read_por()`](https://YannickDiehl.github.io/mariposa/reference/read_por.md),
[`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md),
[`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md),
or
[`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md).

## Usage

``` r
strip_tags(x)
```

## Arguments

- x:

  A numeric vector with tagged NAs.

## Value

A numeric vector where all tagged NAs have been replaced with regular
`NA`. Value labels for missing types are removed; labels for valid
values are preserved.

## See also

[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md),
[`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md),
[`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md),
[`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md),
[`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md),
[`untag_na()`](https://YannickDiehl.github.io/mariposa/reference/untag_na.md)

Other data-import:
[`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md),
[`read_por()`](https://YannickDiehl.github.io/mariposa/reference/read_por.md),
[`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md),
[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md),
[`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md),
[`read_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/read_xlsx.md),
[`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md),
[`untag_na()`](https://YannickDiehl.github.io/mariposa/reference/untag_na.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data <- read_spss("survey.sav")
# Remove tag information, keep only valid labels
clean <- strip_tags(data$satisfaction)
} # }
```
