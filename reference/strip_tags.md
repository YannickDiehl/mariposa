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
strip_tags(x, ...)
```

## Arguments

- x:

  A numeric vector with tagged NAs, or a data frame.

- ...:

  For a data frame: the columns to convert (tidyselect). If empty, every
  numeric column is converted. Ignored for vectors.

## Value

The input with all tagged NAs replaced by regular `NA`. Value labels for
missing types are removed; labels for valid values and the variable
label are preserved. A `haven_labelled` input stays `haven_labelled`, so
the labels survive
[`write_spss()`](https://YannickDiehl.github.io/mariposa/reference/write_spss.md)
and
[`write_stata()`](https://YannickDiehl.github.io/mariposa/reference/write_stata.md).
A data frame is returned with the selected columns converted.

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
# \donttest{
if (requireNamespace("haven", quietly = TRUE)) {
  x <- set_na(c(1, 2, -9, 3, -8), -9, -8)
  # Remove tag information: all missings become plain NA
  strip_tags(x)
}
#> <labelled<double>[5]>
#> [1]  1  2 NA  3 NA
# }
```
