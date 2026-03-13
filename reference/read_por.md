# Read SPSS Portable Data with Tagged Missing Values

Reads an SPSS portable `.por` file and preserves user-defined missing
values as tagged NAs. This is the portable format equivalent of
[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md)
for `.sav` files.

## Usage

``` r
read_por(path, tag.na = TRUE, verbose = FALSE)
```

## Arguments

- path:

  Path to an SPSS `.por` file.

- tag.na:

  If `TRUE` (the default), user-defined missing values are converted to
  tagged NAs using
  [`haven::tagged_na()`](https://haven.tidyverse.org/reference/tagged_na.html).
  If `FALSE`, the file is read with standard
  [`haven::read_por()`](https://haven.tidyverse.org/reference/read_spss.html)
  behavior.

- verbose:

  If `TRUE`, prints a message summarizing how many values were
  converted.

## Value

A tibble with the SPSS data. See
[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md)
for details on tagged NA handling.

## Details

The SPSS portable format (`.por`) is an older, platform-independent
format. Unlike `.sav` files, the portable format does not support
specifying a character encoding. Tagged NA handling is identical to
[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md).

## See also

[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md),
[`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md),
[`untag_na()`](https://YannickDiehl.github.io/mariposa/reference/untag_na.md),
[`strip_tags()`](https://YannickDiehl.github.io/mariposa/reference/strip_tags.md),
[`haven::read_por()`](https://haven.tidyverse.org/reference/read_spss.html)

Other data-import:
[`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md),
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
data <- read_por("survey.por")
na_frequencies(data$satisfaction)
} # }
```
