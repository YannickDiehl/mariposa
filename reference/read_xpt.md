# Read SAS Transport File with Tagged Missing Values

Reads a SAS transport file (`.xpt`) and integrates with mariposa's
tagged NA system. Handles both native SAS special missing values and
user-specified numeric missing codes (via `tag.na`). Transport files are
a platform-independent SAS data format commonly used for FDA submissions
and data exchange.

## Usage

``` r
read_xpt(path, tag.na = NULL, verbose = FALSE)
```

## Arguments

- path:

  Path to a SAS transport file (`.xpt`).

- tag.na:

  Numeric vector of values to treat as missing (e.g., `c(-9, -8, -42)`).
  These values will be converted to tagged NAs across all numeric
  variables. Default: `NULL` (only detect native SAS special missing
  values).

- verbose:

  If `TRUE`, prints a message summarizing how many variables contain
  tagged missing values.

## Value

A tibble with the SAS data. See
[`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md)
for details on tagged NA handling.

## Details

SAS transport files support the same special missing values as
`.sas7bdat` files (`.A`-`.Z`, `._`). This format is self-contained and
does not require a separate catalog file for value labels.

When `tag.na` is used,
[`untag_na()`](https://YannickDiehl.github.io/mariposa/reference/untag_na.md)
can recover the original numeric codes.

## See also

[`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md),
[`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md),
[`strip_tags()`](https://YannickDiehl.github.io/mariposa/reference/strip_tags.md),
[`untag_na()`](https://YannickDiehl.github.io/mariposa/reference/untag_na.md),
[`haven::read_xpt()`](https://haven.tidyverse.org/reference/read_xpt.html)

Other data-import:
[`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md),
[`read_por()`](https://YannickDiehl.github.io/mariposa/reference/read_por.md),
[`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md),
[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md),
[`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md),
[`read_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/read_xlsx.md),
[`strip_tags()`](https://YannickDiehl.github.io/mariposa/reference/strip_tags.md),
[`untag_na()`](https://YannickDiehl.github.io/mariposa/reference/untag_na.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Read transport file with native missing values
data <- read_xpt("survey.xpt")

# Read with numeric missing codes
data <- read_xpt("survey.xpt", tag.na = c(-9, -8, -42))

na_frequencies(data$income)
} # }
```
