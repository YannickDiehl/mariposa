# Read SPSS Data with Tagged Missing Values

Reads an SPSS `.sav` file and preserves user-defined missing values as
tagged NAs instead of converting them to regular `NA`. This allows you
to distinguish between different types of missing data (e.g., "no
answer", "not applicable", "refused") while still treating them as `NA`
in standard R operations.

## Usage

``` r
read_spss(path, tag.na = TRUE, encoding = NULL, verbose = FALSE)
```

## Arguments

- path:

  Path to an SPSS `.sav` file.

- tag.na:

  If `TRUE` (the default), user-defined missing values are converted to
  tagged NAs using
  [`haven::tagged_na()`](https://haven.tidyverse.org/reference/tagged_na.html).
  If `FALSE`, the file is read with standard
  [`haven::read_sav()`](https://haven.tidyverse.org/reference/read_spss.html)
  behavior (all missing values become regular `NA`).

- encoding:

  Character encoding for the file. If `NULL`, haven's default encoding
  detection is used.

- verbose:

  If `TRUE`, prints a message summarizing how many values were
  converted.

## Value

A tibble with the SPSS data. When `tag.na = TRUE`:

- User-defined missing values are stored as tagged NAs

- [`is.na()`](https://rdrr.io/r/base/NA.html) returns `TRUE` for these
  values (standard R behavior)

- The original SPSS missing codes can be recovered via
  [`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md),
  [`untag_na()`](https://YannickDiehl.github.io/mariposa/reference/untag_na.md),
  or
  [`haven::na_tag()`](https://haven.tidyverse.org/reference/tagged_na.html)

- Each tagged variable has an `"na_tag_map"` attribute mapping tag
  characters to original SPSS codes

## Details

SPSS allows defining specific values as "user-defined missing values"
(e.g., -9 = "no answer", -8 = "don't know"). When reading `.sav` files
with
[`haven::read_sav()`](https://haven.tidyverse.org/reference/read_spss.html),
these are silently converted to `NA`, losing the information about *why*
a value is missing.

`read_spss()` preserves this information using haven's tagged NA system:
each missing value type gets a unique tag character (a-z, A-Z, 0-9) that
can be inspected with
[`haven::na_tag()`](https://haven.tidyverse.org/reference/tagged_na.html).
The values still behave as `NA` in all standard R operations
([`mean()`](https://rdrr.io/r/base/mean.html),
[`sum()`](https://rdrr.io/r/base/sum.html),
[`is.na()`](https://rdrr.io/r/base/NA.html), etc.).

Use the companion functions to work with the tagged NAs:

- [`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md) -
  Frequency table of missing types

- [`untag_na()`](https://YannickDiehl.github.io/mariposa/reference/untag_na.md) -
  Convert tagged NAs back to original SPSS codes

- [`strip_tags()`](https://YannickDiehl.github.io/mariposa/reference/strip_tags.md) -
  Convert tagged NAs to regular NAs (drop tags)

## See also

[`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md),
[`untag_na()`](https://YannickDiehl.github.io/mariposa/reference/untag_na.md),
[`strip_tags()`](https://YannickDiehl.github.io/mariposa/reference/strip_tags.md),
[`haven::read_sav()`](https://haven.tidyverse.org/reference/read_spss.html),
[`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md),
[`read_por()`](https://YannickDiehl.github.io/mariposa/reference/read_por.md)

Other data-import:
[`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md),
[`read_por()`](https://YannickDiehl.github.io/mariposa/reference/read_por.md),
[`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md),
[`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md),
[`read_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/read_xlsx.md),
[`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md),
[`strip_tags()`](https://YannickDiehl.github.io/mariposa/reference/strip_tags.md),
[`untag_na()`](https://YannickDiehl.github.io/mariposa/reference/untag_na.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Read SPSS file with tagged missing values
data <- read_spss("survey.sav")

# Check what types of missing values exist
na_frequencies(data$satisfaction)

# Standard R operations work normally (NAs are excluded)
mean(data$satisfaction, na.rm = TRUE)

# frequency() shows each missing type separately
data %>% frequency(satisfaction)

# Recover original SPSS codes
original_codes <- untag_na(data$satisfaction)

# Convert to regular NAs (standard behavior)
data_clean <- strip_tags(data$satisfaction)
} # }
```
