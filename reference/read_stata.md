# Read Stata Data with Tagged Missing Values

Reads a Stata `.dta` file and integrates with mariposa's tagged NA
system. Handles two scenarios:

1.  **Native extended missing values** (`.a` through `.z`):
    Automatically detected and annotated.

2.  **Numeric missing codes** (e.g., -9, -42): When `tag.na` is
    provided, these regular values are converted to tagged NAs, giving
    the same result as
    [`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md)
    with `tag.na = TRUE`.

## Usage

``` r
read_stata(path, encoding = NULL, tag.na = NULL, verbose = FALSE)
```

## Arguments

- path:

  Path to a Stata `.dta` file.

- encoding:

  Character encoding for the file. If `NULL`, haven's default encoding
  detection is used. Generally only needed for Stata 13 files and
  earlier.

- tag.na:

  Numeric vector of values to treat as missing (e.g., `c(-9, -8, -42)`).
  These values will be converted to tagged NAs across all numeric
  variables. Use this when Stata files contain SPSS-style missing codes
  stored as regular values. Default: `NULL` (only detect native Stata
  extended missing values).

- verbose:

  If `TRUE`, prints a message summarizing how many variables contain
  tagged missing values.

## Value

A tibble with the Stata data. Variables with missing value codes have:

- Tagged NAs for each missing type

- An `"na_tag_map"` attribute mapping tag characters to original codes

- [`is.na()`](https://rdrr.io/r/base/NA.html) returns `TRUE` for these
  values (standard R behavior)

## Details

### Native Extended Missing Values

Stata supports 27 distinct missing value types: `.` (system missing) and
`.a` through `.z` (extended missing values). The `haven` package
preserves these as tagged NAs automatically. `read_stata()` adds the
`na_tag_map` attribute so that mariposa's tagged NA functions work
seamlessly.

### Numeric Missing Codes (tag.na)

Many Stata files – especially those converted from SPSS – store missing
value codes as regular numeric values (e.g., -9 = "No answer", -42 =
"Data error"). The `tag.na` parameter converts these to tagged NAs,
enabling proper handling in
[`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md),
[`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md),
and other functions.

When `tag.na` is used,
[`untag_na()`](https://YannickDiehl.github.io/mariposa/reference/untag_na.md)
can recover the original numeric codes.

## See also

[`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md),
[`strip_tags()`](https://YannickDiehl.github.io/mariposa/reference/strip_tags.md),
[`untag_na()`](https://YannickDiehl.github.io/mariposa/reference/untag_na.md),
[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md),
[`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md),
[`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md),
[`haven::read_dta()`](https://haven.tidyverse.org/reference/read_dta.html)

Other data-import:
[`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md),
[`read_por()`](https://YannickDiehl.github.io/mariposa/reference/read_por.md),
[`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md),
[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md),
[`read_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/read_xlsx.md),
[`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md),
[`strip_tags()`](https://YannickDiehl.github.io/mariposa/reference/strip_tags.md),
[`untag_na()`](https://YannickDiehl.github.io/mariposa/reference/untag_na.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Read Stata file with native extended missing values
data <- read_stata("survey.dta")

# Read Stata file with SPSS-style missing codes
data <- read_stata("survey.dta", tag.na = c(-9, -8, -42, -11))

# Check what types of missing values exist
na_frequencies(data$income)

# frequency() and codebook() show each missing type separately
data %>% frequency(income)
codebook(data)

# Recover original codes or convert to regular NAs
untag_na(data$income)   # Recovers -9, -8, etc.
strip_tags(data$income) # Converts all to NA
} # }
```
