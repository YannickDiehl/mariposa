# Read SAS Data with Tagged Missing Values

Reads a SAS `.sas7bdat` file and integrates with mariposa's tagged NA
system. Handles two scenarios:

1.  **Native special missing values** (`.A` through `.Z`, `._`):
    Automatically detected and annotated.

2.  **Numeric missing codes** (e.g., -9, -42): When `tag.na` is
    provided, these regular values are converted to tagged NAs, giving
    the same result as
    [`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md)
    with `tag.na = TRUE`.

## Usage

``` r
read_sas(
  path,
  catalog_file = NULL,
  encoding = NULL,
  catalog_encoding = NULL,
  tag.na = NULL,
  verbose = FALSE
)
```

## Arguments

- path:

  Path to a SAS `.sas7bdat` data file.

- catalog_file:

  Path to a SAS catalog file (`.sas7bcat`) for value labels. If `NULL`,
  no catalog is used.

- encoding:

  Character encoding for the data file. If `NULL`, haven's default
  encoding detection is used.

- catalog_encoding:

  Character encoding for the catalog file. If `NULL`, the same encoding
  as the data file is used.

- tag.na:

  Numeric vector of values to treat as missing (e.g., `c(-9, -8, -42)`).
  These values will be converted to tagged NAs across all numeric
  variables. Use this when SAS files contain missing codes stored as
  regular values. Default: `NULL` (only detect native SAS special
  missing values).

- verbose:

  If `TRUE`, prints a message summarizing how many variables contain
  tagged missing values.

## Value

A tibble with the SAS data. Variables with missing value codes have:

- Tagged NAs for each missing type

- An `"na_tag_map"` attribute mapping tag characters to original codes

- [`is.na()`](https://rdrr.io/r/base/NA.html) returns `TRUE` for these
  values (standard R behavior)

## Details

### Native Special Missing Values

SAS supports 28 distinct missing value types: `.` (system missing), `.A`
through `.Z`, and `._` (underscore). The `haven` package preserves these
as tagged NAs automatically. `read_sas()` adds the `na_tag_map`
attribute so that mariposa's tagged NA functions work seamlessly.

### Numeric Missing Codes (tag.na)

Some SAS files store missing value codes as regular numeric values
(e.g., -9 = "No answer"). The `tag.na` parameter converts these to
tagged NAs, enabling proper handling in
[`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md),
[`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md),
and other functions.

When `tag.na` is used,
[`untag_na()`](https://YannickDiehl.github.io/mariposa/reference/untag_na.md)
can recover the original numeric codes.

## See also

[`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md),
[`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md),
[`strip_tags()`](https://YannickDiehl.github.io/mariposa/reference/strip_tags.md),
[`untag_na()`](https://YannickDiehl.github.io/mariposa/reference/untag_na.md),
[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md),
[`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md),
[`haven::read_sas()`](https://haven.tidyverse.org/reference/read_sas.html)

Other data-import:
[`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md),
[`read_por()`](https://YannickDiehl.github.io/mariposa/reference/read_por.md),
[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md),
[`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md),
[`read_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/read_xlsx.md),
[`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md),
[`strip_tags()`](https://YannickDiehl.github.io/mariposa/reference/strip_tags.md),
[`untag_na()`](https://YannickDiehl.github.io/mariposa/reference/untag_na.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Read SAS file with native special missing values
data <- read_sas("survey.sas7bdat")

# Read with catalog for value labels
data <- read_sas("survey.sas7bdat", catalog_file = "formats.sas7bcat")

# Read SAS file with numeric missing codes
data <- read_sas("survey.sas7bdat", tag.na = c(-9, -8, -42))

# Check what types of missing values exist
na_frequencies(data$income)

# Recover original codes or convert to regular NAs
untag_na(data$income)   # Recovers -9, -8, etc.
strip_tags(data$income) # Converts all to NA
} # }
```
