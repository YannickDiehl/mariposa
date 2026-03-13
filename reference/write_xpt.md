# Export Data to SAS Transport Format

Writes a data frame to a SAS transport file (`.xpt`), preserving
variable labels and missing value types. Tagged NAs from any source
format (SPSS, Stata, SAS) are written as SAS special missing values
(`.A` through `.Z`, `._`).

## Usage

``` r
write_xpt(data, path, version = 5, name = NULL)
```

## Arguments

- data:

  A data frame to export.

- path:

  Path to the output file. Must end in `.xpt`.

- version:

  SAS transport file version. Either `5` (default, SAS Transport v5,
  most compatible) or `8` (SAS Transport v8, supports longer variable
  names).

- name:

  Member name for the dataset within the transport file. If `NULL`,
  derived from the file name. Maximum 8 characters for version 5.

## Value

Invisibly returns the file path.

## Details

### Tagged NA Handling

SAS supports 28 special missing values: `.` (system missing), `.A`
through `.Z`, and `._`. When exporting data with tagged NAs:

- **From SAS**
  ([`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md),
  [`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md)):
  Native special missing values are preserved in a full roundtrip.

- **From SPSS**
  ([`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md)):
  Tagged NAs are written as SAS special missing values. The original
  SPSS numeric codes (e.g., -9, -8) are not preserved, but the distinct
  missing value types are.

- **From Stata**
  ([`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md)):
  Stata extended missing values are mapped to their SAS equivalents.

### Limitations

SAS transport files do **not** store value labels. If your data has
value labels, consider using
[`write_spss()`](https://YannickDiehl.github.io/mariposa/reference/write_spss.md)
or
[`write_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/write_xlsx.md)
instead, which preserve full label metadata. Variable labels are
preserved.

## See also

[`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md)
and
[`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md)
for importing SAS files,
[`write_spss()`](https://YannickDiehl.github.io/mariposa/reference/write_spss.md)
for SPSS export,
[`write_stata()`](https://YannickDiehl.github.io/mariposa/reference/write_stata.md)
for Stata export,
[`write_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/write_xlsx.md)
for Excel export

Other data-export:
[`write_spss()`](https://YannickDiehl.github.io/mariposa/reference/write_spss.md),
[`write_stata()`](https://YannickDiehl.github.io/mariposa/reference/write_stata.md),
[`write_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/write_xlsx.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Roundtrip: read SAS transport, process, write back
data <- read_xpt("survey.xpt")
data_clean <- data[data$age >= 18, ]
write_xpt(data_clean, "survey_adults.xpt")

# Cross-format: SPSS to SAS transport
data <- read_spss("survey.sav")
write_xpt(data, "survey.xpt")
} # }
```
