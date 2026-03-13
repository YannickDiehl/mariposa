# Export Data to Stata Format

Writes a data frame to a Stata `.dta` file, preserving variable labels,
value labels, and missing value types. Tagged NAs from any source format
(SPSS, Stata, SAS) are written as Stata extended missing values (`.a`
through `.z`).

## Usage

``` r
write_stata(data, path, version = 14)
```

## Arguments

- data:

  A data frame to export. Columns of class `haven_labelled` will have
  their labels written to the `.dta` file.

- path:

  Path to the output file. Must end in `.dta`.

- version:

  Stata file version to use. Supported values: 8-15. Default is 14
  (Stata 14/15, supports Unicode). Use 13 for compatibility with older
  Stata versions.

## Value

Invisibly returns the file path.

## Details

### Tagged NA Handling

Stata natively supports 27 extended missing values: `.a` through `.z`
and `.` (system missing). When exporting data with tagged NAs:

- **From Stata**
  ([`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md)):
  Native extended missing values are preserved in a full roundtrip.

- **From SPSS**
  ([`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md)):
  Tagged NAs are written as Stata extended missing values (`.a`, `.b`,
  etc.). The original SPSS numeric codes (e.g., -9, -8) are not
  preserved in the Stata file, but the distinct missing value types and
  their labels are.

- **From SAS**
  ([`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md),
  [`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md)):
  SAS special missing values are mapped to their Stata equivalents.

### Limitations

Stata supports at most 27 distinct missing types per variable. SPSS data
with more than 27 tagged NA types in a single variable may lose some
distinctions (though this is extremely rare in practice).

## See also

[`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md)
for importing Stata files,
[`write_spss()`](https://YannickDiehl.github.io/mariposa/reference/write_spss.md)
for SPSS export,
[`write_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/write_xlsx.md)
for Excel export,
[`write_xpt()`](https://YannickDiehl.github.io/mariposa/reference/write_xpt.md)
for SAS transport export

Other data-export:
[`write_spss()`](https://YannickDiehl.github.io/mariposa/reference/write_spss.md),
[`write_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/write_xlsx.md),
[`write_xpt()`](https://YannickDiehl.github.io/mariposa/reference/write_xpt.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Roundtrip: read Stata, process, write back
data <- read_stata("survey.dta")
data_clean <- data[data$age >= 18, ]
write_stata(data_clean, "survey_adults.dta")

# Cross-format: SPSS to Stata
data <- read_spss("survey.sav")
write_stata(data, "survey.dta")

# Stata 13 compatibility
write_stata(data, "survey_v13.dta", version = 13)
} # }
```
