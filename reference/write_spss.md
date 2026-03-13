# Export Data to SPSS Format

Writes a data frame to an SPSS `.sav` file, preserving variable labels,
value labels, and user-defined missing values. When exporting data that
was imported with
[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md)
(with `tag.na = TRUE`), the tagged NAs are automatically converted back
to SPSS user-defined missing values, enabling full roundtrip fidelity.

## Usage

``` r
write_spss(data, path, compress = c("byte", "none", "zsav"))
```

## Arguments

- data:

  A data frame to export. Columns of class `haven_labelled` will have
  their labels and missing value metadata written to the `.sav` file.

- path:

  Path to the output file. Must end in `.sav` or `.zsav`.

- compress:

  Compression type. One of `"byte"` (default, byte-level compression),
  `"none"` (no compression), or `"zsav"` (zlib compression, requires
  SPSS v21+).

## Value

Invisibly returns the file path.

## Details

### Tagged NA Roundtripping

Data imported via
[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md)
stores SPSS user-defined missing values as tagged NAs with an
`na_tag_map` attribute mapping tag characters to original codes (e.g.,
-9, -8). `write_spss()` reverses this process: tagged NAs are converted
back to their original numeric codes, and the SPSS user-defined missing
value specification is reconstructed so that the exported `.sav` file
has the same missing value definitions as the original.

### Cross-Format Export

When exporting data originally imported from Stata or SAS (with native
extended missing values like `.a`-`.z` or `.A`-`.Z`), these cannot be
represented as SPSS user-defined missing values. In this case, they are
written as system missing (regular `NA`) with a warning.

### When to Use This

Use `write_spss()` when you:

- Need to export processed survey data back to SPSS format

- Want to preserve user-defined missing value definitions

- Need roundtrip fidelity:
  [`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md)
  -\> processing -\> `write_spss()`

## See also

[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md)
for importing SPSS files,
[`write_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/write_xlsx.md)
for Excel export,
[`write_stata()`](https://YannickDiehl.github.io/mariposa/reference/write_stata.md)
for Stata export,
[`write_xpt()`](https://YannickDiehl.github.io/mariposa/reference/write_xpt.md)
for SAS transport export,
[`untag_na()`](https://YannickDiehl.github.io/mariposa/reference/untag_na.md),
[`strip_tags()`](https://YannickDiehl.github.io/mariposa/reference/strip_tags.md)

Other data-export:
[`write_stata()`](https://YannickDiehl.github.io/mariposa/reference/write_stata.md),
[`write_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/write_xlsx.md),
[`write_xpt()`](https://YannickDiehl.github.io/mariposa/reference/write_xpt.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Roundtrip: read SPSS, process, write back
data <- read_spss("survey.sav")
data_clean <- data[data$age >= 18, ]
write_spss(data_clean, "survey_adults.sav")

# Export with zlib compression (smaller file, requires SPSS v21+)
write_spss(data, "survey_compressed.zsav", compress = "zsav")
} # }
```
