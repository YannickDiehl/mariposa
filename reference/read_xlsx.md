# Read Excel Data with Label Reconstruction

Reads an Excel (`.xlsx`) file and returns a tibble. When the file was
created by
[`write_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/write_xlsx.md),
the accompanying "Labels" sheet is automatically detected and used to
reconstruct variable labels, value labels, and tagged NA metadata –
giving you the same labelled structure as the original data.

For plain Excel files (without a "Labels" sheet), this function works
like a convenient wrapper around the `openxlsx2` reader, returning a
clean tibble.

## Usage

``` r
read_xlsx(path, sheet = NULL, labels = TRUE, verbose = FALSE, ...)
```

## Arguments

- path:

  Path to the `.xlsx` file.

- sheet:

  Sheet to read. `NULL` (default) auto-detects: reads the "Data" sheet
  if present, otherwise the first non-metadata sheet. Can be a sheet
  name (character) or index (integer).

- labels:

  If `TRUE` (default), detects a "Labels" sheet and reconstructs
  `haven_labelled` columns, factor levels, variable labels, and tagged
  NA metadata. Set to `FALSE` to read raw data without label
  reconstruction.

- verbose:

  If `TRUE`, prints a summary of reconstructed labels and tagged NA
  mappings.

- ...:

  Additional arguments (currently unused).

## Value

A tibble. When a mariposa-formatted Labels sheet is detected:

- Numeric columns with value labels are returned as `haven_labelled`
  vectors

- Factor columns are reconstructed with their original levels

- Variable labels are attached via `attr(x, "label")`

- Tagged NAs are restored with `na_tag_map` and `na_tag_format`
  attributes

## Details

### Roundtrip Workflow

`read_xlsx()` is designed to work seamlessly with
[`write_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/write_xlsx.md):

    write_xlsx(data, "survey.xlsx")   # Exports data + Labels sheet
    data2 <- read_xlsx("survey.xlsx") # Reconstructs labels from Labels sheet

The reconstructed data will have the same variable labels, value labels,
and tagged NA codes as the original. System NAs (empty cells in Excel)
remain as regular `NA`.

### File Format Detection

`read_xlsx()` auto-detects the type of mariposa export:

- **data.frame export** ("Data" + "Labels" sheets): Reads the "Data"
  sheet and applies labels.

- **list export** (multiple data sheets + "Labels" with "Sheet" column):
  Reads the specified or first data sheet and applies matching labels.

- **codebook export** ("Overview" + "Codebook" sheets): Warns that this
  is a documentation file and reads it as a plain table.

- **plain Excel**: No Labels sheet detected; returns raw data.

### When to Use This

Use `read_xlsx()` when you:

- Want to read back data exported with
  [`write_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/write_xlsx.md)

- Need a quick way to import any `.xlsx` file as a tibble

- Want to share labelled survey data via Excel and read it back with all
  metadata intact

## See also

[`write_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/write_xlsx.md)
for exporting data with labels,
[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md),
[`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md),
[`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md)
for importing from statistical software

Other data-import:
[`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md),
[`read_por()`](https://YannickDiehl.github.io/mariposa/reference/read_por.md),
[`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md),
[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md),
[`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md),
[`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md),
[`strip_tags()`](https://YannickDiehl.github.io/mariposa/reference/strip_tags.md),
[`untag_na()`](https://YannickDiehl.github.io/mariposa/reference/untag_na.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Read back a mariposa-exported file with full label reconstruction
data <- read_xlsx("survey_export.xlsx")
attr(data$gender, "labels")   # Value labels restored
attr(data$gender, "label")    # Variable label restored

# Read a plain Excel file
data <- read_xlsx("plain_data.xlsx")

# Read a specific sheet
data <- read_xlsx("multi_sheet.xlsx", sheet = "Sheet2")

# Skip label reconstruction
raw <- read_xlsx("survey_export.xlsx", labels = FALSE)
} # }
```
