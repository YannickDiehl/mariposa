# Export Data to Excel with Label Support

Writes data to an Excel (`.xlsx`) file with support for variable labels,
value labels, and tagged NA metadata. When exporting labelled survey
data, a "Labels" reference sheet is automatically included so that
reviewers can look up what each numeric code means – even without R
access.

Three input types are supported:

1.  **Data frame**: Exports the data plus a "Labels" reference sheet
    with variable labels, value labels, and missing value codes.

2.  **Codebook object**: Exports the codebook as it appears in the HTML
    viewer – with values, value labels, and frequencies stacked inside
    each cell, tagged NAs separated by a line, and an overview sheet
    with dataset metadata.

3.  **Named list**: Each list element becomes a separate sheet. Elements
    can be data frames,
    [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
    results, or
    [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)
    results – types can be mixed freely. For data frame elements, a
    combined "Labels" sheet is appended.

## Usage

``` r
write_xlsx(x, file, ...)

# S3 method for class 'frequency'
write_xlsx(x, file, overwrite = TRUE, ...)
```

## Arguments

- x:

  Object to export: a data frame, a
  [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)
  result, or a named list of data frames.

- file:

  Path to the output `.xlsx` file. Must end in `.xlsx`.

- ...:

  Additional arguments passed to methods:

  `labels`

  :   (data.frame and list methods) Include a "Labels" reference sheet?
      Default: `TRUE`

  `frequencies`

  :   (codebook method) Include per-variable frequency sheets? Default:
      `FALSE`

- overwrite:

  Overwrite existing file? Default: `TRUE`.

## Value

Invisibly returns the file path.

## Details

### Data Frame Export

The "Labels" sheet provides a complete lookup table for all variable and
value labels in your data, structured in long format. The "Type" column
distinguishes between regular value labels ("valid") and tagged missing
value labels ("missing"), making it easy to filter in Excel.

Data values are written as their underlying numeric or character codes
(not as label text), preserving the original coding scheme.

### Codebook Export

The codebook export reproduces the HTML codebook layout: each variable
occupies one row, with values, value labels, and frequencies stacked
within their cells using line breaks. Tagged NAs are shown below a
separator line, matching the visual style of
[`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)
in the RStudio Viewer.

### When to Use This

Use `write_xlsx()` when you:

- Need to share survey data with non-R users who need label context

- Want to export a codebook for manual review or documentation

- Need to combine multiple tables (data, codebook, frequencies) in one
  Excel file

## Methods (by class)

- `write_xlsx(frequency)`: Export frequency tables to Excel

  Exports one or more frequency tables to a single Excel sheet,
  mirroring the console print output. Each variable gets a header row,
  stats summary, column headers, data rows, and total rows. Multiple
  variables are separated by 3 blank rows.

## See also

[`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)
for generating codebook objects,
[`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
for frequency tables,
[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md),
[`read_por()`](https://YannickDiehl.github.io/mariposa/reference/read_por.md),
[`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md),
[`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md),
[`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md),
[`read_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/read_xlsx.md)
for importing labelled data

Other data-export:
[`write_spss()`](https://YannickDiehl.github.io/mariposa/reference/write_spss.md),
[`write_stata()`](https://YannickDiehl.github.io/mariposa/reference/write_stata.md),
[`write_xpt()`](https://YannickDiehl.github.io/mariposa/reference/write_xpt.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Export data with automatic label reference sheet
write_xlsx(survey_data, "survey_export.xlsx")

# Export a codebook (reproduces HTML layout in Excel)
codebook(survey_data) |> write_xlsx("codebook.xlsx")

# Export a codebook with per-variable frequency sheets
codebook(survey_data) |> write_xlsx("codebook_full.xlsx", frequencies = TRUE)

# Export frequency tables (single or multiple variables)
frequency(survey_data, gender) |> write_xlsx("freq.xlsx")
frequency(survey_data, gender, life_satisfaction, region) |>
  write_xlsx("freq_multi.xlsx")

# Multi-sheet export (mixed types: data frames, frequencies, codebooks)
write_xlsx(
  list(
    "Frequencies" = frequency(survey_data, gender, life_satisfaction),
    "Codebook"    = codebook(survey_data),
    "Data"        = survey_data
  ),
  "combined.xlsx"
)
} # }

if (FALSE) { # \dontrun{
# Single variable
frequency(survey_data, gender) |> write_xlsx("freq.xlsx")

# Multiple variables on one sheet
frequency(survey_data, gender, life_satisfaction, region) |>
  write_xlsx("freq_multi.xlsx")
} # }
```
