# Importing and Exporting Data

``` r
library(mariposa)
library(dplyr)
data(survey_data)
```

## Overview

Survey data lives in many formats — SPSS (.sav), Stata (.dta), SAS
(.sas7bdat), and Excel (.xlsx). mariposa reads all of them and preserves
the metadata that makes survey data special: variable labels, value
labels, and missing value definitions.

| Function                                                                          | Format        | Notes                                             |
|-----------------------------------------------------------------------------------|---------------|---------------------------------------------------|
| [`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md)   | SPSS .sav     | Tagged NA support for user-defined missing values |
| [`read_por()`](https://YannickDiehl.github.io/mariposa/reference/read_por.md)     | SPSS .por     | Portable SPSS format                              |
| [`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md) | Stata .dta    | Extended missing values                           |
| [`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md)     | SAS .sas7bdat | Catalog file support for labels                   |
| [`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md)     | SAS .xpt      | SAS transport format                              |
| [`read_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/read_xlsx.md)   | Excel .xlsx   | Label reconstruction from metadata sheets         |

For export, mariposa writes data back to statistical formats with full
preservation of labels and missing values:

| Function                                                                            | Format      | Notes                                               |
|-------------------------------------------------------------------------------------|-------------|-----------------------------------------------------|
| [`write_spss()`](https://YannickDiehl.github.io/mariposa/reference/write_spss.md)   | SPSS .sav   | Tagged NA roundtripping                             |
| [`write_stata()`](https://YannickDiehl.github.io/mariposa/reference/write_stata.md) | Stata .dta  | Label preservation                                  |
| [`write_xpt()`](https://YannickDiehl.github.io/mariposa/reference/write_xpt.md)     | SAS .xpt    | SAS transport format                                |
| [`write_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/write_xlsx.md)   | Excel .xlsx | Multiple export modes (data, codebook, frequencies) |

## Reading SPSS Files

### Basic Import

SPSS is the most common format in survey research.
[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md)
reads .sav files and preserves all metadata:

``` r
# Read an SPSS file
data <- read_spss("survey_2024.sav")
```

The result is a tibble with `haven_labelled` columns. Each column
carries its variable label, value labels, and missing value definitions
as attributes.

### Tagged Missing Values

SPSS allows researchers to define multiple types of missing values
(e.g., -9 = “refused”, -8 = “don’t know”, -7 = “not applicable”). By
default,
[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md)
preserves these as *tagged NAs* — special NA values that remember which
missing code they came from:

``` r
# Tagged NAs are on by default
data <- read_spss("survey.sav", tag.na = TRUE)

# Turn off if you just want regular NAs
data <- read_spss("survey.sav", tag.na = FALSE)
```

### Working with Tagged NAs

Once imported, you can inspect the tagged NAs:

``` r
# See a breakdown of missing types
na_frequencies(data, q1, q2, q3)

# Convert tagged NAs back to their original codes
data_with_codes <- untag_na(data, q1, q2)

# Remove tags but keep NAs
data_clean <- strip_tags(data)
```

[`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md)
is particularly useful for understanding response patterns — it shows
you how many respondents refused, said “don’t know”, or skipped each
question.

## Reading Other Formats

### Stata

``` r
data <- read_stata("survey.dta")
```

Stata files carry variable labels and value labels, similar to SPSS.
Extended missing values (.a through .z) are preserved.

### SAS

``` r
# SAS data file
data <- read_sas("survey.sas7bdat")

# With a separate catalog file for labels
data <- read_sas("survey.sas7bdat", catalog_file = "formats.sas7bcat")

# SAS transport format
data <- read_xpt("survey.xpt")
```

### SPSS Portable

``` r
data <- read_por("survey.por")
```

### Excel

``` r
# Basic Excel import
data <- read_xlsx("survey.xlsx")

# Excel file with label metadata (exported by write_xlsx)
data <- read_xlsx("survey.xlsx", label_sheet = "labels")
```

[`read_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/read_xlsx.md)
can reconstruct variable and value labels from a metadata sheet,
enabling a full roundtrip through Excel format.

## Inspecting Imported Data

After importing, use
[`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)
to get an interactive overview of your data:

``` r
codebook(survey_data)
```

The codebook displays in the RStudio Viewer and shows:

- Variable names, types, and positions
- Variable labels (descriptions)
- Value labels with frequency counts
- Missing value breakdown (tagged NAs)
- Empirical value ranges for numeric variables

For a quick look at variable names and labels without leaving the
console, use
[`find_var()`](https://YannickDiehl.github.io/mariposa/reference/find_var.md):

``` r
# Find variables related to "trust"
find_var(survey_data, "trust")
#>   col             name                                    label
#> 1  11 trust_government Trust in government (1=none, 5=complete)
#> 2  12      trust_media      Trust in media (1=none, 5=complete)
#> 3  13    trust_science    Trust in science (1=none, 5=complete)

# Search by variable label
find_var(survey_data, "satisfaction", search = "label")
#>   col              name                                           label
#> 1  10 life_satisfaction Life satisfaction (1=dissatisfied, 5=satisfied)
```

## Exporting Data

### Writing SPSS Files

[`write_spss()`](https://YannickDiehl.github.io/mariposa/reference/write_spss.md)
creates .sav files with full preservation of labels and missing values.
If your data contains tagged NAs (from
[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md)),
they are converted back to SPSS user-defined missing values:

``` r
# Basic export
write_spss(survey_data, "output.sav")

# With compression options
write_spss(survey_data, "output.sav", compress = "zsav")  # smaller file
```

### Writing Stata Files

``` r
write_stata(survey_data, "output.dta")
```

### Writing SAS Transport Files

``` r
write_xpt(survey_data, "output.xpt")
```

### Writing Excel Files

[`write_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/write_xlsx.md)
supports multiple export modes:

``` r
# Export data only
write_xlsx(survey_data, "output.xlsx")

# Export a codebook
cb <- codebook(survey_data)
write_xlsx(cb, "codebook.xlsx")

# Export frequency tables
freq <- frequency(survey_data, education, employment)
write_xlsx(freq, "frequencies.xlsx")
```

## Roundtripping: SPSS → R → SPSS

A key strength of mariposa is lossless roundtripping. Data imported from
SPSS can be exported back without losing any information:

``` r
# 1. Import from SPSS
original <- read_spss("survey.sav")

# 2. Work with the data in R
processed <- original %>%
  filter(age >= 18) %>%
  mutate(age_group = rec(., age, rules = "18:29=1; 30:49=2; 50:99=3"))

# 3. Export back to SPSS
write_spss(processed, "survey_processed.sav")

# Variable labels, value labels, and missing value definitions
# are all preserved in the exported file.
```

## Practical Tips

1.  **Always use
    [`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md)
    over
    [`haven::read_sav()`](https://haven.tidyverse.org/reference/read_spss.html)**
    when your SPSS file uses user-defined missing values. The tagged NA
    system preserves the distinction between “refused” and “don’t know”
    responses.

2.  **Inspect before analyzing.** After import, run
    [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)
    or
    [`find_var()`](https://YannickDiehl.github.io/mariposa/reference/find_var.md)
    to understand what variables are available and how they are coded.

3.  **Keep tagged NAs during analysis.** All mariposa functions handle
    tagged NAs correctly — they are treated as missing values in
    computations but retain their type information.

4.  **Use
    [`strip_tags()`](https://YannickDiehl.github.io/mariposa/reference/strip_tags.md)
    when handing data to non-mariposa functions.** Some R functions may
    not handle tagged NAs correctly. Strip the tags first to convert
    them to regular NAs.

5.  **Prefer
    [`write_spss()`](https://YannickDiehl.github.io/mariposa/reference/write_spss.md)
    for SPSS users.** The roundtripping ensures your colleagues can open
    the file in SPSS with all metadata intact.

## Summary

1.  [`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md),
    [`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md),
    [`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md),
    [`read_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/read_xlsx.md)
    import data with full metadata preservation
2.  Tagged NAs preserve SPSS user-defined missing value types
3.  [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)
    and
    [`find_var()`](https://YannickDiehl.github.io/mariposa/reference/find_var.md)
    help you understand imported data quickly
4.  [`write_spss()`](https://YannickDiehl.github.io/mariposa/reference/write_spss.md),
    [`write_stata()`](https://YannickDiehl.github.io/mariposa/reference/write_stata.md),
    [`write_xpt()`](https://YannickDiehl.github.io/mariposa/reference/write_xpt.md),
    [`write_xlsx()`](https://YannickDiehl.github.io/mariposa/reference/write_xlsx.md)
    export with label and missing value roundtripping
5.  The full import-export cycle is lossless for SPSS data

## Next Steps

- Learn how to work with labels and missing values — see
  [`vignette("labels-and-missing-values")`](https://YannickDiehl.github.io/mariposa/articles/labels-and-missing-values.md)
- Transform and recode variables — see
  [`vignette("data-transformation")`](https://YannickDiehl.github.io/mariposa/articles/data-transformation.md)
- Start analyzing your data — see
  [`vignette("descriptive-statistics")`](https://YannickDiehl.github.io/mariposa/articles/descriptive-statistics.md)
