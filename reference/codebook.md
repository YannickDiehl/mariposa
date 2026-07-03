# Create a Codebook for Your Data

`codebook()` creates an interactive HTML data dictionary that opens in
the RStudio Viewer. It shows you everything about your dataset at a
glance: variable names, types, labels, the empirical values found in the
data, value labels, and frequency counts.

Think of it as a professional "cheat sheet" for your dataset –
especially useful when working with labelled survey data imported from
SPSS, Stata, or SAS.

## Usage

``` r
codebook(
  data,
  ...,
  weights = NULL,
  show_id = TRUE,
  show_type = TRUE,
  show_labels = TRUE,
  show_values = TRUE,
  show_freq = TRUE,
  show_na = TRUE,
  show_unused = FALSE,
  max_values = 10,
  max_len = 50,
  sort_by_name = FALSE,
  file = NULL,
  view = interactive()
)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- ...:

  Optional: specific variables to include. If empty, all variables are
  shown. Supports tidyselect helpers like `starts_with("trust")`.

- weights:

  Optional survey weights for weighted frequency calculations

- show_id:

  Show variable position number? (Default: TRUE)

- show_type:

  Show data type? (Default: TRUE)

- show_labels:

  Show variable labels? (Default: TRUE)

- show_values:

  Show empirical values and value labels? (Default: TRUE)

- show_freq:

  Show frequency counts? (Default: TRUE)

- show_na:

  Show tagged missing value types in the codebook? (Default: TRUE). When
  data was imported with
  [`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md),
  [`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md),
  [`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md),
  or
  [`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md)
  using tagged NAs, they are displayed with their original missing value
  codes, labels, and frequencies below the valid values, separated by a
  thin gray line.

- show_unused:

  Show all defined value labels, even those with zero observations?
  (Default: FALSE). Useful for seeing the full codebook including
  response options that no respondent selected.

- max_values:

  Maximum number of values to display per variable before truncating or
  showing a range (Default: 10)

- max_len:

  Maximum character width for labels before truncation (Default: 50)

- sort_by_name:

  Sort variables alphabetically instead of by position? (Default: FALSE)

- file:

  Path to save the HTML codebook. If NULL (default), no file is written
  unless the codebook is opened in the Viewer (then a temporary file is
  used). The directory of `file` must already exist.

- view:

  Open the HTML codebook in the RStudio Viewer (or browser)? Defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html), so
  interactive sessions open the Viewer and scripts/tests do not. Set
  `view = FALSE` to suppress the Viewer side effect entirely; `file =`
  writing is unaffected by this argument.

## Value

Invisibly returns a list of class `"codebook"` containing:

- codebook:

  Tibble with one row per variable and all metadata

- data_info:

  List with dataset-level information (name, nrow, ncol, etc.)

- html:

  The generated HTML as an htmltools object

- weights:

  Name of the weight variable, or NULL

- options:

  List of all display options

- frequencies:

  Named list of frequency tables per variable

## Details

### What the Codebook Shows

For each variable, the codebook displays (depending on options):

- **ID**: Column position in the dataset

- **Name**: Variable name (in monospace font)

- **Type**: Data type (numeric, factor, ordered factor, haven_labelled,
  etc.)

- **Label**: Variable label (from SPSS/Stata/SAS imports or manual
  assignment)

- **Values**: The empirical values found in the data

- **Value Labels**: Labels assigned to those values (if any)

- **Freq.**: Frequency count for each value

### When to Use This

Use `codebook()` when you:

- First receive a new dataset and want to understand its structure

- Work with labelled data (SPSS, Stata, SAS) and need to see all value
  labels

- Want to document your dataset for colleagues or publications

- Need to quickly see value distributions across variables

## See also

[`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
for detailed numeric summaries,
[`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
for detailed frequency tables,
[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md),
[`read_por()`](https://YannickDiehl.github.io/mariposa/reference/read_por.md),
[`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md),
[`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md),
[`read_xpt()`](https://YannickDiehl.github.io/mariposa/reference/read_xpt.md)
for importing data with tagged NAs

## Examples

``` r
data(survey_data)

# Compact console overview (no Viewer)
cb <- codebook(survey_data)
print(cb)
#> 
#> Codebook: survey_data
#> 16 variables | 2,500 observations | 15 labelled
#> Types: 3 dbl, 2 fct(2), 1 fct(3), 1 fct(5), 1 fct(6), 7 int, 1 ord(4)
#> -- Use summary() for details or codebook(..., view = TRUE) for the HTML viewer

# Detailed console output
summary(cb)
#> 
#> Codebook
#> --------
#>   Dataset: survey_data
#>   Observations: 2,500
#>   Variables: 16 (10 numeric, 6 factor, 0 character)
#>   Variables with labels: 15
#>   Variables with missing data: 7
#> 
#> Variable Details:
#> -------------------------------------------------- 
#> 
#> [1] id (integer)
#>     Values: 1 - 2500 (2500 distinct)
#> 
#> [2] age (numeric)
#>     Label: Age in years
#>     Values: 18 - 95 (78 distinct)
#> 
#> [3] gender (factor)
#>     Label: Gender
#>     Values: Male, Female
#>     Value labels:
#>       Male = Male
#>       Female = Female
#> 
#> [4] region (factor)
#>     Label: Region (East/West)
#>     Values: East, West
#>     Value labels:
#>       East = East
#>       West = West
#> 
#> [5] education (ordered factor)
#>     Label: Highest educational attainment
#>     Values: Basic Secondary, Intermediate Secondary, Academic Secondary, Univer...
#>     Value labels:
#>       Basic Secondary = Basic Secondary
#>       Intermediate Secondary = Intermediate Secondary
#>       Academic Secondary = Academic Secondary
#>       University = University
#> 
#> [6] income (numeric)
#>     Label: Monthly household income (EUR)
#>     Values: 800 - 8000 (73 distinct)
#> 
#> [7] employment (factor)
#>     Label: Employment status
#>     Values: Student, Employed, Unemployed, Retired, Other
#>     Value labels:
#>       Student = Student
#>       Employed = Employed
#>       Unemployed = Unemployed
#>       Retired = Retired
#>       Other = Other
#> 
#> [8] political_orientation (integer)
#>     Label: Political orientation (1=left, 5=right)
#>     Values: 1, 2, 3, 4, 5
#> 
#> [9] environmental_concern (integer)
#>     Label: Environmental concern (1=low, 5=high)
#>     Values: 1, 2, 3, 4, 5
#> 
#> [10] life_satisfaction (integer)
#>     Label: Life satisfaction (1=dissatisfied, 5=satisfied)
#>     Values: 1, 2, 3, 4, 5
#> 
#> [11] trust_government (integer)
#>     Label: Trust in government (1=none, 5=complete)
#>     Values: 1, 2, 3, 4, 5
#> 
#> [12] trust_media (integer)
#>     Label: Trust in media (1=none, 5=complete)
#>     Values: 1, 2, 3, 4, 5
#> 
#> [13] trust_science (integer)
#>     Label: Trust in science (1=none, 5=complete)
#>     Values: 1, 2, 3, 4, 5
#> 
#> [14] sampling_weight (numeric)
#>     Label: Weighting factor
#>     Values: 0.7016 - 1.398 (2500 distinct)
#> 
#> [15] stratum (factor)
#>     Label: Stratification variable
#>     Values: East_Middle, East_Old, East_Young, West_Middle, West_Old, West_Young
#>     Value labels:
#>       East_Middle = East_Middle
#>       East_Old = East_Old
#>       East_Young = East_Young
#>       West_Middle = West_Middle
#>       West_Old = West_Old
#>       West_Young = West_Young
#> 
#> [16] interview_mode (factor)
#>     Label: Interview mode
#>     Values: Face-to-face, Telephone, Online
#>     Value labels:
#>       Face-to-face = Face-to-face
#>       Telephone = Telephone
#>       Online = Online

# \donttest{
# Full codebook (opens in RStudio Viewer when interactive)
codebook(survey_data)

# Only trust-related variables
codebook(survey_data, starts_with("trust"))

# Save to file for sharing
codebook(survey_data, file = tempfile(fileext = ".html"))
# }
```
