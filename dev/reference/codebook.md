# Create a Codebook for Your Data

`codebook()` creates an interactive HTML data dictionary that opens in
the RStudio Viewer. It shows you everything about your dataset at a
glance: variable names, types, labels, the empirical values found in the
data, value labels, and frequency counts.

Think of it as a professional "cheat sheet" for your dataset –
especially useful when working with labelled survey data imported from
SPSS.

## Usage

``` r
codebook(
  data,
  ...,
  weights = NULL,
  show.id = TRUE,
  show.type = TRUE,
  show.labels = TRUE,
  show.values = TRUE,
  show.freq = TRUE,
  max.values = 10,
  max.len = 50,
  sort.by.name = FALSE,
  file = NULL
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

- show.id:

  Show variable position number? (Default: TRUE)

- show.type:

  Show data type? (Default: TRUE)

- show.labels:

  Show variable labels? (Default: TRUE)

- show.values:

  Show empirical values and value labels? (Default: TRUE)

- show.freq:

  Show frequency counts? (Default: TRUE)

- max.values:

  Maximum number of values to display per variable before truncating or
  showing a range (Default: 10)

- max.len:

  Maximum character width for labels before truncation (Default: 50)

- sort.by.name:

  Sort variables alphabetically instead of by position? (Default: FALSE)

- file:

  Path to save the HTML codebook. If NULL (default), a temporary file is
  used and the codebook opens in the Viewer.

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

- **Label**: Variable label (from SPSS/Stata imports or manual
  assignment)

- **Values**: The empirical values found in the data

- **Value Labels**: Labels assigned to those values (if any)

- **Freq.**: Frequency count for each value

### When to Use This

Use `codebook()` when you:

- First receive a new dataset and want to understand its structure

- Work with SPSS-imported data and need to see all value labels

- Want to document your dataset for colleagues or publications

- Need to quickly see value distributions across variables

## See also

[`describe()`](https://YannickDiehl.github.io/mariposa/dev/reference/describe.md)
for detailed numeric summaries,
[`frequency()`](https://YannickDiehl.github.io/mariposa/dev/reference/frequency.md)
for detailed frequency tables

## Examples

``` r
# View the full codebook (opens in RStudio Viewer)
data(survey_data)
codebook(survey_data)

# Only trust-related variables
codebook(survey_data, starts_with("trust"))

# Save to file for sharing
codebook(survey_data, file = tempfile(fileext = ".html"))

# Compact console overview (without opening Viewer)
cb <- codebook(survey_data)
print(cb)
#> 
#> Codebook: survey_data
#> 16 variables | 2,500 observations | 15 labelled
#> Types: 3 dbl, 2 fct(2), 1 fct(3), 1 fct(5), 1 fct(6), 7 int, 1 ord(4)
#> -- Open HTML viewer for full codebook with values and frequencies

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
#>     Values: 1 - 2500
#> 
#> [2] age (numeric)
#>     Label: Age in years
#>     Values: 18 - 95
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
#>     Values: 800 - 8000
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
#>     Values: 0.70155162345618 - 1.39769840524532
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
```
