# Detailed Console Summary of the Codebook

Creates a detailed summary object for console display, showing
per-variable information with toggleable sections. Use this when you
want detailed text output without opening the HTML Viewer.

## Usage

``` r
# S3 method for class 'codebook'
summary(
  object,
  overview = TRUE,
  variable_details = TRUE,
  value_labels = TRUE,
  digits = 3,
  ...
)
```

## Arguments

- object:

  A codebook object from
  [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)

- overview:

  Show dataset-level overview? (Default: TRUE)

- variable_details:

  Show per-variable detail blocks? (Default: TRUE)

- value_labels:

  Show value labels within variable details? (Default: TRUE)

- digits:

  Number of decimal places for numeric output (Default: 3)

- ...:

  Additional arguments (ignored)

## Value

A summary.codebook object (printed by
[`print.summary.codebook()`](https://YannickDiehl.github.io/mariposa/reference/print.summary.codebook.md))

## Examples

``` r
data(survey_data)
cb <- codebook(survey_data)
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
summary(cb, value_labels = FALSE)
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
#> 
#> [4] region (factor)
#>     Label: Region (East/West)
#>     Values: East, West
#> 
#> [5] education (ordered factor)
#>     Label: Highest educational attainment
#>     Values: Basic Secondary, Intermediate Secondary, Academic Secondary, Univer...
#> 
#> [6] income (numeric)
#>     Label: Monthly household income (EUR)
#>     Values: 800 - 8000
#> 
#> [7] employment (factor)
#>     Label: Employment status
#>     Values: Student, Employed, Unemployed, Retired, Other
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
#> 
#> [16] interview_mode (factor)
#>     Label: Interview mode
#>     Values: Face-to-face, Telephone, Online
```
