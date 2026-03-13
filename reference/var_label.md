# Get or Set Variable Labels

Retrieves or assigns variable labels (the `"label"` attribute) on
vectors or data frame columns. Variable labels describe what a variable
measures (e.g., "Age of respondent") and are commonly used in survey
data imported from SPSS, Stata, or SAS.

The function operates in two modes:

- **GET mode**: Called with bare variable names or no `...` arguments,
  returns existing labels.

- **SET mode**: Called with `name = "label"` pairs, assigns labels to
  variables and returns the modified data.

## Usage

``` r
var_label(data, ...)
```

## Arguments

- data:

  A data frame, tibble, or a single vector.

- ...:

  In GET mode: unquoted variable names (tidyselect supported). If empty,
  returns labels for all variables. In SET mode: named pairs where the
  name is a variable and the value is the label string. Use `NULL` to
  remove a label.

## Value

- **GET mode (vector input)**: A single character string (the label), or
  `NULL` if no label exists.

- **GET mode (data frame input)**: A named character vector of labels.
  Variables without labels return `NA`.

- **SET mode**: The modified data frame (invisibly), with labels
  assigned.

## Details

Variable labels are stored as the `"label"` attribute on each column,
which is the standard used by the `haven` package for SPSS/Stata/SAS
imports. These labels are preserved by mariposa's
[`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md),
[`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md),
and
[`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
functions.

### When to Use This

Use `var_label()` when you:

- Want to inspect what variables measure in imported survey data

- Need to add labels to manually created data before using
  [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)
  or
  [`write_spss()`](https://YannickDiehl.github.io/mariposa/reference/write_spss.md)

- Want to update or correct labels after import

## See also

[`val_labels()`](https://YannickDiehl.github.io/mariposa/reference/val_labels.md)
for value labels,
[`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)
for viewing all metadata,
[`copy_labels()`](https://YannickDiehl.github.io/mariposa/reference/copy_labels.md)
for preserving labels after dplyr operations

Other labels:
[`copy_labels()`](https://YannickDiehl.github.io/mariposa/reference/copy_labels.md),
[`drop_labels()`](https://YannickDiehl.github.io/mariposa/reference/drop_labels.md),
[`find_var()`](https://YannickDiehl.github.io/mariposa/reference/find_var.md),
[`set_na()`](https://YannickDiehl.github.io/mariposa/reference/set_na.md),
[`to_character()`](https://YannickDiehl.github.io/mariposa/reference/to_character.md),
[`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md),
[`to_labelled()`](https://YannickDiehl.github.io/mariposa/reference/to_labelled.md),
[`to_numeric()`](https://YannickDiehl.github.io/mariposa/reference/to_numeric.md),
[`unlabel()`](https://YannickDiehl.github.io/mariposa/reference/unlabel.md),
[`val_labels()`](https://YannickDiehl.github.io/mariposa/reference/val_labels.md)

## Examples

``` r
# GET: retrieve all variable labels
var_label(survey_data)
#>                                                id 
#>                                                NA 
#>                                               age 
#>                                    "Age in years" 
#>                                            gender 
#>                                          "Gender" 
#>                                            region 
#>                              "Region (East/West)" 
#>                                         education 
#>                  "Highest educational attainment" 
#>                                            income 
#>                  "Monthly household income (EUR)" 
#>                                        employment 
#>                               "Employment status" 
#>                             political_orientation 
#>         "Political orientation (1=left, 5=right)" 
#>                             environmental_concern 
#>           "Environmental concern (1=low, 5=high)" 
#>                                 life_satisfaction 
#> "Life satisfaction (1=dissatisfied, 5=satisfied)" 
#>                                  trust_government 
#>        "Trust in government (1=none, 5=complete)" 
#>                                       trust_media 
#>             "Trust in media (1=none, 5=complete)" 
#>                                     trust_science 
#>           "Trust in science (1=none, 5=complete)" 
#>                                   sampling_weight 
#>                                "Weighting factor" 
#>                                           stratum 
#>                         "Stratification variable" 
#>                                    interview_mode 
#>                                  "Interview mode" 

# GET: specific variables
var_label(survey_data, age, gender)
#>            age         gender 
#> "Age in years"       "Gender" 

# GET: from a single vector
var_label(survey_data$age)
#> [1] "Age in years"

# SET: assign labels
data <- var_label(survey_data,
  age = "Age of respondent",
  gender = "Gender identity"
)

# REMOVE: set to NULL
data <- var_label(data, age = NULL)
```
