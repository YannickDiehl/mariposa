# Labels and Missing Values

``` r
library(mariposa)
library(dplyr)
data(survey_data)
```

## Overview

Survey data from SPSS, Stata, and SAS stores two kinds of labels:

- **Variable labels** describe what a variable measures (e.g., `gender`
  → “Respondent’s gender”)
- **Value labels** map numeric codes to text (e.g., `1` → “Male”, `2` →
  “Female”)

In R, these labels are stored as attributes on `haven_labelled` columns.
mariposa provides 10 functions for inspecting, modifying, and converting
labelled data — plus tools for declaring missing values and searching
variables.

| Function                                                                              | Purpose                                   |
|---------------------------------------------------------------------------------------|-------------------------------------------|
| [`var_label()`](https://YannickDiehl.github.io/mariposa/reference/var_label.md)       | Get or set variable labels                |
| [`val_labels()`](https://YannickDiehl.github.io/mariposa/reference/val_labels.md)     | Get or set value labels                   |
| [`find_var()`](https://YannickDiehl.github.io/mariposa/reference/find_var.md)         | Search variables by name or label pattern |
| [`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md)         | Convert labelled → factor                 |
| [`to_character()`](https://YannickDiehl.github.io/mariposa/reference/to_character.md) | Convert labelled → character              |
| [`to_numeric()`](https://YannickDiehl.github.io/mariposa/reference/to_numeric.md)     | Convert factor/labelled → numeric         |
| [`to_labelled()`](https://YannickDiehl.github.io/mariposa/reference/to_labelled.md)   | Convert factor/character → labelled       |
| [`set_na()`](https://YannickDiehl.github.io/mariposa/reference/set_na.md)             | Declare values as missing (tagged NAs)    |
| [`unlabel()`](https://YannickDiehl.github.io/mariposa/reference/unlabel.md)           | Strip all label metadata                  |
| [`copy_labels()`](https://YannickDiehl.github.io/mariposa/reference/copy_labels.md)   | Restore labels after dplyr operations     |
| [`drop_labels()`](https://YannickDiehl.github.io/mariposa/reference/drop_labels.md)   | Remove unused value labels                |

## Inspecting Labels

### Variable Labels

Variable labels describe what each column contains. Use
[`var_label()`](https://YannickDiehl.github.io/mariposa/reference/var_label.md)
to retrieve them:

``` r
# Get labels for specific variables
var_label(survey_data, gender, education, life_satisfaction)
#>                                            gender 
#>                                          "Gender" 
#>                                         education 
#>                  "Highest educational attainment" 
#>                                 life_satisfaction 
#> "Life satisfaction (1=dissatisfied, 5=satisfied)"
```

``` r
# Get labels for all variables
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
```

### Value Labels

Value labels map numeric codes to meaningful text. Use
[`val_labels()`](https://YannickDiehl.github.io/mariposa/reference/val_labels.md)
to retrieve them:

``` r
# Get value labels for a single variable
val_labels(survey_data, gender)
#> NULL
```

``` r
# Get value labels for multiple variables
val_labels(survey_data, education, employment)
#> $education
#> NULL
#> 
#> $employment
#> NULL
```

### Finding Variables

SPSS datasets often have cryptic variable names like `q104a_1` or `v23`.
Use
[`find_var()`](https://YannickDiehl.github.io/mariposa/reference/find_var.md)
to search by name or label:

``` r
# Search in both names and labels (default)
find_var(survey_data, "trust")
#>   col             name                                    label
#> 1  11 trust_government Trust in government (1=none, 5=complete)
#> 2  12      trust_media      Trust in media (1=none, 5=complete)
#> 3  13    trust_science    Trust in science (1=none, 5=complete)
```

``` r
# Search only in variable labels
find_var(survey_data, "satisfaction", search = "label")
#>   col              name                                           label
#> 1  10 life_satisfaction Life satisfaction (1=dissatisfied, 5=satisfied)
```

``` r
# Search only in variable names
find_var(survey_data, "age|income", search = "name")
#>   col   name                          label
#> 1   2    age                   Age in years
#> 2   6 income Monthly household income (EUR)
```

## Setting Labels

### Setting Variable Labels

``` r
# Set labels for specific variables
labeled_data <- var_label(survey_data,
  age = "Age of respondent in years",
  income = "Monthly net income in euros"
)

# Verify
var_label(labeled_data, age, income)
#>                           age                        income 
#>  "Age of respondent in years" "Monthly net income in euros"
```

### Setting Value Labels

``` r
# Set value labels
labeled_data <- val_labels(survey_data,
  gender = c("Male" = 1, "Female" = 2)
)

# Verify
val_labels(labeled_data, gender)
#>   Male Female 
#>      1      2
```

``` r
# Add labels without replacing existing ones
labeled_data <- val_labels(survey_data,
  gender = c("Diverse" = 3),
  .add = TRUE
)

val_labels(labeled_data, gender)
#> Diverse 
#>       3
```

## Converting Between Formats

Survey data often needs conversion between labelled, factor, character,
and numeric formats depending on the analysis.

### Labelled → Factor

Use
[`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md)
when you need factors for plotting or statistical models:

``` r
# Convert gender from labelled to factor
factor_data <- to_label(survey_data, gender, education)

# Check the result
class(factor_data$gender)
#> [1] "factor"
levels(factor_data$gender)
#> [1] "Male"   "Female"
head(factor_data$gender)
#> [1] Female Male   Male   Female Male   Female
#> Levels: Male Female
```

``` r
# Create ordered factors (useful for ordinal variables)
ordered_data <- to_label(survey_data, education, ordered = TRUE)
levels(ordered_data$education)
#> [1] "Basic Secondary"        "Intermediate Secondary" "Academic Secondary"    
#> [4] "University"
```

### Labelled → Character

Use
[`to_character()`](https://YannickDiehl.github.io/mariposa/reference/to_character.md)
for string-based operations:

``` r
char_data <- to_character(survey_data, gender, region)
head(char_data$gender)
#> [1] "Female" "Male"   "Male"   "Female" "Male"   "Female"
```

### Factor → Numeric

Use
[`to_numeric()`](https://YannickDiehl.github.io/mariposa/reference/to_numeric.md)
to convert factors or labelled vectors back to numbers:

``` r
# First convert to factor, then back to numeric
factor_data <- to_label(survey_data, education)

# Parse numeric values from factor levels
numeric_data <- to_numeric(factor_data, education)
head(numeric_data$education)
#> [1] 2 3 3 1 1 2
```

### Numeric/Factor → Labelled

Use
[`to_labelled()`](https://YannickDiehl.github.io/mariposa/reference/to_labelled.md)
to add labels to plain numeric or factor columns:

``` r
# Convert a factor back to haven_labelled
plain_data <- data.frame(
  gender = factor(c("Male", "Female", "Male")),
  score = c(3, 4, 2)
)

labelled_data <- to_labelled(plain_data, gender)
class(labelled_data$gender)
#> [1] "haven_labelled" "vctrs_vctr"     "double"
```

## Declaring Missing Values

### Setting Values as Missing

Use
[`set_na()`](https://YannickDiehl.github.io/mariposa/reference/set_na.md)
to declare specific numeric codes as missing values:

``` r
# After importing SPSS data where -9 = refused, -8 = don't know
data <- read_spss("survey.sav", tag.na = FALSE)

# Declare -9 and -8 as missing across all numeric columns
data <- set_na(data, -9, -8, tag = TRUE)

# Declare different missing codes for specific variables
data <- set_na(data, q1 = c(-9, -8), q2 = c(99))
```

When `tag = TRUE` (the default), each missing code becomes a distinct
tagged NA, so you can later distinguish “refused” from “don’t know”
responses.

### Removing All Labels

Use
[`unlabel()`](https://YannickDiehl.github.io/mariposa/reference/unlabel.md)
when you need plain numeric data without any label metadata:

``` r
# Strip all labels from entire dataset
plain_data <- unlabel(survey_data)

# Strip labels from specific variables only
plain_data <- unlabel(survey_data, gender, education)
```

This converts `haven_labelled` columns to plain `numeric` or
`character`, tagged NAs to regular `NA`, and removes all label
attributes.

## Preserving Labels Through Pipelines

### The Problem

dplyr operations like
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html),
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html), and
[`select()`](https://dplyr.tidyverse.org/reference/select.html) can
strip label attributes from columns:

``` r
filtered <- survey_data %>%
  filter(age >= 30) %>%
  select(gender, education, income)

# Labels may be lost after certain operations
```

### The Solution: copy_labels()

Use
[`copy_labels()`](https://YannickDiehl.github.io/mariposa/reference/copy_labels.md)
to restore labels from the original data:

``` r
# Restore labels from the source dataset
filtered <- copy_labels(filtered, survey_data)

# Verify labels are back
var_label(filtered, gender, education)
#>                           gender                        education 
#>                         "Gender" "Highest educational attainment"
```

### Cleaning Unused Labels

After filtering, some value label categories may no longer appear in the
data. Use
[`drop_labels()`](https://YannickDiehl.github.io/mariposa/reference/drop_labels.md)
to clean them up:

``` r
# Subset to one region only
subset_data <- survey_data %>% filter(region == 1)

# Remove value labels for regions that are no longer in the data
clean_data <- drop_labels(subset_data, region)
val_labels(clean_data, region)
#> NULL
```

## Complete Example

A typical workflow for preparing SPSS data for analysis:

``` r
# 1. Import SPSS file
data <- read_spss("survey_2024.sav")

# 2. Explore what's in the data
codebook(data)
find_var(data, "satisf")
find_var(data, "trust")

# 3. Check labels
var_label(data, q1, q2, q3)
val_labels(data, q1)

# 4. Rename variables for clarity
data <- data %>%
  rename(life_satisfaction = q1, income = q2, education = q3)

# Update variable labels
data <- var_label(data,
  life_satisfaction = "Overall life satisfaction (1-5)",
  income = "Monthly net income in euros",
  education = "Highest education level"
)

# 5. Convert categorical variables for analysis
data <- to_label(data, education, gender)

# 6. Analyze
data %>%
  describe(life_satisfaction, income, weights = sampling_weight)

data %>%
  t_test(life_satisfaction, group = gender, weights = sampling_weight)
```

## Practical Tips

1.  **Keep labels as long as possible.** Labels carry important context.
    Only convert to factor or character when a specific function
    requires it.

2.  **Use
    [`find_var()`](https://YannickDiehl.github.io/mariposa/reference/find_var.md)
    instead of [`names()`](https://rdrr.io/r/base/names.html).** It
    searches both names and labels, which is essential for SPSS datasets
    with non-descriptive variable names.

3.  **Use
    [`copy_labels()`](https://YannickDiehl.github.io/mariposa/reference/copy_labels.md)
    after complex pipelines.** If your pipeline involves joins,
    reshaping, or other operations that might strip attributes, restore
    labels from the original data.

4.  **Prefer
    [`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md)
    over [`as.factor()`](https://rdrr.io/r/base/factor.html).**
    [`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md)
    uses the value labels as factor levels, giving you meaningful names
    instead of numeric codes.

5.  **Use
    [`set_na()`](https://YannickDiehl.github.io/mariposa/reference/set_na.md)
    early in your workflow.** Declaring missing values immediately after
    import ensures they are handled correctly in all downstream
    analyses.

## Summary

1.  [`var_label()`](https://YannickDiehl.github.io/mariposa/reference/var_label.md)
    and
    [`val_labels()`](https://YannickDiehl.github.io/mariposa/reference/val_labels.md)
    get and set variable and value labels
2.  [`find_var()`](https://YannickDiehl.github.io/mariposa/reference/find_var.md)
    searches variables by name or label pattern
3.  [`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md),
    [`to_character()`](https://YannickDiehl.github.io/mariposa/reference/to_character.md),
    [`to_numeric()`](https://YannickDiehl.github.io/mariposa/reference/to_numeric.md),
    [`to_labelled()`](https://YannickDiehl.github.io/mariposa/reference/to_labelled.md)
    convert between formats
4.  [`set_na()`](https://YannickDiehl.github.io/mariposa/reference/set_na.md)
    declares values as missing;
    [`unlabel()`](https://YannickDiehl.github.io/mariposa/reference/unlabel.md)
    strips all metadata
5.  [`copy_labels()`](https://YannickDiehl.github.io/mariposa/reference/copy_labels.md)
    restores labels after dplyr operations;
    [`drop_labels()`](https://YannickDiehl.github.io/mariposa/reference/drop_labels.md)
    cleans up unused labels

## Next Steps

- Transform and recode variables — see
  [`vignette("data-transformation")`](https://YannickDiehl.github.io/mariposa/articles/data-transformation.md)
- Import data from SPSS, Stata, and SAS — see
  [`vignette("data-io")`](https://YannickDiehl.github.io/mariposa/articles/data-io.md)
- Start analyzing your data — see
  [`vignette("descriptive-statistics")`](https://YannickDiehl.github.io/mariposa/articles/descriptive-statistics.md)
