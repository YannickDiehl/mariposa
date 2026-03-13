# Get or Set Value Labels

Retrieves or assigns value labels (the `"labels"` attribute) on labelled
vectors or data frame columns. Value labels map numeric codes to
descriptive text (e.g., `1 = "Male"`, `2 = "Female"`).

The function operates in two modes:

- **GET mode**: Called with bare variable names, returns existing value
  labels.

- **SET mode**: Called with `name = c(...)` pairs, assigns value labels
  to variables.

## Usage

``` r
val_labels(data, ..., .add = FALSE, drop.na = TRUE)
```

## Arguments

- data:

  A data frame, tibble, or a single vector.

- ...:

  In GET mode: unquoted variable names (tidyselect supported). In SET
  mode: named pairs where the name is a variable and the value is a
  named vector of labels (e.g., `c("Male" = 1, "Female" = 2)`). Use
  `NULL` to remove all value labels from a variable.

- .add:

  If `TRUE`, adds labels to any existing ones instead of replacing them.
  Default: `FALSE`.

- drop.na:

  If `TRUE` (default), tagged NA labels are excluded from GET results.

## Value

- **GET mode (vector input)**: A named numeric vector of value labels,
  or `NULL`.

- **GET mode (data frame, single variable)**: A named numeric vector of
  value labels.

- **GET mode (data frame, multiple variables)**: A named list of label
  vectors.

- **SET mode**: The modified data frame (invisibly).

## Details

Value labels are stored as the `"labels"` attribute in haven's format: a
named numeric vector where names are the label text and values are the
numeric codes. This is the standard used by
[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md),
[`read_stata()`](https://YannickDiehl.github.io/mariposa/reference/read_stata.md),
and
[`read_sas()`](https://YannickDiehl.github.io/mariposa/reference/read_sas.md).

### Adding vs. Replacing Labels

By default, SET mode replaces all existing value labels. Use
`.add = TRUE` to keep existing labels and only add new ones. If a value
already has a label, the new label overwrites it.

## See also

[`var_label()`](https://YannickDiehl.github.io/mariposa/reference/var_label.md)
for variable labels,
[`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md)
for converting labelled vectors to factors,
[`drop_labels()`](https://YannickDiehl.github.io/mariposa/reference/drop_labels.md)
for removing unused labels

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
[`var_label()`](https://YannickDiehl.github.io/mariposa/reference/var_label.md)

## Examples

``` r
# GET: retrieve value labels from a vector
val_labels(survey_data$gender)
#> NULL

# GET: from specific variables (returns list)
val_labels(survey_data, gender, region)
#> $gender
#> NULL
#> 
#> $region
#> NULL
#> 

# SET: assign value labels
data <- val_labels(survey_data,
  gender = c("Male" = 1, "Female" = 2, "Non-binary" = 3)
)

# ADD: add labels without removing existing ones
data <- val_labels(data,
  gender = c("Prefer not to say" = 4),
  .add = TRUE
)

# REMOVE: set to NULL
data <- val_labels(data, gender = NULL)
```
