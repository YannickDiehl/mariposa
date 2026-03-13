# Find Variables by Name or Label

Searches for variables in your data by matching a pattern against
variable names, variable labels, or both. This is especially useful for
SPSS datasets where variable names are often cryptic codes (e.g.,
`v104`, `q23a_1`) and the actual meaning is stored in variable labels.

## Usage

``` r
find_var(data, pattern, search = c("name_label", "name", "label"))
```

## Arguments

- data:

  A data frame (typically imported from SPSS with
  [`read_spss`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md)).

- pattern:

  A search term or regular expression to match against variable names
  and/or labels. Case-insensitive by default.

- search:

  Where to search: `"name_label"` (default) searches both variable names
  and labels, `"name"` searches only names, `"label"` searches only
  labels.

## Value

A data frame with columns:

- col:

  Column position in the data

- name:

  Variable name

- label:

  Variable label (or `""` if none)

## Details

### When to Use This

- You imported an SPSS file and need to find which variable contains
  "trust" or "satisfaction"

- You want to quickly identify all variables related to a topic

- You know the German/English label text but not the variable code

### Pattern Matching

The `pattern` argument supports regular expressions. Matching is
case-insensitive. Some examples:

- `"trust"` — matches "trust", "Trust", "distrust", "trustworthy"

- `"^trust"` — matches only names/labels starting with "trust"

- `"^q[0-9]+"` — matches variable names like q1, q23, q104

- `"zufried"` — finds German labels containing "Zufriedenheit"

## See also

[`var_label()`](https://YannickDiehl.github.io/mariposa/reference/var_label.md)
for getting/setting variable labels,
[`val_labels()`](https://YannickDiehl.github.io/mariposa/reference/val_labels.md)
for value labels

Other labels:
[`copy_labels()`](https://YannickDiehl.github.io/mariposa/reference/copy_labels.md),
[`drop_labels()`](https://YannickDiehl.github.io/mariposa/reference/drop_labels.md),
[`set_na()`](https://YannickDiehl.github.io/mariposa/reference/set_na.md),
[`to_character()`](https://YannickDiehl.github.io/mariposa/reference/to_character.md),
[`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md),
[`to_labelled()`](https://YannickDiehl.github.io/mariposa/reference/to_labelled.md),
[`to_numeric()`](https://YannickDiehl.github.io/mariposa/reference/to_numeric.md),
[`unlabel()`](https://YannickDiehl.github.io/mariposa/reference/unlabel.md),
[`val_labels()`](https://YannickDiehl.github.io/mariposa/reference/val_labels.md),
[`var_label()`](https://YannickDiehl.github.io/mariposa/reference/var_label.md)

## Examples

``` r
library(dplyr)
data(survey_data)

# Find all variables related to "trust"
find_var(survey_data, "trust")
#>   col             name                                    label
#> 1  11 trust_government Trust in government (1=none, 5=complete)
#> 2  12      trust_media      Trust in media (1=none, 5=complete)
#> 3  13    trust_science    Trust in science (1=none, 5=complete)

# Search only in variable labels
find_var(survey_data, "satisfaction", search = "label")
#>   col              name                                           label
#> 1  10 life_satisfaction Life satisfaction (1=dissatisfied, 5=satisfied)

# Use regex to find numbered items
find_var(survey_data, "^q[0-9]+", search = "name")
#> No variables found matching "^q[0-9]+".
#> [1] col   name  label
#> <0 rows> (or 0-length row.names)
```
