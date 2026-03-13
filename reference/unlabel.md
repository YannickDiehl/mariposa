# Remove All Label Metadata

Strips all label-related attributes from variables, converting
`haven_labelled` vectors to their base R types (numeric or character).
This is useful when you need plain data without any labelling for
functions or packages that do not support labelled data.

## Usage

``` r
unlabel(data, ...)
```

## Arguments

- data:

  A data frame, tibble, or a single vector.

- ...:

  Optional: unquoted variable names (tidyselect supported). If empty,
  applies to all columns.

## Value

The data with all labelling removed. `haven_labelled` numeric vectors
become plain `double`, factors remain factors (but lose their `"label"`
attribute).

## Details

The following attributes are removed:

- `"label"` (variable label)

- `"labels"` (value labels)

- `"na_tag_map"` (tagged NA mapping)

- `"na_tag_format"` (tagged NA format)

- `"na_values"`, `"na_range"` (SPSS missing value specs)

- `"format.spss"`, `"format.stata"`, `"format.sas"` (format info)

Tagged NAs are converted to regular `NA`.

## See also

[`strip_tags()`](https://YannickDiehl.github.io/mariposa/reference/strip_tags.md)
for removing only tagged NAs,
[`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md)
for converting to factors (preserving label info)

Other labels:
[`copy_labels()`](https://YannickDiehl.github.io/mariposa/reference/copy_labels.md),
[`drop_labels()`](https://YannickDiehl.github.io/mariposa/reference/drop_labels.md),
[`find_var()`](https://YannickDiehl.github.io/mariposa/reference/find_var.md),
[`set_na()`](https://YannickDiehl.github.io/mariposa/reference/set_na.md),
[`to_character()`](https://YannickDiehl.github.io/mariposa/reference/to_character.md),
[`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md),
[`to_labelled()`](https://YannickDiehl.github.io/mariposa/reference/to_labelled.md),
[`to_numeric()`](https://YannickDiehl.github.io/mariposa/reference/to_numeric.md),
[`val_labels()`](https://YannickDiehl.github.io/mariposa/reference/val_labels.md),
[`var_label()`](https://YannickDiehl.github.io/mariposa/reference/var_label.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Remove all labels from entire dataset
data <- read_spss("survey.sav")
data_plain <- unlabel(data)

# Remove labels from specific variables
data <- unlabel(data, gender, life_satisfaction)
} # }
```
