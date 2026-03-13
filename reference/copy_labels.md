# Copy Labels from One Data Frame to Another

Copies variable labels, value labels, and tagged NA metadata from a
source data frame to a target data frame. This is essential after dplyr
operations like
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html),
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html),
or
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
which can strip label attributes.

## Usage

``` r
copy_labels(data, source)
```

## Arguments

- data:

  The target data frame (e.g., after filtering or subsetting).

- source:

  The source data frame with the original labels.

## Value

The target data frame with labels copied from the source. Only columns
present in both data frames are affected. Columns only in the target are
left unchanged.

## Details

The following attributes are copied for each shared column:

- `"label"` — variable label

- `"labels"` — value labels

- `"na_tag_map"` — tagged NA mapping

- `"na_tag_format"` — tagged NA format (spss/stata/sas)

- `"class"` — vector class (e.g., `haven_labelled`)

## See also

[`var_label()`](https://YannickDiehl.github.io/mariposa/reference/var_label.md),
[`val_labels()`](https://YannickDiehl.github.io/mariposa/reference/val_labels.md)

Other labels:
[`drop_labels()`](https://YannickDiehl.github.io/mariposa/reference/drop_labels.md),
[`find_var()`](https://YannickDiehl.github.io/mariposa/reference/find_var.md),
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
# Labels are lost after dplyr operations
data_subset <- dplyr::filter(survey_data, age >= 18)

# Restore them
data_subset <- copy_labels(data_subset, survey_data)
```
