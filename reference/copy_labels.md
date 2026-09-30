# Copy Labels from One Data Frame to Another

Copies variable labels, value labels, and tagged NA metadata from a
source data frame to a target data frame. dplyr verbs such as
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html),
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
or
[`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)
keep labels; they are lost by base-R conversions and computations (e.g.
[`as.numeric()`](https://rdrr.io/r/base/numeric.html),
[`ifelse()`](https://rdrr.io/r/base/ifelse.html), arithmetic on labelled
vectors), by [`merge()`](https://rdrr.io/r/base/merge.html) /
[`rbind()`](https://rdrr.io/r/base/cbind.html) of plain data frames, or
by a detour through CSV. `copy_labels()` restores them from the original
data.

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

The variable label (`"label"`) is always copied. The following are
copied only when the target column still holds the source's codes (a
numeric column whose values are observed values or labelled codes of the
source column):

- `"labels"` — value labels

- `"na_tag_map"` — tagged NA mapping

- `"na_tag_format"` — tagged NA format (spss/stata/sas)

- `"na_values"`, `"na_range"` — SPSS missing-value definitions

- `"class"` — vector class (e.g., `haven_labelled`)

A converted column (e.g. a factor from
[`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md))
or a summarised one (e.g. group means) keeps its own type and gets the
variable label only.

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
# as.numeric() drops the variable label
data_plain <- dplyr::mutate(survey_data, age = as.numeric(age))
attr(data_plain$age, "label")
#> NULL

# Restore it
data_plain <- copy_labels(data_plain, survey_data)
attr(data_plain$age, "label")
#> [1] "Age in years"
```
