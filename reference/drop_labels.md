# Remove Unused Value Labels

Removes value labels for values that are not present in the data, and
unused levels of factors. This is useful after filtering or subsetting,
when some categories may no longer exist but their labels remain
attached.

## Usage

``` r
drop_labels(data, ..., drop_na = FALSE)
```

## Arguments

- data:

  A data frame or a single vector.

- ...:

  Optional: unquoted variable names (tidyselect supported). If empty,
  applies to all labelled and factor columns.

- drop_na:

  If `TRUE`, also removes tagged NA labels. Default: `FALSE` (tagged NA
  labels are preserved even if no tagged NAs of that type exist).

## Value

The data with unused labels removed.

## Details

This is useful after subsetting data (e.g., filtering out a category).
The removed category's label still exists on the variable, which can
cause confusing output in
[`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
or
[`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md).
`drop_labels()` cleans this up by keeping only labels for values
actually present in the data. For factors, unused levels are dropped;
the variable label and the original codes of a
[`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md)
factor are kept.

By default, tagged NA labels are preserved (`drop_na = FALSE`) because
they represent missing value types, not substantive categories.

## See also

[`val_labels()`](https://YannickDiehl.github.io/mariposa/reference/val_labels.md),
[`copy_labels()`](https://YannickDiehl.github.io/mariposa/reference/copy_labels.md)

Other labels:
[`copy_labels()`](https://YannickDiehl.github.io/mariposa/reference/copy_labels.md),
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
# After filtering, "Basic Secondary" no longer occurs but remains a level
data_subset <- dplyr::filter(survey_data, education != "Basic Secondary")
levels(data_subset$education)
#> [1] "Basic Secondary"        "Intermediate Secondary" "Academic Secondary"    
#> [4] "University"            
data_clean <- drop_labels(data_subset)
levels(data_clean$education)
#> [1] "Intermediate Secondary" "Academic Secondary"     "University"            

# Value labels of labelled vectors
if (requireNamespace("haven", quietly = TRUE)) {
  x <- haven::labelled(c(1, 2, 2), labels = c(low = 1, mid = 2, high = 3))
  drop_labels(x)   # the unused label "high" is removed
}
#> <labelled<double>[3]>
#> [1] 1 2 2
#> 
#> Labels:
#>  value label
#>      1   low
#>      2   mid
```
