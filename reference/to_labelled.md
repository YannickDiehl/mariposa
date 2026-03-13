# Convert Variables to Labelled Format

Converts factors, character vectors, or plain numeric vectors to
`haven_labelled` class, optionally assigning value labels and a variable
label. This is the reverse of
[`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md).

## Usage

``` r
to_labelled(data, ..., labels = NULL, label = NULL)
```

## Arguments

- data:

  A data frame, tibble, or a single vector.

- ...:

  Optional: unquoted variable names (tidyselect supported). If empty on
  a data frame, converts all factor columns.

- labels:

  Optional: a named numeric vector of value labels to assign. If `NULL`
  and the input is a factor, factor levels are used as labels.

- label:

  Optional: a variable label string.

## Value

The input with variables converted to `haven_labelled`. For single
vector input, returns a `haven_labelled` vector.

## Details

### Factor Conversion

When converting a factor, the integer codes (1, 2, 3, ...) become the
numeric values and the factor levels become the value labels.

### Character Conversion

Character vectors are converted to numeric (sequential integers) with
the unique character values as labels.

## See also

[`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md)
for the reverse operation,
[`val_labels()`](https://YannickDiehl.github.io/mariposa/reference/val_labels.md)
for setting labels on existing labelled vectors

Other labels:
[`copy_labels()`](https://YannickDiehl.github.io/mariposa/reference/copy_labels.md),
[`drop_labels()`](https://YannickDiehl.github.io/mariposa/reference/drop_labels.md),
[`find_var()`](https://YannickDiehl.github.io/mariposa/reference/find_var.md),
[`set_na()`](https://YannickDiehl.github.io/mariposa/reference/set_na.md),
[`to_character()`](https://YannickDiehl.github.io/mariposa/reference/to_character.md),
[`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md),
[`to_numeric()`](https://YannickDiehl.github.io/mariposa/reference/to_numeric.md),
[`unlabel()`](https://YannickDiehl.github.io/mariposa/reference/unlabel.md),
[`val_labels()`](https://YannickDiehl.github.io/mariposa/reference/val_labels.md),
[`var_label()`](https://YannickDiehl.github.io/mariposa/reference/var_label.md)

## Examples

``` r
# Factor → haven_labelled
x <- factor(c("Male", "Female", "Male"))
to_labelled(x)
#> <labelled<double>[3]>
#> [1] 2 1 2
#> 
#> Labels:
#>  value  label
#>      1 Female
#>      2   Male

# Numeric with custom labels
to_labelled(c(1, 2, 3),
  labels = c("Low" = 1, "Medium" = 2, "High" = 3),
  label = "Satisfaction level"
)
#> <labelled<double>[3]>: Satisfaction level
#> [1] 1 2 3
#> 
#> Labels:
#>  value  label
#>      1    Low
#>      2 Medium
#>      3   High

# All factors in a data frame
data <- to_labelled(survey_data)
```
