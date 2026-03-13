# Declare Values as Missing

Replaces specific numeric values with `NA` (or tagged NAs) across one or
more variables. This is essential for data cleaning workflows where
missing value codes (e.g., -9, -8, 99) are stored as regular values and
need to be declared as missing after import.

## Usage

``` r
set_na(data, ..., tag = TRUE, verbose = FALSE)
```

## Arguments

- data:

  A data frame, tibble, or a single vector.

- ...:

  Values to set as missing. Can be:

  - **Unnamed numeric values**: Applied to all numeric columns (e.g.,
    `set_na(data, -9, -8)`)

  - **Named pairs**: Applied to specific variables (e.g.,
    `set_na(data, income = c(-9, -8), age = -1)`)

- tag:

  If `TRUE` (default), uses tagged NAs to preserve distinct missing
  types. The resulting tagged NAs integrate with
  [`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md),
  [`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md),
  and
  [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md).
  If `FALSE`, replaces with regular `NA`.

- verbose:

  If `TRUE`, prints a summary of conversions.

## Value

The modified data (invisibly for data frames, visibly for vectors).

## Details

### Tagged vs. Regular NA

When `tag = TRUE` (default), each missing value code gets a unique tag
character, so you can distinguish between "No answer" (-9) and "Not
applicable" (-8) in downstream analysis. This is the same system used by
[`read_spss()`](https://YannickDiehl.github.io/mariposa/reference/read_spss.md)
with `tag.na = TRUE`.

When `tag = FALSE`, all specified values become regular `NA` and the
distinction between different missing types is lost.

### Interaction with Existing Labels

If a value being set to missing has an existing value label, that label
is preserved as a tagged NA label (when `tag = TRUE`), making it visible
in
[`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
and
[`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)
output.

## See also

[`na_frequencies()`](https://YannickDiehl.github.io/mariposa/reference/na_frequencies.md)
for inspecting missing types,
[`strip_tags()`](https://YannickDiehl.github.io/mariposa/reference/strip_tags.md)
for converting tagged NAs to regular NA,
[`untag_na()`](https://YannickDiehl.github.io/mariposa/reference/untag_na.md)
for recovering original codes

Other labels:
[`copy_labels()`](https://YannickDiehl.github.io/mariposa/reference/copy_labels.md),
[`drop_labels()`](https://YannickDiehl.github.io/mariposa/reference/drop_labels.md),
[`find_var()`](https://YannickDiehl.github.io/mariposa/reference/find_var.md),
[`to_character()`](https://YannickDiehl.github.io/mariposa/reference/to_character.md),
[`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md),
[`to_labelled()`](https://YannickDiehl.github.io/mariposa/reference/to_labelled.md),
[`to_numeric()`](https://YannickDiehl.github.io/mariposa/reference/to_numeric.md),
[`unlabel()`](https://YannickDiehl.github.io/mariposa/reference/unlabel.md),
[`val_labels()`](https://YannickDiehl.github.io/mariposa/reference/val_labels.md),
[`var_label()`](https://YannickDiehl.github.io/mariposa/reference/var_label.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Set -9 and -8 as missing across all numeric variables
data <- set_na(survey_data, -9, -8)

# Set missing for specific variables only
data <- set_na(survey_data,
  income = c(-9, -8, -42),
  life_satisfaction = c(-9, -11)
)

# Use regular NA instead of tagged NA
data <- set_na(survey_data, -9, -8, tag = FALSE)

# Check the result
na_frequencies(data$income)
} # }
```
