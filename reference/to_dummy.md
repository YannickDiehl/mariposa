# Create Dummy Variables (One-Hot Encoding)

Creates 0/1 dummy variables from categorical variables. Column names are
derived from value labels when available, making results readable for
SPSS-style data.

Unlike [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html),
`to_dummy()` uses value labels for column naming and handles
`haven_labelled` vectors correctly.

## Usage

``` r
to_dummy(data, ..., suffix = "val", ref = NULL, append = TRUE)
```

## Arguments

- data:

  A data frame or vector. When a data frame is passed, use `...` to
  select variables.

- ...:

  Variables to dummy-code (tidyselect). Only used when `data` is a data
  frame.

- suffix:

  How to name dummy columns: `"val"` (default) uses the raw value (e.g.,
  `gender_1`), `"label"` uses the value label (e.g., `gender_Male`).

- ref:

  A value to use as reference category (omitted from output). If `NULL`
  (default), all categories get a dummy variable. Set to a specific
  value for n-1 coding (e.g., for regression).

- append:

  If `TRUE` (default), the dummy columns are appended to the original
  data frame. If `FALSE`, only the dummy columns are returned. Ignored
  when `data` is a vector.

## Value

If `append = TRUE` (default), the original data frame with dummy columns
appended. If `append = FALSE`, a tibble with only the dummy columns. For
vector input, always a tibble of dummy columns.

## Details

### Column Naming

With `suffix = "val"`: `{varname}_{value}` (e.g., `gender_1`,
`gender_2`). With `suffix = "label"`: `{varname}_{label}` where labels
are cleaned (spaces replaced with `_`, special characters removed).

### Reference Category

For regression, you typically need n-1 dummy variables. Set `ref` to the
value of the reference category to omit it.

## See also

[`rec()`](https://YannickDiehl.github.io/mariposa/reference/rec.md) for
general recoding,
[`to_label()`](https://YannickDiehl.github.io/mariposa/reference/to_label.md)
for converting to factor

Other recode:
[`rec()`](https://YannickDiehl.github.io/mariposa/reference/rec.md)

## Examples

``` r
library(dplyr)
data(survey_data)

# Create dummies and append to data (default)
data <- to_dummy(survey_data, gender)

# Use labels for column names
data <- to_dummy(survey_data, gender, suffix = "label")

# n-1 dummies with reference category
data <- to_dummy(survey_data, gender, ref = 1)

# Multiple variables
data <- to_dummy(survey_data, gender, education, suffix = "label")

# Return only the dummy columns (without original data)
dummies <- to_dummy(survey_data, gender, append = FALSE)
```
