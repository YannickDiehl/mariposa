# Standardize Variables (Z-Scores)

Standardizes variables by centering on the mean and dividing by a
measure of spread. Supports multiple standardization methods including
robust alternatives.

When used on a grouped data frame (via
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)),
standardization is performed within each group.

## Usage

``` r
std(data, ..., method = "sd", weights = NULL, suffix = NULL, na.rm = TRUE)
```

## Arguments

- data:

  A data frame or numeric vector.

- ...:

  Variables to standardize (tidyselect). Only used when `data` is a data
  frame.

- method:

  Standardization method:

  `"sd"`

  :   Standard z-score: `(x - mean) / sd` (default)

  `"2sd"`

  :   Gelman's 2-SD method: `(x - mean) / (2 * sd)`. Useful in
      regression with binary predictors.

  `"mad"`

  :   Robust: `(x - median) / mad`. Resistant to outliers.

  `"gmd"`

  :   Gini's Mean Difference: `(x - mean) / gmd`. A robust alternative.

- weights:

  Optional survey weights (unquoted column name or numeric vector). When
  provided, weighted mean and weighted SD are used for standardization.
  Only supported for methods `"sd"` and `"2sd"`.

- suffix:

  A character string appended to column names (e.g., `"_z"`). If `NULL`
  (default), the original columns are overwritten.

- na.rm:

  Remove missing values before computing mean and SD? Default: `TRUE`.

## Value

If `data` is a vector, a standardized numeric vector. If `data` is a
data frame, the modified data frame (invisibly).

## Details

### Standardization Methods

- **sd (default)**: Standard z-transformation. Mean = 0, SD = 1.

- **2sd**: Divides by 2 standard deviations (Gelman, 2008). This makes
  standardized continuous predictors comparable to binary predictors in
  regression.

- **mad**: Uses the Median Absolute Deviation instead of SD. Robust
  against outliers.

- **gmd**: Uses Gini's Mean Difference — a robust spread measure based
  on all pairwise absolute differences.

### Weighted Standardization

When `weights` is provided, the weighted mean and weighted standard
deviation (using SPSS frequency weight formula) are used. This is only
supported for methods `"sd"` and `"2sd"`. The robust methods `"mad"` and
`"gmd"` do not support weights.

### Group-By Standardization

When `data` is grouped (via
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)),
standardization is performed separately within each group. This is
useful for within-group comparisons. Weights are also subsetted per
group.

## See also

[`center()`](https://YannickDiehl.github.io/mariposa/reference/center.md)
for mean-centering without scaling,
[`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md)
for rescaling to 0-100

Other transform:
[`center()`](https://YannickDiehl.github.io/mariposa/reference/center.md)

## Examples

``` r
library(dplyr)
data(survey_data)

# Standard z-scores
data <- std(survey_data, age, income, suffix = "_z")

# Gelman 2-SD standardization (for regression)
data <- std(survey_data, income, age, method = "2sd",
            suffix = "_z")

# Robust standardization
data <- std(survey_data, income, method = "mad", suffix = "_z")

# Weighted standardization
data <- std(survey_data, income, age,
            weights = sampling_weight, suffix = "_z")

# Group-wise standardization
data <- survey_data %>%
  group_by(region) %>%
  std(income, suffix = "_z")
```
