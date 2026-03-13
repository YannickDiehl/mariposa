# Center Variables (Mean Centering)

Centers variables by subtracting the mean. When used on a grouped data
frame (via
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)),
this becomes group-mean centering — the R equivalent of separate
centering within each group.

## Usage

``` r
center(data, ..., weights = NULL, suffix = NULL, na.rm = TRUE)
```

## Arguments

- data:

  A data frame or numeric vector.

- ...:

  Variables to center (tidyselect). Only used when `data` is a data
  frame.

- weights:

  Optional survey weights (unquoted column name or numeric vector). When
  provided, the weighted mean is subtracted instead of the unweighted
  mean.

- suffix:

  A character string appended to column names (e.g., `"_c"`). If `NULL`
  (default), original columns are overwritten.

- na.rm:

  Remove missing values before computing the mean? Default: `TRUE`.

## Value

If `data` is a vector, a centered numeric vector. If `data` is a data
frame, the modified data frame (invisibly).

## Details

### Grand-Mean vs. Group-Mean Centering

- **Grand-mean centering** (ungrouped): Subtracts the overall mean. A
  centered value of 0 means the respondent is at the sample average.

- **Group-mean centering** (grouped): Subtracts the group mean. Useful
  in multilevel models to separate within-group and between-group
  effects. This replaces sjmisc's separate `de_mean()` function.

### Weighted Centering

When `weights` is provided, the weighted mean is used for centering.
This accounts for survey design in the centering computation.

## See also

[`std()`](https://YannickDiehl.github.io/mariposa/reference/std.md) for
full standardization (centering + scaling)

Other transform:
[`std()`](https://YannickDiehl.github.io/mariposa/reference/std.md)

## Examples

``` r
library(dplyr)
data(survey_data)

# Grand-mean centering
data <- center(survey_data, income, age, suffix = "_c")

# Weighted centering
data <- center(survey_data, income, age,
               weights = sampling_weight, suffix = "_c")

# Group-mean centering (replaces sjmisc::de_mean)
data <- survey_data %>%
  group_by(region) %>%
  center(income, age, suffix = "_gmc")
```
