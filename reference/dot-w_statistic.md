# Internal factory function for all weighted statistics

Handles summarise-context detection, data frame processing,
grouped/ungrouped flow, result structuring, and S3 object creation for
all w\_\* functions.

## Usage

``` r
.w_statistic(
  data,
  ...,
  weights = NULL,
  na.rm = TRUE,
  stat_fn,
  stat_name,
  weighted_col,
  unweighted_col,
  class_name,
  extra_args = list()
)
```

## Arguments

- data:

  Data frame or numeric vector (in summarise context)

- ...:

  Variable selection (tidyselect)

- weights:

  Weight variable (unquoted)

- na.rm:

  Remove missing values

- stat_fn:

  Function(x, w) that computes the statistic. Returns scalar. Takes
  (x, w) where x is numeric vector, w is weight vector or NULL.

- stat_name:

  Short name for the statistic (e.g., "mean", "sd")

- weighted_col:

  Column name for weighted results (e.g., "weighted_mean")

- unweighted_col:

  Column name for unweighted results (e.g., "mean")

- class_name:

  S3 class name (e.g., "w_mean")

- extra_args:

  Named list of extra arguments stored in the result object

## Value

S3 object of class `class_name`, or scalar in summarise context
