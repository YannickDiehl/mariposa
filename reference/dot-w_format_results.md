# Format raw w\_\* results into standardized output structure

Handles multi-variable (long format) vs single-variable, and grouped vs
ungrouped results.

## Usage

``` r
.w_format_results(
  results,
  var_names,
  weights_name,
  is_grouped,
  weighted_col,
  unweighted_col
)
```

## Arguments

- results:

  Raw tibble from computation

- var_names:

  Character vector of variable names

- weights_name:

  Weight variable name or NULL

- is_grouped:

  Logical

- weighted_col:

  Name for weighted statistic column

- unweighted_col:

  Name for unweighted statistic column

## Value

Formatted tibble
