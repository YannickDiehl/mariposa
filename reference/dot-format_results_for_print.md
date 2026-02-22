# Create standardized results data frame for printing

Create standardized results data frame for printing

## Usage

``` r
.format_results_for_print(
  results,
  variables,
  statistic_name,
  weights_name,
  is_grouped,
  group_vars = NULL
)
```

## Arguments

- results:

  Raw results data frame

- variables:

  Character vector of variable names

- statistic_name:

  Name of the statistic (e.g., "Mean", "SD")

- weights_name:

  Name of weights variable or NULL

- is_grouped:

  Logical indicating if data was grouped

- group_vars:

  Character vector of grouping variables

## Value

Formatted data frame for printing
