# Process variable selection using tidyselect

Central function for processing variable selection expressions using
tidyselect. Handles the ... expressions passed to statistical functions
and returns a named vector of column positions.

## Usage

``` r
.process_variables(data, ...)
```

## Arguments

- data:

  A data frame

- ...:

  Variable selection expressions (tidyselect compatible)

## Value

Named integer vector of column positions
