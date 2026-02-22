# Process weights parameter

Central function for processing the weights parameter in statistical
functions. Handles both NULL weights (unweighted) and specified weights
with validation.

## Usage

``` r
.process_weights(data, weights_quo)
```

## Arguments

- data:

  A data frame

- weights_quo:

  Quoted expression for weights (from rlang::enquo)

## Value

List with vector (numeric vector or NULL) and name (character or NULL)
