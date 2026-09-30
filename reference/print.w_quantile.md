# Print method for w_quantile objects

Prints one row per variable with the requested quantiles, N and Missing
(with weights: sums of weights, as in SPSS). For grouped data, one table
per group.

## Usage

``` r
# S3 method for class 'w_quantile'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An object of class "w_quantile"

- digits:

  Number of decimal places to display (default: 3)

- ...:

  Additional arguments passed to print

## Value

Invisibly returns the input object `x`.
