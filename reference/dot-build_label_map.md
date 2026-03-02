# Build a named vector mapping raw values to display labels

Build a named vector mapping raw values to display labels

## Usage

``` r
.build_label_map(x)
```

## Arguments

- x:

  The original data vector (factor or haven-labelled)

## Value

Named character vector (names = raw values as strings, values = display
labels). Returns NULL if no meaningful labels exist.
