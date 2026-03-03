# Apply label map to a set of level names and truncate

Apply label map to a set of level names and truncate

## Usage

``` r
.apply_labels(levels, label_map, max_width = 20)
```

## Arguments

- levels:

  Character vector of raw level names

- label_map:

  Named character vector from .build_label_map(), or NULL

- max_width:

  Maximum character width for truncation

## Value

Character vector of display labels
