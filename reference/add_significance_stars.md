# Format p-value with significance stars

Format p-value with significance stars

## Usage

``` r
add_significance_stars(
  p_value,
  breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
  labels = c("***", "**", "*", "")
)
```

## Arguments

- p_value:

  Numeric p-value

- breaks:

  Cut points for significance levels

- labels:

  Significance symbols
