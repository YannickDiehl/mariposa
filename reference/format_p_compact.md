# Format a compact p-value string

Returns `"p < 0.001"` for very small values, otherwise `"p = 0.XXX"`.

## Usage

``` r
format_p_compact(p, digits = 3)
```

## Arguments

- p:

  Numeric p-value

- digits:

  Decimal places (default 3)

## Value

Character string
