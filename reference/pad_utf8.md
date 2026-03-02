# UTF-8 safe left-pad a string to a given display width

`sprintf` counts bytes instead of display characters for multi-byte
UTF-8 strings (e.g. umlauts), causing misaligned columns. This helper
compensates by adding the byte/character difference to the target width.

## Usage

``` r
pad_utf8(text, width, align = "left")
```

## Arguments

- text:

  Character scalar to pad

- width:

  Target display width

- align:

  "left" for left-aligned (default), "right" for right-aligned

## Value

Padded character string
