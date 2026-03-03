# Print a Compact Console Overview of the Codebook

Displays a concise one-line-per-variable table in the console, showing
position, name, type, and label. The full HTML codebook with values,
value labels, and frequencies is displayed in the RStudio Viewer.

## Usage

``` r
# S3 method for class 'codebook'
print(x, ...)
```

## Arguments

- x:

  A codebook object from
  [`codebook()`](https://YannickDiehl.github.io/mariposa/reference/codebook.md)

- ...:

  Additional arguments (ignored)

## Value

Invisibly returns the codebook object

## Examples

``` r
data(survey_data)
cb <- codebook(survey_data)
print(cb)
#> 
#> Codebook: survey_data
#> 16 variables | 2,500 observations | 15 labelled
#> Types: 3 dbl, 2 fct(2), 1 fct(3), 1 fct(5), 1 fct(6), 7 int, 1 ord(4)
#> -- Open HTML viewer for full codebook with values and frequencies
```
