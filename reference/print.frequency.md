# Print method for frequency objects (compact)

Compact print method for objects of class `"frequency"`. Shows one line
per variable (and group combination) with the number of categories, the
valid N, and the missing count.

For the full frequency tables (counts, percentages, cumulative
percentages), use [`summary()`](https://rdrr.io/r/base/summary.html).

## Usage

``` r
# S3 method for class 'frequency'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An object of class "frequency"

- digits:

  Number of decimal places to display (default: 3)

- ...:

  Additional arguments passed to print

## Value

Invisibly returns the input object `x`.

## Examples

``` r
result <- frequency(survey_data, gender)
result              # compact overview
#> Frequency: gender
#>   2 categories, N valid = 2500, missing = 0
#> Use summary() for detailed output.
summary(result)     # full frequency tables
#> 
#> Frequency Analysis Results
#> --------------------------
#> 
#> gender (Gender)
#> # total N=2500 valid N=2500 mean=NA sd=NA skewness=NA
#> 
#> +--------+--------+--------+--------+--------+--------+
#> |  Value |  Label |      N |  Raw % |Valid % | Cum. % |
#> +--------+--------+--------+--------+--------+--------+
#> |   Male |   Male |   1194 |  47.76 |  47.76 |  47.76 |
#> | Female | Female |   1306 |  52.24 |  52.24 | 100.00 |
#> +--------+--------+--------+--------+--------+--------+
#> |  Total |        |   2500 | 100.00 | 100.00 |        |
#> +--------+--------+--------+--------+--------+--------+
#> 
```
