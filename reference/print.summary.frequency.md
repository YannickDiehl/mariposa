# Print summary of frequency results (detailed output)

Prints formatted frequency statistics with ASCII tables, with sections
controlled by the boolean parameters passed to
[`summary.frequency`](https://YannickDiehl.github.io/mariposa/reference/summary.frequency.md).

## Usage

``` r
# S3 method for class 'summary.frequency'
print(x, ...)
```

## Arguments

- x:

  A `summary.frequency` object created by
  [`summary.frequency`](https://YannickDiehl.github.io/mariposa/reference/summary.frequency.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`frequency`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
for the main analysis,
[`summary.frequency`](https://YannickDiehl.github.io/mariposa/reference/summary.frequency.md)
for summary options.

## Examples

``` r
result <- frequency(survey_data, gender)
summary(result)                          # all sections
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
summary(result, frequency_table = FALSE) # only summary statistics
#> 
#> Frequency Analysis Results
#> --------------------------
#> 
#> gender (Gender)
#> # total N=2500 valid N=2500 mean=NA sd=NA skewness=NA
#> 
```
