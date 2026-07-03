# Summary method for frequency results

Creates a summary object that produces detailed output when printed,
including the per-variable summary statistics line (total N, valid N,
mean, SD, skewness) and the full ASCII frequency tables with counts,
raw/valid/cumulative percentages, and missing value breakdowns.

## Usage

``` r
# S3 method for class 'frequency'
summary(object, frequency_table = TRUE, summary_stats = TRUE, digits = 2, ...)
```

## Arguments

- object:

  A `frequency` result object.

- frequency_table:

  Logical. Show the frequency tables? (Default: TRUE)

- summary_stats:

  Logical. Show the per-variable summary statistics line? (Default:
  TRUE)

- digits:

  Number of decimal places for percentages and summary statistics
  (Default: 2).

- ...:

  Additional arguments (not used).

## Value

A `summary.frequency` object.

## See also

[`frequency`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
for the main analysis function.

## Examples

``` r
result <- frequency(survey_data, gender)
summary(result)
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
summary(result, summary_stats = FALSE)
#> 
#> Frequency Analysis Results
#> --------------------------
#> 
#> gender (Gender)
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
