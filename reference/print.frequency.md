# Print method for frequency objects

Prints the full frequency tables (counts, raw/valid/cumulative
percentages, missing value breakdowns). The output of
[`frequency()`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
is a frequency table by nature, so
[`print()`](https://rdrr.io/r/base/print.html) and
[`summary()`](https://rdrr.io/r/base/summary.html) display the same
tables; [`summary()`](https://rdrr.io/r/base/summary.html) additionally
offers section toggles and a `digits` option.

## Usage

``` r
# S3 method for class 'frequency'
print(x, digits = 2, ...)
```

## Arguments

- x:

  An object of class "frequency"

- digits:

  Number of decimal places of the percentages (default: 2); the summary
  statistics line uses at least two decimals.

- ...:

  Additional arguments passed to print

## Value

Invisibly returns the input object `x`.

## Examples

``` r
result <- frequency(survey_data, gender)
result              # full frequency tables
#> 
#> Frequency Analysis Results
#> --------------------------
#> 
#> gender (Gender)
#> # total N=2500 valid N=2500
#> 
#> +--------+------+--------+---------+--------+
#> | Value  |    N |  Raw % | Valid % | Cum. % |
#> +--------+------+--------+---------+--------+
#> | Male   | 1194 |  47.76 |   47.76 |  47.76 |
#> | Female | 1306 |  52.24 |   52.24 | 100.00 |
#> +--------+------+--------+---------+--------+
#> | Total  | 2500 | 100.00 |  100.00 |        |
#> +--------+------+--------+---------+--------+
#> 
summary(result)     # same tables, with section toggles
#> 
#> Frequency Analysis Results
#> --------------------------
#> 
#> gender (Gender)
#> # total N=2500 valid N=2500
#> 
#> +--------+------+--------+---------+--------+
#> | Value  |    N |  Raw % | Valid % | Cum. % |
#> +--------+------+--------+---------+--------+
#> | Male   | 1194 |  47.76 |   47.76 |  47.76 |
#> | Female | 1306 |  52.24 |   52.24 | 100.00 |
#> +--------+------+--------+---------+--------+
#> | Total  | 2500 | 100.00 |  100.00 |        |
#> +--------+------+--------+---------+--------+
#> 
```
