# Print method for crosstab results (compact)

Compact print method for objects of class `"crosstab"`. Shows the table
variables, dimensions, and valid N, plus a reminder that no significance
test is included.

For the full cross-tabulation table (cell counts and percentages), use
[`summary()`](https://rdrr.io/r/base/summary.html).

## Usage

``` r
# S3 method for class 'crosstab'
print(x, digits = 1, ...)
```

## Arguments

- x:

  A crosstab result object

- digits:

  Number of decimal places for percentages (default: 1)

- ...:

  Additional arguments (currently unused)

## Value

Invisibly returns the input object `x`.

## Examples

``` r
result <- crosstab(survey_data, gender, region)
result              # compact overview
#> Crosstab: gender x region
#>   2 x 2 table, N = 2500
#>   Note: no significance test included - use chi_square() for a test of independence.
#> Use summary() for detailed output.
summary(result)     # full cross-tabulation table
#> 
#> Crosstabulation: gender × region
#> -------------------------------- 
#> - Row variable: gender
#> - Column variable: region
#> - Percentages: Row percentages
#> - N (valid): 2500
#> 
#> +------------+--------+--------+--------+
#> |            |          region          |
#> | gender     |   East |   West |  Total |
#> +------------+--------+--------+--------+
#> | Male       |    238 |    956 |   1194 |
#> |   row %    |  19.9% |  80.1% | 100.0% |
#> +------------+--------+--------+--------+
#> | Female     |    247 |   1059 |   1306 |
#> |   row %    |  18.9% |  81.1% | 100.0% |
#> +============+========+========+========+
#> | Total      |    485 |   2015 |   2500 |
#> +------------+--------+--------+--------+
```
