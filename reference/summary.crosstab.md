# Summary method for crosstab results

Creates a summary object that produces detailed output when printed,
including the full cross-tabulation table with cell counts, marginal
totals, and the requested percentage breakdowns.

## Usage

``` r
# S3 method for class 'crosstab'
summary(object, crosstab_table = TRUE, percentages = TRUE, digits = 1, ...)
```

## Arguments

- object:

  A `crosstab` result object.

- crosstab_table:

  Logical. Show the cross-tabulation table? (Default: TRUE)

- percentages:

  Logical. Show the percentage sub-rows inside the table (as requested
  via the `percentages` argument of
  [`crosstab`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md))?
  (Default: TRUE)

- digits:

  Number of decimal places for percentages (Default: 1).

- ...:

  Additional arguments (not used).

## Value

A `summary.crosstab` object.

## See also

[`crosstab`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md)
for the main analysis function.

## Examples

``` r
result <- crosstab(survey_data, gender, region)
summary(result)
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
summary(result, percentages = FALSE)
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
#> +------------+--------+--------+--------+
#> | Female     |    247 |   1059 |   1306 |
#> +============+========+========+========+
#> | Total      |    485 |   2015 |   2500 |
#> +------------+--------+--------+--------+
```
