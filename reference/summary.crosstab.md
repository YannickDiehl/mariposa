# Summary method for crosstab results

Creates a summary object that produces detailed output when printed,
including the full cross-tabulation table with cell counts, marginal
totals, and the requested percentage breakdowns.

## Usage

``` r
# S3 method for class 'crosstab'
summary(
  object,
  crosstab_table = TRUE,
  percentages = TRUE,
  residuals = FALSE,
  digits = 1,
  ...
)
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

- residuals:

  Logical. Show the adjusted standardized residual as a sub-row in each
  cell (SPSS `CROSSTABS /CELLS=ASRESID`)? After a significant
  [`chi_square`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
  test, cells with an absolute adjusted residual above roughly 2 are the
  ones deviating from independence. (Default: FALSE, matching SPSS's
  opt-in cell display)

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
summary(result, residuals = TRUE)   # which cells drive the association?
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
#> |   adj.res. |    0.6 |   -0.6 |        |
#> +------------+--------+--------+--------+
#> | Female     |    247 |   1059 |   1306 |
#> |   row %    |  18.9% |  81.1% | 100.0% |
#> |   adj.res. |   -0.6 |    0.6 |        |
#> +============+========+========+========+
#> | Total      |    485 |   2015 |   2500 |
#> +------------+--------+--------+--------+
#> adj.res. = adjusted standardized residual; |adj.res.| > 2 marks cells
#> deviating from independence (use chi_square() for the overall test).
```
