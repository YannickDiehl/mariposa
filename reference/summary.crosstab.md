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
  digits = object$digits %||% 1,
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

  Number of decimal places for percentages (Default: the `digits` given
  to
  [`crosstab`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md),
  i.e. 1 unless set).

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
#> Crosstabulation: Gender x Region (East/West)
#> --------------------------------------------
#> - Row variable: gender
#> - Column variable: region
#> - Percentages: Row percentages
#> - N (valid): 2500
#> 
#> +---------+-------+-------+--------+
#> |         |   Region (East/West)   |
#> | Gender  |  East |  West |  Total |
#> +---------+-------+-------+--------+
#> | Male    |   238 |   956 |   1194 |
#> |   row % | 19.9% | 80.1% | 100.0% |
#> +---------+-------+-------+--------+
#> | Female  |   247 |  1059 |   1306 |
#> |   row % | 18.9% | 81.1% | 100.0% |
#> +=========+=======+=======+========+
#> | Total   |   485 |  2015 |   2500 |
#> |   row % | 19.4% | 80.6% | 100.0% |
#> +---------+-------+-------+--------+
summary(result, percentages = FALSE)
#> 
#> Crosstabulation: Gender x Region (East/West)
#> --------------------------------------------
#> - Row variable: gender
#> - Column variable: region
#> - Percentages: Counts only
#> - N (valid): 2500
#> 
#> +--------+------+------+-------+
#> |        | Region (East/West)  |
#> | Gender | East | West | Total |
#> +--------+------+------+-------+
#> | Male   |  238 |  956 |  1194 |
#> +--------+------+------+-------+
#> | Female |  247 | 1059 |  1306 |
#> +========+======+======+=======+
#> | Total  |  485 | 2015 |  2500 |
#> +--------+------+------+-------+
summary(result, residuals = TRUE)   # which cells drive the association?
#> 
#> Crosstabulation: Gender x Region (East/West)
#> --------------------------------------------
#> - Row variable: gender
#> - Column variable: region
#> - Percentages: Row percentages
#> - N (valid): 2500
#> 
#> +------------+-------+-------+--------+
#> |            |   Region (East/West)   |
#> | Gender     |  East |  West |  Total |
#> +------------+-------+-------+--------+
#> | Male       |   238 |   956 |   1194 |
#> |   row %    | 19.9% | 80.1% | 100.0% |
#> |   adj.res. |   0.6 |  -0.6 |        |
#> +------------+-------+-------+--------+
#> | Female     |   247 |  1059 |   1306 |
#> |   row %    | 18.9% | 81.1% | 100.0% |
#> |   adj.res. |  -0.6 |   0.6 |        |
#> +============+=======+=======+========+
#> | Total      |   485 |  2015 |   2500 |
#> |   row %    | 19.4% | 80.6% | 100.0% |
#> +------------+-------+-------+--------+
#> adj.res. = adjusted standardized residual; |adj.res.| > 2 marks cells
#> deviating from independence (use chi_square() for the overall test).
```
