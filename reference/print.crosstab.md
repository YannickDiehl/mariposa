# Print method for crosstab results

Prints the full cross-tabulation table (cell counts and percentages).
The output of
[`crosstab()`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md)
is a contingency table by nature, so
[`print()`](https://rdrr.io/r/base/print.html) and
[`summary()`](https://rdrr.io/r/base/summary.html) display the same
table; [`summary()`](https://rdrr.io/r/base/summary.html) additionally
offers section toggles (including cell `residuals`) and a `digits`
option.

No significance test is included; use
[`chi_square`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
for a test of independence.

## Usage

``` r
# S3 method for class 'crosstab'
print(x, digits = x$digits %||% 1, ...)
```

## Arguments

- x:

  A crosstab result object

- digits:

  Number of decimal places for percentages (default: the `digits` given
  to
  [`crosstab`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md),
  i.e. 1 unless set)

- ...:

  Additional arguments (currently unused)

## Value

Invisibly returns the input object `x`.

## Examples

``` r
result <- crosstab(survey_data, gender, region)
result              # full cross-tabulation table
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
summary(result)     # same table, with section toggles
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
```
