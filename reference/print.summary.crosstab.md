# Print summary of crosstab results (detailed output)

Displays the full cross-tabulation table for a `crosstab` result, with
sections controlled by the boolean parameters passed to
[`summary.crosstab`](https://YannickDiehl.github.io/mariposa/reference/summary.crosstab.md).
For grouped analyses, a separate table is displayed for each group
combination.

## Usage

``` r
# S3 method for class 'summary.crosstab'
print(x, ...)
```

## Arguments

- x:

  A `summary.crosstab` object created by
  [`summary.crosstab`](https://YannickDiehl.github.io/mariposa/reference/summary.crosstab.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`crosstab`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md)
for the main analysis,
[`summary.crosstab`](https://YannickDiehl.github.io/mariposa/reference/summary.crosstab.md)
for summary options.

## Examples

``` r
result <- crosstab(survey_data, gender, region)
summary(result)                       # full table
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
summary(result, percentages = FALSE) # counts only
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
```
