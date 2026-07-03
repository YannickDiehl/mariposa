# Effect Sizes for Contingency Tables

Convenience helpers that run
[`chi_square`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
and return just the requested effect size as a numeric value (named by
group for grouped data): `phi()` for 2x2 tables, `cramers_v()` for
larger tables, and `goodman_gamma()` for ordinal variables.

For the full test output (chi-square statistic, p-value, all effect
sizes), call
[`chi_square`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
directly.

## Usage

``` r
phi(data, ..., weights = NULL)

cramers_v(data, ..., weights = NULL)

goodman_gamma(data, ..., weights = NULL)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- ...:

  Exactly two categorical variables, as in
  [`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)

- weights:

  Optional survey weights

## Value

A numeric vector with the effect size (one element per group for grouped
data).

## See also

[`chi_square`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)

## Examples

``` r
data(survey_data)
phi(survey_data, gender, region)
#> [1] 0.01288807
cramers_v(survey_data, education, region)
#> [1] 0.01964626
goodman_gamma(survey_data, education, life_satisfaction)
#> [1] 0.292086
```
