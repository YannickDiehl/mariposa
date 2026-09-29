# Print multiple response results

Prints the full SPSS-style MULT RESPONSE tables (frequencies and, when
`by=` was used, the crosstab). The output of
[`multiple_response()`](https://YannickDiehl.github.io/mariposa/reference/multiple_response.md)
is a frequency table by nature, so
[`print()`](https://rdrr.io/r/base/print.html) and
[`summary()`](https://rdrr.io/r/base/summary.html) display the same
tables; [`summary()`](https://rdrr.io/r/base/summary.html) additionally
offers section toggles and a `digits` option.

## Usage

``` r
# S3 method for class 'multiple_response'
print(x, digits = 1, ...)
```

## Arguments

- x:

  An object of class `"multiple_response"` returned by
  [`multiple_response`](https://YannickDiehl.github.io/mariposa/reference/multiple_response.md).

- digits:

  Number of decimal places for percentages (default: 1).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## Examples

``` r
d <- survey_data
d$gov <- as.integer(d$trust_government >= 4)
d$media <- as.integer(d$trust_media >= 4)
multiple_response(d, gov, media)
#> 
#> Multiple Response Results
#> -------------------------
#> - Set: gov, media
#> - Counted value: 1
#> 
#> Frequencies
#>   -------------------------------------------- 
#>   Option  Responses n  Responses %  % of Cases 
#>   -------------------------------------------- 
#>   gov           583.0         55.4        23.4 
#>   media         470.0         44.6        18.8 
#>   -------------------------------------------- 
#>   Valid cases: 2494 | Total responses: 1053 | Excluded (all missing): 6
#>   % of Cases can sum above 100% (multiple mentions per case).
```
