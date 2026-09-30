# Print summary of multiple response results (detailed output)

Displays the detailed SPSS-style MULT RESPONSE tables for a
[`multiple_response`](https://YannickDiehl.github.io/mariposa/reference/multiple_response.md)
result, per group for grouped data.

## Usage

``` r
# S3 method for class 'summary.multiple_response'
print(x, ...)
```

## Arguments

- x:

  A `summary.multiple_response` object created by
  [`summary.multiple_response`](https://YannickDiehl.github.io/mariposa/reference/summary.multiple_response.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`multiple_response`](https://YannickDiehl.github.io/mariposa/reference/multiple_response.md)
for the main analysis,
[`summary.multiple_response`](https://YannickDiehl.github.io/mariposa/reference/summary.multiple_response.md)
for summary options.

## Examples

``` r
d <- survey_data
d$gov <- as.integer(d$trust_government >= 4)
d$media <- as.integer(d$trust_media >= 4)
summary(multiple_response(d, gov, media, by = gender))
#> 
#> Multiple Response Results
#> -------------------------
#> - Set: gov, media
#> - Counted value: 1
#> - By: gender
#> 
#> Frequencies
#>   --------------------------------------------
#>   Option  Responses n  Responses %  % of Cases
#>   --------------------------------------------
#>   gov             583         55.4        23.4
#>   media           470         44.6        18.8
#>   --------------------------------------------
#>   Total          1053        100.0        42.2
#>   --------------------------------------------
#>   Valid cases: 2494 | Total responses: 1053 | Excluded (all missing): 6
#>   % of Cases can sum above 100% (multiple mentions per case).
#> 
#> Crosstab: set BY gender (% of cases per column)
#>   ----------------------------------------------------
#>   Option                Male       Female        Total
#>   ----------------------------------------------------
#>   gov            280 (23.5%)  303 (23.3%)  583 (23.4%)
#>   media          213 (17.9%)  257 (19.7%)  470 (18.8%)
#>   ----------------------------------------------------
#>   Total (cases)         1191         1303         2494
#>   ----------------------------------------------------
#>   Percentages and totals are based on cases (respondents).
```
