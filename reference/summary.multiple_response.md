# Summary method for multiple response results

Creates a summary object that produces the detailed SPSS-style MULT
RESPONSE output when printed: the frequencies table (percent of
responses and of cases) and, when `by` was given, the crosstab with
case-based column percentages.

## Usage

``` r
# S3 method for class 'multiple_response'
summary(object, frequencies = TRUE, crosstab = TRUE, digits = 1, ...)
```

## Arguments

- object:

  A `multiple_response` result object.

- frequencies:

  Logical. Show the frequencies table? (Default: TRUE)

- crosstab:

  Logical. Show the by-crosstab (when `by` was given)? (Default: TRUE)

- digits:

  Number of decimal places for percentages (Default: 1).

- ...:

  Additional arguments (not used).

## Value

A `summary.multiple_response` object.

## See also

[`multiple_response`](https://YannickDiehl.github.io/mariposa/reference/multiple_response.md)
for the main analysis function.

## Examples

``` r
d <- survey_data
d$gov <- as.integer(d$trust_government >= 4)
d$media <- as.integer(d$trust_media >= 4)
summary(multiple_response(d, gov, media))
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
