# Print multiple response results (compact)

Compact print method for objects of class `"multiple_response"`: one
line per answer option with mentions and percent of cases.

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
#> Multiple Response Set (2 options)
#>   gov: n = 583 (23.4% of cases)
#>   media: n = 470 (18.8% of cases)
#>   Valid cases: 2494, total responses: 1053
#> Use summary() for detailed output.
```
