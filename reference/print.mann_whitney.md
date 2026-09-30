# Print Mann-Whitney test results (compact)

Compact print method for objects of class `"mann_whitney"`. Shows a
one-line summary per variable with test statistic, p-value, effect size,
and sample size.

For the full detailed output, use
[`summary()`](https://rdrr.io/r/base/summary.html).

## Usage

``` r
# S3 method for class 'mann_whitney'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An object of class `"mann_whitney"` returned by
  [`mann_whitney`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md).

- digits:

  Number of decimal places to display. Default is `3`.

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## Examples

``` r
result <- mann_whitney(survey_data, life_satisfaction, group = gender)
result              # compact one-line overview
#> Mann-Whitney U Test: life_satisfaction by gender
#>   U = 714347, Z = -0.989, p = 0.323, r = 0.020 (negligible), N = 2421
#> Use summary() for detailed output.
summary(result)     # full detailed output
#> Mann-Whitney U Test Results
#> ---------------------------
#> 
#> - Grouping variable: gender
#> - Groups compared: Male vs. Female
#> - Alternative hypothesis: two.sided
#> 
#> 
#> --- life_satisfaction ---
#> 
#>   Male:    rank mean = 1196.71, n = 1149
#>   Female:  rank mean = 1223.91, n = 1272
#> 
#> Mann-Whitney U Test Results:
#> -------------------------------------------------------------
#>                      U        W       Z  p value  Effect r   
#> -------------------------------------------------------------
#> Mann-Whitney U  714347  1375022  -0.989     .323     0.020   
#> -------------------------------------------------------------
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (r):
#> - Negligible: |r| < 0.1
#> - Small: 0.1 <= |r| < 0.3
#> - Medium: 0.3 <= |r| < 0.5
#> - Large: |r| >= 0.5
```
