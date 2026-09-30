# Summary method for Mann-Whitney test results

Creates a summary object that produces detailed output when printed,
including rank statistics per group, test results table with U, W, Z
statistics, and effect size interpretation.

## Usage

``` r
# S3 method for class 'mann_whitney'
summary(
  object,
  ranks = TRUE,
  results = TRUE,
  effect_sizes = TRUE,
  digits = 3,
  ...
)
```

## Arguments

- object:

  A `mann_whitney` result object.

- ranks:

  Logical. Show rank statistics per group? (Default: TRUE)

- results:

  Logical. Show test results table? (Default: TRUE)

- effect_sizes:

  Logical. Show effect size output and interpretation? (Default: TRUE)

- digits:

  Number of decimal places for formatting (Default: 3).

- ...:

  Additional arguments (not used).

## Value

A `summary.mann_whitney` object.

## See also

[`mann_whitney`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md)
for the main analysis function.

## Examples

``` r
result <- mann_whitney(survey_data, life_satisfaction, group = gender)
summary(result)
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
summary(result, effect_sizes = FALSE)
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
```
