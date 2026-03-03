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
#> - Confidence level: 95.0%
#> - Alternative hypothesis: two.sided
#> - Null hypothesis (mu): 0.000
#> 
#> 
#> --- life_satisfaction ---
#> 
#>   Male: rank mean = 1196.7, n = 1149.0
#>   Female: rank mean = 1223.9, n = 1272.0
#> 
#> 
#> Mann-Whitney U Test Results:
#> ------------------------------------------------------------ 
#>            Test       U         W      Z p_value effect_r sig
#>  Mann-Whitney U 714,347 1,375,022 -0.989   0.323     0.02    
#> ------------------------------------------------------------ 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (r):
#> - Negligible effect: |r| < 0.1
#> - Small effect: |r| ~ 0.1
#> - Medium effect: |r| ~ 0.3
#> - Large effect: |r| ~ 0.5
summary(result, effect_sizes = FALSE)
#> Mann-Whitney U Test Results
#> ---------------------------
#> 
#> - Grouping variable: gender
#> - Groups compared: Male vs. Female
#> - Confidence level: 95.0%
#> - Alternative hypothesis: two.sided
#> - Null hypothesis (mu): 0.000
#> 
#> 
#> --- life_satisfaction ---
#> 
#>   Male: rank mean = 1196.7, n = 1149.0
#>   Female: rank mean = 1223.9, n = 1272.0
#> 
#> 
#> Mann-Whitney U Test Results:
#> ------------------------------------------------------------ 
#>            Test       U         W      Z p_value effect_r sig
#>  Mann-Whitney U 714,347 1,375,022 -0.989   0.323     0.02    
#> ------------------------------------------------------------ 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
