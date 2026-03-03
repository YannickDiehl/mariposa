# Print summary of Mann-Whitney test results (detailed output)

Displays the detailed SPSS-style output for a Mann-Whitney U test, with
sections controlled by the boolean parameters passed to
[`summary.mann_whitney`](https://YannickDiehl.github.io/mariposa/dev/reference/summary.mann_whitney.md).
Sections include rank statistics, test results, and effect sizes
(rank-biserial correlation).

## Usage

``` r
# S3 method for class 'summary.mann_whitney'
print(x, ...)
```

## Arguments

- x:

  A `summary.mann_whitney` object created by
  [`summary.mann_whitney`](https://YannickDiehl.github.io/mariposa/dev/reference/summary.mann_whitney.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`mann_whitney`](https://YannickDiehl.github.io/mariposa/dev/reference/mann_whitney.md)
for the main analysis,
[`summary.mann_whitney`](https://YannickDiehl.github.io/mariposa/dev/reference/summary.mann_whitney.md)
for summary options.

## Examples

``` r
result <- mann_whitney(survey_data, life_satisfaction, group = gender)
summary(result)                        # all sections
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
summary(result, effect_sizes = FALSE)  # hide effect sizes
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
