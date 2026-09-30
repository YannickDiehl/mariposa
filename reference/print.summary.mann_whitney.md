# Print summary of Mann-Whitney test results (detailed output)

Displays the detailed SPSS-style output for a Mann-Whitney U test, with
sections controlled by the boolean parameters passed to
[`summary.mann_whitney`](https://YannickDiehl.github.io/mariposa/reference/summary.mann_whitney.md).
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
  [`summary.mann_whitney`](https://YannickDiehl.github.io/mariposa/reference/summary.mann_whitney.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`mann_whitney`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md)
for the main analysis,
[`summary.mann_whitney`](https://YannickDiehl.github.io/mariposa/reference/summary.mann_whitney.md)
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
summary(result, effect_sizes = FALSE)  # hide effect sizes
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
