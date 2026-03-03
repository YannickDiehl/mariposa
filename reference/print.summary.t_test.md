# Print summary of t-test results (detailed output)

Displays the detailed SPSS-style output for a t-test, with sections
controlled by the boolean parameters passed to
[`summary.t_test`](https://YannickDiehl.github.io/mariposa/reference/summary.t_test.md).
Sections include group descriptives, test results (equal and unequal
variances), and effect sizes.

## Usage

``` r
# S3 method for class 'summary.t_test'
print(x, ...)
```

## Arguments

- x:

  A `summary.t_test` object created by
  [`summary.t_test`](https://YannickDiehl.github.io/mariposa/reference/summary.t_test.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`t_test`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)
for the main analysis,
[`summary.t_test`](https://YannickDiehl.github.io/mariposa/reference/summary.t_test.md)
for summary options.

## Examples

``` r
result <- t_test(survey_data, life_satisfaction, group = gender)
summary(result)                        # all sections
#> t-Test Results
#> --------------
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
#>   Male: mean = 3.603, n = 1149.0
#>   Female: mean = 3.651, n = 1272.0
#> 
#> t-test Results:
#> -------------------------------------------------------------------------------- 
#>         Assumption t_stat       df p_value mean_diff        conf_int sig
#>    Equal variances -1.019 2419.000   0.308    -0.048 [-0.140, 0.044]    
#>  Unequal variances -1.018 2384.147   0.309    -0.048 [-0.140, 0.044]    
#> -------------------------------------------------------------------------------- 
#> 
#> Effect Sizes:
#> ------------ 
#>           Variable Cohens_d Hedges_g Glass_Delta Effect_Size
#>  life_satisfaction   -0.041   -0.041      -0.041  negligible
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation:
#> - Cohen's d: pooled standard deviation (classic)
#> - Hedges' g: bias-corrected Cohen's d (preferred)
#> - Glass' Delta: control group standard deviation only
#> - Small effect: |effect| ~ 0.2
#> - Medium effect: |effect| ~ 0.5
#> - Large effect: |effect| ~ 0.8
summary(result, effect_sizes = FALSE)  # hide effect sizes
#> t-Test Results
#> --------------
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
#>   Male: mean = 3.603, n = 1149.0
#>   Female: mean = 3.651, n = 1272.0
#> 
#> t-test Results:
#> -------------------------------------------------------------------------------- 
#>         Assumption t_stat       df p_value mean_diff        conf_int sig
#>    Equal variances -1.019 2419.000   0.308    -0.048 [-0.140, 0.044]    
#>  Unequal variances -1.018 2384.147   0.309    -0.048 [-0.140, 0.044]    
#> -------------------------------------------------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
