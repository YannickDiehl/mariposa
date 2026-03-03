# Print t-test results (compact)

Compact print method for objects of class `"t_test"`. Shows a one-line
summary per variable with test statistic, p-value, effect size, and
sample size.

For the full detailed output, use
[`summary()`](https://rdrr.io/r/base/summary.html).

## Usage

``` r
# S3 method for class 't_test'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An object of class `"t_test"` returned by
  [`t_test`](https://YannickDiehl.github.io/mariposa/dev/reference/t_test.md).

- digits:

  Number of decimal places to display. Default is `3`.

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## Examples

``` r
result <- t_test(survey_data, life_satisfaction, group = gender)
result              # compact one-line overview
#> t-Test: life_satisfaction by gender
#>   t(2384.1) = -1.018, p = 0.309 , g = -0.041 (negligible), N = 2421
summary(result)     # full detailed output
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
```
