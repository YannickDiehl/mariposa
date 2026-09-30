# Summary method for t-test results

Creates a summary object that produces detailed output when printed,
including group descriptives, test results with both variance
assumptions, effect sizes, and confidence intervals.

## Usage

``` r
# S3 method for class 't_test'
summary(
  object,
  descriptives = TRUE,
  results = TRUE,
  effect_sizes = TRUE,
  digits = 3,
  ...
)
```

## Arguments

- object:

  A `t_test` result object.

- descriptives:

  Logical. Show group descriptive statistics? (Default: TRUE)

- results:

  Logical. Show test results table? (Default: TRUE)

- effect_sizes:

  Logical. Show effect size measures? (Default: TRUE)

- digits:

  Number of decimal places for formatting (Default: 3).

- ...:

  Additional arguments (not used).

## Value

A `summary.t_test` object.

## See also

[`t_test`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)
for the main analysis function.

## Examples

``` r
result <- t_test(survey_data, life_satisfaction, group = gender)
summary(result)
#> t-Test Results
#> --------------
#> 
#> - Grouping variable: gender
#> - Groups compared: Male vs. Female
#> - Confidence level: 95.0%
#> - Alternative hypothesis: two.sided
#> - Null hypothesis (mu): 0.000
#> 
#> --- life_satisfaction ---
#> 
#> Group Statistics:
#>   ----------------------------------------------------
#>   gender     N   Mean  Std. Deviation  Std. Error Mean
#>   ----------------------------------------------------
#>   Male    1149  3.603           1.165            0.034
#>   Female  1272  3.651           1.142            0.032
#>   ----------------------------------------------------
#> 
#> Independent Samples Test:
#>   --------------------------------------------------------------------------------------------------------
#>                                     t        df     p  Mean Diff.  SE Diff.  95% CI Lower  95% CI Upper   
#>   --------------------------------------------------------------------------------------------------------
#>   Equal variances assumed      -1.019      2419  .308      -0.048     0.047        -0.140         0.044   
#>   Equal variances not assumed  -1.018  2384.147  .309      -0.048     0.047        -0.140         0.044   
#>   --------------------------------------------------------------------------------------------------------
#> 
#> Effect Sizes:
#>   -----------------------------------------------------------------
#>   Variable           Cohen's d  Hedges' g  Glass' Delta   Magnitude
#>   -----------------------------------------------------------------
#>   life_satisfaction     -0.041     -0.041        -0.041  negligible
#>   -----------------------------------------------------------------
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
summary(result, effect_sizes = FALSE)
#> t-Test Results
#> --------------
#> 
#> - Grouping variable: gender
#> - Groups compared: Male vs. Female
#> - Confidence level: 95.0%
#> - Alternative hypothesis: two.sided
#> - Null hypothesis (mu): 0.000
#> 
#> --- life_satisfaction ---
#> 
#> Group Statistics:
#>   ----------------------------------------------------
#>   gender     N   Mean  Std. Deviation  Std. Error Mean
#>   ----------------------------------------------------
#>   Male    1149  3.603           1.165            0.034
#>   Female  1272  3.651           1.142            0.032
#>   ----------------------------------------------------
#> 
#> Independent Samples Test:
#>   --------------------------------------------------------------------------------------------------------
#>                                     t        df     p  Mean Diff.  SE Diff.  95% CI Lower  95% CI Upper   
#>   --------------------------------------------------------------------------------------------------------
#>   Equal variances assumed      -1.019      2419  .308      -0.048     0.047        -0.140         0.044   
#>   Equal variances not assumed  -1.018  2384.147  .309      -0.048     0.047        -0.140         0.044   
#>   --------------------------------------------------------------------------------------------------------
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
