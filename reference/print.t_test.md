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
  [`t_test`](https://YannickDiehl.github.io/mariposa/reference/t_test.md).

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
#>   t(2384.1) = -1.018, p = 0.309, g = -0.041 (negligible), N = 2421
#> Use summary() for detailed output.
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
```
