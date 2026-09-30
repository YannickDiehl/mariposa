# Print chi-squared test results (compact)

Compact print method for objects of class `"chi_square"`. Shows a
one-line summary per test with test statistic, p-value, effect size, and
sample size.

For the full detailed output, use
[`summary()`](https://rdrr.io/r/base/summary.html).

## Usage

``` r
# S3 method for class 'chi_square'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An object of class `"chi_square"` returned by
  [`chi_square`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md).

- digits:

  Number of decimal places to display. Default is `3`.

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## Examples

``` r
result <- chi_square(survey_data, gender, education)
result              # compact one-line overview
#> Chi-Squared Test: gender x education
#>   chi2(3) = 3.470, p = 0.325, V = 0.037 (negligible), N = 2500
#> Use summary() for detailed output.
summary(result)     # full detailed output
#> 
#> Chi-Squared Test of Independence
#> --------------------------------
#> 
#> - Variables: gender x education
#> 
#> Observed Frequencies:
#>         education
#> gender   Basic Secondary Intermediate Secondary Academic Secondary University
#>   Male               401                    289                320        184
#>   Female             440                    340                311        215
#> 
#> Expected Frequencies:
#>         education
#> gender   Basic Secondary Intermediate Secondary Academic Secondary University
#>   Male           401.662                300.410            301.366    190.562
#>   Female         439.338                328.590            329.634    208.438
#> 
#> Chi-Squared Test Results:
#> -----------------------------------------
#>                     Value  df  p value   
#> -----------------------------------------
#> Pearson Chi-Square  3.470   3     .325   
#> -----------------------------------------
#> 
#> Effect Sizes:
#> ---------------------------------------------
#> Measure     Value  p value     Interpretation
#> ---------------------------------------------
#> Phi         0.037     .325                   
#> Cramer's V  0.037     .325         Negligible
#> ---------------------------------------------
#> Table size: 2 x 4 | N = 2500
#> Note: Gamma is shown for two ordinal variables (ordered factor or numeric) only.
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
