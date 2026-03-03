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
#> Chi-Squared Test: gender × education
#>   chi2(3) = 3.470, p = 0.325 , V = 0.037 (neglig.), N = 2500
summary(result)     # full detailed output
#> 
#> Chi-Squared Test of Independence 
#> ---------------------------------
#> 
#> - Variables: gender × education
#> 
#> Observed Frequencies:
#>         education
#> gender   Basic Secondary Intermediate Seco... Academic Secondary University
#>   Male               401                  289                320        184
#>   Female             440                  340                311        215
#> 
#> Expected Frequencies:
#>         education
#> gender   Basic Secondary Intermediate Seco... Academic Secondary University
#>   Male           401.662               300.41            301.366    190.562
#>   Female         439.338               328.59            329.634    208.438
#> 
#> Chi-Squared Test Results:
#> -------------------------------------------------- 
#>  Chi_squared df p_value sig
#>         3.47  3   0.325    
#> -------------------------------------------------- 
#> 
#> Effect Sizes:
#> ---------------------------------------------------------------------- 
#>     Measure  Value p_value sig Interpretation
#>  Cramer's V  0.037   0.325            Neglig.
#>       Gamma -0.008   0.850               Weak
#> ---------------------------------------------------------------------- 
#> Table size: 2×4 | N = 2500
#> Note: Phi coefficient only shown for 2x2 tables
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
