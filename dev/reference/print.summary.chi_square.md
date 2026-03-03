# Print summary of chi-squared test results (detailed output)

Displays the detailed SPSS-style output for a chi-squared test, with
sections controlled by the boolean parameters passed to
[`summary.chi_square`](https://YannickDiehl.github.io/mariposa/dev/reference/summary.chi_square.md).
Sections include cross-tabulation, test results, and effect sizes
(Cramer's V, Phi).

## Usage

``` r
# S3 method for class 'summary.chi_square'
print(x, ...)
```

## Arguments

- x:

  A `summary.chi_square` object created by
  [`summary.chi_square`](https://YannickDiehl.github.io/mariposa/dev/reference/summary.chi_square.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`chi_square`](https://YannickDiehl.github.io/mariposa/dev/reference/chi_square.md)
for the main analysis,
[`summary.chi_square`](https://YannickDiehl.github.io/mariposa/dev/reference/summary.chi_square.md)
for summary options.

## Examples

``` r
result <- chi_square(survey_data, gender, education)
summary(result)                          # all sections
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
summary(result, cross_tabulation = FALSE) # hide crosstab
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
