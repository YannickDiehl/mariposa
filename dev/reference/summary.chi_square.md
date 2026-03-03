# Summary method for chi-squared test results

Creates a summary object that produces detailed output when printed,
including observed and expected frequency tables, test results, and
effect size measures.

## Usage

``` r
# S3 method for class 'chi_square'
summary(
  object,
  observed = TRUE,
  expected = TRUE,
  results = TRUE,
  effect_sizes = TRUE,
  digits = 3,
  ...
)
```

## Arguments

- object:

  A `chi_square` result object.

- observed:

  Logical. Show observed frequency table? (Default: TRUE)

- expected:

  Logical. Show expected frequency table? (Default: TRUE)

- results:

  Logical. Show chi-squared test results table? (Default: TRUE)

- effect_sizes:

  Logical. Show effect size measures? (Default: TRUE)

- digits:

  Number of decimal places for formatting (Default: 3).

- ...:

  Additional arguments (not used).

## Value

A `summary.chi_square` object.

## See also

[`chi_square`](https://YannickDiehl.github.io/mariposa/dev/reference/chi_square.md)
for the main analysis function.

## Examples

``` r
result <- chi_square(survey_data, gender, education)
summary(result)
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
summary(result, expected = FALSE)
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
