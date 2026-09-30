# Print summary of chi-squared test results (detailed output)

Displays the detailed SPSS-style output for a chi-squared test, with
sections controlled by the boolean parameters passed to
[`summary.chi_square`](https://YannickDiehl.github.io/mariposa/reference/summary.chi_square.md).
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
  [`summary.chi_square`](https://YannickDiehl.github.io/mariposa/reference/summary.chi_square.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`chi_square`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
for the main analysis,
[`summary.chi_square`](https://YannickDiehl.github.io/mariposa/reference/summary.chi_square.md)
for summary options.

## Examples

``` r
result <- chi_square(survey_data, gender, education)
summary(result)                          # all sections
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
summary(result, cross_tabulation = FALSE) # hide crosstab
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
