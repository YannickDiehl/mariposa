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

[`chi_square`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
for the main analysis function.

## Examples

``` r
result <- chi_square(survey_data, gender, education)
summary(result)
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
summary(result, expected = FALSE)
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
