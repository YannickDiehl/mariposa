# Summary method for ANCOVA results

Creates a summary object that produces detailed output when printed,
including the full ANOVA table, parameter estimates, estimated marginal
means, and Levene's test.

## Usage

``` r
# S3 method for class 'ancova'
summary(
  object,
  between_subjects = TRUE,
  parameter_estimates = TRUE,
  marginal_means = TRUE,
  levene_test = TRUE,
  digits = 3,
  ...
)
```

## Arguments

- object:

  An `ancova` result object.

- between_subjects:

  Logical. Show the ANOVA table? (Default: TRUE)

- parameter_estimates:

  Logical. Show parameter estimates? (Default: TRUE)

- marginal_means:

  Logical. Show estimated marginal means? (Default: TRUE)

- levene_test:

  Logical. Show Levene's test? (Default: TRUE)

- digits:

  Number of decimal places for formatting (Default: 3).

- ...:

  Additional arguments (not used).

## Value

A `summary.ancova` object.

## See also

[`ancova`](https://YannickDiehl.github.io/mariposa/dev/reference/ancova.md)
for the main analysis function.

## Examples

``` r
result <- ancova(survey_data, dv = life_satisfaction, between = gender, covariate = age)
summary(result)
#> ANCOVA (One-Way ANCOVA) Results
#> -------------------------------
#> 
#> - Dependent variable: life_satisfaction
#> - Factor(s): gender
#> - Covariate(s): age
#> - Type III Sum of Squares: Type 3
#> - N (complete cases): 2421
#> - Missing: 79
#> 
#> Tests of Between-Subjects Effects
#> ------------------------------------------------------------------------------- 
#>  Source          Type III SS df   Mean Square F        Sig.  Partial Eta Sq    
#>  Corrected Model     4.071      2    2.036       1.532 0.216 0.001             
#>  Intercept        3410.020      1 3410.020    2565.986 <.001 0.515          ***
#>  age                 2.691      1    2.691       2.025 0.155 0.001             
#>  gender              1.418      1    1.418       1.067 0.302 0.000             
#>  Error            3213.356   2418    1.329                                     
#>  Total           35088.000   2421                                              
#>  Corrected Total  3217.428   2420                                              
#> ------------------------------------------------------------------------------- 
#> R Squared = 0.001 (Adjusted R Squared = 0.000)
#> 
#> Parameter Estimates
#> ------------------------------------------------------------------- 
#>  Parameter   B      Std. Error t      Sig.  Lower Bound Upper Bound
#>  (Intercept)  3.726 0.074      50.656 <.001  3.582      3.871      
#>  age         -0.002 0.001      -1.423 0.155 -0.005      0.001      
#>  gender1     -0.024 0.023      -1.033 0.302 -0.070      0.022      
#>  Partial Eta Sq
#>  0.515         
#>  0.001         
#>  0.000         
#> ------------------------------------------------------------------- 
#> 
#> Estimated Marginal Means
#> (Evaluated at covariate means)
#> ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ 
#>  gender Mean  Std. Error Lower Bound Upper Bound
#>  Male   3.603 0.034      3.536       3.669      
#>  Female 3.651 0.032      3.588       3.715      
#> ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ 
#> 
#> Levene's Test of Equality of Error Variances
#>   F(1, 2419) = 1.277, p = 0.258
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
summary(result, marginal_means = FALSE)
#> ANCOVA (One-Way ANCOVA) Results
#> -------------------------------
#> 
#> - Dependent variable: life_satisfaction
#> - Factor(s): gender
#> - Covariate(s): age
#> - Type III Sum of Squares: Type 3
#> - N (complete cases): 2421
#> - Missing: 79
#> 
#> Tests of Between-Subjects Effects
#> ------------------------------------------------------------------------------- 
#>  Source          Type III SS df   Mean Square F        Sig.  Partial Eta Sq    
#>  Corrected Model     4.071      2    2.036       1.532 0.216 0.001             
#>  Intercept        3410.020      1 3410.020    2565.986 <.001 0.515          ***
#>  age                 2.691      1    2.691       2.025 0.155 0.001             
#>  gender              1.418      1    1.418       1.067 0.302 0.000             
#>  Error            3213.356   2418    1.329                                     
#>  Total           35088.000   2421                                              
#>  Corrected Total  3217.428   2420                                              
#> ------------------------------------------------------------------------------- 
#> R Squared = 0.001 (Adjusted R Squared = 0.000)
#> 
#> Parameter Estimates
#> ------------------------------------------------------------------- 
#>  Parameter   B      Std. Error t      Sig.  Lower Bound Upper Bound
#>  (Intercept)  3.726 0.074      50.656 <.001  3.582      3.871      
#>  age         -0.002 0.001      -1.423 0.155 -0.005      0.001      
#>  gender1     -0.024 0.023      -1.033 0.302 -0.070      0.022      
#>  Partial Eta Sq
#>  0.515         
#>  0.001         
#>  0.000         
#> ------------------------------------------------------------------- 
#> 
#> Levene's Test of Equality of Error Variances
#>   F(1, 2419) = 1.277, p = 0.258
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
