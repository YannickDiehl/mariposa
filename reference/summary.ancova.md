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

[`ancova`](https://YannickDiehl.github.io/mariposa/reference/ancova.md)
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
#> - Sum of squares: Type III
#> - N (complete cases): 2421
#> - Missing: 79
#> 
#> Tests of Between-Subjects Effects
#>   ------------------------------------------------------------------------------------------------------
#>   Source           Type III Sum of Squares    df  Mean Square         F    Sig  Partial Eta Squared     
#>   ------------------------------------------------------------------------------------------------------
#>   Corrected Model                    4.071     2        2.036     1.532   .216                0.001     
#>   Intercept                       3410.020     1     3410.020  2565.986  <.001                0.515  ***
#>   age                                2.691     1        2.691     2.025   .155                0.001     
#>   gender                             1.418     1        1.418     1.067   .302                0.000     
#>   Error                           3213.356  2418        1.329                                           
#>   Total                          35088.000  2421                                                        
#>   Corrected Total                 3217.428  2420                                                        
#>   ------------------------------------------------------------------------------------------------------
#> R Squared = 0.001 (Adjusted R Squared = 0.000)
#> 
#> Parameter Estimates
#>   ---------------------------------------------------------------------------------------------------
#>   Parameter             B  Std. Error       t    Sig  95% CI Lower  95% CI Upper  Partial Eta Squared
#>   ---------------------------------------------------------------------------------------------------
#>   Intercept         3.750       0.077  48.670  <.001         3.599         3.902                0.495
#>   age              -0.002       0.001  -1.423   .155        -0.005         0.001                0.001
#>   [gender=Male]    -0.048       0.047  -1.033   .302        -0.140         0.044                0.000
#>   [gender=Female]   0 (a)                                                                            
#>   ---------------------------------------------------------------------------------------------------
#> (a) This parameter is set to zero because it is redundant (SPSS coding: the
#>     last category of each factor is the reference).
#> 
#> Estimated Marginal Means
#> (Evaluated at covariate means)
#>   -----------------------------------------------------
#>   gender   Mean  Std. Error  95% CI Lower  95% CI Upper
#>   -----------------------------------------------------
#>   Male    3.603       0.034         3.536         3.669
#>   Female  3.651       0.032         3.588         3.715
#>   -----------------------------------------------------
#> 
#> Levene's Test of Equality of Error Variances
#>   F(1, 2419) = 1.306, p = 0.253
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
summary(result, marginal_means = FALSE)
#> ANCOVA (One-Way ANCOVA) Results
#> -------------------------------
#> 
#> - Dependent variable: life_satisfaction
#> - Factor(s): gender
#> - Covariate(s): age
#> - Sum of squares: Type III
#> - N (complete cases): 2421
#> - Missing: 79
#> 
#> Tests of Between-Subjects Effects
#>   ------------------------------------------------------------------------------------------------------
#>   Source           Type III Sum of Squares    df  Mean Square         F    Sig  Partial Eta Squared     
#>   ------------------------------------------------------------------------------------------------------
#>   Corrected Model                    4.071     2        2.036     1.532   .216                0.001     
#>   Intercept                       3410.020     1     3410.020  2565.986  <.001                0.515  ***
#>   age                                2.691     1        2.691     2.025   .155                0.001     
#>   gender                             1.418     1        1.418     1.067   .302                0.000     
#>   Error                           3213.356  2418        1.329                                           
#>   Total                          35088.000  2421                                                        
#>   Corrected Total                 3217.428  2420                                                        
#>   ------------------------------------------------------------------------------------------------------
#> R Squared = 0.001 (Adjusted R Squared = 0.000)
#> 
#> Parameter Estimates
#>   ---------------------------------------------------------------------------------------------------
#>   Parameter             B  Std. Error       t    Sig  95% CI Lower  95% CI Upper  Partial Eta Squared
#>   ---------------------------------------------------------------------------------------------------
#>   Intercept         3.750       0.077  48.670  <.001         3.599         3.902                0.495
#>   age              -0.002       0.001  -1.423   .155        -0.005         0.001                0.001
#>   [gender=Male]    -0.048       0.047  -1.033   .302        -0.140         0.044                0.000
#>   [gender=Female]   0 (a)                                                                            
#>   ---------------------------------------------------------------------------------------------------
#> (a) This parameter is set to zero because it is redundant (SPSS coding: the
#>     last category of each factor is the reference).
#> 
#> Levene's Test of Equality of Error Variances
#>   F(1, 2419) = 1.306, p = 0.253
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
