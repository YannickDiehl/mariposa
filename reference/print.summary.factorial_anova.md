# Print summary of factorial ANOVA results (detailed output)

Displays the detailed SPSS-style output for a factorial ANOVA, with
sections controlled by the boolean parameters passed to
[`summary.factorial_anova`](https://YannickDiehl.github.io/mariposa/reference/summary.factorial_anova.md).
Sections include the ANOVA table with Type III sums of squares and
effect sizes, the cell descriptives, and Levene's test for homogeneity
of variances.

## Usage

``` r
# S3 method for class 'summary.factorial_anova'
print(x, ...)
```

## Arguments

- x:

  A `summary.factorial_anova` object created by
  [`summary.factorial_anova`](https://YannickDiehl.github.io/mariposa/reference/summary.factorial_anova.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`factorial_anova`](https://YannickDiehl.github.io/mariposa/reference/factorial_anova.md)
for the main analysis,
[`summary.factorial_anova`](https://YannickDiehl.github.io/mariposa/reference/summary.factorial_anova.md)
for summary options.

## Examples

``` r
result <- factorial_anova(survey_data,
                          dv = life_satisfaction,
                          between = c(gender, education))
summary(result)                          # all sections
#> Factorial ANOVA (2-Way ANOVA) Results
#> -------------------------------------
#> 
#> - Dependent variable: life_satisfaction
#> - Factors: gender x education
#> - Sum of squares: Type III
#> - N (complete cases): 2421
#> - Missing: 79
#> 
#> Tests of Between-Subjects Effects
#>   ----------------------------------------------------------------------------------------------------------
#>   Source              Type III Sum of Squares    df  Mean Square          F    Sig  Partial Eta Squared     
#>   ----------------------------------------------------------------------------------------------------------
#>   Corrected Model                     251.598     7       35.943     29.243  <.001                0.078  ***
#>   Intercept                         30768.008     1    30768.008  25032.862  <.001                0.912  ***
#>   gender                                2.511     1        2.511      2.043   .153                0.001     
#>   education                           244.518     3       81.506     66.313  <.001                0.076  ***
#>   gender * education                    2.606     3        0.869      0.707   .548                0.001     
#>   Error                              2965.830  2413        1.229                                            
#>   Total                             35088.000  2421                                                         
#>   Corrected Total                    3217.428  2420                                                         
#>   ----------------------------------------------------------------------------------------------------------
#> R Squared = 0.078 (Adjusted R Squared = 0.076)
#> 
#> Descriptive Statistics
#>   ----------------------------------------------------------
#>   gender  education                Mean  Std. Deviation    N
#>   ----------------------------------------------------------
#>   Male    Basic Secondary         3.202           1.235  382
#>   Male    Intermediate Secondary  3.648           1.143  281
#>   Male    Academic Secondary      3.856           1.009  305
#>   Male    University              3.956           1.043  181
#>   Female  Basic Secondary         3.206           1.252  427
#>   Female  Intermediate Secondary  3.745           1.086  337
#>   Female  Academic Secondary      3.851           0.989  302
#>   Female  University              4.126           0.869  206
#>   ----------------------------------------------------------
#> 
#> Levene's Test of Equality of Error Variances
#>   F(7, 2413) = 13.816, p < 0.001 ***
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
summary(result, levene_test = FALSE)  # hide Levene's test
#> Factorial ANOVA (2-Way ANOVA) Results
#> -------------------------------------
#> 
#> - Dependent variable: life_satisfaction
#> - Factors: gender x education
#> - Sum of squares: Type III
#> - N (complete cases): 2421
#> - Missing: 79
#> 
#> Tests of Between-Subjects Effects
#>   ----------------------------------------------------------------------------------------------------------
#>   Source              Type III Sum of Squares    df  Mean Square          F    Sig  Partial Eta Squared     
#>   ----------------------------------------------------------------------------------------------------------
#>   Corrected Model                     251.598     7       35.943     29.243  <.001                0.078  ***
#>   Intercept                         30768.008     1    30768.008  25032.862  <.001                0.912  ***
#>   gender                                2.511     1        2.511      2.043   .153                0.001     
#>   education                           244.518     3       81.506     66.313  <.001                0.076  ***
#>   gender * education                    2.606     3        0.869      0.707   .548                0.001     
#>   Error                              2965.830  2413        1.229                                            
#>   Total                             35088.000  2421                                                         
#>   Corrected Total                    3217.428  2420                                                         
#>   ----------------------------------------------------------------------------------------------------------
#> R Squared = 0.078 (Adjusted R Squared = 0.076)
#> 
#> Descriptive Statistics
#>   ----------------------------------------------------------
#>   gender  education                Mean  Std. Deviation    N
#>   ----------------------------------------------------------
#>   Male    Basic Secondary         3.202           1.235  382
#>   Male    Intermediate Secondary  3.648           1.143  281
#>   Male    Academic Secondary      3.856           1.009  305
#>   Male    University              3.956           1.043  181
#>   Female  Basic Secondary         3.206           1.252  427
#>   Female  Intermediate Secondary  3.745           1.086  337
#>   Female  Academic Secondary      3.851           0.989  302
#>   Female  University              4.126           0.869  206
#>   ----------------------------------------------------------
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
