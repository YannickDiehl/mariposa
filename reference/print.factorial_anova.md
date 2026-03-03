# Print factorial ANOVA results (compact)

Compact print method for objects of class `"factorial_anova"`. Shows
main effects and interactions with F statistics, p-values, and effect
sizes.

For the full detailed output, use
[`summary()`](https://rdrr.io/r/base/summary.html).

## Usage

``` r
# S3 method for class 'factorial_anova'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An object of class `"factorial_anova"` returned by
  [`factorial_anova`](https://YannickDiehl.github.io/mariposa/reference/factorial_anova.md).

- digits:

  Number of decimal places to display. Default is `3`.

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## Examples

``` r
result <- factorial_anova(survey_data,
                          dv = life_satisfaction,
                          between = c(gender, education))
result              # compact overview
#> Factorial ANOVA (2-Way): life_satisfaction by gender, education
#>   gender:           F(1, 2413) = 2.043, p = 0.153 , eta2p = 0.001
#>   education:        F(3, 2413) = 66.313, p < 0.001 ***, eta2p = 0.076
#>   gender:education: F(3, 2413) = 0.707, p = 0.548 , eta2p = 0.001, N = 2421
summary(result)     # full detailed output
#> Factorial ANOVA (2-Way ANOVA) Results
#> -------------------------------------
#> 
#> - Dependent variable: life_satisfaction
#> - Factors: gender x education
#> - Type III Sum of Squares: Type 3
#> - N (complete cases): 2421
#> - Missing: 79
#> 
#> Tests of Between-Subjects Effects
#> ------------------------------------------------------------------------------- 
#>  Source             Type III SS df   Mean Square F         Sig.  Partial Eta Sq
#>  Corrected Model      251.598      7    35.943      29.243 <.001 0.078         
#>  Intercept          30768.008      1 30768.008   25032.862 <.001 0.912         
#>  gender                 2.511      1     2.511       2.043 0.153 0.001         
#>  education            244.518      3    81.506      66.313 <.001 0.076         
#>  gender * education     2.606      3     0.869       0.707 0.548 0.001         
#>  Error               2965.830   2413     1.229                                 
#>  Total              35088.000   2421                                           
#>  Corrected Total     3217.428   2420                                           
#>     
#>  ***
#>  ***
#>     
#>  ***
#>     
#>     
#>     
#>     
#> ------------------------------------------------------------------------------- 
#> R Squared = 0.078 (Adjusted R Squared = 0.076)
#> 
#> Descriptive Statistics
#> ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#>  gender education              Mean Std. Deviation N  
#>  Male   Basic Secondary        3.20 1.235          382
#>  Male   Intermediate Secondary 3.65 1.143          281
#>  Male   Academic Secondary     3.86 1.009          305
#>  Male   University             3.96 1.043          181
#>  Female Basic Secondary        3.21 1.252          427
#>  Female Intermediate Secondary 3.74 1.086          337
#>  Female Academic Secondary     3.85 0.989          302
#>  Female University             4.13 0.869          206
#> ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#> 
#> Levene's Test of Equality of Error Variances
#>   F(7, 2413) = 13.816, p = <.001
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
