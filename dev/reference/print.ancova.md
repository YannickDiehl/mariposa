# Print ANCOVA results (compact)

Compact print method for objects of class `"ancova"`. Shows factor
effects and covariates with F statistics, p-values, and effect sizes.

For the full detailed output, use
[`summary()`](https://rdrr.io/r/base/summary.html).

## Usage

``` r
# S3 method for class 'ancova'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An object of class `"ancova"` returned by
  [`ancova`](https://YannickDiehl.github.io/mariposa/dev/reference/ancova.md).

- digits:

  Number of decimal places to display. Default is `3`.

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## Examples

``` r
result <- ancova(survey_data, dv = life_satisfaction, between = gender, covariate = age)
result              # compact overview
#> ANCOVA: life_satisfaction by gender, covariate: age
#>   age (covariate): F(1, 2418) = 2.025, p = 0.155 , eta2p = 0.001
#>   gender:          F(1, 2418) = 1.067, p = 0.302 , eta2p = 0.000, N = 2421
summary(result)     # full detailed output
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
```
