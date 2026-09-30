# Print linear regression results (compact)

Compact print method for objects of class `"linear_regression"`. Shows
R-squared, adjusted R-squared, F statistic, and p-value.

For the full detailed output, use
[`summary()`](https://rdrr.io/r/base/summary.html).

## Usage

``` r
# S3 method for class 'linear_regression'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An object of class `"linear_regression"` returned by
  [`linear_regression`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md).

- digits:

  Number of decimal places for R-squared and the p-value (the F
  statistic keeps one decimal fewer). (Default: 3)

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## Examples

``` r
result <- linear_regression(survey_data, life_satisfaction ~ age + income)
result              # compact one-line overview
#> Linear Regression: life_satisfaction ~ age + income
#>   R2 = 0.201, adj.R2 = 0.200, F(2, 2112) = 265.60, p < 0.001 ***, N = 2115
#> Use summary() for detailed output.
summary(result)     # full detailed output
#> 
#> Linear Regression Results
#> -------------------------
#> - Formula: life_satisfaction ~ age + income
#> - Method: ENTER (all predictors)
#> - N: 2115
#> 
#>   Descriptive Statistics
#>   -------------------------------------------------
#>   Variable               Mean  Std. Deviation     N
#>   -------------------------------------------------
#>   life_satisfaction     3.638           1.148  2115
#>   age                  50.827          16.995  2115
#>   income             3757.683        1430.923  2115
#>   -------------------------------------------------
#> 
#>   Model Summary
#>   ----------------------------------
#>   R                            0.448
#>   R Square                     0.201
#>   Adjusted R Square            0.200
#>   Std. Error of the Estimate   1.026
#>   ----------------------------------
#> 
#>   ANOVA
#>   ------------------------------------------------------------------
#>   Source      Sum of Squares    df  Mean Square        F   Sig.     
#>   ------------------------------------------------------------------
#>   Regression         559.609     2      279.804  265.598  <.001  ***
#>   Residual          2224.965  2112        1.053                     
#>   Total             2784.574  2114                                  
#>   ------------------------------------------------------------------
#> 
#>   Coefficients
#>   -----------------------------------------------------------------------------------------
#>   Term                B  Std. Error    Beta       t   Sig.  95% CI Lower  95% CI Upper     
#>   -----------------------------------------------------------------------------------------
#>   (Intercept)     2.321       0.092          25.237  <.001         2.141         2.502  ***
#>   age            -0.001       0.001  -0.010  -0.508   .611        -0.003         0.002     
#>   income       3.59e-04    1.56e-05   0.448  23.037  <.001      3.29e-04      3.90e-04  ***
#>   -----------------------------------------------------------------------------------------
#> 
#>   Collinearity Statistics
#>   ------------------------
#>   Term    Tolerance    VIF
#>   ------------------------
#>   age         1.000  1.000
#>   income      1.000  1.000
#>   ------------------------
#>   VIF > 10 (Tolerance < 0.1) indicates problematic collinearity.
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
