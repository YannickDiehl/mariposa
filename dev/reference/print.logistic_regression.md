# Print logistic regression results (compact)

Compact print method for objects of class `"logistic_regression"`. Shows
Nagelkerke R-squared, chi-squared test, and classification accuracy.

For the full detailed output, use
[`summary()`](https://rdrr.io/r/base/summary.html).

## Usage

``` r
# S3 method for class 'logistic_regression'
print(x, ...)
```

## Arguments

- x:

  An object of class `"logistic_regression"` returned by
  [`logistic_regression`](https://YannickDiehl.github.io/mariposa/dev/reference/logistic_regression.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## Examples

``` r
survey_data$high_satisfaction <- as.integer(survey_data$life_satisfaction > 3)
result <- logistic_regression(survey_data, high_satisfaction ~ age + income)
result              # compact one-line overview
#> Logistic Regression: high_satisfaction ~ age + income
#>   Nagelkerke R2 = 0.209, chi2(2) = 357.43, p < 0.001 ***, Accuracy = 68.4%, N = 2115
summary(result)     # full detailed output
#> 
#> Logistic Regression Results
#> ---------------------------
#> - Formula: high_satisfaction ~ age + income
#> - Method: ENTER
#> - N: 2115
#> 
#>   Omnibus Tests of Model Coefficients
#>   --------------------------------------------------
#>                          Chi-square    df       Sig.
#>   --------------------------------------------------
#>   Model                     357.432     2      0.000 ***
#>   --------------------------------------------------
#> 
#>   Model Summary
#>   ------------------------------------------------------------
#>   -2 Log Likelihood                  2520.010
#>   Cox & Snell R Square                  0.155
#>   Nagelkerke R Square                   0.209
#>   McFadden R Square                     0.124
#>   ------------------------------------------------------------
#> 
#>   Hosmer and Lemeshow Test
#>   --------------------------------------------------
#>                          Chi-square    df       Sig.
#>   --------------------------------------------------
#>                             150.764     8      0.000
#>   --------------------------------------------------
#> 
#>   Classification Table (cutoff = 0.50)
#>   -----------------------------------------------------------------
#>                                   Predicted                     
#>   Observed                      0          1       % Correct
#>   -----------------------------------------------------------------
#>   0                           508        380           57.2
#>   1                           289        938           76.4
#>   -----------------------------------------------------------------
#>   Overall Percentage                                   68.4
#>   -----------------------------------------------------------------
#> 
#>   Variables in the Equation
#>   -----------------------------------------------------------------------------------------------
#>   Term                         B      S.E.      Wald   df     Sig.     Exp(B)     Lower     Upper 
#>   -----------------------------------------------------------------------------------------------
#>   (Intercept)             -2.252     0.212   112.853    1    0.000      0.105                     ***
#>   age                      0.001     0.003     0.174    1    0.677      1.001     0.996     1.007 
#>   income                   0.001     0.000   268.051    1    0.000      1.001     1.001     1.001 ***
#>   -----------------------------------------------------------------------------------------------
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
