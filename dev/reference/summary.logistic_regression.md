# Summary method for logistic regression results

Creates a summary object that produces detailed output when printed,
including omnibus test, model summary, Hosmer-Lemeshow test,
classification table, and coefficient table.

## Usage

``` r
# S3 method for class 'logistic_regression'
summary(
  object,
  omnibus_test = TRUE,
  model_summary = TRUE,
  hosmer_lemeshow = TRUE,
  classification = TRUE,
  coefficients = TRUE,
  digits = 3,
  ...
)
```

## Arguments

- object:

  A `logistic_regression` result object.

- omnibus_test:

  Logical. Show omnibus test of model coefficients? (Default: TRUE)

- model_summary:

  Logical. Show model summary (pseudo R-squared)? (Default: TRUE)

- hosmer_lemeshow:

  Logical. Show Hosmer-Lemeshow test? (Default: TRUE)

- classification:

  Logical. Show classification table? (Default: TRUE)

- coefficients:

  Logical. Show coefficients table? (Default: TRUE)

- digits:

  Number of decimal places for formatting (Default: 3).

- ...:

  Additional arguments (not used).

## Value

A `summary.logistic_regression` object.

## See also

[`logistic_regression`](https://YannickDiehl.github.io/mariposa/dev/reference/logistic_regression.md)
for the main analysis function.

## Examples

``` r
survey_data$high_satisfaction <- ifelse(survey_data$life_satisfaction >= 4, 1, 0)
result <- logistic_regression(survey_data, high_satisfaction ~ age + income)
summary(result)
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
summary(result, classification = FALSE)
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
