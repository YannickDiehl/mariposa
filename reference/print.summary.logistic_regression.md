# Print summary of logistic regression results (detailed output)

Displays the detailed SPSS-style output for a logistic regression, with
sections controlled by the boolean parameters passed to
[`summary.logistic_regression`](https://YannickDiehl.github.io/mariposa/reference/summary.logistic_regression.md).
Sections include the omnibus test of model coefficients, model fit
statistics (Nagelkerke R-squared, Hosmer-Lemeshow), classification
table, and coefficients with odds ratios.

## Usage

``` r
# S3 method for class 'summary.logistic_regression'
print(x, ...)
```

## Arguments

- x:

  A `summary.logistic_regression` object created by
  [`summary.logistic_regression`](https://YannickDiehl.github.io/mariposa/reference/summary.logistic_regression.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`logistic_regression`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)
for the main analysis,
[`summary.logistic_regression`](https://YannickDiehl.github.io/mariposa/reference/summary.logistic_regression.md)
for summary options.

## Examples

``` r
survey_data$high_satisfaction <- as.integer(survey_data$life_satisfaction > 3)
result <- logistic_regression(survey_data, high_satisfaction ~ age + income)
summary(result)                         # all sections
#> 
#> Logistic Regression Results
#> ---------------------------
#> - Formula: high_satisfaction ~ age + income
#> - Method: ENTER
#> - N: 2115
#> 
#>   Dependent Variable Encoding
#>   ------------------------------
#>   Original Value  Internal Value
#>   ------------------------------
#>   0                            0
#>   1                            1
#>   ------------------------------
#> 
#>   Omnibus Tests of Model Coefficients
#>   ---------------------------------
#>          Chi-square  df   Sig.     
#>   ---------------------------------
#>   Model     357.432   2  <.001  ***
#>   ---------------------------------
#> 
#>   Model Summary
#>   -------------------------------
#>   -2 Log Likelihood      2520.010
#>   Cox & Snell R Square      0.155
#>   Nagelkerke R Square       0.209
#>   McFadden R Square         0.124
#>   -------------------------------
#> 
#>   Hosmer and Lemeshow Test
#>   ------------------------
#>      Chi-square  df   Sig.
#>   ------------------------
#>         150.764   8  <.001
#>   ------------------------
#> 
#>   Classification Table (cutoff = 0.50; rows: observed, columns: predicted)
#>   ---------------------------------------
#>   Observed              0    1  % Correct
#>   ---------------------------------------
#>   0                   508  380       57.2
#>   1                   289  938       76.4
#>   Overall Percentage                 68.4
#>   ---------------------------------------
#> 
#>   Variables in the Equation
#>   ------------------------------------------------------------------------------------------
#>   Term              B      S.E.     Wald  df   Sig.  Exp(B)  95% CI Lower  95% CI Upper     
#>   ------------------------------------------------------------------------------------------
#>   (Intercept)  -2.252     0.212  112.868   1  <.001   0.105                              ***
#>   age           0.001     0.003    0.174   1   .677   1.001         0.996         1.007     
#>   income        0.001  4.26e-05  268.118   1  <.001  1.0007        1.0006        1.0008  ***
#>   ------------------------------------------------------------------------------------------
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
summary(result, classification = FALSE) # hide classification table
#> 
#> Logistic Regression Results
#> ---------------------------
#> - Formula: high_satisfaction ~ age + income
#> - Method: ENTER
#> - N: 2115
#> 
#>   Dependent Variable Encoding
#>   ------------------------------
#>   Original Value  Internal Value
#>   ------------------------------
#>   0                            0
#>   1                            1
#>   ------------------------------
#> 
#>   Omnibus Tests of Model Coefficients
#>   ---------------------------------
#>          Chi-square  df   Sig.     
#>   ---------------------------------
#>   Model     357.432   2  <.001  ***
#>   ---------------------------------
#> 
#>   Model Summary
#>   -------------------------------
#>   -2 Log Likelihood      2520.010
#>   Cox & Snell R Square      0.155
#>   Nagelkerke R Square       0.209
#>   McFadden R Square         0.124
#>   -------------------------------
#> 
#>   Hosmer and Lemeshow Test
#>   ------------------------
#>      Chi-square  df   Sig.
#>   ------------------------
#>         150.764   8  <.001
#>   ------------------------
#> 
#>   Variables in the Equation
#>   ------------------------------------------------------------------------------------------
#>   Term              B      S.E.     Wald  df   Sig.  Exp(B)  95% CI Lower  95% CI Upper     
#>   ------------------------------------------------------------------------------------------
#>   (Intercept)  -2.252     0.212  112.868   1  <.001   0.105                              ***
#>   age           0.001     0.003    0.174   1   .677   1.001         0.996         1.007     
#>   income        0.001  4.26e-05  268.118   1  <.001  1.0007        1.0006        1.0008  ***
#>   ------------------------------------------------------------------------------------------
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
