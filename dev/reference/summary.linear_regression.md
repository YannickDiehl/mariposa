# Summary method for linear regression results

Creates a summary object that produces detailed output when printed,
including model summary, ANOVA table, and coefficient table.

## Usage

``` r
# S3 method for class 'linear_regression'
summary(
  object,
  model_summary = TRUE,
  anova_table = TRUE,
  coefficients = TRUE,
  descriptives = TRUE,
  digits = 3,
  ...
)
```

## Arguments

- object:

  A `linear_regression` result object.

- model_summary:

  Logical. Show model summary (R, R-squared)? (Default: TRUE)

- anova_table:

  Logical. Show ANOVA table? (Default: TRUE)

- coefficients:

  Logical. Show coefficients table? (Default: TRUE)

- descriptives:

  Logical. Reserved for future use. (Default: TRUE)

- digits:

  Number of decimal places for formatting (Default: 3).

- ...:

  Additional arguments (not used).

## Value

A `summary.linear_regression` object.

## See also

[`linear_regression`](https://YannickDiehl.github.io/mariposa/dev/reference/linear_regression.md)
for the main analysis function.

## Examples

``` r
result <- linear_regression(survey_data, life_satisfaction ~ age + trust_government)
summary(result)
#> 
#> Linear Regression Results
#> -------------------------
#> - Formula: life_satisfaction ~ age + trust_government
#> - Method: ENTER (all predictors)
#> - N: 2280
#> 
#>   Model Summary
#>   ------------------------------------------------------------
#>   R                              0.026
#>   R Square                       0.001
#>   Adjusted R Square             -0.000
#>   Std. Error of Estimate         1.151
#>   ------------------------------------------------------------
#> 
#>   ANOVA
#>   ------------------------------------------------------------------------------
#>   Source           Sum of Squares    df      Mean Square          F     Sig.
#>   ------------------------------------------------------------------------------
#>   Regression                2.058     2            1.029      0.776    0.460 
#>   Residual               3018.749  2277            1.326                     
#>   Total                  3020.807  2279                                      
#>   ------------------------------------------------------------------------------
#> 
#>   Coefficients
#>   ----------------------------------------------------------------------------------------
#>   Term                               B  Std.Error     Beta          t     Sig. 
#>   ----------------------------------------------------------------------------------------
#>   (Intercept)                    3.711      0.093              39.828    0.000 ***
#>   age                           -0.002      0.001   -0.025     -1.208    0.227 
#>   trust_government               0.006      0.021    0.006      0.305    0.760 
#>   ----------------------------------------------------------------------------------------
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
summary(result, descriptives = FALSE)
#> 
#> Linear Regression Results
#> -------------------------
#> - Formula: life_satisfaction ~ age + trust_government
#> - Method: ENTER (all predictors)
#> - N: 2280
#> 
#>   Model Summary
#>   ------------------------------------------------------------
#>   R                              0.026
#>   R Square                       0.001
#>   Adjusted R Square             -0.000
#>   Std. Error of Estimate         1.151
#>   ------------------------------------------------------------
#> 
#>   ANOVA
#>   ------------------------------------------------------------------------------
#>   Source           Sum of Squares    df      Mean Square          F     Sig.
#>   ------------------------------------------------------------------------------
#>   Regression                2.058     2            1.029      0.776    0.460 
#>   Residual               3018.749  2277            1.326                     
#>   Total                  3020.807  2279                                      
#>   ------------------------------------------------------------------------------
#> 
#>   Coefficients
#>   ----------------------------------------------------------------------------------------
#>   Term                               B  Std.Error     Beta          t     Sig. 
#>   ----------------------------------------------------------------------------------------
#>   (Intercept)                    3.711      0.093              39.828    0.000 ***
#>   age                           -0.002      0.001   -0.025     -1.208    0.227 
#>   trust_government               0.006      0.021    0.006      0.305    0.760 
#>   ----------------------------------------------------------------------------------------
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
