# Print summary of linear regression results (detailed output)

Displays the detailed SPSS-style output for a linear regression, with
sections controlled by the boolean parameters passed to
[`summary.linear_regression`](https://YannickDiehl.github.io/mariposa/dev/reference/summary.linear_regression.md).
Sections include model summary (R-squared, F-test), coefficients table
(B, SE, Beta, t, p), and collinearity diagnostics (Tolerance, VIF).

## Usage

``` r
# S3 method for class 'summary.linear_regression'
print(x, ...)
```

## Arguments

- x:

  A `summary.linear_regression` object created by
  [`summary.linear_regression`](https://YannickDiehl.github.io/mariposa/dev/reference/summary.linear_regression.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`linear_regression`](https://YannickDiehl.github.io/mariposa/dev/reference/linear_regression.md)
for the main analysis,
[`summary.linear_regression`](https://YannickDiehl.github.io/mariposa/dev/reference/summary.linear_regression.md)
for summary options.

## Examples

``` r
result <- linear_regression(survey_data, life_satisfaction ~ age + income)
summary(result)                           # all sections
#> 
#> Linear Regression Results
#> -------------------------
#> - Formula: life_satisfaction ~ age + income
#> - Method: ENTER (all predictors)
#> - N: 2115
#> 
#>   Model Summary
#>   ------------------------------------------------------------
#>   R                              0.448
#>   R Square                       0.201
#>   Adjusted R Square              0.200
#>   Std. Error of Estimate         1.026
#>   ------------------------------------------------------------
#> 
#>   ANOVA
#>   ------------------------------------------------------------------------------
#>   Source           Sum of Squares    df      Mean Square          F     Sig.
#>   ------------------------------------------------------------------------------
#>   Regression              559.609     2          279.804    265.598    0.000 ***
#>   Residual               2224.965  2112            1.053                     
#>   Total                  2784.574  2114                                      
#>   ------------------------------------------------------------------------------
#> 
#>   Coefficients
#>   ----------------------------------------------------------------------------------------
#>   Term                               B  Std.Error     Beta          t     Sig. 
#>   ----------------------------------------------------------------------------------------
#>   (Intercept)                    2.321      0.092              25.237    0.000 ***
#>   age                           -0.001      0.001   -0.010     -0.508    0.611 
#>   income                         0.000      0.000    0.448     23.037    0.000 ***
#>   ----------------------------------------------------------------------------------------
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
summary(result, collinearity = FALSE)     # hide VIF/Tolerance
#> 
#> Linear Regression Results
#> -------------------------
#> - Formula: life_satisfaction ~ age + income
#> - Method: ENTER (all predictors)
#> - N: 2115
#> 
#>   Model Summary
#>   ------------------------------------------------------------
#>   R                              0.448
#>   R Square                       0.201
#>   Adjusted R Square              0.200
#>   Std. Error of Estimate         1.026
#>   ------------------------------------------------------------
#> 
#>   ANOVA
#>   ------------------------------------------------------------------------------
#>   Source           Sum of Squares    df      Mean Square          F     Sig.
#>   ------------------------------------------------------------------------------
#>   Regression              559.609     2          279.804    265.598    0.000 ***
#>   Residual               2224.965  2112            1.053                     
#>   Total                  2784.574  2114                                      
#>   ------------------------------------------------------------------------------
#> 
#>   Coefficients
#>   ----------------------------------------------------------------------------------------
#>   Term                               B  Std.Error     Beta          t     Sig. 
#>   ----------------------------------------------------------------------------------------
#>   (Intercept)                    2.321      0.092              25.237    0.000 ***
#>   age                           -0.001      0.001   -0.010     -0.508    0.611 
#>   income                         0.000      0.000    0.448     23.037    0.000 ***
#>   ----------------------------------------------------------------------------------------
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
