# Print linear regression results (compact)

Compact print method for objects of class `"linear_regression"`. Shows
R-squared, adjusted R-squared, F statistic, and p-value.

For the full detailed output, use
[`summary()`](https://rdrr.io/r/base/summary.html).

## Usage

``` r
# S3 method for class 'linear_regression'
print(x, ...)
```

## Arguments

- x:

  An object of class `"linear_regression"` returned by
  [`linear_regression`](https://YannickDiehl.github.io/mariposa/dev/reference/linear_regression.md).

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
summary(result)     # full detailed output
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
