# Print summary of average marginal effects (detailed output)

Displays the detailed AME table for a
[`marginal_effects`](https://YannickDiehl.github.io/mariposa/reference/marginal_effects.md)
result, per group for grouped models.

## Usage

``` r
# S3 method for class 'summary.marginal_effects'
print(x, ...)
```

## Arguments

- x:

  A `summary.marginal_effects` object created by
  [`summary.marginal_effects`](https://YannickDiehl.github.io/mariposa/reference/summary.marginal_effects.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`marginal_effects`](https://YannickDiehl.github.io/mariposa/reference/marginal_effects.md)
for the main analysis,
[`summary.marginal_effects`](https://YannickDiehl.github.io/mariposa/reference/summary.marginal_effects.md)
for summary options.

## Examples

``` r
survey_data$high_satisfaction <- as.integer(survey_data$life_satisfaction >= 4)
model <- logistic_regression(survey_data, high_satisfaction ~ age + income)
summary(marginal_effects(model))
#> 
#> Average Marginal Effects Results
#> --------------------------------
#> - Formula: high_satisfaction ~ age + income
#> - Scale: Predicted probability
#> - Std. errors: Delta method
#> - N: 2115
#> 
#>   --------------------------------------------------------------------------
#>   Term         AME        SE       z      p  95% CI Lower  95% CI Upper     
#>   --------------------------------------------------------------------------
#>   age     2.41e-04     0.001   0.417   .677        -0.001         0.001     
#>   income  1.43e-04  6.51e-06  22.019  <.001      1.31e-04      1.56e-04  ***
#>   --------------------------------------------------------------------------
#> 
#> Factor rows show the average discrete change vs. the reference level.
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
