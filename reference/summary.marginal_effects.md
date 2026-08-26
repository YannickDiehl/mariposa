# Summary method for average marginal effects

Creates a summary object that produces the detailed AME table (estimate,
delta-method SE, z, p, confidence interval) when printed.

## Usage

``` r
# S3 method for class 'marginal_effects'
summary(object, effects = TRUE, digits = 3, ...)
```

## Arguments

- object:

  A `marginal_effects` result object.

- effects:

  Logical. Show the AME table? (Default: TRUE)

- digits:

  Number of decimal places for formatting (Default: 3).

- ...:

  Additional arguments (not used).

## Value

A `summary.marginal_effects` object.

## See also

[`marginal_effects`](https://YannickDiehl.github.io/mariposa/reference/marginal_effects.md)
for the main analysis function.

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
#>   ------------------------------------------------------------ 
#>   Term      AME     SE       z      p  CI Lower  CI Upper  sig 
#>   ------------------------------------------------------------ 
#>   age     0.000  0.001   0.417   .677    -0.001     0.001      
#>   income  0.000  0.000  22.016  <.001     0.000     0.000  *** 
#>   ------------------------------------------------------------ 
#> 
#> Factor rows show the average discrete change vs. the reference level.
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
