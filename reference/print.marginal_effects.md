# Print average marginal effects (compact)

Compact print method for objects of class `"marginal_effects"`: one line
per predictor with the AME on the probability scale.

## Usage

``` r
# S3 method for class 'marginal_effects'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An object of class `"marginal_effects"` returned by
  [`marginal_effects`](https://YannickDiehl.github.io/mariposa/reference/marginal_effects.md).

- digits:

  Number of decimal places (default: 3).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## Examples

``` r
survey_data$high_satisfaction <- as.integer(survey_data$life_satisfaction >= 4)
model <- logistic_regression(survey_data, high_satisfaction ~ age + income)
marginal_effects(model)
#> Average Marginal Effects: high_satisfaction ~ age + income
#>   age: AME = 0.000, p = 0.677 
#>   income: AME = 0.000, p < 0.001 ***
#> AME = average change in predicted probability. Use summary() for detailed output.
```
