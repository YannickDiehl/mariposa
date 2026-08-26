# Average Marginal Effects

`marginal_effects()` computes average marginal effects (AMEs) for a
fitted
[`logistic_regression`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)
model — the Stata `margins, dydx(*)` workhorse. AMEs translate logit
coefficients into the language reports actually use: *percentage-point
changes in the predicted probability*.

For each continuous predictor, the AME is the derivative of the
predicted probability with respect to that predictor, averaged over the
observed sample. For each factor level, it is the average *discrete
change* in predicted probability compared to the reference level.

## Usage

``` r
marginal_effects(model, conf.level = 0.95, ...)
```

## Arguments

- model:

  A fitted model object. Currently implemented for
  [`logistic_regression`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)
  results (ungrouped or grouped).

- conf.level:

  Confidence level for the AME intervals (default 0.95).

- ...:

  Additional arguments (not used).

## Value

An object of class `"marginal_effects"` whose `$results` tibble holds
one row per predictor term (and group combination) with:

- Term:

  Predictor name; factor rows read `"var: level vs. ref"`

- Type:

  `"dydx"` (continuous derivative) or `"discrete"` (factor-level
  contrast)

- AME:

  Average marginal effect on the probability scale

- SE:

  Delta-method standard error

- z, p_value:

  Wald test of the AME

- CI_lower, CI_upper:

  Confidence interval at `conf.level`

## Details

### Understanding the Output

An AME of 0.03 for `age` means: averaged over the sample, one additional
year of age increases the predicted probability of the outcome by 3
percentage points. Unlike odds ratios, AMEs are directly comparable
across models and across groups, which is why they are the preferred
reporting style in much of the social sciences.

### When to Use This

- To report logistic regression results on the probability scale instead
  of (or alongside) odds ratios

- To compare predictor effects across groups (run on a
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)-fitted
  model)

### Technical Details

Continuous predictors use a centered numerical derivative of the
predicted probability; factor predictors use the average discrete change
against the reference level. Standard errors come from the delta method
with the analytic gradient of the AME with respect to the coefficients
and the model's Wald covariance matrix, matching the default in Stata's
`margins` and R's margins package. Weighted models average with their
frequency weights (unrounded, Charter §5.1); at `weights == 1` the
weighted AME reduces exactly to the unweighted one.

**Linear regression**: AMEs are not needed there — for a linear model
without interactions or transformations, the unstandardized coefficient
B *is* the marginal effect, already shown in the
[`linear_regression`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)
coefficients table. Calling `marginal_effects()` on a
`linear_regression` result says so instead of duplicating the table.

**Validation**: SPSS has no AME procedure, so this function is Tier 4
(Internal) under the Validation Charter — verified against the analytic
logit-AME formula and an independent finite-difference delta-method
recomputation, not against SPSS output.

## See also

[`logistic_regression`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)
for fitting the model.

[`summary.marginal_effects`](https://YannickDiehl.github.io/mariposa/reference/summary.marginal_effects.md)
for detailed output.

Other regression:
[`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md),
[`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)

## Examples

``` r
data(survey_data)
survey_data$high_satisfaction <- as.integer(survey_data$life_satisfaction >= 4)

model <- logistic_regression(survey_data,
                             high_satisfaction ~ age + income + education)
marginal_effects(model)
#> Average Marginal Effects: high_satisfaction ~ age + income + education
#>   age: AME = 0.000, p = 0.730 
#>   income: AME = 0.000, p < 0.001 ***
#>   education: Intermediate Secondary vs. Basic Secondary: AME = 0.059, p = 0.023 *
#>   education: Academic Secondary vs. Basic Secondary: AME = 0.015, p = 0.600 
#>   education: University vs. Basic Secondary: AME = -0.014, p = 0.715 
#> AME = average change in predicted probability. Use summary() for detailed output.

# Weighted model
model_w <- logistic_regression(survey_data, high_satisfaction ~ age + income,
                               weights = sampling_weight)
marginal_effects(model_w)
#> Average Marginal Effects: high_satisfaction ~ age + income [Weighted]
#>   age: AME = 0.000, p = 0.639 
#>   income: AME = 0.000, p < 0.001 ***
#> AME = average change in predicted probability. Use summary() for detailed output.

# --- Three-layer output ---
ame <- marginal_effects(model)
ame                 # compact overview
#> Average Marginal Effects: high_satisfaction ~ age + income + education
#>   age: AME = 0.000, p = 0.730 
#>   income: AME = 0.000, p < 0.001 ***
#>   education: Intermediate Secondary vs. Basic Secondary: AME = 0.059, p = 0.023 *
#>   education: Academic Secondary vs. Basic Secondary: AME = 0.015, p = 0.600 
#>   education: University vs. Basic Secondary: AME = -0.014, p = 0.715 
#> AME = average change in predicted probability. Use summary() for detailed output.
summary(ame)        # full detailed output
#> 
#> Average Marginal Effects Results
#> --------------------------------
#> - Formula: high_satisfaction ~ age + income + education
#> - Scale: Predicted probability
#> - Std. errors: Delta method
#> - N: 2115
#> 
#>   ------------------------------------------------------------------------------------------------------------ 
#>   Term                                                      AME     SE       z      p  CI Lower  CI Upper  sig 
#>   ------------------------------------------------------------------------------------------------------------ 
#>   age                                                     0.000  0.001   0.344   .730    -0.001     0.001      
#>   income                                                  0.000  0.000  16.833  <.001     0.000     0.000  *** 
#>   education: Intermediate Secondary vs. Basic Secondary   0.059  0.026   2.268   .023     0.008     0.110    * 
#>   education: Academic Secondary vs. Basic Secondary       0.015  0.029   0.525   .600    -0.041     0.071      
#>   education: University vs. Basic Secondary              -0.014  0.038  -0.365   .715    -0.088     0.061      
#>   ------------------------------------------------------------------------------------------------------------ 
#> 
#> Factor rows show the average discrete change vs. the reference level.
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
