# Confidence intervals for logistic regression coefficients

Confidence intervals for the coefficients B (log-odds scale) of a
[`logistic_regression`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)
model.

The default `method = "wald"` gives Wald intervals (\\B \pm
z\_{1-\alpha/2} \cdot SE\\), the interval SPSS LOGISTIC REGRESSION
reports as "95\\ [`summary()`](https://rdrr.io/r/base/summary.html)
prints: `exp(confint(model))` reproduces the Lower/Upper columns of the
coefficients table. `method = "profile"` gives the profile-likelihood
intervals of [`stats::confint()`](https://rdrr.io/r/stats/confint.html)
for `glm` objects.

## Usage

``` r
# S3 method for class 'logistic_regression'
confint(object, parm, level = 0.95, method = c("wald", "profile"), ...)
```

## Arguments

- object:

  A `logistic_regression` result (ungrouped).

- parm:

  Coefficients to compute intervals for (names or indices; default all).

- level:

  Confidence level (default 0.95).

- method:

  `"wald"` (default, SPSS) or `"profile"`.

- ...:

  Passed to the underlying `confint` method.

## Value

A matrix with one row per coefficient and columns for the lower and
upper limits (log-odds scale).

## Examples

``` r
survey_data$high_satisfaction <- as.integer(survey_data$life_satisfaction >= 4)
model <- logistic_regression(survey_data, high_satisfaction ~ age + gender)
confint(model)        # Wald (SPSS)
#>                     2.5 %    97.5 %
#> (Intercept)   0.043750737 0.5762897
#> age          -0.006071836 0.0034194
#> genderFemale -0.032012673 0.2909808
exp(confint(model))   # SPSS "95% C.I. for EXP(B)"
#>                  2.5 %   97.5 %
#> (Intercept)  1.0447219 1.779424
#> age          0.9939466 1.003425
#> genderFemale 0.9684943 1.337739
```
