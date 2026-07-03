# Run a Logistic Regression

`logistic_regression()` performs binary logistic regression with
SPSS-compatible output. Wraps `stats::glm(family = binomial)` and adds
odds ratios, pseudo R-squared measures, classification table, and model
tests matching SPSS LOGISTIC REGRESSION output.

Supports two interface styles:

- **Formula interface:**
  `logistic_regression(data, high_satisfaction ~ age + income)`

- **SPSS-style:**
  `logistic_regression(data, dependent = high_satisfaction, predictors = c(age, income))`

## Usage

``` r
logistic_regression(
  data,
  formula = NULL,
  dependent = NULL,
  predictors = NULL,
  weights = NULL,
  conf.level = 0.95,
  factors = c("dummy", "numeric")
)
```

## Arguments

- data:

  Your survey data (a data frame or tibble). If grouped (via
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)),
  separate regressions are run for each group.

- formula:

  A formula specifying the model (e.g., `y ~ x1 + x2`). If provided,
  `dependent` and `predictors` are ignored.

- dependent:

  The dependent variable (unquoted). Used with `predictors` when no
  formula is given. Must be binary (0/1 or two-level factor).

- predictors:

  Predictor variable(s) (unquoted, supports tidyselect). Used with
  `dependent` when no formula is given.

- weights:

  Optional survey weights (unquoted variable name). When specified,
  weighted maximum likelihood estimation is used, matching SPSS WEIGHT
  BY behavior.

- conf.level:

  Confidence level for odds ratio intervals (default 0.95).

- factors:

  How factor predictors are entered into the model: `"dummy"` (default,
  matches base R [`glm()`](https://rdrr.io/r/stats/glm.html)) expands a
  factor with `L` levels into `L - 1` dummy contrasts; `"numeric"`
  silently coerces factor levels to their integer codes, matching SPSS
  `LOGISTIC REGRESSION` default behavior when no `/CATEGORICAL`
  subcommand is given. The "numeric" mode emits a one-line
  [`cli::cli_inform()`](https://cli.r-lib.org/reference/cli_abort.html)
  listing the coerced variables.

## Value

For ungrouped data, an object of class
`c("logistic_regression", "glm", "lm")` — **the fitted `glm` itself**,
with mariposa-specific slots attached:

- coef_table:

  Tibble with B, S.E., Wald, df, Sig., Exp(B), CI_lower, CI_upper

- model_summary:

  List with minus2LL, cox_snell_r2, nagelkerke_r2, mcfadden_r2

- omnibus_test:

  List with chi_sq, df, p for overall model test

- classification:

  List with table, overall_pct, pct_correct_0, pct_correct_1

- hosmer_lemeshow:

  List with chi_sq, df, p (goodness-of-fit test)

- n:

  Sample size (listwise complete cases; weighted N when weighted)

- formula, dependent, predictor_names, weighted, weight_name,
  is_grouped, conf.level:

  Call metadata.

Because the object inherits from `"glm"`, all standard generics
([`predict()`](https://rdrr.io/r/stats/predict.html),
[`anova()`](https://rdrr.io/r/stats/anova.html),
[`vcov()`](https://rdrr.io/r/stats/vcov.html),
[`confint()`](https://rdrr.io/r/stats/confint.html),
[`residuals()`](https://rdrr.io/r/stats/residuals.html),
[`fitted()`](https://rdrr.io/r/stats/fitted.values.html),
[`coef()`](https://rdrr.io/r/stats/coef.html),
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html),
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html),
[`broom::augment()`](https://generics.r-lib.org/reference/augment.html))
dispatch natively without unwrapping.
[`summary()`](https://rdrr.io/r/base/summary.html) returns the
SPSS-style mariposa summary; for the raw glm summary use
[`stats::summary.glm()`](https://rdrr.io/r/stats/summary.glm.html) on
the same object.

For grouped data, returns a list of class `"logistic_regression"` with
`$groups` holding one fitted glm-inheriting result per group.

## Details

### Understanding the Results

The output includes five sections matching SPSS LOGISTIC REGRESSION
output:

- **Omnibus Test**: Tests whether the model as a whole is significant. A
  significant chi-square means the model predicts better than chance.

- **Model Summary**: -2 Log Likelihood and pseudo R-squared values.
  Lower -2LL = better fit. Higher R-squared = more variance explained.

- **Hosmer-Lemeshow Test**: Goodness-of-fit test. A non-significant
  result (p \> 0.05) means the model fits the data well.

- **Classification Table**: How well the model classifies cases. Shows
  percentage correctly predicted for each group and overall.

- **Coefficients**: B, Wald test, odds ratios (Exp(B)), and CIs.

Interpreting odds ratios (Exp(B)):

- **Exp(B) \> 1**: Predictor increases the odds of the outcome

- **Exp(B) \< 1**: Predictor decreases the odds of the outcome

- **Exp(B) = 1**: Predictor has no effect on the odds

### When to Use This

Use `logistic_regression()` when:

- Your dependent variable is binary (yes/no, 0/1, pass/fail)

- You want to predict group membership from one or more predictors

- You need odds ratios to interpret predictor effects

For continuous outcomes, use
[`linear_regression`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)
instead.

### Technical Details

**Dependent Variable**: Must be binary. Factors with exactly 2 levels
are automatically converted to 0/1 (first level = 0, second level = 1).
Numeric variables must contain only 0 and 1 values.

**Missing Data**: Listwise deletion is used (matching SPSS LOGISTIC
REGRESSION default behavior).

**Weights**: When weights are specified, they are treated as frequency
weights (matching SPSS WEIGHT BY behavior).

**Pseudo R-squared**: Three measures are reported:

- Cox & Snell R-squared (bounded below 1)

- Nagelkerke R-squared (adjusted to reach 1)

- McFadden R-squared (1 - LL_model/LL_null)

**Factor Predictors**: By default (`factors = "dummy"`), factor
predictors are expanded into `L - 1` dummy contrasts via R's
[`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html),
matching base R [`glm()`](https://rdrr.io/r/stats/glm.html). Pass
`factors = "numeric"` to silently coerce factor levels to their integer
codes (SPSS `LOGISTIC REGRESSION` default without an explicit
`/CATEGORICAL` subcommand).

**Grouped Analysis**: When `data` is grouped via
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),
a separate regression is run for each group (matching SPSS SPLIT FILE
BY).

## See also

[`linear_regression`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)
for continuous outcome variables.

[`chi_square`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
for testing associations between categorical variables.

[`summary.logistic_regression`](https://YannickDiehl.github.io/mariposa/reference/summary.logistic_regression.md)
for detailed output with toggleable sections.

Other regression:
[`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)

## Examples

``` r
library(dplyr)
data(survey_data)

# Create binary DV
survey_data$high_satisfaction <- ifelse(survey_data$life_satisfaction >= 4, 1, 0)

# Bivariate logistic regression
logistic_regression(survey_data, high_satisfaction ~ age)
#> Logistic Regression: high_satisfaction ~ age
#>   Nagelkerke R2 = 0.000, chi2(1) = 0.28, p = 0.595 , Accuracy = 57.7%, N = 2421

# Multiple logistic regression
logistic_regression(survey_data, high_satisfaction ~ age + income + education)
#> Logistic Regression: high_satisfaction ~ age + income + education
#>   Nagelkerke R2 = 0.213, chi2(5) = 364.62, p < 0.001 ***, Accuracy = 68.3%, N = 2115

# SPSS-style interface
logistic_regression(survey_data,
                    dependent = high_satisfaction,
                    predictors = c(age, income))
#> Logistic Regression: high_satisfaction ~ age + income
#>   Nagelkerke R2 = 0.209, chi2(2) = 357.43, p < 0.001 ***, Accuracy = 68.4%, N = 2115

# Weighted logistic regression
logistic_regression(survey_data, high_satisfaction ~ age,
                    weights = sampling_weight)
#> Logistic Regression: high_satisfaction ~ age [Weighted]
#>   Nagelkerke R2 = 0.000, chi2(1) = 0.28, p = 0.595 , Accuracy = 57.6%, N = 2437

# Grouped by region
survey_data |>
  dplyr::group_by(region) |>
  logistic_regression(high_satisfaction ~ age)
#> Logistic Regression: high_satisfaction ~ age [Grouped: region]
#>   region = East: Nagelkerke R2 = 0.002, chi2(1) = 0.74, p = 0.390 , Accuracy = 58.3%, N = 465
#>   region = West: Nagelkerke R2 = 0.000, chi2(1) = 0.03, p = 0.860 , Accuracy = 57.6%, N = 1956

# Factor predictors: dummy-coding (default, matches base R glm())
logistic_regression(survey_data, high_satisfaction ~ age + education)
#> Logistic Regression: high_satisfaction ~ age + education
#>   Nagelkerke R2 = 0.084, chi2(4) = 156.25, p < 0.001 ***, Accuracy = 63.4%, N = 2421

# Factor predictors: SPSS-style ordinal-as-scale
logistic_regression(survey_data, high_satisfaction ~ age + education,
                    factors = "numeric")
#> ℹ Factor predictor(s) coerced to numeric (SPSS-style ordinal scaling):
#> • `education`
#> Logistic Regression: high_satisfaction ~ age + education
#>   Nagelkerke R2 = 0.078, chi2(2) = 144.84, p < 0.001 ***, Accuracy = 63.4%, N = 2421

# --- Three-layer output ---
result <- logistic_regression(survey_data, high_satisfaction ~ age + income)
result                                    # compact one-line overview
#> Logistic Regression: high_satisfaction ~ age + income
#>   Nagelkerke R2 = 0.209, chi2(2) = 357.43, p < 0.001 ***, Accuracy = 68.4%, N = 2115
summary(result)                           # full detailed SPSS-style output
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
summary(result, classification = FALSE)   # hide classification table
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
