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
  conf.level = 0.95
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

## Value

An object of class `"logistic_regression"` containing:

- coefficients:

  Tibble with B, S.E., Wald, df, Sig., Exp(B), CI_lower, CI_upper

- model_summary:

  List with minus2LL, cox_snell_r2, nagelkerke_r2, mcfadden_r2

- omnibus_test:

  List with chi_sq, df, p for overall model test

- classification:

  List with table, overall_pct, pct_correct_0, pct_correct_1

- hosmer_lemeshow:

  List with chi_sq, df, p (goodness-of-fit test)

- model:

  The underlying `glm` object

- formula:

  The formula used

- n:

  Sample size (listwise complete cases)

- dependent:

  Name of the dependent variable

- predictor_names:

  Names of predictor variables

- weighted:

  Logical indicating whether weights were used

- weight_name:

  Name of the weight variable (or NULL)

Use [`summary()`](https://rdrr.io/r/base/summary.html) for the full
SPSS-style output with toggleable sections.

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
[`linear_regression`](https://YannickDiehl.github.io/mariposa/dev/reference/linear_regression.md)
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

**Grouped Analysis**: When `data` is grouped via
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),
a separate regression is run for each group (matching SPSS SPLIT FILE
BY).

## See also

[`linear_regression`](https://YannickDiehl.github.io/mariposa/dev/reference/linear_regression.md)
for continuous outcome variables.

[`chi_square`](https://YannickDiehl.github.io/mariposa/dev/reference/chi_square.md)
for testing associations between categorical variables.

[`summary.logistic_regression`](https://YannickDiehl.github.io/mariposa/dev/reference/summary.logistic_regression.md)
for detailed output with toggleable sections.

Other regression:
[`linear_regression()`](https://YannickDiehl.github.io/mariposa/dev/reference/linear_regression.md)

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
#>   Nagelkerke R2 = 0.209, chi2(3) = 357.46, p < 0.001 ***, Accuracy = 68.4%, N = 2115

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

# --- Three-layer output ---
result <- logistic_regression(survey_data, high_satisfaction ~ age + income)
result              # compact one-line overview
#> Logistic Regression: high_satisfaction ~ age + income
#>   Nagelkerke R2 = 0.209, chi2(2) = 357.43, p < 0.001 ***, Accuracy = 68.4%, N = 2115
summary(result)     # full detailed SPSS-style output
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
summary(result, classification_table = FALSE)  # hide classification
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
```
