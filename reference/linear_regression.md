# Run a Linear Regression

`linear_regression()` performs bivariate or multiple linear regression
with SPSS-compatible output. Wraps
[`stats::lm()`](https://rdrr.io/r/stats/lm.html) and adds standardized
coefficients (Beta), a formatted ANOVA table, and a model summary
matching SPSS REGRESSION output.

Supports two interface styles:

- **Formula interface:**
  `linear_regression(data, life_satisfaction ~ age + education)`

- **SPSS-style:**
  `linear_regression(data, dependent = life_satisfaction, predictors = c(age, education))`

## Usage

``` r
linear_regression(
  data,
  formula = NULL,
  dependent = NULL,
  predictors = NULL,
  weights = NULL,
  use = c("listwise", "pairwise"),
  standardized = TRUE,
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
  formula is given.

- predictors:

  Predictor variable(s) (unquoted, supports tidyselect). Used with
  `dependent` when no formula is given.

- weights:

  Optional survey weights (unquoted variable name). When specified,
  weighted least squares (WLS) is used, matching SPSS WEIGHT BY.

- use:

  How to handle missing data: `"listwise"` (default) drops any case with
  a missing value on any variable (matching SPSS /MISSING LISTWISE).
  `"pairwise"` computes the regression from a pairwise
  covariance/correlation matrix, retaining more cases (matching SPSS
  /MISSING PAIRWISE).

- standardized:

  Logical. If `TRUE` (default), standardized coefficients (Beta) are
  calculated and included in the output.

- conf.level:

  Confidence level for coefficient intervals (default 0.95).

- factors:

  How factor predictors are entered into the model: `"dummy"` (default,
  matches base R [`lm()`](https://rdrr.io/r/stats/lm.html)) expands a
  factor with `L` levels into `L - 1` dummy contrasts; `"numeric"`
  silently coerces factor levels to their integer codes, matching SPSS
  `REGRESSION` default behavior (ordinal-as-scale). The "numeric" mode
  emits a one-line
  [`cli::cli_inform()`](https://cli.r-lib.org/reference/cli_abort.html)
  listing the coerced variables. The "numeric" mode is required to
  reproduce SPSS results when factor predictors carry ordered meaning
  (e.g., 4-level education).

## Value

For ungrouped + listwise data, an object of class
`c("linear_regression", "lm")` — **the fitted `lm` itself**, with
mariposa-specific slots attached:

- coef_table:

  SPSS-style tibble with B, Std.Error, Beta, t, p, CI_lower, CI_upper.
  For weighted models, SE / t / p are adjusted to SPSS's
  frequency-weight df (see Technical Details).

- anova_table:

  SPSS-style overall-model ANOVA tibble (Source × Sum_of_Squares / df /
  Mean_Square / F_statistic / Sig).

- model_summary:

  List with R, R_squared, adj_R_squared, std_error.

- descriptives:

  Tibble with Mean, Std.Deviation, N for all variables.

- n:

  Sample size (listwise complete cases; weighted N when weighted).

- formula, dependent, predictor_names, weighted, weight_name, use,
  is_grouped, standardized, conf.level:

  Call metadata.

Because the object inherits from `"lm"`, all standard generics
([`predict()`](https://rdrr.io/r/stats/predict.html),
[`anova()`](https://rdrr.io/r/stats/anova.html),
[`vcov()`](https://rdrr.io/r/stats/vcov.html),
[`confint()`](https://rdrr.io/r/stats/confint.html),
[`residuals()`](https://rdrr.io/r/stats/residuals.html),
[`fitted()`](https://rdrr.io/r/stats/fitted.values.html),
[`coef()`](https://rdrr.io/r/stats/coef.html),
[`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html),
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html),
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html),
[`broom::augment()`](https://generics.r-lib.org/reference/augment.html))
dispatch natively without unwrapping.
[`summary()`](https://rdrr.io/r/base/summary.html) returns the
SPSS-style mariposa summary; for the raw lm summary use
[`stats::summary.lm()`](https://rdrr.io/r/stats/summary.lm.html) on the
same object.

For `use = "pairwise"` (no single fitted lm available) or for grouped
data, returns a list of class `"linear_regression"`. Pairwise results
expose the same SPSS-style tables but not the lm generics; grouped
results hold one fitted lm-inheriting model per group under `$groups`.

## Details

### Understanding the Results

The output includes four sections matching SPSS REGRESSION output:

- **Model Summary**: R, R-squared, Adjusted R-squared, and Standard
  Error of the Estimate. R-squared tells you how much variance in the
  dependent variable is explained by the predictors.

- **ANOVA**: Tests whether the overall model is significant. A
  significant F-test means at least one predictor matters.

- **Coefficients**: B (unstandardized), Beta (standardized), t-value,
  p-value, and confidence intervals for each predictor.

- **Descriptives**: Mean, SD, and N for all variables in the model.

Interpreting coefficients:

- **B (unstandardized)**: For each 1-unit increase in the predictor, the
  dependent variable changes by B units

- **Beta (standardized)**: Allows comparison across predictors with
  different scales. Larger absolute Beta = stronger effect

- **p-value**: Values below 0.05 indicate statistically significant
  predictors

### When to Use This

Use `linear_regression()` when:

- Your dependent variable is continuous (e.g., income, satisfaction
  score)

- You want to predict an outcome from one or more predictors

- You need standardized coefficients to compare predictor importance

For binary outcomes (yes/no, 0/1), use
[`logistic_regression`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)
instead.

### Technical Details

**Missing Data**: By default, listwise deletion is used (matching SPSS
REGRESSION /MISSING LISTWISE). Set `use = "pairwise"` to match SPSS
/MISSING PAIRWISE, which computes the regression from a pairwise
covariance matrix. Pairwise deletion retains more cases and produces
results closer to SPSS output when data has varying patterns of
missingness.

**Weights**: When weights are specified, they are treated as frequency
weights (matching SPSS WEIGHT BY behavior). The model is fitted using
weighted least squares via `lm(weights = ...)`.

**Standardized Coefficients**: Beta = B \* (SD_x / SD_y). This matches
the SPSS standardized coefficient output. Not available for the
intercept. For dummy-coded factor terms (`factors = "dummy"`), the SD of
the contrast column from the design matrix is used.

**Factor Predictors**: By default (`factors = "dummy"`), factor
predictors are expanded into `L - 1` dummy contrasts via R's
[`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html),
matching base R [`lm()`](https://rdrr.io/r/stats/lm.html). Pass
`factors = "numeric"` to silently coerce factor levels to their integer
codes (SPSS `REGRESSION` default). The "numeric" mode is required to
reproduce SPSS results for ordinal predictors like education or Likert
scales that SPSS treats as continuous.

**Grouped Analysis**: When `data` is grouped via
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),
a separate regression is run for each group (matching SPSS SPLIT FILE
BY).

## See also

[`logistic_regression`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)
for binary outcome variables.

[`describe`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
for checking variable distributions before regression.

[`pearson_cor`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md)
for checking bivariate correlations.

[`summary.linear_regression`](https://YannickDiehl.github.io/mariposa/reference/summary.linear_regression.md)
for detailed output with toggleable sections.

Other regression:
[`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)

## Examples

``` r
library(dplyr)
data(survey_data)

# Bivariate regression
linear_regression(survey_data, life_satisfaction ~ age)
#> Linear Regression: life_satisfaction ~ age
#>   R2 = 0.001, adj.R2 = 0.000, F(1, 2419) = 2.00, p = 0.158 , N = 2421

# Multiple regression
linear_regression(survey_data, income ~ age + education + life_satisfaction)
#> Linear Regression: income ~ age + education + life_satisfaction
#>   R2 = 0.477, adj.R2 = 0.476, F(5, 2109) = 385.29, p < 0.001 ***, N = 2115

# SPSS-style interface
linear_regression(survey_data,
                  dependent = life_satisfaction,
                  predictors = c(trust_government, trust_media, trust_science))
#> Linear Regression: life_satisfaction ~ trust_government + trust_media + trust_science
#>   R2 = 0.002, adj.R2 = 0.000, F(3, 2062) = 1.16, p = 0.322 , N = 2066

# Weighted regression
linear_regression(survey_data, life_satisfaction ~ age, weights = sampling_weight)
#> Linear Regression: life_satisfaction ~ age [Weighted]
#>   R2 = 0.001, adj.R2 = 0.000, F(1, 2435) = 2.08, p = 0.150 , N = 2437

# Grouped by region
survey_data |>
  dplyr::group_by(region) |>
  linear_regression(life_satisfaction ~ age)
#> Linear Regression: life_satisfaction ~ age [Grouped: region]
#>   region = East: R2 = 0.002, adj.R2 = -0.000, F(1, 463) = 0.88, p = 0.350 , N = 465
#>   region = West: R2 = 0.001, adj.R2 = 0.000, F(1, 1954) = 1.20, p = 0.274 , N = 1956

# Factor predictors: dummy-coding (default, matches base R lm())
linear_regression(survey_data, income ~ age + education)
#> Linear Regression: income ~ age + education
#>   R2 = 0.391, adj.R2 = 0.390, F(4, 2181) = 349.72, p < 0.001 ***, N = 2186

# Factor predictors: SPSS-style ordinal-as-scale
linear_regression(survey_data, income ~ age + education,
                  factors = "numeric")
#> ℹ Factor predictor(s) coerced to numeric (SPSS-style ordinal scaling):
#> • `education`
#> Linear Regression: income ~ age + education
#>   R2 = 0.386, adj.R2 = 0.386, F(2, 2183) = 686.59, p < 0.001 ***, N = 2186

# --- Three-layer output ---
result <- linear_regression(survey_data, life_satisfaction ~ age + income)
result                                  # compact one-line overview
#> Linear Regression: life_satisfaction ~ age + income
#>   R2 = 0.201, adj.R2 = 0.200, F(2, 2112) = 265.60, p < 0.001 ***, N = 2115
summary(result)                         # full detailed SPSS-style output
#> 
#> Linear Regression Results
#> -------------------------
#> - Formula: life_satisfaction ~ age + income
#> - Method: ENTER (all predictors)
#> - N: 2115
#> 
#>   Descriptive Statistics
#>   ----------------------------------------------------------------------
#>   Variable                                    Mean     Std.Dev.      N
#>   ----------------------------------------------------------------------
#>   life_satisfaction                          3.638        1.148   2115
#>   age                                       50.827       16.995   2115
#>   income                                  3757.683     1430.923   2115
#>   ----------------------------------------------------------------------
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
#>   Collinearity Statistics
#>   --------------------------------------------------
#>   Term                       Tolerance        VIF
#>   --------------------------------------------------
#>   age                            1.000      1.000
#>   income                         1.000      1.000
#>   --------------------------------------------------
#>   VIF > 10 (Tolerance < 0.1) indicates problematic collinearity.
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
summary(result, descriptives = FALSE)   # hide descriptives section
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
#>   Collinearity Statistics
#>   --------------------------------------------------
#>   Term                       Tolerance        VIF
#>   --------------------------------------------------
#>   age                            1.000      1.000
#>   income                         1.000      1.000
#>   --------------------------------------------------
#>   VIF > 10 (Tolerance < 0.1) indicates problematic collinearity.
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
