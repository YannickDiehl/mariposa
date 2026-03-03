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

## Value

An object of class `"linear_regression"` containing:

- coefficients:

  Tibble with B, Std.Error, Beta, t, p, CI_lower, CI_upper

- model_summary:

  List with R, R_squared, adj_R_squared, std_error

- anova:

  Tibble with Sum of Squares, df, Mean Square, F, Sig.

- descriptives:

  Tibble with Mean, Std.Deviation, N for all variables

- model:

  The underlying `lm` object

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
[`logistic_regression`](https://YannickDiehl.github.io/mariposa/dev/reference/logistic_regression.md)
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
intercept.

**Grouped Analysis**: When `data` is grouped via
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),
a separate regression is run for each group (matching SPSS SPLIT FILE
BY).

## See also

[`logistic_regression`](https://YannickDiehl.github.io/mariposa/dev/reference/logistic_regression.md)
for binary outcome variables.

[`describe`](https://YannickDiehl.github.io/mariposa/dev/reference/describe.md)
for checking variable distributions before regression.

[`pearson_cor`](https://YannickDiehl.github.io/mariposa/dev/reference/pearson_cor.md)
for checking bivariate correlations.

Other regression:
[`logistic_regression()`](https://YannickDiehl.github.io/mariposa/dev/reference/logistic_regression.md)

## Examples

``` r
library(dplyr)
data(survey_data)

# Bivariate regression
linear_regression(survey_data, life_satisfaction ~ age)
#> 
#> Linear Regression Results
#> -------------------------
#> - Formula: life_satisfaction ~ age
#> - Method: ENTER (all predictors)
#> - N: 2421
#> 
#>   Model Summary
#>   ------------------------------------------------------------
#>   R                              0.029
#>   R Square                       0.001
#>   Adjusted R Square              0.000
#>   Std. Error of Estimate         1.153
#>   ------------------------------------------------------------
#> 
#>   ANOVA
#>   ------------------------------------------------------------------------------
#>   Source           Sum of Squares    df      Mean Square          F     Sig.
#>   ------------------------------------------------------------------------------
#>   Regression                2.653     1            2.653      1.996    0.158 
#>   Residual               3214.775  2419            1.329                     
#>   Total                  3217.428  2420                                      
#>   ------------------------------------------------------------------------------
#> 
#>   Coefficients
#>   ----------------------------------------------------------------------------------------
#>   Term                               B  Std.Error     Beta          t     Sig. 
#>   ----------------------------------------------------------------------------------------
#>   (Intercept)                    3.727      0.074              50.663    0.000 ***
#>   age                           -0.002      0.001   -0.029     -1.413    0.158 
#>   ----------------------------------------------------------------------------------------
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05

# Multiple regression
linear_regression(survey_data, income ~ age + education + life_satisfaction)
#> 
#> Linear Regression Results
#> -------------------------
#> - Formula: income ~ age + education + life_satisfaction
#> - Method: ENTER (all predictors)
#> - N: 2115
#> 
#>   Model Summary
#>   ------------------------------------------------------------
#>   R                              0.686
#>   R Square                       0.471
#>   Adjusted R Square              0.470
#>   Std. Error of Estimate      1041.889
#>   ------------------------------------------------------------
#> 
#>   ANOVA
#>   ------------------------------------------------------------------------------
#>   Source           Sum of Squares    df      Mean Square          F     Sig.
#>   ------------------------------------------------------------------------------
#>   Regression       2036941094.578     3    678980364.859    625.481    0.000 ***
#>   Residual         2291561553.177  2111      1085533.659                     
#>   Total            4328502647.754  2114                                      
#>   ------------------------------------------------------------------------------
#> 
#>   Coefficients
#>   ----------------------------------------------------------------------------------------
#>   Term                               B  Std.Error     Beta          t     Sig. 
#>   ----------------------------------------------------------------------------------------
#>   (Intercept)                  800.493    105.893               7.559    0.000 ***
#>   age                           -0.194      1.333   -0.002     -0.145    0.885 
#>   education                    711.841     21.706    0.539     32.794    0.000 ***
#>   life_satisfaction            377.527     20.505    0.303     18.412    0.000 ***
#>   ----------------------------------------------------------------------------------------
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05

# SPSS-style interface
linear_regression(survey_data,
                  dependent = life_satisfaction,
                  predictors = c(trust_government, trust_media, trust_science))
#> 
#> Linear Regression Results
#> -------------------------
#> - Formula: life_satisfaction ~ trust_government + trust_media + trust_science
#> - Method: ENTER (all predictors)
#> - N: 2066
#> 
#>   Model Summary
#>   ------------------------------------------------------------
#>   R                              0.041
#>   R Square                       0.002
#>   Adjusted R Square              0.000
#>   Std. Error of Estimate         1.144
#>   ------------------------------------------------------------
#> 
#>   ANOVA
#>   ------------------------------------------------------------------------------
#>   Source           Sum of Squares    df      Mean Square          F     Sig.
#>   ------------------------------------------------------------------------------
#>   Regression                4.570     3            1.523      1.164    0.322 
#>   Residual               2699.432  2062            1.309                     
#>   Total                  2704.002  2065                                      
#>   ------------------------------------------------------------------------------
#> 
#>   Coefficients
#>   ----------------------------------------------------------------------------------------
#>   Term                               B  Std.Error     Beta          t     Sig. 
#>   ----------------------------------------------------------------------------------------
#>   (Intercept)                    3.675      0.118              31.054    0.000 ***
#>   trust_government              -0.008      0.022   -0.008     -0.361    0.718 
#>   trust_media                    0.035      0.022    0.035      1.598    0.110 
#>   trust_science                 -0.023      0.024   -0.021     -0.932    0.351 
#>   ----------------------------------------------------------------------------------------
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05

# Weighted regression
linear_regression(survey_data, life_satisfaction ~ age, weights = sampling_weight)
#> 
#> Weighted Linear Regression Results
#> ----------------------------------
#> - Formula: life_satisfaction ~ age
#> - Method: ENTER (all predictors)
#> - N: 2437
#> - Weights: sampling_weight
#> 
#>   Model Summary
#>   ------------------------------------------------------------
#>   R                              0.029
#>   R Square                       0.001
#>   Adjusted R Square              0.000
#>   Std. Error of Estimate         1.152
#>   ------------------------------------------------------------
#> 
#>   ANOVA
#>   ------------------------------------------------------------------------------
#>   Source           Sum of Squares    df      Mean Square          F     Sig.
#>   ------------------------------------------------------------------------------
#>   Regression                2.757     1            2.757      2.078    0.150 
#>   Residual               3230.392  2435            1.327                     
#>   Total                  3233.149  2436                                      
#>   ------------------------------------------------------------------------------
#> 
#>   Coefficients
#>   ----------------------------------------------------------------------------------------
#>   Term                               B  Std.Error     Beta          t     Sig. 
#>   ----------------------------------------------------------------------------------------
#>   (Intercept)                    3.724      0.073              51.156    0.000 ***
#>   age                           -0.002      0.001   -0.029     -1.441    0.150 
#>   ----------------------------------------------------------------------------------------
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05

# Grouped by region
survey_data |>
  dplyr::group_by(region) |>
  linear_regression(life_satisfaction ~ age)
#> 
#> Linear Regression Results
#> -------------------------
#> - Formula: life_satisfaction ~ age
#> - Method: ENTER (all predictors)
#> - Grouped by: region
#> 
#> 
#> Group: region = East
#> --------------------
#>   N: 465
#> 
#>   Model Summary
#>   ------------------------------------------------------------
#>   R                              0.043
#>   R Square                       0.002
#>   Adjusted R Square             -0.000
#>   Std. Error of Estimate         1.207
#>   ------------------------------------------------------------
#> 
#>   ANOVA
#>   ------------------------------------------------------------------------------
#>   Source           Sum of Squares    df      Mean Square          F     Sig.
#>   ------------------------------------------------------------------------------
#>   Regression                1.276     1            1.276      0.876    0.350 
#>   Residual                674.350   463            1.456                     
#>   Total                   675.626   464                                      
#>   ------------------------------------------------------------------------------
#> 
#>   Coefficients
#>   ----------------------------------------------------------------------------------------
#>   Term                               B  Std.Error     Beta          t     Sig. 
#>   ----------------------------------------------------------------------------------------
#>   (Intercept)                    3.775      0.176              21.467    0.000 ***
#>   age                           -0.003      0.003   -0.043     -0.936    0.350 
#>   ----------------------------------------------------------------------------------------
#> 
#> 
#> Group: region = West
#> --------------------
#>   N: 1956
#> 
#>   Model Summary
#>   ------------------------------------------------------------
#>   R                              0.025
#>   R Square                       0.001
#>   Adjusted R Square              0.000
#>   Std. Error of Estimate         1.140
#>   ------------------------------------------------------------
#> 
#>   ANOVA
#>   ------------------------------------------------------------------------------
#>   Source           Sum of Squares    df      Mean Square          F     Sig.
#>   ------------------------------------------------------------------------------
#>   Regression                1.556     1            1.556      1.197    0.274 
#>   Residual               2540.200  1954            1.300                     
#>   Total                  2541.756  1955                                      
#>   ------------------------------------------------------------------------------
#> 
#>   Coefficients
#>   ----------------------------------------------------------------------------------------
#>   Term                               B  Std.Error     Beta          t     Sig. 
#>   ----------------------------------------------------------------------------------------
#>   (Intercept)                    3.714      0.081              45.860    0.000 ***
#>   age                           -0.002      0.002   -0.025     -1.094    0.274 
#>   ----------------------------------------------------------------------------------------
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
