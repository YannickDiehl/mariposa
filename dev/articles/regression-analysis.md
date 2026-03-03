# Regression Analysis

``` r
library(mariposa)
library(dplyr)
data(survey_data)
```

## Overview

Regression analysis predicts an outcome from one or more predictors.
mariposa provides two regression functions with SPSS-compatible output:

| Function                                                                                                | Use when…                                                     |
|---------------------------------------------------------------------------------------------------------|---------------------------------------------------------------|
| [`linear_regression()`](https://YannickDiehl.github.io/mariposa/dev/reference/linear_regression.md)     | Your outcome is continuous (e.g., income, satisfaction score) |
| [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/dev/reference/logistic_regression.md) | Your outcome is binary (e.g., yes/no, high/low)               |

Both functions support two interface styles:

- **Formula interface:** `linear_regression(data, y ~ x1 + x2)` –
  standard R style
- **SPSS-style:**
  `linear_regression(data, dependent = y, predictors = c(x1, x2))` –
  familiar for SPSS users

## Linear Regression

### Bivariate Regression

Start with a single predictor:

``` r
linear_regression(survey_data, life_satisfaction ~ age)
#> Linear Regression: life_satisfaction ~ age
#>   R2 = 0.001, adj.R2 = 0.000, F(1, 2419) = 2.00, p = 0.158 , N = 2421
```

The output includes four sections matching SPSS REGRESSION:

- **Model Summary**: R, R-squared, Adjusted R-squared
- **ANOVA Table**: Overall model significance
- **Coefficients**: B (unstandardized), Beta (standardized), t, p, and
  confidence intervals
- **Descriptives**: Mean and SD for all variables

### Understanding the Coefficients

- **B (unstandardized):** For each 1-unit increase in the predictor, the
  outcome changes by B units. The intercept is the predicted value when
  all predictors are zero.
- **Beta (standardized):** Allows comparison across predictors measured
  on different scales. A Beta of 0.30 means a 1-SD increase in the
  predictor is associated with a 0.30-SD change in the outcome.
- **p-value:** Values below 0.05 indicate that the predictor’s effect is
  statistically significant.

### Multiple Regression

Add more predictors to explain more variance:

``` r
linear_regression(survey_data, life_satisfaction ~ age + income + trust_government)
#> Linear Regression: life_satisfaction ~ age + income + trust_government
#>   R2 = 0.198, adj.R2 = 0.197, F(3, 1991) = 163.89, p < 0.001 ***, N = 1995
```

Compare Beta values to see which predictor has the strongest effect.

### SPSS-Style Interface

If you prefer the SPSS approach with separate dependent and predictor
arguments:

``` r
linear_regression(survey_data,
                  dependent = life_satisfaction,
                  predictors = c(age, income, trust_government))
#> Linear Regression: life_satisfaction ~ age + income + trust_government
#>   R2 = 0.198, adj.R2 = 0.197, F(3, 1991) = 163.89, p < 0.001 ***, N = 1995
```

Results are identical regardless of which interface you use.

### With Survey Weights

``` r
linear_regression(survey_data,
                  life_satisfaction ~ age + income,
                  weights = sampling_weight)
#> Linear Regression: life_satisfaction ~ age + income [Weighted]
#>   R2 = 0.203, adj.R2 = 0.202, F(2, 2127) = 270.45, p < 0.001 ***, N = 2130
```

Weights are treated as frequency weights, matching SPSS WEIGHT BY
behavior.

### Grouped Analysis

Run separate regressions for each subgroup:

``` r
survey_data %>%
  group_by(region) %>%
  linear_regression(life_satisfaction ~ age + income)
#> Linear Regression: life_satisfaction ~ age + income [Grouped: region]
#>   region = East: R2 = 0.203, adj.R2 = 0.199, F(2, 407) = 51.95, p < 0.001 ***, N = 410
#>   region = West: R2 = 0.201, adj.R2 = 0.200, F(2, 1702) = 214.58, p < 0.001 ***, N = 1705
```

This matches SPSS SPLIT FILE BY – a separate model is fitted for each
group.

### Interpreting R-squared

R-squared tells you how much variance your predictors explain:

- R-squared = 0.01 – 0.05: Small effect
- R-squared = 0.06 – 0.13: Medium effect
- R-squared = 0.14+: Large effect

These benchmarks follow Cohen’s (1988) conventions for the social
sciences. Always check the ANOVA table to confirm the model is
statistically significant.

## Logistic Regression

### When to Use

Use
[`logistic_regression()`](https://YannickDiehl.github.io/mariposa/dev/reference/logistic_regression.md)
when your outcome is binary. First, create a binary variable:

``` r
survey_data <- survey_data %>%
  mutate(high_satisfaction = ifelse(life_satisfaction >= 4, 1, 0))
```

### Basic Logistic Regression

``` r
logistic_regression(survey_data, high_satisfaction ~ age + income)
#> Logistic Regression: high_satisfaction ~ age + income
#>   Nagelkerke R2 = 0.209, chi2(2) = 357.43, p < 0.001 ***, Accuracy = 68.4%, N = 2115
```

The output includes five sections matching SPSS LOGISTIC REGRESSION:

- **Omnibus Test**: Is the overall model significant?
- **Model Summary**: -2 Log Likelihood and pseudo R-squared values
- **Hosmer-Lemeshow Test**: Does the model fit the data well?
- **Classification Table**: How well does the model classify cases?
- **Coefficients**: B, Wald, Exp(B) (odds ratios), and confidence
  intervals

### Understanding Odds Ratios

The key statistic in logistic regression is Exp(B), the odds ratio:

- **Exp(B) \> 1:** Each unit increase in the predictor increases the
  odds of the outcome
- **Exp(B) \< 1:** Each unit increase in the predictor decreases the
  odds
- **Exp(B) = 1:** No effect

For example, Exp(B) = 1.50 means the odds increase by 50% for each
1-unit increase in the predictor. Exp(B) = 0.80 means the odds decrease
by 20%.

### With Multiple Predictors

``` r
logistic_regression(survey_data,
                    high_satisfaction ~ age + income + trust_government + education)
#> Logistic Regression: high_satisfaction ~ age + income + trust_government + education
#>   Nagelkerke R2 = 0.207, chi2(4) = 333.77, p < 0.001 ***, Accuracy = 68.3%, N = 1995
```

### SPSS-Style Interface

``` r
logistic_regression(survey_data,
                    dependent = high_satisfaction,
                    predictors = c(age, income, trust_government))
#> Logistic Regression: high_satisfaction ~ age + income + trust_government
#>   Nagelkerke R2 = 0.207, chi2(3) = 333.76, p < 0.001 ***, Accuracy = 68.4%, N = 1995
```

### With Survey Weights

``` r
logistic_regression(survey_data,
                    high_satisfaction ~ age + income,
                    weights = sampling_weight)
#> Logistic Regression: high_satisfaction ~ age + income [Weighted]
#>   Nagelkerke R2 = 0.208, chi2(2) = 357.40, p < 0.001 ***, Accuracy = 68.3%, N = 2130
```

### Grouped Analysis

``` r
survey_data %>%
  group_by(region) %>%
  logistic_regression(high_satisfaction ~ age + income)
#> Logistic Regression: high_satisfaction ~ age + income [Grouped: region]
#>   region = East: Nagelkerke R2 = 0.178, chi2(2) = 57.88, p < 0.001 ***, Accuracy = 66.8%, N = 410
#>   region = West: Nagelkerke R2 = 0.218, chi2(2) = 301.11, p < 0.001 ***, Accuracy = 68.8%, N = 1705
```

### Interpreting Model Fit

**Pseudo R-squared** values are not directly comparable to linear
regression R-squared, but provide a rough indication of model quality:

- **Nagelkerke R-squared**: Adjusted to reach 1.0, most commonly
  reported
- **Cox & Snell R-squared**: Cannot reach 1.0, always lower
- **McFadden R-squared**: Values above 0.20 indicate good fit

**Hosmer-Lemeshow Test**: A non-significant result ($p > .05$) means the
model fits well. A significant result suggests the model may not
adequately describe the data.

**Classification Table**: Shows the percentage of cases correctly
predicted. Compare this to the base rate (percentage in the larger
group) – your model should do better than simply guessing the most
common outcome.

## Complete Example

A full regression analysis workflow:

``` r
# 1. Explore relationships first
survey_data %>%
  pearson_cor(life_satisfaction, age, income, trust_government)
#> Pearson Correlation: 4 variables
#>   life_satisfaction x age:       r = -0.029, p = 0.158  
#>   life_satisfaction x income:    r = 0.448, p < 0.001 *** 
#>   life_satisfaction x trust_government: r = 0.006, p = 0.761  
#>   age x income:                  r = -0.007, p = 0.761  
#>   age x trust_government:        r = 0.002, p = 0.904  
#>   income x trust_government:     r = 0.000, p = 0.991  
#>   1/6 pairs significant (p < .05), N = 2421

# 2. Run the regression
lm_result <- linear_regression(survey_data,
                               life_satisfaction ~ age + income + trust_government,
                               weights = sampling_weight)
lm_result              # compact: R-squared + significant predictors
#> Linear Regression: life_satisfaction ~ age + income + trust_government [Weighted]
#>   R2 = 0.200, adj.R2 = 0.199, F(3, 2005) = 167.49, p < 0.001 ***, N = 2009
summary(lm_result)     # detailed: coefficients, ANOVA table, collinearity
#> 
#> Weighted Linear Regression Results
#> ----------------------------------
#> - Formula: life_satisfaction ~ age + income + trust_government
#> - Method: ENTER (all predictors)
#> - N: 2009
#> - Weights: sampling_weight
#> 
#>   Model Summary
#>   ------------------------------------------------------------
#>   R                              0.448
#>   R Square                       0.200
#>   Adjusted R Square              0.199
#>   Std. Error of Estimate         1.026
#>   ------------------------------------------------------------
#> 
#>   ANOVA
#>   ------------------------------------------------------------------------------
#>   Source           Sum of Squares    df      Mean Square          F     Sig.
#>   ------------------------------------------------------------------------------
#>   Regression              529.253     3          176.418    167.490    0.000 ***
#>   Residual               2111.871  2005            1.053                     
#>   Total                  2641.124  2008                                      
#>   ------------------------------------------------------------------------------
#> 
#>   Coefficients
#>   ----------------------------------------------------------------------------------------
#>   Term                               B  Std.Error     Beta          t     Sig. 
#>   ----------------------------------------------------------------------------------------
#>   (Intercept)                    2.320      0.108              21.559    0.000 ***
#>   age                           -0.001      0.001   -0.009     -0.441    0.660 
#>   income                         0.000      0.000    0.448     22.409    0.000 ***
#>   trust_government               0.002      0.020    0.002      0.113    0.910 
#>   ----------------------------------------------------------------------------------------
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05

# 3. Create binary outcome for logistic regression
survey_data <- survey_data %>%
  mutate(high_satisfaction = ifelse(life_satisfaction >= 4, 1, 0))

# 4. Run logistic regression
log_result <- logistic_regression(survey_data,
                                  high_satisfaction ~ age + income + trust_government,
                                  weights = sampling_weight)
log_result             # compact: model significance + pseudo R-squared
#> Logistic Regression: high_satisfaction ~ age + income + trust_government [Weighted]
#>   Nagelkerke R2 = 0.207, chi2(3) = 333.71, p < 0.001 ***, Accuracy = 68.5%, N = 2009
summary(log_result)    # detailed: odds ratios, classification table
#> 
#> Weighted Logistic Regression Results
#> ------------------------------------
#> - Formula: high_satisfaction ~ age + income + trust_government
#> - Method: ENTER
#> - N: 2009
#> - Weights: sampling_weight
#> 
#>   Omnibus Tests of Model Coefficients
#>   --------------------------------------------------
#>                          Chi-square    df       Sig.
#>   --------------------------------------------------
#>   Model                     333.708     3      0.000 ***
#>   --------------------------------------------------
#> 
#>   Model Summary
#>   ------------------------------------------------------------
#>   -2 Log Likelihood                  2379.425
#>   Cox & Snell R Square                  0.153
#>   Nagelkerke R Square                   0.207
#>   McFadden R Square                     0.123
#>   ------------------------------------------------------------
#> 
#>   Hosmer and Lemeshow Test
#>   --------------------------------------------------
#>                          Chi-square    df       Sig.
#>   --------------------------------------------------
#>                             129.020     8      0.000
#>   --------------------------------------------------
#> 
#>   Classification Table (cutoff = 0.50)
#>   -----------------------------------------------------------------
#>                                   Predicted                     
#>   Observed                      0          1       % Correct
#>   -----------------------------------------------------------------
#>   0                           481        362           57.1
#>   1                           271        895           76.7
#>   -----------------------------------------------------------------
#>   Overall Percentage                                   68.5
#>   -----------------------------------------------------------------
#> 
#>   Variables in the Equation
#>   -----------------------------------------------------------------------------------------------
#>   Term                         B      S.E.      Wald   df     Sig.     Exp(B)     Lower     Upper 
#>   -----------------------------------------------------------------------------------------------
#>   (Intercept)             -2.289     0.245    87.436    1    0.000      0.101                     ***
#>   age                      0.002     0.003     0.524    1    0.469      1.002     0.996     1.008 
#>   income                   0.001     0.000   254.712    1    0.000      1.001     1.001     1.001 ***
#>   trust_government        -0.007     0.043     0.029    1    0.864      0.993     0.913     1.079 
#>   -----------------------------------------------------------------------------------------------
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```

## Practical Tips

1.  **Check correlations first.** Use
    [`pearson_cor()`](https://YannickDiehl.github.io/mariposa/dev/reference/pearson_cor.md)
    to explore bivariate relationships before building a regression
    model.

2.  **Examine descriptives.** Use
    [`describe()`](https://YannickDiehl.github.io/mariposa/dev/reference/describe.md)
    to check variable distributions, ranges, and missing data before
    running regression.

3.  **Compare Beta values.** In multiple regression, standardized Beta
    coefficients tell you which predictor has the strongest effect,
    regardless of measurement scale.

4.  **Report completely.** Always include R-squared, F-test (for linear)
    or Omnibus test (for logistic), individual coefficients, and sample
    size.

5.  **Watch for multicollinearity.** If predictors are highly correlated
    with each other, coefficients may be unstable. Check bivariate
    correlations before interpreting results.

6.  **Binary outcomes need logistic regression.** Never use
    [`linear_regression()`](https://YannickDiehl.github.io/mariposa/dev/reference/linear_regression.md)
    with a 0/1 outcome – predicted values can fall outside 0-1, and
    statistical tests are invalid.

## Summary

1.  [`linear_regression()`](https://YannickDiehl.github.io/mariposa/dev/reference/linear_regression.md)
    predicts continuous outcomes, providing B, Beta, ANOVA table, and
    R-squared
2.  [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/dev/reference/logistic_regression.md)
    predicts binary outcomes, providing odds ratios, classification
    table, and pseudo R-squared
3.  Both support formula and SPSS-style interfaces, survey weights, and
    grouped analysis
4.  Always check correlations and descriptives before building
    regression models

## Next Steps

- Create reliable scale scores first – see
  [`vignette("scale-analysis")`](https://YannickDiehl.github.io/mariposa/dev/articles/scale-analysis.md)
- Compare group means directly – see
  [`vignette("hypothesis-testing")`](https://YannickDiehl.github.io/mariposa/dev/articles/hypothesis-testing.md)
- Explore bivariate relationships – see
  [`vignette("correlation-analysis")`](https://YannickDiehl.github.io/mariposa/dev/articles/correlation-analysis.md)
- Handle survey weights properly – see
  [`vignette("survey-weights")`](https://YannickDiehl.github.io/mariposa/dev/articles/survey-weights.md)
