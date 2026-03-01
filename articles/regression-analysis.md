# Regression Analysis

``` r
library(mariposa)
library(dplyr)
data(survey_data)
```

## Overview

Regression analysis predicts an outcome from one or more predictors.
mariposa provides two regression functions with SPSS-compatible output:

| Function                                                                                            | Use when…                                                     |
|-----------------------------------------------------------------------------------------------------|---------------------------------------------------------------|
| [`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)     | Your outcome is continuous (e.g., income, satisfaction score) |
| [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md) | Your outcome is binary (e.g., yes/no, high/low)               |

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
#> 
#> Linear Regression Results
#> -------------------------
#> - Formula: life_satisfaction ~ age + income + trust_government
#> - Method: ENTER (all predictors)
#> - N: 1995
#> 
#>   Model Summary
#>   ------------------------------------------------------------
#>   R                              0.445
#>   R Square                       0.198
#>   Adjusted R Square              0.197
#>   Std. Error of Estimate         1.028
#>   ------------------------------------------------------------
#> 
#>   ANOVA
#>   ------------------------------------------------------------------------------
#>   Source           Sum of Squares    df      Mean Square          F     Sig.
#>   ------------------------------------------------------------------------------
#>   Regression              519.387     3          173.129    163.886    0.000 ***
#>   Residual               2103.296  1991            1.056                     
#>   Total                  2622.684  1994                                      
#>   ------------------------------------------------------------------------------
#> 
#>   Coefficients
#>   ----------------------------------------------------------------------------------------
#>   Term                               B  Std.Error     Beta          t     Sig. 
#>   ----------------------------------------------------------------------------------------
#>   (Intercept)                    2.346      0.108              21.624    0.000 ***
#>   age                           -0.001      0.001   -0.009     -0.444    0.657 
#>   income                         0.000      0.000    0.445     22.164    0.000 ***
#>   trust_government              -0.002      0.020   -0.002     -0.117    0.907 
#>   ----------------------------------------------------------------------------------------
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```

Compare Beta values to see which predictor has the strongest effect.

### SPSS-Style Interface

If you prefer the SPSS approach with separate dependent and predictor
arguments:

``` r
linear_regression(survey_data,
                  dependent = life_satisfaction,
                  predictors = c(age, income, trust_government))
#> 
#> Linear Regression Results
#> -------------------------
#> - Formula: life_satisfaction ~ age + income + trust_government
#> - Method: ENTER (all predictors)
#> - N: 1995
#> 
#>   Model Summary
#>   ------------------------------------------------------------
#>   R                              0.445
#>   R Square                       0.198
#>   Adjusted R Square              0.197
#>   Std. Error of Estimate         1.028
#>   ------------------------------------------------------------
#> 
#>   ANOVA
#>   ------------------------------------------------------------------------------
#>   Source           Sum of Squares    df      Mean Square          F     Sig.
#>   ------------------------------------------------------------------------------
#>   Regression              519.387     3          173.129    163.886    0.000 ***
#>   Residual               2103.296  1991            1.056                     
#>   Total                  2622.684  1994                                      
#>   ------------------------------------------------------------------------------
#> 
#>   Coefficients
#>   ----------------------------------------------------------------------------------------
#>   Term                               B  Std.Error     Beta          t     Sig. 
#>   ----------------------------------------------------------------------------------------
#>   (Intercept)                    2.346      0.108              21.624    0.000 ***
#>   age                           -0.001      0.001   -0.009     -0.444    0.657 
#>   income                         0.000      0.000    0.445     22.164    0.000 ***
#>   trust_government              -0.002      0.020   -0.002     -0.117    0.907 
#>   ----------------------------------------------------------------------------------------
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```

Results are identical regardless of which interface you use.

### With Survey Weights

``` r
linear_regression(survey_data,
                  life_satisfaction ~ age + income,
                  weights = sampling_weight)
#> 
#> Weighted Linear Regression Results
#> ----------------------------------
#> - Formula: life_satisfaction ~ age + income
#> - Method: ENTER (all predictors)
#> - N: 2130
#> - Weights: sampling_weight
#> 
#>   Model Summary
#>   ------------------------------------------------------------
#>   R                              0.450
#>   R Square                       0.203
#>   Adjusted R Square              0.202
#>   Std. Error of Estimate         1.025
#>   ------------------------------------------------------------
#> 
#>   ANOVA
#>   ------------------------------------------------------------------------------
#>   Source           Sum of Squares    df      Mean Square          F     Sig.
#>   ------------------------------------------------------------------------------
#>   Regression              568.112     2          284.056    270.446    0.000 ***
#>   Residual               2234.041  2127            1.050                     
#>   Total                  2802.153  2129                                      
#>   ------------------------------------------------------------------------------
#> 
#>   Coefficients
#>   ----------------------------------------------------------------------------------------
#>   Term                               B  Std.Error     Beta          t     Sig. 
#>   ----------------------------------------------------------------------------------------
#>   (Intercept)                    2.310      0.091              25.321    0.000 ***
#>   age                           -0.001      0.001   -0.010     -0.525    0.600 
#>   income                         0.000      0.000    0.450     23.248    0.000 ***
#>   ----------------------------------------------------------------------------------------
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```

Weights are treated as frequency weights, matching SPSS WEIGHT BY
behavior.

### Grouped Analysis

Run separate regressions for each subgroup:

``` r
survey_data %>%
  group_by(region) %>%
  linear_regression(life_satisfaction ~ age + income)
#> 
#> Linear Regression Results
#> -------------------------
#> - Formula: life_satisfaction ~ age + income
#> - Method: ENTER (all predictors)
#> - Grouped by: region
#> 
#> 
#> Group: region = East
#> --------------------
#>   N: 410
#> 
#>   Model Summary
#>   ------------------------------------------------------------
#>   R                              0.451
#>   R Square                       0.203
#>   Adjusted R Square              0.199
#>   Std. Error of Estimate         1.081
#>   ------------------------------------------------------------
#> 
#>   ANOVA
#>   ------------------------------------------------------------------------------
#>   Source           Sum of Squares    df      Mean Square          F     Sig.
#>   ------------------------------------------------------------------------------
#>   Regression              121.386     2           60.693     51.954    0.000 ***
#>   Residual                475.465   407            1.168                     
#>   Total                   596.851   409                                      
#>   ------------------------------------------------------------------------------
#> 
#>   Coefficients
#>   ----------------------------------------------------------------------------------------
#>   Term                               B  Std.Error     Beta          t     Sig. 
#>   ----------------------------------------------------------------------------------------
#>   (Intercept)                    2.351      0.218              10.804    0.000 ***
#>   age                           -0.004      0.003   -0.053     -1.201    0.231 
#>   income                         0.000      0.000    0.450     10.170    0.000 ***
#>   ----------------------------------------------------------------------------------------
#> 
#> 
#> Group: region = West
#> --------------------
#>   N: 1705
#> 
#>   Model Summary
#>   ------------------------------------------------------------
#>   R                              0.449
#>   R Square                       0.201
#>   Adjusted R Square              0.200
#>   Std. Error of Estimate         1.013
#>   ------------------------------------------------------------
#> 
#>   ANOVA
#>   ------------------------------------------------------------------------------
#>   Source           Sum of Squares    df      Mean Square          F     Sig.
#>   ------------------------------------------------------------------------------
#>   Regression              440.543     2          220.272    214.576    0.000 ***
#>   Residual               1747.179  1702            1.027                     
#>   Total                  2187.722  1704                                      
#>   ------------------------------------------------------------------------------
#> 
#>   Coefficients
#>   ----------------------------------------------------------------------------------------
#>   Term                               B  Std.Error     Beta          t     Sig. 
#>   ----------------------------------------------------------------------------------------
#>   (Intercept)                    2.311      0.101              22.769    0.000 ***
#>   age                            0.000      0.001    0.001      0.042    0.966 
#>   income                         0.000      0.000    0.449     20.712    0.000 ***
#>   ----------------------------------------------------------------------------------------
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
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
[`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)
when your outcome is binary. First, create a binary variable:

``` r
survey_data <- survey_data %>%
  mutate(high_satisfaction = ifelse(life_satisfaction >= 4, 1, 0))
```

### Basic Logistic Regression

``` r
logistic_regression(survey_data, high_satisfaction ~ age + income)
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
#> 
#> Logistic Regression Results
#> ---------------------------
#> - Formula: high_satisfaction ~ age + income + trust_government + education
#> - Method: ENTER
#> - N: 1995
#> 
#>   Omnibus Tests of Model Coefficients
#>   --------------------------------------------------
#>                          Chi-square    df       Sig.
#>   --------------------------------------------------
#>   Model                     333.774     4      0.000 ***
#>   --------------------------------------------------
#> 
#>   Model Summary
#>   ------------------------------------------------------------
#>   -2 Log Likelihood                  2379.357
#>   Cox & Snell R Square                  0.154
#>   Nagelkerke R Square                   0.207
#>   McFadden R Square                     0.123
#>   ------------------------------------------------------------
#> 
#>   Hosmer and Lemeshow Test
#>   --------------------------------------------------
#>                          Chi-square    df       Sig.
#>   --------------------------------------------------
#>                             127.026     8      0.000
#>   --------------------------------------------------
#> 
#>   Classification Table (cutoff = 0.50)
#>   -----------------------------------------------------------------
#>                                   Predicted                     
#>   Observed                      0          1       % Correct
#>   -----------------------------------------------------------------
#>   0                           476        360           56.9
#>   1                           272        887           76.5
#>   -----------------------------------------------------------------
#>   Overall Percentage                                   68.3
#>   -----------------------------------------------------------------
#> 
#>   Variables in the Equation
#>   -----------------------------------------------------------------------------------------------
#>   Term                         B      S.E.      Wald   df     Sig.     Exp(B)     Lower     Upper 
#>   -----------------------------------------------------------------------------------------------
#>   (Intercept)             -2.244     0.247    82.726    1    0.000      0.106                     ***
#>   age                      0.002     0.003     0.424    1    0.515      1.002     0.996     1.008 
#>   income                   0.001     0.000   182.145    1    0.000      1.001     1.001     1.001 ***
#>   trust_government        -0.009     0.043     0.043    1    0.836      0.991     0.912     1.078 
#>   education               -0.006     0.056     0.013    1    0.910      0.994     0.890     1.110 
#>   -----------------------------------------------------------------------------------------------
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```

### SPSS-Style Interface

``` r
logistic_regression(survey_data,
                    dependent = high_satisfaction,
                    predictors = c(age, income, trust_government))
#> 
#> Logistic Regression Results
#> ---------------------------
#> - Formula: high_satisfaction ~ age + income + trust_government
#> - Method: ENTER
#> - N: 1995
#> 
#>   Omnibus Tests of Model Coefficients
#>   --------------------------------------------------
#>                          Chi-square    df       Sig.
#>   --------------------------------------------------
#>   Model                     333.761     3      0.000 ***
#>   --------------------------------------------------
#> 
#>   Model Summary
#>   ------------------------------------------------------------
#>   -2 Log Likelihood                  2379.370
#>   Cox & Snell R Square                  0.154
#>   Nagelkerke R Square                   0.207
#>   McFadden R Square                     0.123
#>   ------------------------------------------------------------
#> 
#>   Hosmer and Lemeshow Test
#>   --------------------------------------------------
#>                          Chi-square    df       Sig.
#>   --------------------------------------------------
#>                             136.994     8      0.000
#>   --------------------------------------------------
#> 
#>   Classification Table (cutoff = 0.50)
#>   -----------------------------------------------------------------
#>                                   Predicted                     
#>   Observed                      0          1       % Correct
#>   -----------------------------------------------------------------
#>   0                           476        360           56.9
#>   1                           271        888           76.6
#>   -----------------------------------------------------------------
#>   Overall Percentage                                   68.4
#>   -----------------------------------------------------------------
#> 
#>   Variables in the Equation
#>   -----------------------------------------------------------------------------------------------
#>   Term                         B      S.E.      Wald   df     Sig.     Exp(B)     Lower     Upper 
#>   -----------------------------------------------------------------------------------------------
#>   (Intercept)             -2.246     0.246    83.653    1    0.000      0.106                     ***
#>   age                      0.002     0.003     0.422    1    0.516      1.002     0.996     1.008 
#>   income                   0.001     0.000   250.425    1    0.000      1.001     1.001     1.001 ***
#>   trust_government        -0.009     0.043     0.042    1    0.837      0.991     0.912     1.078 
#>   -----------------------------------------------------------------------------------------------
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```

### With Survey Weights

``` r
logistic_regression(survey_data,
                    high_satisfaction ~ age + income,
                    weights = sampling_weight)
#> 
#> Weighted Logistic Regression Results
#> ------------------------------------
#> - Formula: high_satisfaction ~ age + income
#> - Method: ENTER
#> - N: 2130
#> - Weights: sampling_weight
#> 
#>   Omnibus Tests of Model Coefficients
#>   --------------------------------------------------
#>                          Chi-square    df       Sig.
#>   --------------------------------------------------
#>   Model                     357.399     2      0.000 ***
#>   --------------------------------------------------
#> 
#>   Model Summary
#>   ------------------------------------------------------------
#>   -2 Log Likelihood                  2520.045
#>   Cox & Snell R Square                  0.154
#>   Nagelkerke R Square                   0.208
#>   McFadden R Square                     0.124
#>   ------------------------------------------------------------
#> 
#>   Hosmer and Lemeshow Test
#>   --------------------------------------------------
#>                          Chi-square    df       Sig.
#>   --------------------------------------------------
#>                             144.598     8      0.000
#>   --------------------------------------------------
#> 
#>   Classification Table (cutoff = 0.50)
#>   -----------------------------------------------------------------
#>                                   Predicted                     
#>   Observed                      0          1       % Correct
#>   -----------------------------------------------------------------
#>   0                           513        382           57.2
#>   1                           292        942           76.3
#>   -----------------------------------------------------------------
#>   Overall Percentage                                   68.3
#>   -----------------------------------------------------------------
#> 
#>   Variables in the Equation
#>   -----------------------------------------------------------------------------------------------
#>   Term                         B      S.E.      Wald   df     Sig.     Exp(B)     Lower     Upper 
#>   -----------------------------------------------------------------------------------------------
#>   (Intercept)             -2.281     0.211   116.513    1    0.000      0.102                     ***
#>   age                      0.001     0.003     0.220    1    0.639      1.001     0.996     1.007 
#>   income                   0.001     0.000   271.805    1    0.000      1.001     1.001     1.001 ***
#>   -----------------------------------------------------------------------------------------------
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```

### Grouped Analysis

``` r
survey_data %>%
  group_by(region) %>%
  logistic_regression(high_satisfaction ~ age + income)
#> 
#> Logistic Regression Results
#> ---------------------------
#> - Formula: high_satisfaction ~ age + income
#> - Method: ENTER
#> - Grouped by: region
#> 
#> 
#> Group: region = East
#> --------------------
#>   N: 410
#> 
#>   Omnibus Tests of Model Coefficients
#>   --------------------------------------------------
#>                          Chi-square    df       Sig.
#>   --------------------------------------------------
#>   Model                      57.878     2      0.000 ***
#>   --------------------------------------------------
#> 
#>   Model Summary
#>   ------------------------------------------------------------
#>   -2 Log Likelihood                   496.333
#>   Cox & Snell R Square                  0.132
#>   Nagelkerke R Square                   0.178
#>   McFadden R Square                     0.104
#>   ------------------------------------------------------------
#> 
#>   Hosmer and Lemeshow Test
#>   --------------------------------------------------
#>                          Chi-square    df       Sig.
#>   --------------------------------------------------
#>                              21.569     8      0.006
#>   --------------------------------------------------
#> 
#>   Classification Table (cutoff = 0.50)
#>   -----------------------------------------------------------------
#>                                   Predicted                     
#>   Observed                      0          1       % Correct
#>   -----------------------------------------------------------------
#>   0                            80         87           47.9
#>   1                            49        194           79.8
#>   -----------------------------------------------------------------
#>   Overall Percentage                                   66.8
#>   -----------------------------------------------------------------
#> 
#>   Variables in the Equation
#>   -----------------------------------------------------------------------------------------------
#>   Term                         B      S.E.      Wald   df     Sig.     Exp(B)     Lower     Upper 
#>   -----------------------------------------------------------------------------------------------
#>   (Intercept)             -1.709     0.462    13.707    1    0.000      0.181                     ***
#>   age                     -0.005     0.006     0.657    1    0.418      0.995     0.983     1.007 
#>   income                   0.001     0.000    44.939    1    0.000      1.001     1.000     1.001 ***
#>   -----------------------------------------------------------------------------------------------
#> 
#> 
#> Group: region = West
#> --------------------
#>   N: 1705
#> 
#>   Omnibus Tests of Model Coefficients
#>   --------------------------------------------------
#>                          Chi-square    df       Sig.
#>   --------------------------------------------------
#>   Model                     301.113     2      0.000 ***
#>   --------------------------------------------------
#> 
#>   Model Summary
#>   ------------------------------------------------------------
#>   -2 Log Likelihood                  2021.788
#>   Cox & Snell R Square                  0.162
#>   Nagelkerke R Square                   0.218
#>   McFadden R Square                     0.130
#>   ------------------------------------------------------------
#> 
#>   Hosmer and Lemeshow Test
#>   --------------------------------------------------
#>                          Chi-square    df       Sig.
#>   --------------------------------------------------
#>                             130.728     8      0.000
#>   --------------------------------------------------
#> 
#>   Classification Table (cutoff = 0.50)
#>   -----------------------------------------------------------------
#>                                   Predicted                     
#>   Observed                      0          1       % Correct
#>   -----------------------------------------------------------------
#>   0                           422        299           58.5
#>   1                           233        751           76.3
#>   -----------------------------------------------------------------
#>   Overall Percentage                                   68.8
#>   -----------------------------------------------------------------
#> 
#>   Variables in the Equation
#>   -----------------------------------------------------------------------------------------------
#>   Term                         B      S.E.      Wald   df     Sig.     Exp(B)     Lower     Upper 
#>   -----------------------------------------------------------------------------------------------
#>   (Intercept)             -2.393     0.239   100.044    1    0.000      0.091                     ***
#>   age                      0.003     0.003     0.783    1    0.376      1.003     0.997     1.009 
#>   income                   0.001     0.000   223.355    1    0.000      1.001     1.001     1.001 ***
#>   -----------------------------------------------------------------------------------------------
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
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
#> 
#> Pearson Correlation 
#> --------------------
#> 
#> - Missing data handling: pairwise deletion
#> - Confidence level: 95.0%
#> 
#> 
#> Correlation Matrix:
#> ------------------- 
#>                   life_satisfaction     age  income trust_government
#> life_satisfaction             1.000  -0.029   0.448            0.006
#> age                          -0.029   1.000  -0.007            0.002
#> income                        0.448  -0.007   1.000            0.000
#> trust_government              0.006   0.002   0.000            1.000
#> ------------------- 
#> 
#> Significance Matrix (p-values):
#> ------------------------------- 
#>                   life_satisfaction     age  income trust_government
#> life_satisfaction            0.0000  0.1578  0.0000           0.7607
#> age                          0.1578  0.0000  0.7608           0.9035
#> income                       0.0000  0.7608  0.0000           0.9909
#> trust_government             0.7607  0.9035  0.9909           0.0000
#> ------------------------------- 
#> 
#> Sample Size Matrix:
#> ------------------- 
#>                   life_satisfaction   age income trust_government
#> life_satisfaction              2421  2421   2115             2280
#> age                            2421  2500   2186             2354
#> income                         2115  2186   2186             2061
#> trust_government               2280  2354   2061             2354
#> ------------------- 
#> 
#> Pairwise Results:
#> ---------------- 
#>                         Variable_Pair      r r_squared p_value           CI_95
#>               life_satisfaction × age -0.029     0.001  0.1578 [-0.068, 0.011]
#>            life_satisfaction × income  0.448     0.201  0.0000  [0.413, 0.482]
#>  life_satisfaction × trust_government  0.006     0.000  0.7607 [-0.035, 0.047]
#>                          age × income -0.007     0.000  0.7608 [-0.048, 0.035]
#>                age × trust_government  0.002     0.000  0.9035 [-0.038, 0.043]
#>             income × trust_government  0.000     0.000  0.9909 [-0.043, 0.043]
#>     n sig
#>  2421    
#>  2115 ***
#>  2280    
#>  2186    
#>  2354    
#>  2061    
#> ---------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Correlation Strength Interpretation:
#>   |r| < 0.30:        Weak correlation
#>   0.30 ≤ |r| < 0.70: Moderate correlation
#>   |r| ≥ 0.70:        Strong correlation
#> 
#> r² represents the proportion of variance explained

# 2. Run the regression
linear_regression(survey_data,
                  life_satisfaction ~ age + income + trust_government,
                  weights = sampling_weight)
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
logistic_regression(survey_data,
                    high_satisfaction ~ age + income + trust_government,
                    weights = sampling_weight)
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
    [`pearson_cor()`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md)
    to explore bivariate relationships before building a regression
    model.

2.  **Examine descriptives.** Use
    [`describe()`](https://YannickDiehl.github.io/mariposa/reference/describe.md)
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
    [`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)
    with a 0/1 outcome – predicted values can fall outside 0-1, and
    statistical tests are invalid.

## Summary

1.  [`linear_regression()`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)
    predicts continuous outcomes, providing B, Beta, ANOVA table, and
    R-squared
2.  [`logistic_regression()`](https://YannickDiehl.github.io/mariposa/reference/logistic_regression.md)
    predicts binary outcomes, providing odds ratios, classification
    table, and pseudo R-squared
3.  Both support formula and SPSS-style interfaces, survey weights, and
    grouped analysis
4.  Always check correlations and descriptives before building
    regression models

## Next Steps

- Create reliable scale scores first – see
  [`vignette("scale-analysis")`](https://YannickDiehl.github.io/mariposa/articles/scale-analysis.md)
- Compare group means directly – see
  [`vignette("hypothesis-testing")`](https://YannickDiehl.github.io/mariposa/articles/hypothesis-testing.md)
- Explore bivariate relationships – see
  [`vignette("correlation-analysis")`](https://YannickDiehl.github.io/mariposa/articles/correlation-analysis.md)
- Handle survey weights properly – see
  [`vignette("survey-weights")`](https://YannickDiehl.github.io/mariposa/articles/survey-weights.md)
