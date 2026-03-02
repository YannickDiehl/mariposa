# Comparing Groups and Testing Hypotheses

``` r
library(mariposa)
library(dplyr)
data(survey_data)
```

## Overview

Statistical tests help you determine whether differences between groups
are real or just due to random chance. This guide covers all hypothesis
tests available in mariposa:

### Parametric Tests

| Test                                                                                        | Use when…                                              |
|---------------------------------------------------------------------------------------------|--------------------------------------------------------|
| [`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)                   | Comparing means between **two** groups                 |
| [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)       | Comparing means across **three or more** groups        |
| [`factorial_anova()`](https://YannickDiehl.github.io/mariposa/reference/factorial_anova.md) | Testing effects of **multiple factors** simultaneously |
| [`ancova()`](https://YannickDiehl.github.io/mariposa/reference/ancova.md)                   | Comparing groups while **controlling for a covariate** |

### Non-Parametric Tests

| Test                                                                                      | Use when…                                              |
|-------------------------------------------------------------------------------------------|--------------------------------------------------------|
| [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md)     | Two-group comparison with **non-normal** data          |
| [`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md) | Three or more groups with **non-normal** data          |
| [`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/reference/wilcoxon_test.md)   | **Paired** comparison with non-normal data             |
| [`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md)   | **Repeated measures** with non-normal data             |
| [`binomial_test()`](https://YannickDiehl.github.io/mariposa/reference/binomial_test.md)   | Testing whether a **proportion** differs from expected |

### Categorical Tests

| Test                                                                                  | Use when…                                                      |
|---------------------------------------------------------------------------------------|----------------------------------------------------------------|
| [`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)     | Testing relationships between **categorical** variables        |
| [`fisher_test()`](https://YannickDiehl.github.io/mariposa/reference/fisher_test.md)   | Like chi-square but for **small samples**                      |
| [`chisq_gof()`](https://YannickDiehl.github.io/mariposa/reference/chisq_gof.md)       | Testing if observed frequencies match **expected** proportions |
| [`mcnemar_test()`](https://YannickDiehl.github.io/mariposa/reference/mcnemar_test.md) | Comparing **paired proportions** (before/after)                |

## t-Tests

### Independent Samples t-Test

Compare average life satisfaction between genders:

``` r
survey_data %>%
  t_test(life_satisfaction, group = gender)
#> t-Test Results
#> --------------
#> 
#> - Grouping variable: gender
#> - Groups compared: Male vs. Female
#> - Confidence level: 95.0%
#> - Alternative hypothesis: two.sided
#> - Null hypothesis (mu): 0.000
#> 
#> 
#> --- life_satisfaction ---
#> 
#>   Male: mean = 3.603, n = 1149.0
#>   Female: mean = 3.651, n = 1272.0
#> 
#> t-test Results:
#> -------------------------------------------------------------------------------- 
#>         Assumption t_stat       df p_value mean_diff        conf_int sig
#>    Equal variances -1.019 2419.000   0.308    -0.048 [-0.140, 0.044]    
#>  Unequal variances -1.018 2384.147   0.309    -0.048 [-0.140, 0.044]    
#> -------------------------------------------------------------------------------- 
#> 
#> Effect Sizes:
#> ------------ 
#>           Variable Cohens_d Hedges_g Glass_Delta Effect_Size
#>  life_satisfaction   -0.041   -0.041      -0.041  negligible
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation:
#> - Cohen's d: pooled standard deviation (classic)
#> - Hedges' g: bias-corrected Cohen's d (preferred)
#> - Glass' Delta: control group standard deviation only
#> - Small effect: |effect| ~ 0.2
#> - Medium effect: |effect| ~ 0.5
#> - Large effect: |effect| ~ 0.8
```

With survey weights for representative results:

``` r
survey_data %>%
  t_test(life_satisfaction, group = gender, weights = sampling_weight)
#> Weighted t-Test Results
#> -----------------------
#> 
#> - Grouping variable: gender
#> - Groups compared: Male vs. Female
#> - Weights variable: sampling_weight
#> - Confidence level: 95.0%
#> - Alternative hypothesis: two.sided
#> - Null hypothesis (mu): 0.000
#> 
#> 
#> --- life_satisfaction ---
#> 
#>   Male: mean = 3.598, n = 1149.0
#>   Female: mean = 3.648, n = 1287.0
#> 
#> Weighted t-test Results:
#> -------------------------------------------------------------------------------- 
#>         Assumption t_stat       df p_value mean_diff        conf_int sig
#>    Equal variances -1.070 2434.000   0.285     -0.05 [-0.142, 0.042]    
#>  Unequal variances -1.069 2390.755   0.285     -0.05 [-0.142, 0.042]    
#> -------------------------------------------------------------------------------- 
#> 
#> Effect Sizes:
#> ------------ 
#>           Variable Cohens_d Hedges_g Glass_Delta Effect_Size
#>  life_satisfaction   -0.043   -0.043      -0.043  negligible
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation:
#> - Cohen's d: pooled standard deviation (classic)
#> - Hedges' g: bias-corrected Cohen's d (preferred)
#> - Glass' Delta: control group standard deviation only
#> - Small effect: |effect| ~ 0.2
#> - Medium effect: |effect| ~ 0.5
#> - Large effect: |effect| ~ 0.8
```

The output shows two versions: one assuming equal variances (Student’s
t-test) and one not assuming equal variances (Welch’s t-test). When in
doubt, use the Welch version — it is more robust.

### Multiple Variables at Once

Test several outcome variables simultaneously:

``` r
survey_data %>%
  t_test(trust_government, trust_science, trust_media,
         group = gender, weights = sampling_weight)
#> Weighted t-Test Results
#> -----------------------
#> 
#> - Grouping variable: gender
#> - Groups compared: Male vs. Female
#> - Weights variable: sampling_weight
#> - Confidence level: 95.0%
#> - Alternative hypothesis: two.sided
#> - Null hypothesis (mu): 0.000
#> 
#> 
#> --- trust_government ---
#> 
#>   Male: mean = 2.603, n = 1124.0
#>   Female: mean = 2.636, n = 1247.0
#> 
#> Weighted t-test Results:
#> -------------------------------------------------------------------------------- 
#>         Assumption t_stat       df p_value mean_diff        conf_int sig
#>    Equal variances -0.683 2369.000   0.495    -0.033 [-0.127, 0.061]    
#>  Unequal variances -0.682 2322.704   0.495    -0.033 [-0.127, 0.061]    
#> -------------------------------------------------------------------------------- 
#> 
#> Effect Sizes:
#> ------------ 
#>          Variable Cohens_d Hedges_g Glass_Delta Effect_Size
#>  trust_government   -0.028   -0.028      -0.028  negligible
#> 
#> 
#> --- trust_science ---
#> 
#>   Male: mean = 3.610, n = 1143.0
#>   Female: mean = 3.669, n = 1271.0
#> 
#> Weighted t-test Results:
#> -------------------------------------------------------------------------------- 
#>         Assumption t_stat      df p_value mean_diff        conf_int sig
#>    Equal variances -1.424 2412.00   0.155     -0.06 [-0.142, 0.022]    
#>  Unequal variances -1.421 2360.95   0.156     -0.06 [-0.142, 0.023]    
#> -------------------------------------------------------------------------------- 
#> 
#> Effect Sizes:
#> ------------ 
#>       Variable Cohens_d Hedges_g Glass_Delta Effect_Size
#>  trust_science   -0.058   -0.058      -0.057  negligible
#> 
#> 
#> --- trust_media ---
#> 
#>   Male: mean = 2.400, n = 1128.0
#>   Female: mean = 2.505, n = 1254.0
#> 
#> Weighted t-test Results:
#> -------------------------------------------------------------------------------- 
#>         Assumption t_stat       df p_value mean_diff         conf_int sig
#>    Equal variances -2.197 2380.000   0.028    -0.105 [-0.199, -0.011]   *
#>  Unequal variances -2.196 2350.189   0.028    -0.105 [-0.199, -0.011]   *
#> -------------------------------------------------------------------------------- 
#> 
#> Effect Sizes:
#> ------------ 
#>     Variable Cohens_d Hedges_g Glass_Delta Effect_Size
#>  trust_media    -0.09    -0.09       -0.09  negligible
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation:
#> - Cohen's d: pooled standard deviation (classic)
#> - Hedges' g: bias-corrected Cohen's d (preferred)
#> - Glass' Delta: control group standard deviation only
#> - Small effect: |effect| ~ 0.2
#> - Medium effect: |effect| ~ 0.5
#> - Large effect: |effect| ~ 0.8
```

### One-Sample t-Test

Test whether a population mean differs from a specific value. For
example, is average life satisfaction different from 3 (the scale
midpoint)?

``` r
survey_data %>%
  t_test(life_satisfaction, mu = 3, weights = sampling_weight)
#> Weighted t-Test Results
#> -----------------------
#> 
#> --- life_satisfaction ---
#> 
#> 
#> Weighted t-test Results:
#> ---------------------------------------------------------------------- 
#>  Assumption t_stat   df p_value mean_diff       conf_int sig
#>      t-test 26.776 2436       0     3.625 [3.579, 3.671] ***
#> ---------------------------------------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation:
#> - Cohen's d: pooled standard deviation (classic)
#> - Hedges' g: bias-corrected Cohen's d (preferred)
#> - Glass' Delta: control group standard deviation only
#> - Small effect: |effect| ~ 0.2
#> - Medium effect: |effect| ~ 0.5
#> - Large effect: |effect| ~ 0.8
```

### Grouped Analysis

Run separate tests for each region:

``` r
survey_data %>%
  group_by(region) %>%
  t_test(income, group = gender, weights = sampling_weight)
#> Weighted t-Test Results
#> -----------------------
#> 
#> - Grouping variable: gender
#> - Groups compared: Male vs. Female
#> - Weights variable: sampling_weight
#> - Confidence level: 95.0%
#> - Alternative hypothesis: two.sided
#> - Null hypothesis (mu): 0.000
#> 
#> 
#> Group: region = East
#> --------------------
#> 
#> --- income ---
#> 
#>   Male: mean = 3873.994, n = 218.0
#>   Female: mean = 3654.168, n = 232.0
#> 
#> Weighted t-test Results:
#> -------------------------------------------------------------------------------- 
#>         Assumption t_stat      df p_value mean_diff           conf_int sig
#>    Equal variances  1.683 448.000   0.093   219.826 [-36.870, 476.523]    
#>  Unequal variances  1.676 431.573   0.094   219.826 [-37.966, 477.619]    
#> -------------------------------------------------------------------------------- 
#> 
#> Effect Sizes:
#> ------------ 
#>  Variable Cohens_d Hedges_g Glass_Delta Effect_Size
#>    income    0.159    0.158       0.149  negligible
#> 
#> 
#> Group: region = West
#> --------------------
#> 
#> --- income ---
#> 
#>   Male: mean = 3738.924, n = 830.0
#>   Female: mean = 3738.281, n = 921.0
#> 
#> Weighted t-test Results:
#> -------------------------------------------------------------------------------- 
#>         Assumption t_stat       df p_value mean_diff            conf_int sig
#>    Equal variances  0.009 1749.000   0.993     0.644 [-133.957, 135.244]    
#>  Unequal variances  0.009 1739.865   0.993     0.644 [-133.736, 135.023]    
#> -------------------------------------------------------------------------------- 
#> 
#> Effect Sizes:
#> ------------ 
#>  Variable Cohens_d Hedges_g Glass_Delta Effect_Size
#>    income        0        0           0  negligible
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation:
#> - Cohen's d: pooled standard deviation (classic)
#> - Hedges' g: bias-corrected Cohen's d (preferred)
#> - Glass' Delta: control group standard deviation only
#> - Small effect: |effect| ~ 0.2
#> - Medium effect: |effect| ~ 0.5
#> - Large effect: |effect| ~ 0.8
```

## One-Way ANOVA

### Basic ANOVA

Compare means across multiple groups (3+ categories):

``` r
survey_data %>%
  oneway_anova(life_satisfaction, group = education)
#> 
#> One-Way ANOVA Results
#> ---------------------
#> 
#> - Dependent variable: life_satisfaction
#> - Grouping variable: education
#> - Confidence level: 95.0%
#>   Null hypothesis: All group means are equal
#>   Alternative hypothesis: At least one group mean differs
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Descriptive Statistics by Group:
#>   Basic Secondary: mean = 3.204, sd = 1.243, n = 809
#>   Intermediate Secondary: mean = 3.701, sd = 1.112, n = 618
#>   Academic Secondary: mean = 3.853, sd = 0.998, n = 607
#>   University: mean = 4.047, sd = 0.957, n = 387
#> 
#> ANOVA Results:
#> -------------------------------------------------------------------------------- 
#>          Source Sum_Squares   df Mean_Square      F p_value sig
#>  Between Groups     247.347    3      82.449 67.096   <.001   1
#>   Within Groups    2970.080 2417       1.229                   
#>           Total    3217.428 2420                               
#> -------------------------------------------------------------------------------- 
#> 
#> Assumption Tests:
#> ---------------- 
#>  Assumption Statistic df1  df2 p_value sig
#>       Welch    64.489   3 1229   <.001 ***
#> 
#> Effect Sizes:
#> ------------ 
#>           Variable Eta_Squared Epsilon_Squared Omega_Squared Effect_Size
#>  life_satisfaction       0.077           0.076         0.076      medium
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation:
#> - Eta-squared: Proportion of variance explained (biased upward)
#> - Epsilon-squared: Less biased than eta-squared
#> - Omega-squared: Unbiased estimate (preferred for publication)
#> - Small effect: eta-squared ~ 0.01, Medium effect: eta-squared ~ 0.06, Large effect: eta-squared ~ 0.14
#> 
#> Post-hoc tests: Use tukey_test() for pairwise comparisons
```

### With Survey Weights

``` r
survey_data %>%
  oneway_anova(life_satisfaction, group = education,
               weights = sampling_weight)
#> 
#> Weighted One-Way ANOVA Results
#> ------------------------------
#> 
#> - Dependent variable: life_satisfaction
#> - Grouping variable: education
#> - Weights variable: sampling_weight
#> - Confidence level: 95.0%
#>   Null hypothesis: All group means are equal
#>   Alternative hypothesis: At least one group mean differs
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Weighted Descriptive Statistics by Group:
#>   Basic Secondary: mean = 3.208, sd = 1.243, n = 815.7
#>   Intermediate Secondary: mean = 3.698, sd = 1.109, n = 629.6
#>   Academic Secondary: mean = 3.851, sd = 0.996, n = 618.2
#>   University: mean = 4.040, sd = 0.961, n = 373.2
#> 
#> Weighted ANOVA Results:
#> -------------------------------------------------------------------------------- 
#>          Source Sum_Squares   df Mean_Square      F p_value sig
#>  Between Groups     241.130    3      80.377 65.359   <.001   1
#>   Within Groups    2992.019 2433        1.23                   
#>           Total    3233.149 2436                               
#> -------------------------------------------------------------------------------- 
#> 
#> Assumption Tests:
#> ---------------- 
#>  Assumption Statistic df1  df2 p_value sig
#>       Welch    62.636   3 1216   <.001 ***
#> 
#> Effect Sizes:
#> ------------ 
#>           Variable Eta_Squared Epsilon_Squared Omega_Squared Effect_Size
#>  life_satisfaction       0.075           0.073         0.073      medium
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation:
#> - Eta-squared: Proportion of variance explained (biased upward)
#> - Epsilon-squared: Less biased than eta-squared
#> - Omega-squared: Unbiased estimate (preferred for publication)
#> - Small effect: eta-squared ~ 0.01, Medium effect: eta-squared ~ 0.06, Large effect: eta-squared ~ 0.14
#> 
#> Post-hoc tests: Use tukey_test() for pairwise comparisons
```

The output includes $\eta^{2}$ (eta-squared), which tells you how much
of the variance in the outcome is explained by group membership:

- **Small effect**: $\eta^{2} \approx 0.01$
- **Medium effect**: $\eta^{2} \approx 0.06$
- **Large effect**: $\eta^{2} \approx 0.14$

### Post-Hoc Tests

When ANOVA is significant, it tells you that *at least one* group
differs — but not *which* groups differ. Use post-hoc tests to find out:

``` r
# Save the ANOVA result
anova_result <- survey_data %>%
  oneway_anova(life_satisfaction, group = education,
               weights = sampling_weight)

# Tukey HSD: identifies which pairs of groups differ
tukey_test(anova_result)
#> Weighted Tukey HSD Post-Hoc Test Results
#> ----------------------------------------
#> 
#> - Dependent variable: life_satisfaction
#> - Grouping variable: education
#> - Weights variable: sampling_weight
#> - Confidence level: 95.0%
#>   Family-wise error rate controlled using Tukey HSD
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Weighted Tukey Results:
#> ------------------------------------------------------------------------------------ 
#>                                   Comparison Difference Lower CI Upper CI
#>     Basic Secondary - Intermediate Secondary     -0.490   -0.641   -0.339
#>         Basic Secondary - Academic Secondary     -0.643   -0.795   -0.491
#>                 Basic Secondary - University     -0.832   -1.011   -0.654
#>  Intermediate Secondary - Academic Secondary     -0.153   -0.314    0.008
#>          Intermediate Secondary - University     -0.342   -0.529   -0.156
#>              Academic Secondary - University     -0.189   -0.376   -0.003
#>  p-value Sig
#>    <.001 ***
#>    <.001 ***
#>    <.001 ***
#>    0.071    
#>    <.001 ***
#>    0.046   *
#> ------------------------------------------------------------------------------------ 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - Positive differences: First group > Second group
#> - Negative differences: First group < Second group
#> - Confidence intervals not containing 0 indicate significant differences
#> - p-values are adjusted for multiple comparisons (family-wise error control)

# Scheffe test: more conservative (fewer false positives)
scheffe_test(anova_result)
#> Weighted Scheffe Post-Hoc Test Results
#> --------------------------------------
#> 
#> - Dependent variable: life_satisfaction
#> - Grouping variable: education
#> - Weights variable: sampling_weight
#> - Confidence level: 95.0%
#>   Family-wise error rate controlled using Scheffe's method
#>   Note: Most conservative post-hoc test (widest confidence intervals)
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Weighted Scheffe Results:
#> ------------------------------------------------------------------------------------ 
#>                                   Comparison Difference Lower CI Upper CI
#>     Basic Secondary - Intermediate Secondary     -0.490   -0.655   -0.325
#>         Basic Secondary - Academic Secondary     -0.643   -0.808   -0.477
#>                 Basic Secondary - University     -0.832   -1.026   -0.638
#>  Intermediate Secondary - Academic Secondary     -0.153   -0.329    0.023
#>          Intermediate Secondary - University     -0.342   -0.545   -0.140
#>              Academic Secondary - University     -0.189   -0.393    0.014
#>  p-value Sig
#>    <.001 ***
#>    <.001 ***
#>    <.001 ***
#>    0.115    
#>    <.001 ***
#>    0.079    
#> ------------------------------------------------------------------------------------ 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - Positive differences: First group > Second group
#> - Negative differences: First group < Second group
#> - Confidence intervals not containing 0 indicate significant differences
#> - p-values are adjusted for all possible contrasts (most conservative)
#> - Scheffe test has wider CIs than Tukey HSD
```

### Testing Assumptions

ANOVA assumes equal variances across groups. Check this with Levene’s
test:

``` r
levene_test(anova_result)
#> 
#> Weighted Levene's Test for Homogeneity of Variance 
#> ---------------------------------------------------
#> 
#> - Grouping variable: education
#> - Weights variable: sampling_weight
#> - Center: mean
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Weighted Levene's Test Results:
#> -------------------------------------------------------------------- 
#>           Variable F_statistic df1  df2 p_value sig        Conclusion
#>  life_satisfaction      31.287   3 2433       0 *** Variances unequal
#> -------------------------------------------------------------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - p > 0.05: Variances are homogeneous (equal variances assumed)
#> - p <= 0.05: Variances are heterogeneous (equal variances NOT assumed)
#> 
#> Recommendation based on Levene test:
#> - Use Welch's t-test (unequal variances)
```

If Levene’s test is significant ($p < 0.05$), variances are unequal. In
that case, the Welch correction (included in the ANOVA output) is more
appropriate.

## Chi-Square Test

### Basic Chi-Square

Test whether two categorical variables are related:

``` r
survey_data %>%
  chi_square(education, employment)
#> 
#> Chi-Squared Test of Independence 
#> ---------------------------------
#> 
#> - Variables: education × employment
#> 
#> Observed Frequencies:
#>                       employment
#> education              Student Employed Unemployed Retired Other
#>   Basic Secondary            0      571         65     171    34
#>   Intermediate Seco...       0      412         51     137    29
#>   Academic Secondary        44      366         44     145    32
#>   University                34      251         22      72    20
#> 
#> Expected Frequencies:
#>                       employment
#> education              Student Employed Unemployed Retired  Other
#>   Basic Secondary       26.239   538.24     61.225  176.61 38.686
#>   Intermediate Seco...  19.625   402.56     45.791  132.09 28.934
#>   Academic Secondary    19.687   403.84     45.937  132.51 29.026
#>   University            12.449   255.36     29.047   83.79 18.354
#> 
#> Chi-Squared Test Results:
#> -------------------------------------------------- 
#>  Chi_squared df p_value sig
#>      125.867 12   <.001 ***
#> -------------------------------------------------- 
#> 
#> Effect Sizes:
#> ---------------------------------------------------------------------- 
#>     Measure  Value p_value sig Interpretation
#>  Cramer's V  0.130   <.001 ***          Small
#>       Gamma -0.065   0.107               Weak
#> ---------------------------------------------------------------------- 
#> Table size: 4×5 | N = 2500
#> Note: Phi coefficient only shown for 2x2 tables
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```

A significant result means the variables are not independent — knowing
someone’s education tells you something about their employment status
(or vice versa).

### With Survey Weights

``` r
survey_data %>%
  chi_square(education, employment, weights = sampling_weight)
#> 
#> Weighted Chi-Squared Test of Independence 
#> ------------------------------------------
#> 
#> - Variables: education × employment
#> - Weights variable: sampling_weight
#> 
#> Observed Frequencies:
#>                       employment
#> education              Student Employed Unemployed Retired Other
#>   Basic Secondary            0      573         66     175    34
#>   Intermediate Seco...       0      420         52     139    29
#>   Academic Secondary        46      370         45     149    33
#>   University                34      240         21      72    20
#> 
#> Expected Frequencies:
#>                       employment
#> education              Student Employed Unemployed Retired  Other
#>   Basic Secondary       26.942  539.851     61.967 180.175 39.066
#>   Intermediate Seco...  20.334  407.434     46.767 135.981 29.484
#>   Academic Secondary    20.429  409.344     46.986 136.618 29.622
#>   University            12.295  246.371     28.280  82.226 17.828
#> 
#> Chi-Squared Test Results:
#> -------------------------------------------------- 
#>  Chi_squared df p_value sig
#>      130.696 12   <.001 ***
#> -------------------------------------------------- 
#> 
#> Effect Sizes:
#> ---------------------------------------------------------------------- 
#>     Measure  Value p_value sig Interpretation
#>  Cramer's V  0.132   <.001 ***          Small
#>       Gamma -0.062   0.122               Weak
#> ---------------------------------------------------------------------- 
#> Table size: 4×5 | N = 2518
#> Note: Phi coefficient only shown for 2x2 tables
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```

### Multiple Comparisons

Test several relationships:

``` r
survey_data %>%
  chi_square(employment, education, weights = sampling_weight)
#> 
#> Weighted Chi-Squared Test of Independence 
#> ------------------------------------------
#> 
#> - Variables: employment × education
#> - Weights variable: sampling_weight
#> 
#> Observed Frequencies:
#>             education
#> employment   Basic Secondary Intermediate Seco... Academic Secondary University
#>   Student                  0                    0                 46         34
#>   Employed               573                  420                370        240
#>   Unemployed              66                   52                 45         21
#>   Retired                175                  139                149         72
#>   Other                   34                   29                 33         20
#> 
#> Expected Frequencies:
#>             education
#> employment   Basic Secondary Intermediate Seco... Academic Secondary University
#>   Student             26.942               20.334             20.429     12.295
#>   Employed           539.851              407.434            409.344    246.371
#>   Unemployed          61.967               46.767             46.986     28.280
#>   Retired            180.175              135.981            136.618     82.226
#>   Other               39.066               29.484             29.622     17.828
#> 
#> Chi-Squared Test Results:
#> -------------------------------------------------- 
#>  Chi_squared df p_value sig
#>      130.696 12   <.001 ***
#> -------------------------------------------------- 
#> 
#> Effect Sizes:
#> ---------------------------------------------------------------------- 
#>     Measure  Value p_value sig Interpretation
#>  Cramer's V  0.132   <.001 ***          Small
#>       Gamma -0.062   0.109               Weak
#> ---------------------------------------------------------------------- 
#> Table size: 5×4 | N = 2518
#> Note: Phi coefficient only shown for 2x2 tables
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05

survey_data %>%
  chi_square(employment, gender, weights = sampling_weight)
#> 
#> Weighted Chi-Squared Test of Independence 
#> ------------------------------------------
#> 
#> - Variables: employment × gender
#> - Weights variable: sampling_weight
#> 
#> Observed Frequencies:
#>             gender
#> employment   Male Female
#>   Student      36     44
#>   Employed    748    855
#>   Unemployed   83    101
#>   Retired     260    273
#>   Other        68     47
#> 
#> Expected Frequencies:
#>             gender
#> employment      Male  Female
#>   Student     38.012  41.988
#>   Employed   761.664 841.336
#>   Unemployed  87.427  96.573
#>   Retired    253.254 279.746
#>   Other       54.642  60.358
#> 
#> Chi-Squared Test Results:
#> -------------------------------------------------- 
#>  Chi_squared df p_value sig
#>        7.661  4   0.105    
#> -------------------------------------------------- 
#> 
#> Effect Sizes:
#> ---------------------------------------------------------------------- 
#>     Measure  Value p_value sig Interpretation
#>  Cramer's V  0.055   0.105            Neglig.
#>       Gamma -0.064   0.181               Weak
#> ---------------------------------------------------------------------- 
#> Table size: 5×2 | N = 2515
#> Note: Phi coefficient only shown for 2x2 tables
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```

## Mann-Whitney Test

### When to Use

Use the Mann-Whitney *U* test (also called Wilcoxon rank-sum test) when:

- Your data is **not normally distributed**
- Sample sizes are **small**
- The variable is **ordinal** (ranked categories)

It compares the *distributions* rather than the means.

### Basic Usage

``` r
survey_data %>%
  mann_whitney(political_orientation, group = region)
#> 
#> Mann-Whitney U Test Results
#> ---------------------------
#> 
#> - Grouping variable: region
#> - Groups compared: East vs. West
#> - Confidence level: 95.0%
#> - Alternative hypothesis: two.sided
#> - Null hypothesis (mu): 0.000
#> 
#> 
#> --- political_orientation ---
#> 
#>   East: rank mean = 1145.3, n = 443.0
#>   West: rank mean = 1151.1, n = 1856.0
#> 
#> 
#> Mann-Whitney U Test Results:
#> ---------------------------------------------------------- 
#>            Test       U       W      Z p_value effect_r sig
#>  Mann-Whitney U 409,008 507,354 -0.173   0.862    0.004    
#> ---------------------------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (r):
#> - Small effect: |r| ~ 0.1
#> - Medium effect: |r| ~ 0.3
#> - Large effect: |r| ~ 0.5
```

### With Survey Weights

``` r
survey_data %>%
  mann_whitney(political_orientation, group = region,
               weights = sampling_weight)
#> 
#> Weighted Mann-Whitney U Test Results
#> ------------------------------------
#> 
#> - Grouping variable: region
#> - Groups compared: East vs. West
#> - Weights variable: sampling_weight
#> - Confidence level: 95.0%
#> - Alternative hypothesis: two.sided
#> - Null hypothesis (mu): 0.000
#> 
#> 
#> --- political_orientation ---
#> 
#>   East: rank mean = 1150.6, n = 464.0
#>   West: rank mean = 1157.8, n = 1848.5
#> 
#> 
#> Weighted Mann-Whitney U Test Results:
#> --------------------------------------------------------- 
#>            Test       U       W     Z p_value effect_r sig
#>  Mann-Whitney U 426,033 533,921 0.207   0.836    0.004    
#> --------------------------------------------------------- 
#> 
#> 
#> Note: Weighted analysis uses design-based rank test (Lumley & Scott, 2013).
#> U and W are descriptive statistics derived from weighted ranks.
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (r):
#> - Small effect: |r| ~ 0.1
#> - Medium effect: |r| ~ 0.3
#> - Large effect: |r| ~ 0.5
```

## Factorial ANOVA

### When to Use

Use
[`factorial_anova()`](https://YannickDiehl.github.io/mariposa/reference/factorial_anova.md)
when you want to test the effects of **two or more factors** on a
continuous outcome variable. It answers questions like: “Does income
differ by gender, education, and their interaction?”

### Two-Way ANOVA

``` r
survey_data %>%
  factorial_anova(dv = income, between = c(gender, education))
#> 
#> Factorial ANOVA (2-Way ANOVA) Results
#> -------------------------------------
#> 
#> - Dependent variable: income
#> - Factors: gender x education
#> - Type III Sum of Squares: Type 3
#> - N (complete cases): 2186
#> - Missing: 314
#> 
#> Tests of Between-Subjects Effects
#> ------------------------------------------------------------------ 
#>  Source             Type III SS  df   Mean Square  F         Sig. 
#>  Corrected Model    1.754652e+09    7 2.506646e+08   199.909 <.001
#>  Intercept          3.221261e+10    1 3.221261e+10 25690.075 <.001
#>  gender             1.226376e+05    1 1.226376e+05     0.098 0.755
#>  education          1.743618e+09    3 5.812059e+08   463.521 <.001
#>  gender * education 1.499441e+06    3 4.998136e+05     0.399 0.754
#>  Error              2.730979e+09 2178 1.253893e+06                
#>  Total              3.529079e+10 2186                             
#>  Corrected Total    4.485631e+09 2185                             
#>  Partial Eta Sq    
#>  0.391          ***
#>  0.922          ***
#>  0.000             
#>  0.390          ***
#>  0.001             
#>                    
#>                    
#>                    
#> ------------------------------------------------------------------ 
#> R Squared = 0.391 (Adjusted R Squared = 0.389)
#> 
#> Descriptive Statistics
#> ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#>  gender education              Mean    Std. Deviation N  
#>  Male   Basic Secondary        2803.43  774.959       350
#>  Male   Intermediate Secondary 3574.09  996.649       247
#>  Male   Academic Secondary     4246.45 1180.779       282
#>  Male   University             5318.56 1718.805       167
#>  Female Basic Secondary        2718.70  795.831       385
#>  Female Intermediate Secondary 3607.64  996.214       301
#>  Female Academic Secondary     4200.38 1178.118       266
#>  Female University             5353.72 1612.265       188
#> ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#> 
#> Levene's Test of Equality of Error Variances
#>   F(7, 2178) = 44.988, p = <.001
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```

The output includes Type III sums of squares, main effects for each
factor, interaction effects, partial eta-squared, and descriptive
statistics per cell.

### With Survey Weights

``` r
survey_data %>%
  factorial_anova(dv = life_satisfaction, between = c(gender, region),
                  weights = sampling_weight)
#> 
#> Weighted Factorial ANOVA (2-Way ANOVA) Results
#> ----------------------------------------------
#> 
#> - Dependent variable: life_satisfaction
#> - Factors: gender x region
#> - Type III Sum of Squares: Type 3
#> - Weights variable: sampling_weight
#> - N (complete cases): 2421
#> - Missing: 79
#> 
#> Tests of Between-Subjects Effects
#> ---------------------------------------------------------------------------- 
#>  Source          Type III SS df   Mean Square F         Sig.  Partial Eta Sq
#>  Corrected Model     3.714      3     1.238       0.927 0.427 0.001         
#>  Intercept       20468.612      1 20468.612   15319.285 <.001 0.864         
#>  gender              0.010      1     0.010       0.008 0.930 0.000         
#>  region              0.001      1     0.001       0.001 0.979 0.000         
#>  gender * region     2.194      1     2.194       1.642 0.200 0.001         
#>  Error            3229.435   2417     1.336                                 
#>  Total           35249.294   2421                                           
#>  Corrected Total  3233.149   2420                                           
#>     
#>     
#>  ***
#>     
#>     
#>     
#>     
#>     
#>     
#> ---------------------------------------------------------------------------- 
#> R Squared = 0.001 (Adjusted R Squared = 0.000)
#> 
#> Descriptive Statistics
#> -------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#>  gender region Mean Std. Deviation N   
#>  Male   East   3.66 1.207           228
#>  Male   West   3.58 1.152           921
#>  Female East   3.59 1.197           237
#>  Female West   3.66 1.126          1035
#> -------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#> Note: Means and SDs are weighted (WLS)
#> 
#> Levene's Test of Equality of Error Variances
#>   F(3, 2417) = 2.470, p = 0.060
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```

Weighted factorial ANOVA uses WLS (Weighted Least Squares) estimation,
matching SPSS UNIANOVA results.

## ANCOVA

### When to Use

Use
[`ancova()`](https://YannickDiehl.github.io/mariposa/reference/ancova.md)
when you want to compare groups on a continuous outcome while
**controlling for** (adjusting for) a covariate. For example: “Does
income differ by education, after controlling for age?”

### Basic ANCOVA

``` r
survey_data %>%
  ancova(dv = income, between = education, covariate = age)
#> 
#> ANCOVA (One-Way ANCOVA) Results
#> -------------------------------
#> 
#> - Dependent variable: income
#> - Factor(s): education
#> - Covariate(s): age
#> - Type III Sum of Squares: Type 3
#> - N (complete cases): 2186
#> - Missing: 314
#> 
#> Tests of Between-Subjects Effects
#> ----------------------------------------------------------------------------- 
#>  Source          Type III SS  df   Mean Square  F        Sig.  Partial Eta Sq
#>  Corrected Model 1.752821e+09    4 4.382053e+08  349.723 <.001 0.391         
#>  Intercept       3.472401e+09    1 3.472401e+09 2771.252 <.001 0.560         
#>  age             3.772141e+04    1 3.772141e+04    0.030 0.862 0.000         
#>  education       1.752631e+09    3 5.842102e+08  466.246 <.001 0.391         
#>  Error           2.732810e+09 2181 1.253008e+06                              
#>  Total           3.529079e+10 2186                                           
#>  Corrected Total 4.485631e+09 2185                                           
#>     
#>  ***
#>  ***
#>     
#>  ***
#>     
#>     
#>     
#> ----------------------------------------------------------------------------- 
#> R Squared = 0.391 (Adjusted R Squared = 0.390)
#> 
#> Parameter Estimates
#> --------------------------------------------------------------------- 
#>  Parameter   B        Std. Error t      Sig.  Lower Bound Upper Bound
#>  (Intercept) 3990.641 75.806     52.643 <.001 3841.981    4139.301   
#>  age           -0.245  1.412     -0.174 0.862   -3.014       2.524   
#>  education.L 1870.578 50.838     36.795 <.001 1770.881    1970.274   
#>  education.Q  139.427 49.566      2.813 0.005   42.226     236.629   
#>  education.C  152.695 48.167      3.170 0.002   58.236     247.154   
#>  Partial Eta Sq
#>  0.560         
#>  0.000         
#>  0.383         
#>  0.004         
#>  0.005         
#> --------------------------------------------------------------------- 
#> 
#> Estimated Marginal Means
#> (Evaluated at covariate means)
#> ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#>  education              Mean     Std. Error Lower Bound Upper Bound
#>  Basic Secondary        2758.939 41.294     2677.960    2839.918   
#>  Intermediate Secondary 3592.634 47.822     3498.852    3686.416   
#>  Academic Secondary     4224.320 47.836     4130.511    4318.130   
#>  University             5336.870 59.438     5220.309    5453.431   
#> ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#> 
#> Levene's Test of Equality of Error Variances
#>   F(3, 2182) = 103.901, p = <.001
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```

The output shows the covariate effect, the factor effect after
adjustment, and estimated marginal means (group means adjusted for the
covariate).

### With Survey Weights

``` r
survey_data %>%
  ancova(dv = income, between = education, covariate = age,
         weights = sampling_weight)
#> 
#> Weighted ANCOVA (One-Way ANCOVA) Results
#> ----------------------------------------
#> 
#> - Dependent variable: income
#> - Factor(s): education
#> - Covariate(s): age
#> - Type III Sum of Squares: Type 3
#> - Weights variable: sampling_weight
#> - N (complete cases): 2186
#> - Missing: 314
#> 
#> Tests of Between-Subjects Effects
#> ----------------------------------------------------------------------------- 
#>  Source          Type III SS  df   Mean Square  F        Sig.  Partial Eta Sq
#>  Corrected Model 1.726314e+09    4 4.315784e+08  344.227 <.001 0.387         
#>  Intercept       3.522092e+09    1 3.522092e+09 2809.219 <.001 0.563         
#>  age             2.421785e+04    1 2.421785e+04    0.019 0.889 0.000         
#>  education       1.726217e+09    3 5.754058e+08  458.943 <.001 0.387         
#>  Error           2.734455e+09 2181 1.253762e+06                              
#>  Total           3.529768e+10 2186                                           
#>  Corrected Total 4.460769e+09 2185                                           
#>     
#>  ***
#>  ***
#>     
#>  ***
#>     
#>     
#>     
#> ----------------------------------------------------------------------------- 
#> R Squared = 0.387 (Adjusted R Squared = 0.386)
#> 
#> Parameter Estimates
#> --------------------------------------------------------------------- 
#>  Parameter   B        Std. Error t      Sig.  Lower Bound Upper Bound
#>  (Intercept) 3986.404 75.212     53.002 <.001 3838.909    4133.899   
#>  age           -0.195  1.400     -0.139 0.889   -2.941       2.551   
#>  education.L 1867.342 51.277     36.417 <.001 1766.784    1967.899   
#>  education.Q  137.250 49.626      2.766 0.006   39.931     234.570   
#>  education.C  148.968 47.846      3.113 0.002   55.139     242.797   
#>  Partial Eta Sq
#>  0.563         
#>  0.000         
#>  0.378         
#>  0.003         
#>  0.004         
#> --------------------------------------------------------------------- 
#> 
#> Estimated Marginal Means
#> (Evaluated at covariate means)
#> ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#>  education              Mean     Std. Error Lower Bound Upper Bound
#>  Basic Secondary        2759.183 41.135     2678.516    2839.850   
#>  Intermediate Secondary 3590.275 47.390     3497.341    3683.208   
#>  Academic Secondary     4225.514 47.413     4132.533    4318.494   
#>  University             5331.106 60.451     5212.558    5449.653   
#> ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#> 
#> Levene's Test of Equality of Error Variances
#>   F(3, 2182) = 95.327, p = <.001
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```

## Non-Parametric Tests

### Kruskal-Wallis Test

The non-parametric alternative to one-way ANOVA. Compare three or more
independent groups when data is not normally distributed:

``` r
survey_data %>%
  kruskal_wallis(life_satisfaction, group = education)
#> 
#> Kruskal-Wallis Test Results
#> ---------------------------
#> 
#> - Grouping variable: education
#> - Groups: Basic Secondary, Intermediate Secondary, Academic Secondary, University
#> 
#>   Ranks:
#>   --------------------------------------
#>                     Group    N Mean Rank
#>           Basic Secondary  809    974.29
#>    Intermediate Secondary  618   1250.73
#>        Academic Secondary  607   1329.56
#>                University  387   1456.42
#>                     Total 2421        NA
#>   --------------------------------------
#> 
#>   Test Statistics:
#>   --------------------------------------------
#>    Kruskal-Wallis H df p value Eta-squared sig
#>             171.178  3       0       0.071 ***
#>   --------------------------------------------
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (Eta-squared):
#> - Small effect: 0.01 - 0.06
#> - Medium effect: 0.06 - 0.14
#> - Large effect: > 0.14
```

When the result is significant, use Dunn’s post-hoc test to find which
groups differ:

``` r
kw_result <- survey_data %>%
  kruskal_wallis(life_satisfaction, group = education)

kw_result %>% dunn_test()
#> Dunn Post-Hoc Test (Bonferroni) Results
#> ---------------------------------------
#> 
#> - Dependent variable: life_satisfaction
#> - Grouping variable: education
#> - P-value adjustment: Bonferroni
#> 
#> ---------------------------------------------------------------------------- 
#>                 Group 1                Group 2       Z p (unadj) p (adj) Sig 
#>         Basic Secondary Intermediate Secondary  -7.402     <.001   <.001 *** 
#>         Basic Secondary     Academic Secondary  -9.465     <.001   <.001 *** 
#>         Basic Secondary             University -11.159     <.001   <.001 *** 
#>  Intermediate Secondary     Academic Secondary  -1.973     0.048   0.291     
#>  Intermediate Secondary             University  -4.539     <.001   <.001 *** 
#>      Academic Secondary             University  -2.790     0.005   0.032   * 
#> ---------------------------------------------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - Positive Z: First group has higher mean rank
#> - Negative Z: First group has lower mean rank
#> - p-values are adjusted for multiple comparisons
```

### Wilcoxon Signed-Rank Test

The non-parametric alternative to the paired t-test. Compare two related
measurements:

``` r
data(longitudinal_data_wide)

longitudinal_data_wide %>%
  wilcoxon_test(score_T1, score_T2)
#> 
#> Wilcoxon Signed-Rank Test Results
#> ---------------------------------
#> 
#> - Pair: score_T2 vs score_T1
#> 
#>   Ranks:
#>   ------------------------------------------
#>                     N Mean Rank Sum of Ranks
#>    Negative Ranks  27     40.19         1085
#>    Positive Ranks  78     57.44         4480
#>              Ties   0        NA           NA
#>             Total 105        NA           NA
#>   ------------------------------------------
#> 
#>   a score_T2 < score_T1
#>   b score_T2 > score_T1
#>   c score_T2 = score_T1
#> 
#>   Test Statistics:
#>   ---------------------------
#>        Z p value Effect r sig
#>    5.427       0     0.53 ***
#>   ---------------------------
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (r):
#> - Small effect: 0.1 - 0.3
#> - Medium effect: 0.3 - 0.5
#> - Large effect: > 0.5
```

### Friedman Test

The non-parametric alternative to repeated-measures ANOVA. Compare three
or more related measurements:

``` r
longitudinal_data_wide %>%
  friedman_test(score_T1, score_T2, score_T3)
#> 
#> Friedman Test Results
#> ---------------------
#> 
#> - Variables: score_T1, score_T2, score_T3
#> - Number of conditions: 3
#> 
#>   Ranks:
#>   -------------------
#>    Variable Mean Rank
#>    score_T1      1.48
#>    score_T2      2.04
#>    score_T3      2.48
#>   -------------------
#> 
#>   Test Statistics:
#>   -----------------------------------------
#>     N Chi-Square df p value Kendall's W sig
#>    94     47.255  2       0       0.251 ***
#>   -----------------------------------------
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (Kendall's W):
#> - Weak agreement: 0.1 - 0.3
#> - Moderate agreement: 0.3 - 0.5
#> - Strong agreement: > 0.5
```

When significant, use pairwise Wilcoxon post-hoc tests:

``` r
friedman_result <- longitudinal_data_wide %>%
  friedman_test(score_T1, score_T2, score_T3)

friedman_result %>% pairwise_wilcoxon()
#> Pairwise Wilcoxon Post-Hoc Test (Bonferroni) Results
#> ----------------------------------------------------
#> 
#> - Variables: score_T1, score_T2, score_T3
#> - P-value adjustment: Bonferroni
#> - Number of comparisons: 3
#> 
#> ---------------------------------------------- 
#>     Var 1    Var 2     Z p (unadj) p (adj) Sig 
#>  score_T1 score_T2 5.427     <.001   <.001 *** 
#>  score_T1 score_T3 6.132     <.001   <.001 *** 
#>  score_T2 score_T3 4.413     <.001   <.001 *** 
#> ---------------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - Positive Z: First variable tends to have higher values
#> - Negative Z: Second variable tends to have higher values
#> - p-values are adjusted for multiple comparisons
```

### Binomial Test

Test whether an observed proportion differs from an expected value:

``` r
survey_data %>%
  binomial_test(gender)
#> 
#> Binomial Test Results
#> ---------------------
#> 
#> - Test proportion: 0.5
#> - Confidence level: 95.0%
#> 
#>   Categories:
#>   ------------------------------------
#>                       N Observed Prop.
#>      Group 1: Male 1194          0.478
#>    Group 2: Female 1306          0.522
#>              Total 2500          1.000
#>   ------------------------------------
#> 
#>   Test Statistics:
#>   -----------------------------------------
#>    Test Prop. p value CI lower CI upper sig
#>           0.5   0.026    0.458    0.497   *
#>   -----------------------------------------
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```

## Exact and Goodness-of-Fit Tests

### Fisher’s Exact Test

Use instead of chi-square when sample sizes are small or expected cell
frequencies are below 5:

``` r
# Create a small subsample
small_sample <- survey_data %>% slice_sample(n = 30)

small_sample %>%
  fisher_test(gender, region)
#> Fisher's Exact Test Results
#> ---------------------------
#> 
#> - Row variable: gender
#> - Column variable: region
#> 
#> Contingency Table:
#> ---------------------------------------- 
#>         cc
#> r        East West
#>   Male      3    9
#>   Female    4   14
#> ---------------------------------------- 
#> 
#> Test Results:
#> -------------------------------------------------- 
#>                              Method p-value  N Sig 
#>  Fisher's Exact Test for Count Data  1.0000 30     
#> -------------------------------------------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```

### Chi-Square Goodness-of-Fit

Test whether observed frequencies match expected proportions. For
example, does the education distribution match the general population?

``` r
survey_data %>%
  chisq_gof(education)
#> Chi-Square Goodness-of-Fit Test Results
#> ---------------------------------------
#> 
#> - Variables: education
#> - Expected: Equal proportions
#> 
#>   education - Frequency Table:
#>   --------------------------------------------------
#>                  category observed expected residual
#>           Basic Secondary      841      625      216
#>    Intermediate Secondary      629      625        4
#>        Academic Secondary      631      625        6
#>                University      399      625     -226
#>   --------------------------------------------------
#> 
#> ----------------------------------------- 
#>   Variable Chi-Square df p-value    N Sig 
#>  education    156.454  3   <.001 2500 *** 
#> ----------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```

You can also specify expected proportions:

``` r
survey_data %>%
  chisq_gof(education, expected = c(0.30, 0.25, 0.25, 0.20))
#> Chi-Square Goodness-of-Fit Test Results
#> ---------------------------------------
#> 
#> - Variables: education
#> - Expected: 0.3, 0.25, 0.25, 0.2
#> 
#>   education - Frequency Table:
#>   --------------------------------------------------
#>                  category observed expected residual
#>           Basic Secondary      841      750       91
#>    Intermediate Secondary      629      625        4
#>        Academic Secondary      631      625        6
#>                University      399      500     -101
#>   --------------------------------------------------
#> 
#> ----------------------------------------- 
#>   Variable Chi-Square df p-value    N Sig 
#>  education     31.527  3   <.001 2500 *** 
#> ----------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```

### McNemar’s Test

Test whether proportions change between two paired measurements (e.g.,
before and after an intervention). Requires two dichotomous variables
measured on the same subjects:

``` r
# Example with dichotomized trust variables
test_data <- survey_data %>%
  mutate(
    trust_gov_high = ifelse(trust_government > 3, 1, 0),
    trust_media_high = ifelse(trust_media > 3, 1, 0)
  )

test_data %>%
  mcnemar_test(var1 = trust_gov_high, var2 = trust_media_high)
```

## Interpreting Results

### Understanding p-values

- $p < 0.05$: The difference is statistically significant at the 5%
  level
- $p \geq 0.05$: No significant difference detected

Important: “not significant” does **not** mean “no difference” — it
means we cannot rule out that the difference is due to chance, given the
sample size.

### Effect Sizes Matter

With large samples, even tiny differences can be statistically
significant. Always check effect sizes:

``` r
result <- survey_data %>%
  oneway_anova(income, group = education, weights = sampling_weight)
print(result)
#> 
#> Weighted One-Way ANOVA Results
#> ------------------------------
#> 
#> - Dependent variable: income
#> - Grouping variable: education
#> - Weights variable: sampling_weight
#> - Confidence level: 95.0%
#>   Null hypothesis: All group means are equal
#>   Alternative hypothesis: At least one group mean differs
#> 
#> 
#> --- income ---
#> 
#> Weighted Descriptive Statistics by Group:
#>   Basic Secondary: mean = 2759.261, sd = 787.243, n = 741.1
#>   Intermediate Secondary: mean = 3590.218, sd = 993.577, n = 558.3
#>   Academic Secondary: mean = 4225.325, sd = 1179.065, n = 558.2
#>   University: mean = 5331.337, sd = 1661.679, n = 343.3
#> 
#> Weighted ANOVA Results:
#> -------------------------------------------------------------------------------- 
#>          Source Sum_Squares   df   Mean_Square       F p_value sig
#>  Between Groups  1726289513    3 575429837.501 462.325   <.001   1
#>   Within Groups  2734479256 2197   1244642.356                    
#>           Total  4460768768 2200                                  
#> -------------------------------------------------------------------------------- 
#> 
#> Assumption Tests:
#> ---------------- 
#>  Assumption Statistic df1 df2 p_value sig
#>       Welch   413.705   3 970   <.001 ***
#> 
#> Effect Sizes:
#> ------------ 
#>  Variable Eta_Squared Epsilon_Squared Omega_Squared Effect_Size
#>    income       0.387           0.386         0.386       large
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation:
#> - Eta-squared: Proportion of variance explained (biased upward)
#> - Epsilon-squared: Less biased than eta-squared
#> - Omega-squared: Unbiased estimate (preferred for publication)
#> - Small effect: eta-squared ~ 0.01, Medium effect: eta-squared ~ 0.06, Large effect: eta-squared ~ 0.14
#> 
#> Post-hoc tests: Use tukey_test() for pairwise comparisons
```

The t-test provides Cohen’s *d* (small: 0.2, medium: 0.5, large: 0.8)
and ANOVA provides $\eta^{2}$.

### Sample Size Effects

Large samples detect small effects. Small samples may miss real effects.
Consider:

- Always report effect sizes alongside p-values
- Look at confidence intervals for practical significance
- Do not interpret $p = 0.049$ and $p = 0.051$ as fundamentally
  different

## Complete Example

A typical hypothesis testing workflow:

``` r
# 1. Descriptive overview
survey_data %>%
  group_by(education) %>%
  describe(life_satisfaction, weights = sampling_weight)
#> 
#> Weighted Descriptive Statistics
#> -------------------------------
#> 
#> Group: education = Basic Secondary
#> ----------------------------------
#> ----------------------------------------
#>           Variable  Mean Median    SD Range IQR Skewness Effective_N
#>  life_satisfaction 3.208      3 1.243     4   2   -0.056       801.2
#> ----------------------------------------
#> 
#> Group: education = Intermediate Secondary
#> -----------------------------------------
#> ----------------------------------------
#>           Variable  Mean Median   SD Range IQR Skewness Effective_N
#>  life_satisfaction 3.698      4 1.11     4   2    -0.59       611.8
#> ----------------------------------------
#> 
#> Group: education = Academic Secondary
#> -------------------------------------
#> ----------------------------------------
#>           Variable  Mean Median    SD Range IQR Skewness Effective_N
#>  life_satisfaction 3.851      4 0.997     4   2    -0.58       600.6
#> ----------------------------------------
#> 
#> Group: education = University
#> -----------------------------
#> ----------------------------------------
#>           Variable Mean Median    SD Range IQR Skewness Effective_N
#>  life_satisfaction 4.04      4 0.962     4   1   -0.963       377.8
#> ----------------------------------------

# 2. Test for overall differences
anova_result <- survey_data %>%
  oneway_anova(life_satisfaction, group = education,
               weights = sampling_weight)
print(anova_result)
#> 
#> Weighted One-Way ANOVA Results
#> ------------------------------
#> 
#> - Dependent variable: life_satisfaction
#> - Grouping variable: education
#> - Weights variable: sampling_weight
#> - Confidence level: 95.0%
#>   Null hypothesis: All group means are equal
#>   Alternative hypothesis: At least one group mean differs
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Weighted Descriptive Statistics by Group:
#>   Basic Secondary: mean = 3.208, sd = 1.243, n = 815.7
#>   Intermediate Secondary: mean = 3.698, sd = 1.109, n = 629.6
#>   Academic Secondary: mean = 3.851, sd = 0.996, n = 618.2
#>   University: mean = 4.040, sd = 0.961, n = 373.2
#> 
#> Weighted ANOVA Results:
#> -------------------------------------------------------------------------------- 
#>          Source Sum_Squares   df Mean_Square      F p_value sig
#>  Between Groups     241.130    3      80.377 65.359   <.001   1
#>   Within Groups    2992.019 2433        1.23                   
#>           Total    3233.149 2436                               
#> -------------------------------------------------------------------------------- 
#> 
#> Assumption Tests:
#> ---------------- 
#>  Assumption Statistic df1  df2 p_value sig
#>       Welch    62.636   3 1216   <.001 ***
#> 
#> Effect Sizes:
#> ------------ 
#>           Variable Eta_Squared Epsilon_Squared Omega_Squared Effect_Size
#>  life_satisfaction       0.075           0.073         0.073      medium
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation:
#> - Eta-squared: Proportion of variance explained (biased upward)
#> - Epsilon-squared: Less biased than eta-squared
#> - Omega-squared: Unbiased estimate (preferred for publication)
#> - Small effect: eta-squared ~ 0.01, Medium effect: eta-squared ~ 0.06, Large effect: eta-squared ~ 0.14
#> 
#> Post-hoc tests: Use tukey_test() for pairwise comparisons

# 3. Post-hoc: which groups differ?
tukey_test(anova_result)
#> Weighted Tukey HSD Post-Hoc Test Results
#> ----------------------------------------
#> 
#> - Dependent variable: life_satisfaction
#> - Grouping variable: education
#> - Weights variable: sampling_weight
#> - Confidence level: 95.0%
#>   Family-wise error rate controlled using Tukey HSD
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Weighted Tukey Results:
#> ------------------------------------------------------------------------------------ 
#>                                   Comparison Difference Lower CI Upper CI
#>     Basic Secondary - Intermediate Secondary     -0.490   -0.641   -0.339
#>         Basic Secondary - Academic Secondary     -0.643   -0.795   -0.491
#>                 Basic Secondary - University     -0.832   -1.011   -0.654
#>  Intermediate Secondary - Academic Secondary     -0.153   -0.314    0.008
#>          Intermediate Secondary - University     -0.342   -0.529   -0.156
#>              Academic Secondary - University     -0.189   -0.376   -0.003
#>  p-value Sig
#>    <.001 ***
#>    <.001 ***
#>    <.001 ***
#>    0.071    
#>    <.001 ***
#>    0.046   *
#> ------------------------------------------------------------------------------------ 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - Positive differences: First group > Second group
#> - Negative differences: First group < Second group
#> - Confidence intervals not containing 0 indicate significant differences
#> - p-values are adjusted for multiple comparisons (family-wise error control)

# 4. Check assumptions
levene_test(anova_result)
#> 
#> Weighted Levene's Test for Homogeneity of Variance 
#> ---------------------------------------------------
#> 
#> - Grouping variable: education
#> - Weights variable: sampling_weight
#> - Center: mean
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Weighted Levene's Test Results:
#> -------------------------------------------------------------------- 
#>           Variable F_statistic df1  df2 p_value sig        Conclusion
#>  life_satisfaction      31.287   3 2433       0 *** Variances unequal
#> -------------------------------------------------------------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - p > 0.05: Variances are homogeneous (equal variances assumed)
#> - p <= 0.05: Variances are heterogeneous (equal variances NOT assumed)
#> 
#> Recommendation based on Levene test:
#> - Use Welch's t-test (unequal variances)
```

## Best Practices

1.  **Check assumptions first.** Inspect skewness and kurtosis before
    choosing a test. For non-normal data, consider
    [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md)
    or
    [`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md).
2.  **Use appropriate tests.** Normal data: t-test/ANOVA. Non-normal or
    ordinal: Mann-Whitney/Kruskal-Wallis. Categorical: chi-square or
    Fisher’s exact test. Small samples: use exact tests.
3.  **Use post-hoc tests.** After a significant omnibus test, use
    [`tukey_test()`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md)
    for ANOVA,
    [`dunn_test()`](https://YannickDiehl.github.io/mariposa/reference/dunn_test.md)
    for Kruskal-Wallis, or
    [`pairwise_wilcoxon()`](https://YannickDiehl.github.io/mariposa/reference/pairwise_wilcoxon.md)
    for Friedman.
4.  **Report completely.** Always include the test statistic, degrees of
    freedom, p-value, effect size, and confidence intervals.
5.  **Watch for multiple comparisons.** Running many tests inflates the
    chance of false positives. Post-hoc tests handle this automatically
    with Bonferroni correction.
6.  **Significance is not importance.** A statistically significant
    result with a negligible effect size may not be practically
    relevant.

## Summary

### Parametric Tests

1.  **[`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)**
    compares means between two groups
2.  **[`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)**
    extends this to three or more groups, with
    [`tukey_test()`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md)
    and
    [`scheffe_test()`](https://YannickDiehl.github.io/mariposa/reference/scheffe_test.md)
    for post-hoc analysis
3.  **[`factorial_anova()`](https://YannickDiehl.github.io/mariposa/reference/factorial_anova.md)**
    tests effects of multiple factors and their interactions
4.  **[`ancova()`](https://YannickDiehl.github.io/mariposa/reference/ancova.md)**
    compares groups while controlling for a covariate

### Non-Parametric Tests

5.  **[`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md)**
    is the non-parametric alternative to the t-test
6.  **[`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md)**
    is the non-parametric alternative to ANOVA, with
    [`dunn_test()`](https://YannickDiehl.github.io/mariposa/reference/dunn_test.md)
    for post-hoc analysis
7.  **[`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/reference/wilcoxon_test.md)**
    is the non-parametric alternative to the paired t-test
8.  **[`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md)**
    is the non-parametric alternative to repeated-measures ANOVA, with
    [`pairwise_wilcoxon()`](https://YannickDiehl.github.io/mariposa/reference/pairwise_wilcoxon.md)
    for post-hoc analysis
9.  **[`binomial_test()`](https://YannickDiehl.github.io/mariposa/reference/binomial_test.md)**
    tests proportions against expected values

### Categorical Tests

10. **[`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)**
    tests whether categorical variables are related
11. **[`fisher_test()`](https://YannickDiehl.github.io/mariposa/reference/fisher_test.md)**
    is the exact alternative for small samples
12. **[`chisq_gof()`](https://YannickDiehl.github.io/mariposa/reference/chisq_gof.md)**
    tests whether observed frequencies match expected proportions
13. **[`mcnemar_test()`](https://YannickDiehl.github.io/mariposa/reference/mcnemar_test.md)**
    compares paired proportions

Always report **effect sizes** alongside p-values

## Next Steps

- Explore relationships between continuous variables — see
  [`vignette("correlation-analysis")`](https://YannickDiehl.github.io/mariposa/articles/correlation-analysis.md)
- Learn about weighted analysis — see
  [`vignette("survey-weights")`](https://YannickDiehl.github.io/mariposa/articles/survey-weights.md)
- Revisit your data — see
  [`vignette("descriptive-statistics")`](https://YannickDiehl.github.io/mariposa/articles/descriptive-statistics.md)
