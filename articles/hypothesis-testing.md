# Comparing Groups and Testing Hypotheses

``` r
library(mariposa)
library(dplyr)
data(survey_data)
```

## Overview

Statistical tests determine whether observed differences between groups
are real or due to random chance. This guide covers all hypothesis tests
in mariposa, organized by the type of data and research question.

### Choosing the Right Test

| Your data                     | 2 groups                                                                                | 3+ groups                                                                                   | Paired                                                                                  |
|-------------------------------|-----------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------|
| **Continuous, normal**        | [`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)               | [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)       | `t_test(paired)`                                                                        |
| **Continuous, non-normal**    | [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md)   | [`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md)   | [`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/reference/wilcoxon_test.md) |
| **Categorical**               | [`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)       | [`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)           | [`mcnemar_test()`](https://YannickDiehl.github.io/mariposa/reference/mcnemar_test.md)   |
| **Small sample, categorical** | [`fisher_test()`](https://YannickDiehl.github.io/mariposa/reference/fisher_test.md)     | —                                                                                           | —                                                                                       |
| **Multiple factors**          | —                                                                                       | [`factorial_anova()`](https://YannickDiehl.github.io/mariposa/reference/factorial_anova.md) | [`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md) |
| **With covariate**            | —                                                                                       | [`ancova()`](https://YannickDiehl.github.io/mariposa/reference/ancova.md)                   | —                                                                                       |
| **Proportion vs. expected**   | [`binomial_test()`](https://YannickDiehl.github.io/mariposa/reference/binomial_test.md) | [`chisq_gof()`](https://YannickDiehl.github.io/mariposa/reference/chisq_gof.md)             | —                                                                                       |

## t-Tests

### Independent Samples

Compare two groups on a continuous variable:

``` r
survey_data %>%
  t_test(life_satisfaction, group = gender, weights = sampling_weight)
#> t-Test: life_satisfaction by gender [Weighted]
#>   t(2390.8) = -1.069, p = 0.285 , g = -0.043 (negligible), N = 2436
```

The output includes both Student’s t-test (equal variances assumed) and
Welch’s t-test (not assumed). When in doubt, use Welch — it is more
robust.

For the detailed output with group descriptives, Levene’s test, and
confidence intervals:

``` r
survey_data %>%
  t_test(life_satisfaction, group = gender, weights = sampling_weight) %>%
  summary()
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

### Multiple Variables at Once

``` r
survey_data %>%
  t_test(trust_government, trust_media, trust_science,
         group = gender, weights = sampling_weight)
#> t-Test: trust_government by gender [Weighted]
#>   t(2322.7) = -0.682, p = 0.495 , g = -0.028 (negligible), N = 2371
#> t-Test: trust_media by gender [Weighted]
#>   t(2350.2) = -2.196, p = 0.028 *, g = -0.090 (negligible), N = 2382
#> t-Test: trust_science by gender [Weighted]
#>   t(2360.9) = -1.421, p = 0.156 , g = -0.058 (negligible), N = 2414
```

### One-Sample t-Test

Test whether a mean differs from a specific value:

``` r
# Is average life satisfaction different from the scale midpoint (3)?
survey_data %>%
  t_test(life_satisfaction, mu = 3, weights = sampling_weight)
#> t-Test: life_satisfaction [Weighted]
#>   t(2436.0) = 26.776, p < 0.001 ***
```

### Grouped Analysis

Run separate tests per subgroup:

``` r
survey_data %>%
  group_by(region) %>%
  t_test(income, group = gender, weights = sampling_weight)
#> [region = 1]
#> t-Test: income by gender [Weighted]
#>   t(431.6) = 1.676, p = 0.094 , g = 0.158 (negligible), N = 450
#> [region = 2]
#> t-Test: income by gender [Weighted]
#>   t(1739.9) = 0.009, p = 0.993 , g = 0.000 (negligible), N = 1751
```

## One-Way ANOVA

Compare means across three or more groups:

``` r
result <- survey_data %>%
  oneway_anova(life_satisfaction, group = education, weights = sampling_weight)
result
#> One-Way ANOVA: life_satisfaction by education [Weighted]
#>   F(3, 2433) = 65.359, p < 0.001 ***, eta2 = 0.075 (medium), N = 2437
```

The effect size $\eta^{2}$ (eta-squared) indicates how much variance is
explained by group membership:

- **Small**: $\eta^{2} \approx 0.01$
- **Medium**: $\eta^{2} \approx 0.06$
- **Large**: $\eta^{2} \approx 0.14$

### Post-Hoc Tests

A significant ANOVA tells you that groups differ, but not *which*
groups. Use post-hoc tests:

``` r
# Tukey HSD: balanced comparison of all pairs
tukey_test(result)
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
```

``` r
# Scheffe: more conservative (fewer false positives)
scheffe_test(result)
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

### Assumption Check

ANOVA assumes equal variances. Test with Levene’s test:

``` r
levene_test(result)
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

If Levene’s test is significant ($p < .05$), variances are unequal. Use
the Welch correction included in the ANOVA output.

## Factorial ANOVA

Test the effects of two or more factors and their interactions:

``` r
survey_data %>%
  factorial_anova(dv = income, between = c(gender, education),
                  weights = sampling_weight)
#> Factorial ANOVA (2-Way): income by gender, education [Weighted]
#>   gender:           F(1, 2178) = 0.115, p = 0.735 , eta2p = 0.000
#>   education:        F(3, 2178) = 455.835, p < 0.001 ***, eta2p = 0.386
#>   gender:education: F(3, 2178) = 0.300, p = 0.825 , eta2p = 0.000, N = 2186
```

The output uses Type III sums of squares and reports partial $\eta^{2}$
for each effect. Weighted analysis uses WLS estimation, matching SPSS
UNIANOVA.

For the full output with descriptive statistics per cell:

``` r
survey_data %>%
  factorial_anova(dv = life_satisfaction, between = c(gender, region),
                  weights = sampling_weight) %>%
  summary()
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

## ANCOVA

Compare groups while controlling for a covariate:

``` r
survey_data %>%
  ancova(dv = income, between = education, covariate = age,
         weights = sampling_weight)
#> ANCOVA: income by education, covariate: age [Weighted]
#>   age (covariate): F(1, 2181) = 0.019, p = 0.889 , eta2p = 0.000
#>   education:       F(3, 2181) = 458.943, p < 0.001 ***, eta2p = 0.387, N = 2186
```

The output includes the covariate effect, the adjusted factor effect,
and estimated marginal means (group means adjusted for the covariate).

## Non-Parametric Tests

Use these when data is not normally distributed, ordinal, or based on
small samples.

### Mann-Whitney U Test

The non-parametric alternative to the independent t-test:

``` r
survey_data %>%
  mann_whitney(political_orientation, group = region,
               weights = sampling_weight)
#> Mann-Whitney U Test: political_orientation by region [Weighted]
#>   U = 426,033, Z = 0.207, p = 0.836 , r = 0.004 (negligible), N = 2312
```

### Kruskal-Wallis H Test

The non-parametric alternative to one-way ANOVA (3+ groups):

``` r
kw_result <- survey_data %>%
  kruskal_wallis(life_satisfaction, group = education)

kw_result
#> 
#> Kruskal-Wallis Test Results
#> ---------------------------
#> 
#> - Grouping variable: education
#> - Groups: Basic Secondary, Intermediate Secondary, Academic Secondary, University
#> 
#> life_satisfaction
#> -----------------
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

When significant, use Dunn’s post-hoc test with Bonferroni correction:

``` r
dunn_test(kw_result)
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

The non-parametric alternative to the paired t-test:

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
#> score_T2 - score_T1
#> -------------------
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

The non-parametric alternative to repeated-measures ANOVA (3+
measurements):

``` r
friedman_result <- longitudinal_data_wide %>%
  friedman_test(score_T1, score_T2, score_T3)

friedman_result
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
pairwise_wilcoxon(friedman_result)
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
#> gender
#> ------
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

## Categorical Tests

### Chi-Square Test of Independence

Test whether two categorical variables are related:

``` r
survey_data %>%
  chi_square(education, employment, weights = sampling_weight)
#> Chi-Squared Test: education × employment [Weighted]
#>   chi2(12) = 130.696, p < 0.001 ***, V = 0.132 (small), N = 2518
```

A significant result means the variables are not independent — knowing
one tells you something about the other.

### Effect Sizes for Categorical Data

``` r
# Phi coefficient (2x2 tables)
survey_data %>%
  phi(gender, employment, weights = sampling_weight)
#> Chi-Squared Test: gender × employment [Weighted]
#>   chi2(4) = 7.661, p = 0.105 , V = 0.055 (neglig.), N = 2515
```

``` r
# Cramer's V (larger tables)
survey_data %>%
  cramers_v(education, employment, weights = sampling_weight)
#> Chi-Squared Test: education × employment [Weighted]
#>   chi2(12) = 130.696, p < 0.001 ***, V = 0.132 (small), N = 2518
```

### Fisher’s Exact Test

Use when expected cell frequencies are below 5:

``` r
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

Test whether observed frequencies match expected proportions:

``` r
# Equal proportions (default)
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

``` r
# Custom expected proportions
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

Compare paired proportions (e.g., before/after):

``` r
test_data <- survey_data %>%
  mutate(
    trust_gov_high = ifelse(trust_government > 3, 1, 0),
    trust_media_high = ifelse(trust_media > 3, 1, 0)
  )

test_data %>%
  mcnemar_test(var1 = trust_gov_high, var2 = trust_media_high)
```

## Interpreting Results

### p-Values

- $p < .05$: The difference is statistically significant
- $p \geq .05$: No significant difference detected

“Not significant” does **not** mean “no difference” — it means we cannot
rule out chance given the sample size.

### Effect Sizes

With large samples, even tiny differences can be significant. Always
check effect sizes:

| Test        | Effect size  | Small | Medium | Large |
|-------------|--------------|-------|--------|-------|
| t-test      | Cohen’s *d*  | 0.20  | 0.50   | 0.80  |
| ANOVA       | $\eta^{2}$   | 0.01  | 0.06   | 0.14  |
| Chi-square  | Cramer’s *V* | 0.10  | 0.30   | 0.50  |
| Correlation | *r*          | 0.10  | 0.30   | 0.50  |

### Multiple Comparisons

Running many tests inflates false positive rates. Post-hoc tests
([`tukey_test()`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md),
[`dunn_test()`](https://YannickDiehl.github.io/mariposa/reference/dunn_test.md),
[`pairwise_wilcoxon()`](https://YannickDiehl.github.io/mariposa/reference/pairwise_wilcoxon.md))
handle this automatically with corrections.

## Complete Example

A typical hypothesis testing workflow:

``` r
# 1. Describe the groups
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
anova_result
#> One-Way ANOVA: life_satisfaction by education [Weighted]
#>   F(3, 2433) = 65.359, p < 0.001 ***, eta2 = 0.075 (medium), N = 2437

# 3. Check assumptions
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

# 4. Post-hoc: which groups differ?
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
```

## Practical Tips

1.  **Check assumptions first.** Use `describe(show = "all")` to inspect
    skewness. For non-normal data, use non-parametric tests.

2.  **Match the test to the data.** Normal continuous data: t-test /
    ANOVA. Non-normal or ordinal: Mann-Whitney / Kruskal-Wallis.
    Categorical: chi-square / Fisher.

3.  **Always follow up significant omnibus tests.** Use
    [`tukey_test()`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md)
    for ANOVA,
    [`dunn_test()`](https://YannickDiehl.github.io/mariposa/reference/dunn_test.md)
    for Kruskal-Wallis,
    [`pairwise_wilcoxon()`](https://YannickDiehl.github.io/mariposa/reference/pairwise_wilcoxon.md)
    for Friedman.

4.  **Report effect sizes alongside p-values.** A significant result
    with a negligible effect size may not be practically meaningful.

5.  **Use weights when available.** They ensure results represent the
    population, not just the sample.

## Summary

### Parametric Tests

- **[`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)**
  compares means between two groups
- **[`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)**
  extends to three or more groups, with
  [`tukey_test()`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md)
  /
  [`scheffe_test()`](https://YannickDiehl.github.io/mariposa/reference/scheffe_test.md)
  post-hoc
- **[`factorial_anova()`](https://YannickDiehl.github.io/mariposa/reference/factorial_anova.md)**
  tests multiple factors and interactions
- **[`ancova()`](https://YannickDiehl.github.io/mariposa/reference/ancova.md)**
  controls for a covariate

### Non-Parametric Tests

- **[`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md)**,
  **[`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md)**
  (with
  [`dunn_test()`](https://YannickDiehl.github.io/mariposa/reference/dunn_test.md)),
  **[`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/reference/wilcoxon_test.md)**,
  **[`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md)**
  (with
  [`pairwise_wilcoxon()`](https://YannickDiehl.github.io/mariposa/reference/pairwise_wilcoxon.md)),
  **[`binomial_test()`](https://YannickDiehl.github.io/mariposa/reference/binomial_test.md)**

### Categorical Tests

- **[`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)**,
  **[`fisher_test()`](https://YannickDiehl.github.io/mariposa/reference/fisher_test.md)**,
  **[`chisq_gof()`](https://YannickDiehl.github.io/mariposa/reference/chisq_gof.md)**,
  **[`mcnemar_test()`](https://YannickDiehl.github.io/mariposa/reference/mcnemar_test.md)**

### Effect Sizes

- **[`phi()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)**,
  **[`cramers_v()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)**,
  **[`goodman_gamma()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)**

## Next Steps

- Measure relationships between continuous variables — see
  [`vignette("correlation-analysis")`](https://YannickDiehl.github.io/mariposa/articles/correlation-analysis.md)
- Build predictive models — see
  [`vignette("regression-analysis")`](https://YannickDiehl.github.io/mariposa/articles/regression-analysis.md)
- Construct reliable scales — see
  [`vignette("scale-analysis")`](https://YannickDiehl.github.io/mariposa/articles/scale-analysis.md)
