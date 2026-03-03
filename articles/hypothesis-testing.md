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
#> t-Test: life_satisfaction by gender
#>   t(2384.1) = -1.018, p = 0.309 , g = -0.041 (negligible), N = 2421
```

With survey weights for representative results:

``` r
survey_data %>%
  t_test(life_satisfaction, group = gender, weights = sampling_weight)
#> t-Test: life_satisfaction by gender [Weighted]
#>   t(2390.8) = -1.069, p = 0.285 , g = -0.043 (negligible), N = 2436
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
#> t-Test: trust_government by gender [Weighted]
#>   t(2322.7) = -0.682, p = 0.495 , g = -0.028 (negligible), N = 2371
#> t-Test: trust_science by gender [Weighted]
#>   t(2360.9) = -1.421, p = 0.156 , g = -0.058 (negligible), N = 2414
#> t-Test: trust_media by gender [Weighted]
#>   t(2350.2) = -2.196, p = 0.028 *, g = -0.090 (negligible), N = 2382
```

### One-Sample t-Test

Test whether a population mean differs from a specific value. For
example, is average life satisfaction different from 3 (the scale
midpoint)?

``` r
survey_data %>%
  t_test(life_satisfaction, mu = 3, weights = sampling_weight)
#> t-Test: life_satisfaction [Weighted]
#>   t(2436.0) = 26.776, p < 0.001 ***
```

### Grouped Analysis

Run separate tests for each region:

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

### Basic ANOVA

Compare means across multiple groups (3+ categories):

``` r
survey_data %>%
  oneway_anova(life_satisfaction, group = education)
#> One-Way ANOVA: life_satisfaction by education
#>   F(3, 2417) = 67.096, p < 0.001 ***, eta2 = 0.077 (medium), N = 2421
```

### With Survey Weights

``` r
survey_data %>%
  oneway_anova(life_satisfaction, group = education,
               weights = sampling_weight)
#> One-Way ANOVA: life_satisfaction by education [Weighted]
#>   F(3, 2433) = 65.359, p < 0.001 ***, eta2 = 0.075 (medium), N = 2437
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
#> Chi-Squared Test: education × employment
#>   chi2(12) = 125.867, p < 0.001 ***, V = 0.130 (small), N = 2500
```

A significant result means the variables are not independent — knowing
someone’s education tells you something about their employment status
(or vice versa).

### With Survey Weights

``` r
survey_data %>%
  chi_square(education, employment, weights = sampling_weight)
#> Chi-Squared Test: education × employment [Weighted]
#>   chi2(12) = 130.696, p < 0.001 ***, V = 0.132 (small), N = 2518
```

### Multiple Comparisons

Test several relationships:

``` r
survey_data %>%
  chi_square(employment, education, weights = sampling_weight)
#> Chi-Squared Test: employment × education [Weighted]
#>   chi2(12) = 130.696, p < 0.001 ***, V = 0.132 (small), N = 2518

survey_data %>%
  chi_square(employment, gender, weights = sampling_weight)
#> Chi-Squared Test: employment × gender [Weighted]
#>   chi2(4) = 7.661, p = 0.105 , V = 0.055 (neglig.), N = 2515
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
#> Mann-Whitney U Test: political_orientation by region
#>   U = 409,008, Z = -0.173, p = 0.862 , r = 0.004 (negligible), N = 2299
```

### With Survey Weights

``` r
survey_data %>%
  mann_whitney(political_orientation, group = region,
               weights = sampling_weight)
#> Mann-Whitney U Test: political_orientation by region [Weighted]
#>   U = 426,033, Z = 0.207, p = 0.836 , r = 0.004 (negligible), N = 2312
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
#> Factorial ANOVA (2-Way): income by gender, education
#>   gender:           F(1, 2178) = 0.098, p = 0.755 , eta2p = 0.000
#>   education:        F(3, 2178) = 463.521, p < 0.001 ***, eta2p = 0.390
#>   gender:education: F(3, 2178) = 0.399, p = 0.754 , eta2p = 0.001, N = 2186
```

The output includes Type III sums of squares, main effects for each
factor, interaction effects, partial eta-squared, and descriptive
statistics per cell.

### With Survey Weights

``` r
survey_data %>%
  factorial_anova(dv = life_satisfaction, between = c(gender, region),
                  weights = sampling_weight)
#> Factorial ANOVA (2-Way): life_satisfaction by gender, region [Weighted]
#>   gender:        F(1, 2417) = 0.008, p = 0.930 , eta2p = 0.000
#>   region:        F(1, 2417) = 0.001, p = 0.979 , eta2p = 0.000
#>   gender:region: F(1, 2417) = 1.642, p = 0.200 , eta2p = 0.001, N = 2421
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
#> ANCOVA: income by education, covariate: age
#>   age (covariate): F(1, 2181) = 0.030, p = 0.862 , eta2p = 0.000
#>   education:       F(3, 2181) = 466.246, p < 0.001 ***, eta2p = 0.391, N = 2186
```

The output shows the covariate effect, the factor effect after
adjustment, and estimated marginal means (group means adjusted for the
covariate).

### With Survey Weights

``` r
survey_data %>%
  ancova(dv = income, between = education, covariate = age,
         weights = sampling_weight)
#> ANCOVA: income by education, covariate: age [Weighted]
#>   age (covariate): F(1, 2181) = 0.019, p = 0.889 , eta2p = 0.000
#>   education:       F(3, 2181) = 458.943, p < 0.001 ***, eta2p = 0.387, N = 2186
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
#> One-Way ANOVA: income by education [Weighted]
#>   F(3, 2197) = 462.325, p < 0.001 ***, eta2 = 0.387 (large), N = 2201
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
anova_result                                      # compact overview
#> One-Way ANOVA: life_satisfaction by education [Weighted]
#>   F(3, 2433) = 65.359, p < 0.001 ***, eta2 = 0.075 (medium), N = 2437
summary(anova_result, descriptives = FALSE)       # detailed output
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
