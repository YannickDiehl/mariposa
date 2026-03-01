# Comparing Groups and Testing Hypotheses

``` r
library(mariposa)
library(dplyr)
data(survey_data)
```

## Overview

Statistical tests help you determine whether differences between groups
are real or just due to random chance. This guide covers the main tests
available in mariposa:

| Test                                                                                  | Use when…                                                        |
|---------------------------------------------------------------------------------------|------------------------------------------------------------------|
| [`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)             | Comparing means between **two** groups                           |
| [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md) | Comparing means across **three or more** groups                  |
| [`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)     | Testing relationships between **categorical** variables          |
| [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md) | Non-parametric alternative when data is not normally distributed |

## t-Tests

### Independent Samples t-Test

Compare average life satisfaction between genders:

``` r
survey_data %>%
  t_test(life_satisfaction, group = gender)
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
#> Interpretation:
#> - Positive differences: First group > Second group
#> - Negative differences: First group < Second group
#> - Confidence intervals not containing 0 indicate significant differences
#> - p-values are adjusted for multiple comparisons (family-wise error control)

# Scheffe test: more conservative (fewer false positives)
scheffe_test(anova_result)
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
#> 
#> --- life_satisfaction ---
#> 
#> Weighted Levene's Test Results:
#> -------------------------------------------------------------------- 
#>           Variable F_statistic df1  df2 p_value sig        Conclusion
#>  life_satisfaction      31.287   3 2433       0 *** Variances unequal
#> --------------------------------------------------------------------
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
#> Observed Frequencies:
#>                         var2
#> var1                     Student Employed Unemployed Retired Other
#>   Basic Secondary              0      571         65     171    34
#>   Intermediate Secondary       0      412         51     137    29
#>   Academic Secondary          44      366         44     145    32
#>   University                  34      251         22      72    20
#> 
#> Expected Frequencies:
#>                         var2
#> var1                     Student Employed Unemployed Retired  Other
#>   Basic Secondary         26.239   538.24     61.225  176.61 38.686
#>   Intermediate Secondary  19.625   402.56     45.791  132.09 28.934
#>   Academic Secondary      19.687   403.84     45.937  132.51 29.026
#>   University              12.449   255.36     29.047   83.79 18.354
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
```

A significant result means the variables are not independent — knowing
someone’s education tells you something about their employment status
(or vice versa).

### With Survey Weights

``` r
survey_data %>%
  chi_square(education, employment, weights = sampling_weight)
#> 
#> Observed Frequencies:
#>                         var2
#> var1                     Student Employed Unemployed Retired Other
#>   Basic Secondary              0      573         66     175    34
#>   Intermediate Secondary       0      420         52     139    29
#>   Academic Secondary          46      370         45     149    33
#>   University                  34      240         21      72    20
#> 
#> Expected Frequencies:
#>                         var2
#> var1                     Student Employed Unemployed Retired  Other
#>   Basic Secondary         26.942  539.851     61.967 180.175 39.066
#>   Intermediate Secondary  20.334  407.434     46.767 135.981 29.484
#>   Academic Secondary      20.429  409.344     46.986 136.618 29.622
#>   University              12.295  246.371     28.280  82.226 17.828
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
```

### Multiple Comparisons

Test several relationships:

``` r
survey_data %>%
  chi_square(employment, education, weights = sampling_weight)
#> 
#> Observed Frequencies:
#>             var2
#> var1         Basic Secondary Intermediate Secondary Academic Secondary
#>   Student                  0                      0                 46
#>   Employed               573                    420                370
#>   Unemployed              66                     52                 45
#>   Retired                175                    139                149
#>   Other                   34                     29                 33
#>             var2
#> var1         University
#>   Student            34
#>   Employed          240
#>   Unemployed         21
#>   Retired            72
#>   Other              20
#> 
#> Expected Frequencies:
#>             var2
#> var1         Basic Secondary Intermediate Secondary Academic Secondary
#>   Student             26.942                 20.334             20.429
#>   Employed           539.851                407.434            409.344
#>   Unemployed          61.967                 46.767             46.986
#>   Retired            180.175                135.981            136.618
#>   Other               39.066                 29.484             29.622
#>             var2
#> var1         University
#>   Student        12.295
#>   Employed      246.371
#>   Unemployed     28.280
#>   Retired        82.226
#>   Other          17.828
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

survey_data %>%
  chi_square(employment, gender, weights = sampling_weight)
#> 
#> Observed Frequencies:
#>             var2
#> var1         Male Female
#>   Student      36     44
#>   Employed    748    855
#>   Unemployed   83    101
#>   Retired     260    273
#>   Other        68     47
#> 
#> Expected Frequencies:
#>             var2
#> var1            Male  Female
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
#> Effect Size Interpretation (r):
#> - Small effect: |r| ~ 0.1
#> - Medium effect: |r| ~ 0.3
#> - Large effect: |r| ~ 0.5
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
#> Group: education = Basic Secondary
#> ----------------------------------
#>           Variable  Mean Median    SD Range IQR Skewness Effective_N
#>  life_satisfaction 3.208      3 1.243     4   2   -0.056       801.2
#> 
#> Group: education = Intermediate Secondary
#> -----------------------------------------
#>           Variable  Mean Median   SD Range IQR Skewness Effective_N
#>  life_satisfaction 3.698      4 1.11     4   2    -0.59       611.8
#> 
#> Group: education = Academic Secondary
#> -------------------------------------
#>           Variable  Mean Median    SD Range IQR Skewness Effective_N
#>  life_satisfaction 3.851      4 0.997     4   2    -0.58       600.6
#> 
#> Group: education = University
#> -----------------------------
#>           Variable Mean Median    SD Range IQR Skewness Effective_N
#>  life_satisfaction 4.04      4 0.962     4   1   -0.963       377.8

# 2. Test for overall differences
anova_result <- survey_data %>%
  oneway_anova(life_satisfaction, group = education,
               weights = sampling_weight)
print(anova_result)
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
#> Effect Size Interpretation:
#> - Eta-squared: Proportion of variance explained (biased upward)
#> - Epsilon-squared: Less biased than eta-squared
#> - Omega-squared: Unbiased estimate (preferred for publication)
#> - Small effect: eta-squared ~ 0.01, Medium effect: eta-squared ~ 0.06, Large effect: eta-squared ~ 0.14
#> 
#> Post-hoc tests: Use tukey_test() for pairwise comparisons

# 3. Post-hoc: which groups differ?
tukey_test(anova_result)
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
#> Interpretation:
#> - Positive differences: First group > Second group
#> - Negative differences: First group < Second group
#> - Confidence intervals not containing 0 indicate significant differences
#> - p-values are adjusted for multiple comparisons (family-wise error control)

# 4. Check assumptions
levene_test(anova_result)
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
    [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md).
2.  **Use appropriate tests.** Normal data with equal variances:
    t-test/ANOVA. Non-normal or ordinal: Mann-Whitney. Categorical:
    chi-square.
3.  **Report completely.** Always include the test statistic, degrees of
    freedom, p-value, effect size, and confidence intervals.
4.  **Watch for multiple comparisons.** Running many tests inflates the
    chance of false positives. Consider Bonferroni correction or
    pre-register your hypotheses.
5.  **Significance is not importance.** A statistically significant
    result with a negligible effect size may not be practically
    relevant.

## Summary

1.  **[`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)**
    compares means between two groups — use it for questions like “Do
    men and women differ?”
2.  **[`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)**
    extends this to three or more groups, with post-hoc tests to
    identify which groups differ
3.  **[`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)**
    tests whether categorical variables are related
4.  **[`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md)**
    is the non-parametric alternative when assumptions are not met
5.  Always report **effect sizes** alongside p-values

## Next Steps

- Explore relationships between continuous variables — see
  [`vignette("correlation-analysis")`](https://YannickDiehl.github.io/mariposa/articles/correlation-analysis.md)
- Learn about weighted analysis — see
  [`vignette("survey-weights")`](https://YannickDiehl.github.io/mariposa/articles/survey-weights.md)
- Revisit your data — see
  [`vignette("descriptive-statistics")`](https://YannickDiehl.github.io/mariposa/articles/descriptive-statistics.md)
