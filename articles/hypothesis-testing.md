# Comparing Groups and Testing Hypotheses

``` r
library(mariposa)
#> 
#> Attaching package: 'mariposa'
#> The following object is masked from 'package:stats':
#> 
#>     frequency
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
data(survey_data)
```

## When to Use These Tests

Statistical tests help you determine whether differences between groups
are real or just due to random chance. Here’s when to use each test:

- **t-test**: Compare means between two groups
- **ANOVA**: Compare means across three or more groups
- **Chi-square**: Test relationships between categorical variables
- **Mann-Whitney**: Non-parametric alternative when data isn’t normal

## t-Tests

### Independent Samples t-Test

Compare average life satisfaction between genders:

``` r
survey_data %>%
  t_test(life_satisfaction, group = gender)
#> ── t-Test Results ──────────────────────────────────────────────────────────────
#> • Grouping variable: gender
#> • Groups compared: Male vs. Female
#> • Confidence level: 95.0%
#> • Alternative hypothesis: two.sided
#> • Null hypothesis (mu): 0.000
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
#> Signif. codes: 0 *** 0.001 ** 0.01 * 0.05
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
#> ── Weighted t-Test Results ─────────────────────────────────────────────────────
#> • Grouping variable: gender
#> • Groups compared: Male vs. Female
#> • Weights variable: sampling_weight
#> • Confidence level: 95.0%
#> • Alternative hypothesis: two.sided
#> • Null hypothesis (mu): 0.000
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
#> Signif. codes: 0 *** 0.001 ** 0.01 * 0.05
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

Test several outcomes simultaneously:

``` r
# Test all trust variables between genders
survey_data %>%
  t_test(trust_government, trust_science, trust_media,
         group = gender, weights = sampling_weight)
#> ── Weighted t-Test Results ─────────────────────────────────────────────────────
#> • Grouping variable: gender
#> • Groups compared: Male vs. Female
#> • Weights variable: sampling_weight
#> • Confidence level: 95.0%
#> • Alternative hypothesis: two.sided
#> • Null hypothesis (mu): 0.000
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
#> Signif. codes: 0 *** 0.001 ** 0.01 * 0.05
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

Test whether the population mean differs from a specific value:

``` r
# Is average life satisfaction different from 3 (neutral)?
survey_data %>%
  t_test(life_satisfaction, mu = 3, weights = sampling_weight)
#> ── Weighted t-Test Results ─────────────────────────────────────────────────────
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
#> Signif. codes: 0 *** 0.001 ** 0.01 * 0.05
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
#> ── Weighted t-Test Results ─────────────────────────────────────────────────────
#> • Grouping variable: gender
#> • Groups compared: Male vs. Female
#> • Weights variable: sampling_weight
#> • Confidence level: 95.0%
#> • Alternative hypothesis: two.sided
#> • Null hypothesis (mu): 0.000
#> 
#> 
#> Group: region = East
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
#> Signif. codes: 0 *** 0.001 ** 0.01 * 0.05
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
# Compare life satisfaction across education levels
survey_data %>%
  oneway_anova(life_satisfaction, group = education)
#> ── One-Way ANOVA Results ───────────────────────────────────────────────────────
#> 
#> Dependent Variable: life_satisfaction
#> Grouping Variable: education
#> Null hypothesis: All group means are equal
#> Alternative hypothesis: At least one group mean differs
#> Confidence level: 95.0%
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
#>  Between Groups     247.347    3      82.449 67.096   <.001 ***
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
#> ── Weighted One-Way ANOVA Results ──────────────────────────────────────────────
#> 
#> Dependent Variable: life_satisfaction
#> Grouping Variable: education
#> Weights Variable: sampling_weight
#> Null hypothesis: All group means are equal
#> Alternative hypothesis: At least one group mean differs
#> Confidence level: 95.0%
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
#>  Between Groups     241.130    3      80.377 65.359   <.001 ***
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

### Post-Hoc Tests

When ANOVA is significant, use post-hoc tests to see which groups
differ:

``` r
# First run ANOVA and save result
anova_result <- survey_data %>%
  oneway_anova(life_satisfaction, group = education,
               weights = sampling_weight)

# Then run Tukey test on the result
tukey_test(anova_result)
#> ── Weighted Tukey HSD Post-Hoc Test Results ────────────────────────────────────
#> 
#> Dependent Variable: life_satisfaction
#> Grouping Variable: education
#> Weights Variable: sampling_weight
#> Confidence level: 95.0%
#> Family-wise error rate controlled using Tukey HSD
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

# Or Scheffe test (more conservative)
scheffe_test(anova_result)
#> ── Weighted Scheffe Post-Hoc Test Results ──────────────────────────────────────
#> 
#> Dependent Variable: life_satisfaction
#> Grouping Variable: education
#> Weights Variable: sampling_weight
#> Confidence level: 95.0%
#> Family-wise error rate controlled using Scheffe's method
#> Note: Most conservative post-hoc test (widest confidence intervals)
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

Check if variances are equal across groups:

``` r
levene_test(anova_result)
#> ── Weighted Levene's Test for Homogeneity of Variance  ─────────────────────────
#> 
#> Grouping variable: education
#> Weights variable: sampling_weight
#> Center: mean
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

## Chi-Square Test

### Basic Chi-Square

Test if two categorical variables are related:

``` r
# Is education related to employment status?
survey_data %>%
  chi_square(education, employment)
#> ── Chi-Squared Test of Independence  ───────────────────────────────────────────
#> 
#> Variables: education x employment
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
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```

### With Survey Weights

``` r
survey_data %>%
  chi_square(education, employment, weights = sampling_weight)
#> ── Weighted Chi-Squared Test of Independence  ──────────────────────────────────
#> 
#> Variables: education x employment
#> Weights variable: sampling_weight
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
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```

### Multiple Tests

Test several relationships separately:

``` r
# Test association between employment and education
survey_data %>%
  chi_square(employment, education, weights = sampling_weight)
#> ── Weighted Chi-Squared Test of Independence  ──────────────────────────────────
#> 
#> Variables: employment x education
#> Weights variable: sampling_weight
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
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05

# Test association between employment and gender
survey_data %>%
  chi_square(employment, gender, weights = sampling_weight)
#> ── Weighted Chi-Squared Test of Independence  ──────────────────────────────────
#> 
#> Variables: employment x gender
#> Weights variable: sampling_weight
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
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05

# Test association between employment and region
survey_data %>%
  chi_square(employment, region, weights = sampling_weight)
#> ── Weighted Chi-Squared Test of Independence  ──────────────────────────────────
#> 
#> Variables: employment x region
#> Weights variable: sampling_weight
#> 
#> Observed Frequencies:
#>             var2
#> var1         East West
#>   Student      12   68
#>   Employed    320 1282
#>   Unemployed   33  151
#>   Retired     123  411
#>   Other        21   94
#> 
#> Expected Frequencies:
#>             var2
#> var1            East     West
#>   Student     16.191   63.809
#>   Employed   324.222 1277.778
#>   Unemployed  37.239  146.761
#>   Retired    108.074  425.926
#>   Other       23.274   91.726
#> 
#> Chi-Squared Test Results:
#> -------------------------------------------------- 
#>  Chi_squared df p_value sig
#>        4.897  4   0.298    
#> -------------------------------------------------- 
#> 
#> Effect Sizes:
#> ---------------------------------------------------------------------- 
#>     Measure  Value p_value sig Interpretation
#>  Cramer's V  0.044   0.298            Neglig.
#>       Gamma -0.054   0.350               Weak
#> ---------------------------------------------------------------------- 
#> Table size: 5×2 | N = 2515
#> Note: Phi coefficient only shown for 2x2 tables
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```

## Mann-Whitney Test

### When to Use

Use when: - Data is not normally distributed - Sample sizes are small -
Data is ordinal (ranked)

### Basic Usage

``` r
# Compare political orientation between regions
survey_data %>%
  mann_whitney(political_orientation, group = region)
#> ── Mann-Whitney U Test Results ─────────────────────────────────────────────────
#> 
#> Grouping variable: region
#> Groups compared: East vs. West
#> Null hypothesis (mu): 0.000
#> Alternative hypothesis: two.sided
#> Confidence level: 95.0%
#> 
#> 
#> --- political_orientation ---
#> 
#>   East: rank mean = 1145.3, n = 443.0
#>   West: rank mean = 1151.1, n = 1856.0
#> 
#> 
#> Mann-Whitney U Test Results:
#> ---------------------------------------------------------------------- 
#>            Test       U       W      Z p_value effect_r sig
#>  Mann-Whitney U 409,008 507,354 -0.173   0.862    0.004    
#> ---------------------------------------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (r):
#> - Small effect: |r| ≈ 0.1
#> - Medium effect: |r| ≈ 0.3
#> - Large effect: |r| ≈ 0.5
```

### With Survey Weights

``` r
survey_data %>%
  mann_whitney(political_orientation, group = region,
               weights = sampling_weight)
#> ── Weighted Mann-Whitney U Test Results ────────────────────────────────────────
#> 
#> Grouping variable: region
#> Groups compared: East vs. West
#> Weights variable: sampling_weight
#> Null hypothesis (mu): 0.000
#> Alternative hypothesis: two.sided
#> Confidence level: 95.0%
#> 
#> 
#> --- political_orientation ---
#> 
#>   East: rank mean = 918.3, n = 464.0
#>   West: rank mean = 1216.0, n = 1848.5
#> 
#> 
#> Weighted Mann-Whitney U Test Results:
#> ---------------------------------------------------------------------- 
#>            Test       U         W      Z p_value effect_r sig
#>  Mann-Whitney U 318,211 2,247,673 -21.44   0.862    0.446    
#> ---------------------------------------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (r):
#> - Small effect: |r| ≈ 0.1
#> - Medium effect: |r| ≈ 0.3
#> - Large effect: |r| ≈ 0.5
```

## Interpreting Results

### Understanding p-values

- **p \< 0.05**: Statistically significant difference
- **p ≥ 0.05**: No significant difference detected
- Remember: “not significant” ≠ “no difference”

### Effect Sizes Matter

Statistical significance doesn’t always mean practical importance:

``` r
# ANOVA provides eta-squared effect size
result <- survey_data %>%
  oneway_anova(income, group = education, weights = sampling_weight)
print(result)
#> ── Weighted One-Way ANOVA Results ──────────────────────────────────────────────
#> 
#> Dependent Variable: income
#> Grouping Variable: education
#> Weights Variable: sampling_weight
#> Null hypothesis: All group means are equal
#> Alternative hypothesis: At least one group mean differs
#> Confidence level: 95.0%
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
#>  Between Groups  1726289513    3 575429837.501 462.325   <.001 ***
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

Effect size guidelines: - Small: η² = 0.01 - Medium: η² = 0.06 - Large:
η² = 0.14

### Sample Size Effects

Large samples can make tiny differences “significant”: - Always check
effect sizes - Consider practical significance - Look at confidence
intervals

## Complete Example

Here’s a typical hypothesis testing workflow:

``` r
# 1. Descriptive statistics first
cat("=== Descriptive Summary ===\n")
#> === Descriptive Summary ===
survey_data %>%
  group_by(education) %>%
  describe(life_satisfaction, weights = sampling_weight)
#> ── Weighted Descriptive Statistics ─────────────────────────────────────────────
#> 
#> ── Group: education = Basic Secondary ──
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#>           Variable  Mean Median    SD Range IQR Skewness Effective_N
#>  life_satisfaction 3.208      3 1.243     4   2   -0.056       801.2
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> ── Group: education = Intermediate Secondary ──
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#>           Variable  Mean Median   SD Range IQR Skewness Effective_N
#>  life_satisfaction 3.698      4 1.11     4   2    -0.59       611.8
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> ── Group: education = Academic Secondary ──
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#>           Variable  Mean Median    SD Range IQR Skewness Effective_N
#>  life_satisfaction 3.851      4 0.997     4   2    -0.58       600.6
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> ── Group: education = University ──
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#>           Variable Mean Median    SD Range IQR Skewness Effective_N
#>  life_satisfaction 4.04      4 0.962     4   1   -0.963       377.8
#> ────────────────────────────────────────────────────────────────────────────────

# 2. Test for overall difference
cat("\n=== ANOVA Test ===\n")
#> 
#> === ANOVA Test ===
anova_result <- survey_data %>%
  oneway_anova(life_satisfaction, group = education,
               weights = sampling_weight)
print(anova_result)
#> ── Weighted One-Way ANOVA Results ──────────────────────────────────────────────
#> 
#> Dependent Variable: life_satisfaction
#> Grouping Variable: education
#> Weights Variable: sampling_weight
#> Null hypothesis: All group means are equal
#> Alternative hypothesis: At least one group mean differs
#> Confidence level: 95.0%
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
#>  Between Groups     241.130    3      80.377 65.359   <.001 ***
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

# 3. If significant, see which groups differ
if (!is.null(anova_result) && "p_value" %in% names(anova_result) && !is.na(anova_result$p_value[1]) && anova_result$p_value[1] < 0.05) {
  cat("\n=== Post-hoc Comparisons ===\n")
  tukey_test(anova_result)
}

# 4. Check assumptions
cat("\n=== Assumption Check ===\n")
#> 
#> === Assumption Check ===
levene_test(anova_result)
#> ── Weighted Levene's Test for Homogeneity of Variance  ─────────────────────────
#> 
#> Grouping variable: education
#> Weights variable: sampling_weight
#> Center: mean
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

### 1. Check Assumptions First

Before t-test or ANOVA:

``` r
# Visual check for normality
# (In practice, create histograms or Q-Q plots)
survey_data %>%
  group_by(gender) %>%
  describe(life_satisfaction, show = c("skew", "kurtosis"))
#> ── Descriptive Statistics ──────────────────────────────────────────────────────
#> 
#> ── Group: gender = Male ──
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#>           Variable Skewness Kurtosis    N Missing
#>  life_satisfaction   -0.468   -0.644 1149      45
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> ── Group: gender = Female ──
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#>           Variable Skewness Kurtosis    N Missing
#>  life_satisfaction   -0.531   -0.559 1272      34
#> ────────────────────────────────────────────────────────────────────────────────
```

### 2. Use Appropriate Tests

- **Normal data + equal variances**: t-test/ANOVA
- **Non-normal or ordinal**: Mann-Whitney
- **Categorical**: Chi-square

### 3. Report Completely

Always report: - Test statistic - Degrees of freedom - p-value - Effect
size - Confidence intervals

### 4. Multiple Comparisons

When running many tests, consider: - Bonferroni correction - False
Discovery Rate (FDR) - Pre-registration of hypotheses

## Common Mistakes to Avoid

1.  **Ignoring assumptions**: Check normality and variance
2.  **p-hacking**: Don’t test everything hoping for significance
3.  **Ignoring weights**: Unweighted tests can be misleading
4.  **Over-interpreting p-values**: p = 0.049 vs p = 0.051 is not
    meaningfully different
5.  **Forgetting effect sizes**: Significance ≠ importance

## Next Steps

After finding significant differences: - Explore with
[`crosstab()`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md)
for categorical relationships - Use
[`pearson_cor()`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md)
for continuous associations - Consider regression models for controlling
confounds - Validate findings with different subgroups
