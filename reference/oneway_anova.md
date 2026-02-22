# Compare Multiple Groups: Are Their Averages Different?

`oneway_anova()` helps you determine if average values differ across
three or more groups. For example, does average income vary by education
level? Or is customer satisfaction different across regions?

Think of it as:

- An extension of t-test for more than two groups

- A way to test if group membership affects outcomes

- A tool to identify meaningful group differences

The test tells you:

- Whether at least one group is different from the others

- How much variation is explained by group membership

- Which variance assumption fits your data best

## Usage

``` r
oneway_anova(
  data,
  ...,
  group,
  weights = NULL,
  var.equal = TRUE,
  conf.level = 0.95
)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- ...:

  The numeric variables you want to analyze. You can list multiple
  variables or use helpers like `starts_with("income")`

- group:

  The categorical variable that defines your groups (e.g., education,
  region, age_group). Must have at least 3 groups for ANOVA.

- weights:

  Optional survey weights for population-representative results

- var.equal:

  Should we assume all groups have similar variance? (Default: TRUE)

  - TRUE: Standard ANOVA (assumes equal variances)

  - FALSE: Welch's ANOVA (allows unequal variances)

- conf.level:

  Confidence level for group statistics (Default: 0.95 = 95%)

## Value

ANOVA results showing whether groups differ, including:

- F-statistic and p-value (are groups different?)

- Effect sizes (how much do groups matter?)

- Group statistics (means and standard deviations)

- Both standard and Welch ANOVA results

## Details

### Understanding the Results

**P-value**: If p \< 0.05, at least one group average is different

- p \< 0.001: Very strong evidence of group differences

- p \< 0.01: Strong evidence of group differences

- p \< 0.05: Moderate evidence of group differences

- p ≥ 0.05: No significant group differences found

**Effect Sizes** (How much do groups matter?):

- **Eta-squared**: Proportion of variance explained by groups

  - \< 0.01: Negligible effect

  - 0.01-0.06: Small effect

  - 0.06-0.14: Medium effect

  - 0.14 or higher: Large effect

- **Omega-squared**: More conservative estimate (usually preferred)

### When to Use This

Use ANOVA when:

- You have one numeric outcome (income, satisfaction score, etc.)

- You have one categorical grouping variable with 3+ groups

- You want to know if group averages differ

- Your data is roughly normally distributed within groups

### Choosing Variance Assumptions

- **Standard ANOVA** (var.equal = TRUE): Use when group variances are
  similar

- **Welch's ANOVA** (var.equal = FALSE): Use when group variances differ
  or you're unsure

- The function shows both results for comparison

### What Comes Next?

If ANOVA is significant:

1.  Look at group means to see the pattern

2.  Use
    [`tukey_test()`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md)
    to find which specific groups differ

3.  Consider effect sizes to judge practical importance

### Tips for Success

- Check that each group has sufficient observations (ideally 20+)

- Look at both p-values and effect sizes

- Use Welch's ANOVA if group sizes are very unequal

- Follow up with post-hoc tests to identify specific differences

- Welch's robust test for equality of means

### Interpretation Guidelines

- **p-values**: p \< 0.05 indicates significant group differences at
  alpha = 0.05

- **Effect sizes**: eta-squared ~ 0.01 (small), eta-squared ~ 0.06
  (medium), eta-squared ~ 0.14 (large)

- **Eta-squared**: Proportion of variance explained (tends to
  overestimate)

- **Epsilon-squared**: Less biased than eta-squared

- **Omega-squared**: Unbiased estimate (preferred for publication)

## References

Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences
(2nd ed.). Lawrence Erlbaum Associates.

Welch, B. L. (1951). On the comparison of several mean values: an
alternative approach. Biometrika, 38(3/4), 330-336.

Olejnik, S., & Algina, J. (2003). Generalized eta and omega squared
statistics: measures of effect size for some common research designs.
Psychological Methods, 8(4), 434-447.

## See also

[`aov`](https://rdrr.io/r/stats/aov.html) for the base R ANOVA function.

[`oneway.test`](https://rdrr.io/r/stats/oneway.test.html) for Welch's
ANOVA.

[`tukey_test`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md)
for post-hoc pairwise comparisons.

[`levene_test`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md)
for testing homogeneity of variances.

Other hypothesis_tests:
[`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md),
[`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md),
[`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic one-way ANOVA (comparing across education levels)
survey_data %>%
  oneway_anova(life_satisfaction, group = education)
#> 
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

# Multiple dependent variables
survey_data %>%
  oneway_anova(life_satisfaction, trust_government, group = education)
#> 
#> ── One-Way ANOVA Results ───────────────────────────────────────────────────────
#> Grouping Variable: education
#> Null hypothesis: All group means are equal for each variable
#> Alternative hypothesis: At least one group mean differs for each variable
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
#> --- trust_government ---
#> 
#> Descriptive Statistics by Group:
#>   Basic Secondary: mean = 2.647, sd = 1.149, n = 791
#>   Intermediate Secondary: mean = 2.581, sd = 1.175, n = 592
#>   Academic Secondary: mean = 2.610, sd = 1.183, n = 595
#>   University: mean = 2.644, sd = 1.146, n = 376
#> 
#> ANOVA Results:
#> -------------------------------------------------------------------------------- 
#>          Source Sum_Squares   df Mean_Square     F p_value sig
#>  Between Groups       1.753    3       0.584 0.431   0.731    
#>   Within Groups    3182.484 2350       1.354                  
#>           Total    3184.237 2353                              
#> -------------------------------------------------------------------------------- 
#> 
#> Assumption Tests:
#> ---------------- 
#>  Assumption Statistic df1  df2 p_value sig
#>       Welch     0.431   3 1156   0.731    
#> 
#> Effect Sizes:
#> ------------ 
#>          Variable Eta_Squared Epsilon_Squared Omega_Squared Effect_Size
#>  trust_government       0.001               0             0  negligible
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
#> Post-hoc tests: Use tukey_test() for pairwise comparisons on each variable

# Using tidyselect helpers
survey_data %>%
  oneway_anova(starts_with("trust_"), group = education)
#> 
#> ── One-Way ANOVA Results ───────────────────────────────────────────────────────
#> Grouping Variable: education
#> Null hypothesis: All group means are equal for each variable
#> Alternative hypothesis: At least one group mean differs for each variable
#> Confidence level: 95.0%
#> 
#> 
#> --- trust_government ---
#> 
#> Descriptive Statistics by Group:
#>   Basic Secondary: mean = 2.647, sd = 1.149, n = 791
#>   Intermediate Secondary: mean = 2.581, sd = 1.175, n = 592
#>   Academic Secondary: mean = 2.610, sd = 1.183, n = 595
#>   University: mean = 2.644, sd = 1.146, n = 376
#> 
#> ANOVA Results:
#> -------------------------------------------------------------------------------- 
#>          Source Sum_Squares   df Mean_Square     F p_value sig
#>  Between Groups       1.753    3       0.584 0.431   0.731    
#>   Within Groups    3182.484 2350       1.354                  
#>           Total    3184.237 2353                              
#> -------------------------------------------------------------------------------- 
#> 
#> Assumption Tests:
#> ---------------- 
#>  Assumption Statistic df1  df2 p_value sig
#>       Welch     0.431   3 1156   0.731    
#> 
#> Effect Sizes:
#> ------------ 
#>          Variable Eta_Squared Epsilon_Squared Omega_Squared Effect_Size
#>  trust_government       0.001               0             0  negligible
#> 
#> 
#> --- trust_media ---
#> 
#> Descriptive Statistics by Group:
#>   Basic Secondary: mean = 2.443, sd = 1.163, n = 797
#>   Intermediate Secondary: mean = 2.500, sd = 1.188, n = 594
#>   Academic Secondary: mean = 2.464, sd = 1.149, n = 599
#>   University: mean = 2.377, sd = 1.149, n = 377
#> 
#> ANOVA Results:
#> -------------------------------------------------------------------------------- 
#>          Source Sum_Squares   df Mean_Square     F p_value sig
#>  Between Groups       3.662    3       1.221 0.902   0.439    
#>   Within Groups    3198.645 2363       1.354                  
#>           Total    3202.308 2366                              
#> -------------------------------------------------------------------------------- 
#> 
#> Assumption Tests:
#> ---------------- 
#>  Assumption Statistic df1  df2 p_value sig
#>       Welch     0.901   3 1162    0.44    
#> 
#> Effect Sizes:
#> ------------ 
#>     Variable Eta_Squared Epsilon_Squared Omega_Squared Effect_Size
#>  trust_media       0.001               0             0  negligible
#> 
#> 
#> --- trust_science ---
#> 
#> Descriptive Statistics by Group:
#>   Basic Secondary: mean = 3.638, sd = 1.051, n = 807
#>   Intermediate Secondary: mean = 3.610, sd = 1.005, n = 610
#>   Academic Secondary: mean = 3.687, sd = 1.040, n = 597
#>   University: mean = 3.628, sd = 0.996, n = 384
#> 
#> ANOVA Results:
#> -------------------------------------------------------------------------------- 
#>          Source Sum_Squares   df Mean_Square     F p_value sig
#>  Between Groups       1.918    3       0.639 0.605   0.612    
#>   Within Groups    2529.658 2394       1.057                  
#>           Total    2531.576 2397                              
#> -------------------------------------------------------------------------------- 
#> 
#> Assumption Tests:
#> ---------------- 
#>  Assumption Statistic df1  df2 p_value sig
#>       Welch     0.605   3 1186   0.612    
#> 
#> Effect Sizes:
#> ------------ 
#>       Variable Eta_Squared Epsilon_Squared Omega_Squared Effect_Size
#>  trust_science       0.001               0             0  negligible
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
#> Post-hoc tests: Use tukey_test() for pairwise comparisons on each variable

# Weighted analysis
survey_data %>%
  oneway_anova(income, group = education, weights = sampling_weight)
#> 
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

# Grouped analysis (separate ANOVA for each region)
survey_data %>%
  group_by(region) %>%
  oneway_anova(life_satisfaction, group = education)
#> 
#> ── One-Way ANOVA Results ───────────────────────────────────────────────────────
#> 
#> Grouping Variable: education
#> Null hypothesis: All group means are equal
#> Alternative hypothesis: At least one group mean differs
#> Confidence level: 95.0%
#> 
#> 
#> 
#> ── Group: region = East ──
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Descriptive Statistics by Group:
#>   Basic Secondary: mean = 3.304, sd = 1.337, n = 161
#>   Intermediate Secondary: mean = 3.639, sd = 1.140, n = 119
#>   Academic Secondary: mean = 3.836, sd = 1.088, n = 110
#>   University: mean = 3.947, sd = 1.025, n = 75
#> 
#> ANOVA Results:
#> -------------------------------------------------------------------------------- 
#>          Source Sum_Squares  df Mean_Square    F p_value sig
#>  Between Groups      29.235   3       9.745 6.95   <.001 ***
#>   Within Groups     646.390 461       1.402                 
#>           Total     675.626 464                             
#> -------------------------------------------------------------------------------- 
#> 
#> Assumption Tests:
#> ---------------- 
#>  Assumption Statistic df1 df2 p_value sig
#>       Welch     6.682   3 233   <.001 ***
#> 
#> Effect Sizes:
#> ------------ 
#>           Variable Eta_Squared Epsilon_Squared Omega_Squared Effect_Size
#>  life_satisfaction       0.043           0.037         0.037       small
#> 
#> 
#> 
#> ── Group: region = West ──
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Descriptive Statistics by Group:
#>   Basic Secondary: mean = 3.179, sd = 1.219, n = 648
#>   Intermediate Secondary: mean = 3.715, sd = 1.106, n = 499
#>   Academic Secondary: mean = 3.857, sd = 0.978, n = 497
#>   University: mean = 4.071, sd = 0.939, n = 312
#> 
#> ANOVA Results:
#> -------------------------------------------------------------------------------- 
#>          Source Sum_Squares   df Mean_Square      F p_value sig
#>  Between Groups     221.625    3      73.875 62.153   <.001 ***
#>   Within Groups    2320.132 1952       1.189                   
#>           Total    2541.756 1955                               
#> -------------------------------------------------------------------------------- 
#> 
#> Assumption Tests:
#> ---------------- 
#>  Assumption Statistic df1 df2 p_value sig
#>       Welch    59.852   3 993   <.001 ***
#> 
#> Effect Sizes:
#> ------------ 
#>           Variable Eta_Squared Epsilon_Squared Omega_Squared Effect_Size
#>  life_satisfaction       0.087           0.086         0.086      medium
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
#> Post-hoc tests: Use tukey_test() for pairwise comparisons on each variable within each group

# Unequal variances (Welch's ANOVA)
survey_data %>%
  oneway_anova(income, group = education, var.equal = FALSE)
#> 
#> ── One-Way ANOVA Results ───────────────────────────────────────────────────────
#> 
#> Dependent Variable: income
#> Grouping Variable: education
#> Null hypothesis: All group means are equal
#> Alternative hypothesis: At least one group mean differs
#> Confidence level: 95.0%
#> 
#> 
#> --- income ---
#> 
#> Descriptive Statistics by Group:
#>   Basic Secondary: mean = 2759.048, sd = 786.568, n = 735
#>   Intermediate Secondary: mean = 3592.518, sd = 995.639, n = 548
#>   Academic Secondary: mean = 4224.088, sd = 1178.635, n = 548
#>   University: mean = 5337.183, sd = 1660.958, n = 355
#> 
#> ANOVA Results:
#> -------------------------------------------------------------------------------- 
#>          Source Sum_Squares   df   Mean_Square       F p_value sig
#>  Between Groups  1752783281    3 584261093.823 466.494   <.001 ***
#>   Within Groups  2732847885 2182    1252450.91                    
#>           Total  4485631167 2185                                  
#> -------------------------------------------------------------------------------- 
#> 
#> Assumption Tests:
#> ---------------- 
#>  Assumption Statistic df1 df2 p_value sig
#>       Welch    418.25   3 978   <.001 ***
#> 
#> Effect Sizes:
#> ------------ 
#>  Variable Eta_Squared Epsilon_Squared Omega_Squared Effect_Size
#>    income       0.391            0.39          0.39       large
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

# Store results for post-hoc analysis
result <- survey_data %>%
  oneway_anova(life_satisfaction, group = education)

# Follow up with post-hoc tests
result %>% tukey_test()
#> ── Tukey HSD Post-Hoc Test Results ─────────────────────────────────────────────
#> 
#> Dependent Variable: life_satisfaction
#> Grouping Variable: education
#> Confidence level: 95.0%
#> Family-wise error rate controlled using Tukey HSD
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Tukey Results:
#> ---------------------------------------------------------------------------------- 
#>                                 Comparison Difference Lower CI Upper CI p-value
#>     Intermediate Secondary-Basic Secondary      0.497    0.344    0.649   <.001
#>         Academic Secondary-Basic Secondary      0.649    0.496    0.802   <.001
#>                 University-Basic Secondary      0.843    0.666    1.019   <.001
#>  Academic Secondary-Intermediate Secondary      0.153   -0.010    0.316   0.075
#>          University-Intermediate Secondary      0.346    0.161    0.531   <.001
#>              University-Academic Secondary      0.193    0.008    0.379   0.037
#>  Sig
#>  ***
#>  ***
#>  ***
#>     
#>  ***
#>    *
#> ---------------------------------------------------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - Positive differences: First group > Second group
#> - Negative differences: First group < Second group
#> - Confidence intervals not containing 0 indicate significant differences
#> - p-values are adjusted for multiple comparisons (family-wise error control)
result %>% levene_test()  # Check homogeneity of variances
#> 
#> ── Levene's Test for Homogeneity of Variance  ──────────────────────────────────
#> 
#> Grouping variable: education
#> Center: mean
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Levene's Test Results:
#> -------------------------------------------------------------------- 
#>           Variable F_statistic df1  df2 p_value sig        Conclusion
#>  life_satisfaction      31.634   3 2417       0 *** Variances unequal
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

# Note: For repeated measures ANOVA, use rm_anova_test() function instead
```
