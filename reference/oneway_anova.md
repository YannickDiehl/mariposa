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

  Deprecated and ignored (a warning is issued when set to FALSE). Like
  SPSS ONEWAY, the classical ANOVA table and Welch's robust test are
  always both computed and displayed.

- conf.level:

  Confidence level for intervals (Default: 0.95 = 95%)

## Value

ANOVA results showing whether groups differ, including:

- F-statistic (`F_statistic`) and p-value (are groups different?)

- Effect sizes (how much do groups matter?)

- Group statistics (means and standard deviations)

- Both standard and Welch ANOVA results Use
  [`summary()`](https://rdrr.io/r/base/summary.html) for the full
  SPSS-style output with toggleable sections.

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

- Omega-squared and epsilon-squared are truncated at 0; a negative raw
  estimate (which occurs when F \< 1) is reported as 0.

### When to Use This

Use ANOVA when:

- You have one numeric outcome (income, satisfaction score, etc.)

- You have one categorical grouping variable with 3+ groups

- You want to know if group averages differ

- Your data is roughly normally distributed within groups

### Variance Assumptions

Like SPSS ONEWAY, both results are always shown for comparison:

- **Standard ANOVA**: valid when group variances are similar

- **Welch's ANOVA**: robust when group variances differ

- Use
  [`levene_test`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md)
  to check which situation applies

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

[`summary.oneway_anova`](https://YannickDiehl.github.io/mariposa/reference/summary.oneway_anova.md)
for detailed output with toggleable sections.

Other hypothesis_tests:
[`ancova()`](https://YannickDiehl.github.io/mariposa/reference/ancova.md),
[`binomial_test()`](https://YannickDiehl.github.io/mariposa/reference/binomial_test.md),
[`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md),
[`chisq_gof()`](https://YannickDiehl.github.io/mariposa/reference/chisq_gof.md),
[`factorial_anova()`](https://YannickDiehl.github.io/mariposa/reference/factorial_anova.md),
[`fisher_test()`](https://YannickDiehl.github.io/mariposa/reference/fisher_test.md),
[`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md),
[`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md),
[`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md),
[`mcnemar_test()`](https://YannickDiehl.github.io/mariposa/reference/mcnemar_test.md),
[`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md),
[`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/reference/wilcoxon_test.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic one-way ANOVA (comparing across education levels)
survey_data %>%
  oneway_anova(life_satisfaction, group = education)
#> One-Way ANOVA: life_satisfaction by education
#>   F(3, 2417) = 67.096, p < 0.001 ***, eta2 = 0.077 (medium), N = 2421

# Multiple dependent variables
survey_data %>%
  oneway_anova(life_satisfaction, trust_government, group = education)
#> One-Way ANOVA: life_satisfaction by education
#>   F(3, 2417) = 67.096, p < 0.001 ***, eta2 = 0.077 (medium), N = 2421
#> One-Way ANOVA: trust_government by education
#>   F(3, 2350) = 0.431, p = 0.731 , eta2 = 0.001 (negligible), N = 2354

# Using tidyselect helpers
survey_data %>%
  oneway_anova(starts_with("trust_"), group = education)
#> One-Way ANOVA: trust_government by education
#>   F(3, 2350) = 0.431, p = 0.731 , eta2 = 0.001 (negligible), N = 2354
#> One-Way ANOVA: trust_media by education
#>   F(3, 2363) = 0.902, p = 0.439 , eta2 = 0.001 (negligible), N = 2367
#> One-Way ANOVA: trust_science by education
#>   F(3, 2394) = 0.605, p = 0.612 , eta2 = 0.001 (negligible), N = 2398

# Weighted analysis
survey_data %>%
  oneway_anova(income, group = education, weights = sampling_weight)
#> One-Way ANOVA: income by education [Weighted]
#>   F(3, 2196) = 462.115, p < 0.001 ***, eta2 = 0.387 (large), N = 2200

# Grouped analysis (separate ANOVA for each region)
survey_data %>%
  group_by(region) %>%
  oneway_anova(life_satisfaction, group = education)
#> [region = 1]
#> One-Way ANOVA: life_satisfaction by education
#>   F(3, 461) = 6.950, p < 0.001 ***, eta2 = 0.043 (small), N = 465
#> [region = 2]
#> One-Way ANOVA: life_satisfaction by education
#>   F(3, 1952) = 62.153, p < 0.001 ***, eta2 = 0.087 (medium), N = 1956

# Store results for post-hoc analysis
result <- survey_data %>%
  oneway_anova(life_satisfaction, group = education)

# Follow up with post-hoc tests
result %>% tukey_test()
#> Tukey HSD Post-Hoc Test by education
#>   life_satisfaction: 6 comparisons, 5 significant (p < .05)
#> Use summary() for the full comparison table.
result %>% levene_test()  # Check homogeneity of variances
#> Levene's Test: life_satisfaction by education
#>   F(3, 2417) = 31.634, p < 0.001 ***, variances unequal
#> Use summary() for detailed output.

# --- Three-layer output ---
result              # compact one-line overview
#> One-Way ANOVA: life_satisfaction by education
#>   F(3, 2417) = 67.096, p < 0.001 ***, eta2 = 0.077 (medium), N = 2421
summary(result)     # full detailed output with all sections
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
summary(result, descriptives = FALSE)  # hide group statistics
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
