# Test If Two Groups Differ

`t_test()` helps you determine if two groups have different average
values. For example, do men and women have different satisfaction
scores? Does Region A spend more than Region B? Is this year better than
last year?

The test tells you:

- **Whether the difference is statistically significant**

- **How big the difference is** (effect sizes)

- **The likely range of the true difference** (confidence interval)

## Usage

``` r
t_test(
  data,
  ...,
  group = NULL,
  weights = NULL,
  var.equal = FALSE,
  mu = 0,
  alternative = c("two.sided", "less", "greater"),
  conf.level = 0.95
)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- ...:

  The numeric variables to test. List multiple variables or use
  tidyselect helpers like `starts_with("trust")`.

- group:

  The variable that defines your two groups (e.g., gender, region). Must
  have exactly two categories. Leave empty for one-sample tests.

- weights:

  Optional survey weights for population-representative results

- var.equal:

  Should we assume equal variances? (Default: FALSE)

  - `FALSE`: Safer, works even if groups vary differently (Welch's test)

  - `TRUE`: Traditional approach, assumes equal spread (Student's test)

- mu:

  Comparison value (Default: 0). For two-sample tests, usually 0. For
  one-sample tests, your benchmark value.

- alternative:

  Direction of the test:

  - `"two.sided"` (default): Any difference

  - `"greater"`: First group is higher

  - `"less"`: First group is lower

- conf.level:

  Confidence level (Default: 0.95 = 95%)

## Value

Test results showing whether groups differ, including:

- t-statistic and p-value for statistical significance

- Mean difference and confidence interval

- Effect sizes (Cohen's d, Hedges' g, Glass' Delta)

- Group statistics (mean, SD, sample size)

## Details

### Understanding the Results

**P-value**: Is the difference statistically significant?

- p \< 0.001: Very strong evidence of a difference

- p \< 0.01: Strong evidence of a difference

- p \< 0.05: Moderate evidence of a difference

- p ≥ 0.05: No significant difference found

**Effect Sizes** (How big is the difference?):

- **Cohen's d**: The standard measure

  - \|d\| \< 0.2: Negligible difference

  - \|d\| = 0.2-0.5: Small difference

  - \|d\| = 0.5-0.8: Medium difference

  - \|d\| \> 0.8: Large difference

- **Hedges' g**: Corrected for small samples

- **Glass' Delta**: Uses control group SD only

**Confidence Interval**: Range where the true difference likely falls

- If it includes 0, groups may not differ

- Width indicates precision (narrower = more precise)

### When to Use This

Use t-test when:

- You have exactly two groups to compare

- Your outcome variable is numeric (continuous)

- Groups are independent (different people in each)

- Data is roughly normally distributed (or n ≥ 30 per group)

Don't use when:

- You have more than two groups (use ANOVA)

- Data is severely skewed with small samples (use Mann-Whitney)

- Groups are paired/matched (use paired t-test - coming soon)

- Variables are categorical (use chi-square)

### Reading the Output

The results show both variance assumptions:

- **Welch's test** (var.equal = FALSE): Safer, doesn't assume equal
  variances

- **Student's test** (var.equal = TRUE): Traditional, assumes equal
  variances

A result with t = 2.45, p = 0.015, d = 0.62 means:

- Groups are 2.45 standard errors apart (t-statistic)

- 1.5% chance this is due to random variation (p-value)

- Medium-sized practical difference (Cohen's d)

### Tips for Success

- Always check sample sizes (aim for 30+ per group)

- Consider both statistical significance AND effect size

- Use weights for population-level conclusions

- Plot your data first to check assumptions

- Report confidence intervals along with p-values

## References

Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences
(2nd ed.). Lawrence Erlbaum Associates.

Hedges, L. V. (1981). Distribution theory for Glass's estimator of
effect size and related estimators. Journal of Educational Statistics,
6(2), 107-128.

Glass, G. V. (1976). Primary, secondary, and meta-analysis of research.
Educational Researcher, 5(10), 3-8.

Welch, B. L. (1947). The generalization of "Student's" problem when
several different population variances are involved. Biometrika,
34(1-2), 28-35.

## See also

[`t.test`](https://rdrr.io/r/stats/t.test.html) for the base R t-test
function.

[`print.t_test`](https://YannickDiehl.github.io/mariposa/reference/print.t_test.md)
for printing results.

[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html) for
grouped analyses.

Other hypothesis_tests:
[`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md),
[`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md),
[`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic two-sample test
survey_data %>%
  t_test(life_satisfaction, group = gender)
#> ── t-Test Results ──────────────────────────────────────────────────────────────
#> 
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

# With survey weights
survey_data %>%
  t_test(life_satisfaction, group = gender, weights = sampling_weight)
#> ── Weighted t-Test Results ─────────────────────────────────────────────────────
#> 
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

# Multiple variables
survey_data %>%
  t_test(age, income, life_satisfaction, group = region, weights = sampling_weight)
#> ── Weighted t-Test Results ─────────────────────────────────────────────────────
#> 
#> • Grouping variable: region
#> • Groups compared: East vs. West
#> • Weights variable: sampling_weight
#> • Confidence level: 95.0%
#> • Alternative hypothesis: two.sided
#> • Null hypothesis (mu): 0.000
#> 
#> 
#> --- age ---
#> 
#>   East: mean = 52.278, n = 509.0
#>   West: mean = 50.067, n = 2007.0
#> 
#> Weighted t-test Results:
#> -------------------------------------------------------------------------------- 
#>         Assumption t_stat      df p_value mean_diff       conf_int sig
#>    Equal variances  2.610 2514.00   0.009     2.211 [0.550, 3.871]  **
#>  Unequal variances  2.551  763.78   0.011     2.211 [0.509, 3.912]   *
#> -------------------------------------------------------------------------------- 
#> 
#> Effect Sizes:
#> ------------ 
#>  Variable Cohens_d Hedges_g Glass_Delta Effect_Size
#>       age     0.13     0.13       0.126  negligible
#> 
#> 
#> --- income ---
#> 
#>   East: mean = 3760.687, n = 449.0
#>   West: mean = 3738.586, n = 1751.0
#> 
#> Weighted t-test Results:
#> -------------------------------------------------------------------------------- 
#>         Assumption t_stat       df p_value mean_diff            conf_int sig
#>    Equal variances  0.293 2198.000   0.769    22.101 [-125.679, 169.881]    
#>  Unequal variances  0.299  712.493   0.765    22.101 [-123.118, 167.319]    
#> -------------------------------------------------------------------------------- 
#> 
#> Effect Sizes:
#> ------------ 
#>  Variable Cohens_d Hedges_g Glass_Delta Effect_Size
#>    income    0.016    0.016       0.016  negligible
#> 
#> 
#> --- life_satisfaction ---
#> 
#>   East: mean = 3.623, n = 488.0
#>   West: mean = 3.625, n = 1949.0
#> 
#> Weighted t-test Results:
#> -------------------------------------------------------------------------------- 
#>         Assumption t_stat       df p_value mean_diff        conf_int sig
#>    Equal variances -0.032 2435.000   0.974    -0.002 [-0.116, 0.113]    
#>  Unequal variances -0.031  720.956   0.975    -0.002 [-0.120, 0.116]    
#> -------------------------------------------------------------------------------- 
#> 
#> Effect Sizes:
#> ------------ 
#>           Variable Cohens_d Hedges_g Glass_Delta Effect_Size
#>  life_satisfaction   -0.002   -0.002      -0.002  negligible
#> 
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

# One-sample test against a benchmark
survey_data %>%
  t_test(life_satisfaction, mu = 5, weights = sampling_weight)
#> ── Weighted t-Test Results ─────────────────────────────────────────────────────
#> 
#> --- life_satisfaction ---
#> 
#> 
#> Weighted t-test Results:
#> ---------------------------------------------------------------------- 
#>  Assumption  t_stat   df p_value mean_diff       conf_int sig
#>      t-test -58.925 2436       0     3.625 [3.579, 3.671] ***
#> ---------------------------------------------------------------------- 
#> 
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

# Grouped analysis
survey_data %>%
  group_by(region) %>%
  t_test(life_satisfaction, group = gender, weights = sampling_weight)
#> ── Weighted t-Test Results ─────────────────────────────────────────────────────
#> 
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
#> --- life_satisfaction ---
#> 
#>   Male: mean = 3.659, n = 239.0
#>   Female: mean = 3.589, n = 249.0
#> 
#> Weighted t-test Results:
#> -------------------------------------------------------------------------------- 
#>         Assumption t_stat      df p_value mean_diff        conf_int sig
#>    Equal variances  0.641 486.000   0.522      0.07 [-0.144, 0.284]    
#>  Unequal variances  0.641 484.867   0.522      0.07 [-0.144, 0.284]    
#> -------------------------------------------------------------------------------- 
#> 
#> Effect Sizes:
#> ------------ 
#>           Variable Cohens_d Hedges_g Glass_Delta Effect_Size
#>  life_satisfaction    0.058    0.058       0.058  negligible
#> 
#> 
#> Group: region = West
#> 
#> --- life_satisfaction ---
#> 
#>   Male: mean = 3.583, n = 911.0
#>   Female: mean = 3.663, n = 1038.0
#> 
#> Weighted t-test Results:
#> -------------------------------------------------------------------------------- 
#>         Assumption t_stat      df p_value mean_diff        conf_int sig
#>    Equal variances -1.551 1947.00   0.121     -0.08 [-0.182, 0.021]    
#>  Unequal variances -1.548 1901.97   0.122     -0.08 [-0.182, 0.021]    
#> -------------------------------------------------------------------------------- 
#> 
#> Effect Sizes:
#> ------------ 
#>           Variable Cohens_d Hedges_g Glass_Delta Effect_Size
#>  life_satisfaction    -0.07    -0.07       -0.07  negligible
#> 
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

# Equal variance assumption
survey_data %>%
  t_test(life_satisfaction, group = gender, var.equal = TRUE)
#> ── t-Test Results ──────────────────────────────────────────────────────────────
#> 
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

# One-sided test
survey_data %>%
  t_test(income, group = gender, alternative = "greater")
#> ── t-Test Results ──────────────────────────────────────────────────────────────
#> 
#> • Grouping variable: gender
#> • Groups compared: Male vs. Female
#> • Confidence level: 95.0%
#> • Alternative hypothesis: greater
#> • Null hypothesis (mu): 0.000
#> 
#> 
#> --- income ---
#> 
#>   Male: mean = 3776.004, n = 1046.0
#>   Female: mean = 3733.684, n = 1140.0
#> 
#> t-test Results:
#> -------------------------------------------------------------------------------- 
#>         Assumption t_stat       df p_value mean_diff       conf_int sig
#>    Equal variances   0.69 2184.000   0.245     42.32 [-58.642, Inf]    
#>  Unequal variances   0.69 2169.337   0.245     42.32 [-58.625, Inf]    
#> -------------------------------------------------------------------------------- 
#> 
#> Effect Sizes:
#> ------------ 
#>  Variable Cohens_d Hedges_g Glass_Delta Effect_Size
#>    income     0.03     0.03        0.03  negligible
#> 
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

# Using tidyselect helpers
survey_data %>%
  t_test(starts_with("trust"), group = gender, weights = sampling_weight)
#> ── Weighted t-Test Results ─────────────────────────────────────────────────────
#> 
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
#> Signif. codes: 0 *** 0.001 ** 0.01 * 0.05
#> 
#> Effect Size Interpretation:
#> - Cohen's d: pooled standard deviation (classic)
#> - Hedges' g: bias-corrected Cohen's d (preferred)
#> - Glass' Delta: control group standard deviation only
#> - Small effect: |effect| ~ 0.2
#> - Medium effect: |effect| ~ 0.5
#> - Large effect: |effect| ~ 0.8

# Store results for further analysis
result <- survey_data %>%
  t_test(life_satisfaction, group = gender, weights = sampling_weight)
print(result)
#> ── Weighted t-Test Results ─────────────────────────────────────────────────────
#> 
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
