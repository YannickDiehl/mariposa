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

  Confidence level for intervals (Default: 0.95 = 95%)

## Value

Test results showing whether groups differ, including:

- t-statistic and p-value for statistical significance

- Mean difference and confidence interval

- Effect sizes (Cohen's d, Hedges' g, Glass' Delta)

- Group statistics (mean, SD, sample size) Use
  [`summary()`](https://rdrr.io/r/base/summary.html) for the full
  SPSS-style output with toggleable sections.

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

[`summary.t_test`](https://YannickDiehl.github.io/mariposa/reference/summary.t_test.md)
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
[`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md),
[`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/reference/wilcoxon_test.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic two-sample test
survey_data %>%
  t_test(life_satisfaction, group = gender)
#> t-Test: life_satisfaction by gender
#>   t(2384.1) = -1.018, p = 0.309 , g = -0.041 (negligible), N = 2421

# With survey weights
survey_data %>%
  t_test(life_satisfaction, group = gender, weights = sampling_weight)
#> t-Test: life_satisfaction by gender [Weighted]
#>   t(2390.8) = -1.069, p = 0.285 , g = -0.043 (negligible), N = 2436

# Multiple variables
survey_data %>%
  t_test(age, income, life_satisfaction, group = region, weights = sampling_weight)
#> t-Test: age by region [Weighted]
#>   t(763.8) = 2.551, p = 0.011 *, g = 0.130 (negligible), N = 2516
#> t-Test: income by region [Weighted]
#>   t(712.5) = 0.299, p = 0.765 , g = 0.016 (negligible), N = 2200
#> t-Test: life_satisfaction by region [Weighted]
#>   t(721.0) = -0.031, p = 0.975 , g = -0.002 (negligible), N = 2437

# One-sample test against a benchmark
survey_data %>%
  t_test(life_satisfaction, mu = 5, weights = sampling_weight)
#> t-Test: life_satisfaction [Weighted]
#>   t(2436.0) = -58.925, p < 0.001 ***

# Grouped analysis
survey_data %>%
  group_by(region) %>%
  t_test(life_satisfaction, group = gender, weights = sampling_weight)
#> [region = 1]
#> t-Test: life_satisfaction by gender [Weighted]
#>   t(484.9) = 0.641, p = 0.522 , g = 0.058 (negligible), N = 488
#> [region = 2]
#> t-Test: life_satisfaction by gender [Weighted]
#>   t(1902.0) = -1.548, p = 0.122 , g = -0.070 (negligible), N = 1949

# Equal variance assumption
survey_data %>%
  t_test(life_satisfaction, group = gender, var.equal = TRUE)
#> t-Test: life_satisfaction by gender
#>   t(2419.0) = -1.019, p = 0.308 , g = -0.041 (negligible), N = 2421

# One-sided test
survey_data %>%
  t_test(income, group = gender, alternative = "greater")
#> t-Test: income by gender
#>   t(2169.3) = 0.690, p = 0.245 , g = 0.030 (negligible), N = 2186

# Using tidyselect helpers
survey_data %>%
  t_test(starts_with("trust"), group = gender, weights = sampling_weight)
#> t-Test: trust_government by gender [Weighted]
#>   t(2322.7) = -0.682, p = 0.495 , g = -0.028 (negligible), N = 2371
#> t-Test: trust_media by gender [Weighted]
#>   t(2350.2) = -2.196, p = 0.028 *, g = -0.090 (negligible), N = 2382
#> t-Test: trust_science by gender [Weighted]
#>   t(2360.9) = -1.421, p = 0.156 , g = -0.058 (negligible), N = 2414

# Store results for further analysis
result <- survey_data %>%
  t_test(life_satisfaction, group = gender, weights = sampling_weight)
print(result)
#> t-Test: life_satisfaction by gender [Weighted]
#>   t(2390.8) = -1.069, p = 0.285 , g = -0.043 (negligible), N = 2436

# --- Three-layer output ---
result <- t_test(survey_data, life_satisfaction, group = gender)
result              # compact one-line summary
#> t-Test: life_satisfaction by gender
#>   t(2384.1) = -1.018, p = 0.309 , g = -0.041 (negligible), N = 2421
summary(result)     # full detailed output with all sections
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
summary(result, effect_sizes = FALSE)  # hide effect sizes
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
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
