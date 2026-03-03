# Compare Two Groups Without Assuming Normal Data

`mann_whitney()` compares two groups when your data isn't normally
distributed or when you have ordinal data (like ratings or rankings).
It's the go-to alternative when t-tests aren't appropriate.

Think of it as:

- A robust way to compare groups that works with any data shape

- Perfect for Likert scales, ratings, or skewed distributions

- A test that compares the typical values between groups

The test tells you:

- Whether one group tends to have higher values than the other

- How strong the difference is (effect size)

- Which group has higher average ranks

## Usage

``` r
mann_whitney(
  data,
  ...,
  group,
  weights = NULL,
  mu = 0,
  alternative = c("two.sided", "less", "greater"),
  conf.level = 0.95
)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- ...:

  The variables you want to compare between groups. You can list
  multiple variables or use helpers like `starts_with("satisfaction")`

- group:

  The categorical variable that defines your two groups (e.g., gender,
  treatment/control). Must have exactly two groups.

- weights:

  Optional survey weights for population-representative results

- mu:

  The hypothesized difference (Default: 0, meaning no difference)

- alternative:

  Direction of the test:

  - `"two.sided"` (default): Test if groups are different

  - `"greater"`: Test if group 1 \> group 2

  - `"less"`: Test if group 1 \< group 2

- conf.level:

  Confidence level for intervals (Default: 0.95 = 95%)

## Value

Test results showing whether groups differ, including:

- U and W statistics (test statistics)

- Z-score and p-value (are groups different?)

- Effect size r (how big is the difference?)

- Rank means for each group (which group is higher?) Use
  [`summary()`](https://rdrr.io/r/base/summary.html) for the full
  SPSS-style output with toggleable sections.

## Details

### Understanding the Results

**P-value**: If p \< 0.05, the groups are significantly different

- p \< 0.001: Very strong evidence of difference

- p \< 0.01: Strong evidence of difference

- p \< 0.05: Moderate evidence of difference

- p ≥ 0.05: No significant difference found

**Effect Size r** (How big is the difference?):

- \|r\| \< 0.1: Negligible difference

- \|r\| ~ 0.1: Small difference

- \|r\| ~ 0.3: Medium difference

- \|r\| ~ 0.5: Large difference

- \|r\| \> 0.5: Very large difference

**Rank Mean Difference**:

- Positive: Group 1 tends to have higher values

- Negative: Group 2 tends to have higher values

- Zero: Groups have similar distributions

### When to Use This

Use Mann-Whitney test when:

- Your data is not normally distributed (skewed, outliers)

- You have ordinal data (rankings, Likert scales)

- Sample sizes are small (\< 30 per group)

- You want a robust alternative to the t-test

- You're comparing satisfaction ratings, income, or other skewed
  variables

### Advantages Over t-test

- Works with any data distribution

- Not affected by outliers

- Valid for ordinal data

- No assumptions about variance

- More robust for real-world data

### Tips for Success

- Each group should have at least 5 observations

- The test compares distributions, not just means

- Look at both p-values and effect sizes

- Consider plotting the data to see the pattern

- Use this for Likert scales and rating data

## References

Mann, H. B., & Whitney, D. R. (1947). On a test of whether one of two
random variables is stochastically larger than the other. The Annals of
Mathematical Statistics, 18(1), 50-60.

Wilcoxon, F. (1945). Individual comparisons by ranking methods.
Biometrics Bulletin, 1(6), 80-83.

Lumley, T., & Scott, A. (2013). Two-sample rank tests under complex
sampling. Biometrika, 100(4), 831-842.

## See also

[`wilcox.test`](https://rdrr.io/r/stats/wilcox.test.html) for the base R
Wilcoxon test function.

[`svyranktest`](https://rdrr.io/pkg/survey/man/svyranktest.html) for
survey-weighted rank tests.

[`t_test`](https://YannickDiehl.github.io/mariposa/dev/reference/t_test.md)
for parametric t-tests.

[`summary.mann_whitney`](https://YannickDiehl.github.io/mariposa/dev/reference/summary.mann_whitney.md)
for detailed output with toggleable sections.

Other hypothesis_tests:
[`ancova()`](https://YannickDiehl.github.io/mariposa/dev/reference/ancova.md),
[`binomial_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/binomial_test.md),
[`chi_square()`](https://YannickDiehl.github.io/mariposa/dev/reference/chi_square.md),
[`chisq_gof()`](https://YannickDiehl.github.io/mariposa/dev/reference/chisq_gof.md),
[`factorial_anova()`](https://YannickDiehl.github.io/mariposa/dev/reference/factorial_anova.md),
[`fisher_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/fisher_test.md),
[`friedman_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/friedman_test.md),
[`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/dev/reference/kruskal_wallis.md),
[`mcnemar_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/mcnemar_test.md),
[`oneway_anova()`](https://YannickDiehl.github.io/mariposa/dev/reference/oneway_anova.md),
[`t_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/t_test.md),
[`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/wilcoxon_test.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic Mann-Whitney test (non-parametric comparison)
survey_data %>%
  mann_whitney(age, group = gender)
#> Mann-Whitney U Test: age by gender
#>   U = 776,732, Z = -0.164, p = 0.870 , r = 0.003 (negligible), N = 2500

# Multiple variables
survey_data %>%
  mann_whitney(age, income, life_satisfaction, group = region)
#> Mann-Whitney U Test: age by region
#>   U = 462,234, Z = -1.850, p = 0.064 , r = 0.037 (negligible), N = 2500
#> Mann-Whitney U Test: income by region
#>   U = 374,226, Z = -0.226, p = 0.821 , r = 0.005 (negligible), N = 2186
#> Mann-Whitney U Test: life_satisfaction by region
#>   U = 452,568, Z = -0.168, p = 0.867 , r = 0.003 (negligible), N = 2421

# Using tidyselect helpers
survey_data %>%
  mann_whitney(starts_with("trust_"), group = gender)
#> Mann-Whitney U Test: trust_government by gender
#>   U = 678,806, Z = -0.775, p = 0.438 , r = 0.016 (negligible), N = 2354
#> Mann-Whitney U Test: trust_media by gender
#>   U = 661,523, Z = -2.320, p = 0.020 *, r = 0.048 (negligible), N = 2367
#> Mann-Whitney U Test: trust_science by gender
#>   U = 697,309, Z = -1.234, p = 0.217 , r = 0.025 (negligible), N = 2398

# Weighted analysis
survey_data %>%
  mann_whitney(income, group = region, weights = sampling_weight)
#> Mann-Whitney U Test: income by region [Weighted]
#>   U = 386,321, Z = -0.536, p = 0.592 , r = 0.011 (negligible), N = 2201

# Grouped analysis (separate tests for each education level)
survey_data %>%
  group_by(education) %>%
  mann_whitney(life_satisfaction, group = gender)
#> [education = 1]
#> Mann-Whitney U Test: life_satisfaction by gender
#>   U = 81,439, Z = -0.036, p = 0.971 , r = 0.001 (negligible), N = 809
#> [education = 2]
#> Mann-Whitney U Test: life_satisfaction by gender
#>   U = 45,218, Z = -1.001, p = 0.317 , r = 0.040 (negligible), N = 618
#> [education = 3]
#> Mann-Whitney U Test: life_satisfaction by gender
#>   U = 45,788, Z = -0.130, p = 0.897 , r = 0.005 (negligible), N = 607
#> [education = 4]
#> Mann-Whitney U Test: life_satisfaction by gender
#>   U = 17,319, Z = -1.281, p = 0.200 , r = 0.065 (negligible), N = 387

# One-sided test
survey_data %>%
  mann_whitney(life_satisfaction, group = region, alternative = "greater")
#> Mann-Whitney U Test: life_satisfaction by region
#>   U = 452,568, Z = -0.168, p = 0.433 , r = 0.003 (negligible), N = 2421

# --- Three-layer output ---
result <- survey_data %>%
  mann_whitney(income, group = gender, weights = sampling_weight)
result              # compact one-line overview
#> Mann-Whitney U Test: income by gender [Weighted]
#>   U = 592,273, Z = -0.755, p = 0.450 , r = 0.016 (negligible), N = 2201
summary(result)     # full detailed output with all sections
#> Weighted Mann-Whitney U Test Results
#> ------------------------------------
#> 
#> - Grouping variable: gender
#> - Groups compared: Male vs. Female
#> - Weights variable: sampling_weight
#> - Confidence level: 95.0%
#> - Alternative hypothesis: two.sided
#> - Null hypothesis (mu): 0.000
#> 
#> 
#> --- income ---
#> 
#>   Male: rank mean = 1111.3, n = 1047.9
#>   Female: rank mean = 1090.7, n = 1153.1
#> 
#> 
#> Weighted Mann-Whitney U Test Results:
#> ------------------------------------------------------------ 
#>            Test       U         W      Z p_value effect_r sig
#>  Mann-Whitney U 592,273 1,257,619 -0.755    0.45    0.016    
#> ------------------------------------------------------------ 
#> 
#> 
#> Note: Weighted analysis uses design-based rank test (Lumley & Scott, 2013).
#> U and W are descriptive statistics derived from weighted ranks.
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (r):
#> - Negligible effect: |r| < 0.1
#> - Small effect: |r| ~ 0.1
#> - Medium effect: |r| ~ 0.3
#> - Large effect: |r| ~ 0.5
summary(result, effect_sizes = FALSE)  # hide effect sizes
#> Weighted Mann-Whitney U Test Results
#> ------------------------------------
#> 
#> - Grouping variable: gender
#> - Groups compared: Male vs. Female
#> - Weights variable: sampling_weight
#> - Confidence level: 95.0%
#> - Alternative hypothesis: two.sided
#> - Null hypothesis (mu): 0.000
#> 
#> 
#> --- income ---
#> 
#>   Male: rank mean = 1111.3, n = 1047.9
#>   Female: rank mean = 1090.7, n = 1153.1
#> 
#> 
#> Weighted Mann-Whitney U Test Results:
#> ------------------------------------------------------------ 
#>            Test       U         W      Z p_value effect_r sig
#>  Mann-Whitney U 592,273 1,257,619 -0.755    0.45    0.016    
#> ------------------------------------------------------------ 
#> 
#> 
#> Note: Weighted analysis uses design-based rank test (Lumley & Scott, 2013).
#> U and W are descriptive statistics derived from weighted ranks.
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
