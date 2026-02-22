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

  Confidence level for effect size (Default: 0.95 = 95%)

## Value

Test results showing whether groups differ, including:

- U and W statistics (test statistics)

- Z-score and p-value (are groups different?)

- Effect size r (how big is the difference?)

- Rank means for each group (which group is higher?)

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

### When to Use This Test

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

## See also

[`wilcox.test`](https://rdrr.io/r/stats/wilcox.test.html) for the base R
Wilcoxon test function.

`svyranktest` for survey-weighted rank tests.

[`t_test`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)
for parametric t-tests.

Other hypothesis_tests:
[`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md),
[`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md),
[`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic Mann-Whitney test (non-parametric comparison)
survey_data %>%
  mann_whitney(age, group = gender)
#> 
#> ── Mann-Whitney U Test Results ─────────────────────────────────────────────────
#> 
#> Grouping variable: gender
#> Groups compared: Male vs. Female
#> Null hypothesis (mu): 0.000
#> Alternative hypothesis: two.sided
#> Confidence level: 95.0%
#> 
#> 
#> --- age ---
#> 
#>   Male: rank mean = 1248.0, n = 1194.0
#>   Female: rank mean = 1252.8, n = 1306.0
#> 
#> 
#> Mann-Whitney U Test Results:
#> ---------------------------------------------------------------------- 
#>            Test       U         W      Z p_value effect_r sig
#>  Mann-Whitney U 776,732 1,490,147 -0.164    0.87    0.003    
#> ---------------------------------------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (r):
#> - Small effect: |r| ≈ 0.1
#> - Medium effect: |r| ≈ 0.3
#> - Large effect: |r| ≈ 0.5

# Multiple variables
survey_data %>%
  mann_whitney(age, income, life_satisfaction, group = region)
#> 
#> ── Mann-Whitney U Test Results ─────────────────────────────────────────────────
#> 
#> Grouping variable: region
#> Groups compared: East vs. West
#> Null hypothesis (mu): 0.000
#> Alternative hypothesis: two.sided
#> Confidence level: 95.0%
#> 
#> 
#> --- age ---
#> 
#>   East: rank mean = 1304.9, n = 485.0
#>   West: rank mean = 1237.4, n = 2015.0
#> 
#> 
#> Mann-Whitney U Test Results:
#> ---------------------------------------------------------------------- 
#>            Test       U         W     Z p_value effect_r sig
#>  Mann-Whitney U 462,234 2,493,354 -1.85   0.064    0.037    
#> ---------------------------------------------------------------------- 
#> 
#> 
#> --- income ---
#> 
#>   East: rank mean = 1099.7, n = 429.0
#>   West: rank mean = 1092.0, n = 1757.0
#> 
#> 
#> Mann-Whitney U Test Results:
#> ---------------------------------------------------------------------- 
#>            Test       U         W      Z p_value effect_r sig
#>  Mann-Whitney U 374,226 1,918,630 -0.226   0.821    0.005    
#> ---------------------------------------------------------------------- 
#> 
#> 
#> --- life_satisfaction ---
#> 
#>   East: rank mean = 1215.7, n = 465.0
#>   West: rank mean = 1209.9, n = 1956.0
#> 
#> 
#> Mann-Whitney U Test Results:
#> ---------------------------------------------------------------------- 
#>            Test       U         W      Z p_value effect_r sig
#>  Mann-Whitney U 452,568 2,366,514 -0.168   0.867    0.003    
#> ---------------------------------------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (r):
#> - Small effect: |r| ≈ 0.1
#> - Medium effect: |r| ≈ 0.3
#> - Large effect: |r| ≈ 0.5

# Using tidyselect helpers
survey_data %>%
  mann_whitney(starts_with("trust_"), group = gender)
#> 
#> ── Mann-Whitney U Test Results ─────────────────────────────────────────────────
#> 
#> Grouping variable: gender
#> Groups compared: Male vs. Female
#> Null hypothesis (mu): 0.000
#> Alternative hypothesis: two.sided
#> Confidence level: 95.0%
#> 
#> 
#> --- trust_government ---
#> 
#>   Male: rank mean = 1166.5, n = 1123.0
#>   Female: rank mean = 1187.6, n = 1231.0
#> 
#> 
#> Mann-Whitney U Test Results:
#> ---------------------------------------------------------------------- 
#>            Test       U         W      Z p_value effect_r sig
#>  Mann-Whitney U 678,806 1,309,932 -0.775   0.438    0.016    
#> ---------------------------------------------------------------------- 
#> 
#> 
#> --- trust_media ---
#> 
#>   Male: rank mean = 1150.9, n = 1129.0
#>   Female: rank mean = 1214.2, n = 1238.0
#> 
#> 
#> Mann-Whitney U Test Results:
#> ---------------------------------------------------------------------- 
#>            Test       U         W     Z p_value effect_r sig
#>  Mann-Whitney U 661,523 1,299,408 -2.32    0.02    0.048   *
#> ---------------------------------------------------------------------- 
#> 
#> 
#> --- trust_science ---
#> 
#>   Male: rank mean = 1182.1, n = 1142.0
#>   Female: rank mean = 1215.3, n = 1256.0
#> 
#> 
#> Mann-Whitney U Test Results:
#> ---------------------------------------------------------------------- 
#>            Test       U         W      Z p_value effect_r sig
#>  Mann-Whitney U 697,309 1,349,962 -1.234   0.217    0.025    
#> ---------------------------------------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (r):
#> - Small effect: |r| ≈ 0.1
#> - Medium effect: |r| ≈ 0.3
#> - Large effect: |r| ≈ 0.5

# Weighted analysis
survey_data %>%
  mann_whitney(income, group = region, weights = sampling_weight)
#> 
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
#> --- income ---
#> 
#>   East: rank mean = 1096.1, n = 449.5
#>   West: rank mean = 1101.6, n = 1751.4
#> 
#> 
#> Weighted Mann-Whitney U Test Results:
#> ---------------------------------------------------------------------- 
#>            Test       U         W      Z p_value effect_r sig
#>  Mann-Whitney U 391,442 1,929,377 -0.406   0.821    0.009    
#> ---------------------------------------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (r):
#> - Small effect: |r| ≈ 0.1
#> - Medium effect: |r| ≈ 0.3
#> - Large effect: |r| ≈ 0.5

# Grouped analysis (separate tests for each education level)
survey_data %>%
  group_by(education) %>%
  mann_whitney(life_satisfaction, group = gender)
#> 
#> ── Mann-Whitney U Test Results ─────────────────────────────────────────────────
#> 
#> Grouping variable: gender
#> Groups compared: Male vs. Female
#> Null hypothesis (mu): 0.000
#> Alternative hypothesis: two.sided
#> Confidence level: 95.0%
#> 
#> 
#> Group: education = Basic Secondary
#> 
#> --- life_satisfaction ---
#> 
#>   Male: rank mean = 404.7, n = 382.0
#>   Female: rank mean = 405.3, n = 427.0
#> 
#> Mann-Whitney U Test Results:
#> ---------------------------------------------------------------------- 
#>            Test      U       W      Z p_value effect_r sig
#>  Mann-Whitney U 81,439 154,592 -0.036   0.971    0.001    
#> ---------------------------------------------------------------------- 
#> 
#> 
#> Group: education = Intermediate Secondary
#> 
#> --- life_satisfaction ---
#> 
#>   Male: rank mean = 301.9, n = 281.0
#>   Female: rank mean = 315.8, n = 337.0
#> 
#> Mann-Whitney U Test Results:
#> ---------------------------------------------------------------------- 
#>            Test      U      W      Z p_value effect_r sig
#>  Mann-Whitney U 45,218 84,839 -1.001   0.317     0.04    
#> ---------------------------------------------------------------------- 
#> 
#> 
#> Group: education = Academic Secondary
#> 
#> --- life_satisfaction ---
#> 
#>   Male: rank mean = 304.9, n = 305.0
#>   Female: rank mean = 303.1, n = 302.0
#> 
#> Mann-Whitney U Test Results:
#> ---------------------------------------------------------------------- 
#>            Test      U      W     Z p_value effect_r sig
#>  Mann-Whitney U 45,788 91,540 -0.13   0.897    0.005    
#> ---------------------------------------------------------------------- 
#> 
#> 
#> Group: education = University
#> 
#> --- life_satisfaction ---
#> 
#>   Male: rank mean = 186.7, n = 181.0
#>   Female: rank mean = 200.4, n = 206.0
#> 
#> Mann-Whitney U Test Results:
#> ---------------------------------------------------------------------- 
#>            Test      U      W      Z p_value effect_r sig
#>  Mann-Whitney U 17,319 33,790 -1.281     0.2    0.065    
#> ---------------------------------------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (r):
#> - Small effect: |r| ≈ 0.1
#> - Medium effect: |r| ≈ 0.3
#> - Large effect: |r| ≈ 0.5

# One-sided test
survey_data %>%
  mann_whitney(life_satisfaction, group = region, alternative = "greater")
#> 
#> ── Mann-Whitney U Test Results ─────────────────────────────────────────────────
#> 
#> Grouping variable: region
#> Groups compared: East vs. West
#> Null hypothesis (mu): 0.000
#> Alternative hypothesis: greater
#> Confidence level: 95.0%
#> 
#> 
#> --- life_satisfaction ---
#> 
#>   East: rank mean = 1215.7, n = 465.0
#>   West: rank mean = 1209.9, n = 1956.0
#> 
#> 
#> Mann-Whitney U Test Results:
#> ---------------------------------------------------------------------- 
#>            Test       U         W      Z p_value effect_r sig
#>  Mann-Whitney U 452,568 2,366,514 -0.168   0.433    0.003    
#> ---------------------------------------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (r):
#> - Small effect: |r| ≈ 0.1
#> - Medium effect: |r| ≈ 0.3
#> - Large effect: |r| ≈ 0.5

# Store results for further analysis
result <- survey_data %>%
  mann_whitney(income, group = gender, weights = sampling_weight)
print(result)
#> 
#> ── Weighted Mann-Whitney U Test Results ────────────────────────────────────────
#> 
#> Grouping variable: gender
#> Groups compared: Male vs. Female
#> Weights variable: sampling_weight
#> Null hypothesis (mu): 0.000
#> Alternative hypothesis: two.sided
#> Confidence level: 95.0%
#> 
#> 
#> --- income ---
#> 
#>   Male: rank mean = 1099.0, n = 1047.9
#>   Female: rank mean = 1101.8, n = 1153.1
#> 
#> 
#> Weighted Mann-Whitney U Test Results:
#> ---------------------------------------------------------------------- 
#>            Test       U         W      Z p_value effect_r sig
#>  Mann-Whitney U 602,040 1,270,474 -0.211   0.486    0.005    
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
