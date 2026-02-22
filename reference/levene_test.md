# Test If Groups Vary Similarly

`levene_test()` checks if different groups have similar amounts of
variation. This is an important assumption for many statistical tests -
groups should spread out in similar ways.

The test tells you:

- Whether variance is consistent across groups

- If you can trust standard ANOVA and t-test results

- When to use alternative tests that don't assume equal variance

## Usage

``` r
levene_test(x, ...)

# S3 method for class 'data.frame'
levene_test(x, ..., group, weights = NULL, center = c("mean", "median"))

# S3 method for class 'oneway_anova'
levene_test(x, center = c("mean", "median"), ...)

# S3 method for class 't_test'
levene_test(x, center = c("mean", "median"), ...)

# S3 method for class 'mann_whitney'
levene_test(x, ...)

# S3 method for class 'grouped_df'
levene_test(x, variable, group = NULL, weights = NULL, center = "mean", ...)
```

## Arguments

- x:

  Either your data or test results from
  [`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)
  or
  [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)

- ...:

  Variables to test (when using data frame)

- group:

  The grouping variable for comparison

- weights:

  Optional survey weights for population-representative results

- center:

  How to measure center: `"mean"` (default) or `"median"` (more robust)

- data:

  Your survey data (when x is not a test result)

## Value

Test results showing:

- Whether groups have equal variances (p-value)

- F-statistic measuring variance differences

- Which variables meet the assumption

## Details

### Understanding the Results

**P-value interpretation**:

- p \> 0.05: Good! Groups have similar variance (assumption met)

- p ≤ 0.05: Problem - groups vary differently (assumption violated)

Think of it like checking if all groups are equally "spread out":

- Similar spread = can use standard tests

- Different spread = need special methods

### When to Use This

Check variance equality when:

- Before running t-tests or ANOVA

- Comparing groups with different sizes

- Your statistical test assumes equal variances

- You see very different standard deviations

### What If Variances Are Unequal?

If Levene's test is significant (p ≤ 0.05):

- For t-tests: Use Welch's t-test (var.equal = FALSE)

- For ANOVA: Use Welch's ANOVA

- Consider transforming your data

- Use non-parametric alternatives

- Report that equal variance assumption was violated

### Usage Flexibility

You can use this function two ways:

- **Standalone**: Check any variables for equal variance

- **After tests**: Pipe after t_test() or oneway_anova() to verify
  assumptions

### Tips for Success

- Always check this assumption for group comparisons

- Visual inspection (boxplots) can supplement the test

- Large samples make the test very sensitive

- Use median-based test for skewed data (center = "median")

- Don't panic if violated - alternatives exist!

## See also

Other posthoc:
[`scheffe_test()`](https://YannickDiehl.github.io/mariposa/reference/scheffe_test.md),
[`tukey_test()`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Standalone Levene test (test homogeneity of variances)
survey_data %>% levene_test(life_satisfaction, group = region)
#> 
#> ── Levene's Test for Homogeneity of Variance  ──────────────────────────────────
#> 
#> Grouping variable: region
#> Center: mean
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Levene's Test Results:
#> ------------------------------------------------------------------ 
#>           Variable F_statistic df1  df2 p_value sig      Conclusion
#>  life_satisfaction       3.164   1 2419   0.075     Variances equal
#> ------------------------------------------------------------------ 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - p > 0.05: Variances are homogeneous (equal variances assumed)
#> - p <= 0.05: Variances are heterogeneous (equal variances NOT assumed)

# Multiple variables
survey_data %>% levene_test(life_satisfaction, trust_government, group = region)
#> 
#> ── Levene's Test for Homogeneity of Variance  ──────────────────────────────────
#> 
#> Grouping variable: region
#> Center: mean
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Levene's Test Results:
#> ------------------------------------------------------------------ 
#>           Variable F_statistic df1  df2 p_value sig      Conclusion
#>  life_satisfaction       3.164   1 2419   0.075     Variances equal
#> ------------------------------------------------------------------ 
#> 
#> 
#> --- trust_government ---
#> 
#> Levene's Test Results:
#> ----------------------------------------------------------------- 
#>          Variable F_statistic df1  df2 p_value sig      Conclusion
#>  trust_government       0.145   1 2352   0.703     Variances equal
#> ----------------------------------------------------------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - p > 0.05: Variances are homogeneous (equal variances assumed)
#> - p <= 0.05: Variances are heterogeneous (equal variances NOT assumed)

# Weighted analysis
survey_data %>% levene_test(income, group = education, weights = sampling_weight)
#> 
#> ── Weighted Levene's Test for Homogeneity of Variance  ─────────────────────────
#> 
#> Grouping variable: education
#> Weights variable: sampling_weight
#> Center: mean
#> 
#> 
#> --- income ---
#> 
#> Weighted Levene's Test Results:
#> ----------------------------------------------------------- 
#>  Variable F_statistic df1  df2 p_value sig        Conclusion
#>    income      102.05   3 2197       0 *** Variances unequal
#> ----------------------------------------------------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - p > 0.05: Variances are homogeneous (equal variances assumed)
#> - p <= 0.05: Variances are heterogeneous (equal variances NOT assumed)

# Piped after ANOVA (common workflow)
result <- survey_data %>%
  oneway_anova(life_satisfaction, group = education)
result %>% levene_test()
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

# Piped after t-test
survey_data %>%
  t_test(age, group = gender) %>%
  levene_test()
#> 
#> ── Levene's Test for Homogeneity of Variance  ──────────────────────────────────
#> 
#> Grouping variable: gender
#> Center: mean
#> 
#> 
#> --- age ---
#> 
#> Levene's Test Results:
#> --------------------------------------------------------- 
#>  Variable F_statistic df1  df2 p_value sig      Conclusion
#>       age       0.534   1 2498   0.465     Variances equal
#> --------------------------------------------------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - p > 0.05: Variances are homogeneous (equal variances assumed)
#> - p <= 0.05: Variances are heterogeneous (equal variances NOT assumed)
#> 
#> Recommendation based on Levene test:
#> - Student's t-test or Welch's t-test both appropriate

# Using mean instead of median as center
survey_data %>% levene_test(income, group = region, center = "mean")
#> 
#> ── Levene's Test for Homogeneity of Variance  ──────────────────────────────────
#> 
#> Grouping variable: region
#> Center: mean
#> 
#> 
#> --- income ---
#> 
#> Levene's Test Results:
#> --------------------------------------------------------- 
#>  Variable F_statistic df1  df2 p_value sig      Conclusion
#>    income       1.631   1 2184   0.202     Variances equal
#> --------------------------------------------------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - p > 0.05: Variances are homogeneous (equal variances assumed)
#> - p <= 0.05: Variances are heterogeneous (equal variances NOT assumed)
```
