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

# Default S3 method
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

- variable:

  Variable to test (when using grouped data frame)

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

## References

Levene, H. (1960). Robust tests for equality of variances. In I. Olkin
(Ed.), *Contributions to Probability and Statistics* (pp. 278–292).
Stanford University Press.

Brown, M. B., & Forsythe, A. B. (1974). Robust tests for the equality of
variances. *Journal of the American Statistical Association*, 69(346),
364–367.

IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.

## See also

[`oneway_anova`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)
for one-way ANOVA (which assumes equal variances).

[`t_test`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)
for group mean comparisons.

[`var.test`](https://rdrr.io/r/stats/var.test.html) for the base R
F-test of variance equality.

Other posthoc:
[`dunn_test()`](https://YannickDiehl.github.io/mariposa/reference/dunn_test.md),
[`pairwise_wilcoxon()`](https://YannickDiehl.github.io/mariposa/reference/pairwise_wilcoxon.md),
[`scheffe_test()`](https://YannickDiehl.github.io/mariposa/reference/scheffe_test.md),
[`tukey_test()`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Standalone Levene test (test homogeneity of variances)
survey_data %>% levene_test(life_satisfaction, group = region)
#> Levene's Test: life_satisfaction by region
#>   F(1, 2419) = 3.164, p = 0.075 , variances equal
#> Use summary() for detailed output.

# Multiple variables
survey_data %>% levene_test(life_satisfaction, trust_government, group = region)
#> Levene's Test: life_satisfaction by region
#>   F(1, 2419) = 3.164, p = 0.075 , variances equal
#> Levene's Test: trust_government by region
#>   F(1, 2352) = 0.145, p = 0.703 , variances equal
#> Use summary() for detailed output.

# Weighted analysis
survey_data %>% levene_test(income, group = education, weights = sampling_weight)
#> Levene's Test: income by education [Weighted]
#>   F(3, 2196.9) = 102.048, p < 0.001 ***, variances unequal
#> Use summary() for detailed output.

# Piped after ANOVA (common workflow)
result <- survey_data %>%
  oneway_anova(life_satisfaction, group = education)
result %>% levene_test()
#> Levene's Test: life_satisfaction by education
#>   F(3, 2417) = 31.634, p < 0.001 ***, variances unequal
#> Use summary() for detailed output.

# Piped after t-test
survey_data %>%
  t_test(age, group = gender) %>%
  levene_test()
#> Levene's Test: age by gender
#>   F(1, 2498) = 0.534, p = 0.465 , variances equal
#> Use summary() for detailed output.

# Using mean instead of median as center
survey_data %>% levene_test(income, group = region, center = "mean")
#> Levene's Test: income by region
#>   F(1, 2184) = 1.631, p = 0.202 , variances equal
#> Use summary() for detailed output.
```
