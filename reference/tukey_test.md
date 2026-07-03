# Find Which Specific Groups Differ After ANOVA

`tukey_test()` tells you exactly which groups are different from each
other after ANOVA finds overall differences. It's like a follow-up
investigation that pinpoints where the differences lie.

Think of it as:

- ANOVA says "there are differences somewhere"

- Tukey test says "specifically, Group A differs from Group C"

- A way to make all possible comparisons while controlling error rates

## Usage

``` r
tukey_test(x, conf.level = 0.95, ...)

# Default S3 method
tukey_test(x, conf.level = 0.95, ...)
```

## Arguments

- x:

  ANOVA results from
  [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)

- conf.level:

  Confidence level for intervals (Default: 0.95 = 95%)

- ...:

  Additional arguments (currently unused)

## Value

Pairwise comparison results showing:

- Which group pairs are significantly different

- Size of the difference between each pair

- Adjusted p-values (controlling for multiple comparisons)

- Confidence intervals for each difference

## Details

### Understanding the Results

**Adjusted P-values**: Control for multiple comparisons

- p \< 0.05: Groups are significantly different

- p ≥ 0.05: No significant difference between these groups

- When you make many comparisons, chance alone could produce false
  positives

- Tukey adjustment protects against this by being more conservative

**Mean Differences**:

- Positive: First group has higher average than second

- Negative: Second group has higher average than first

- Zero in confidence interval: No significant difference

### When to Use Tukey Test

Use Tukey test when:

- Your ANOVA shows significant differences (p \< 0.05)

- You want to know which specific groups differ

- You need to compare all possible pairs

- Group sizes are roughly equal

- Variances are roughly equal across groups

### Tukey vs. Scheffe

**Tukey Test:**

- Less conservative (easier to find differences)

- Best for equal group sizes

- Protects only pairwise comparisons

- Narrower confidence intervals

**Scheffe Test:**

- Most conservative (hardest to find differences)

- Best for unequal group sizes

- Protects against all possible comparisons

- Wider confidence intervals

### Reading the Output

Example: "Group A - Group B: Diff = 3.2, p = 0.012"

- Group A's average is 3.2 units higher than Group B's

- This difference is statistically significant (p \< 0.05)

- You can be confident these groups truly differ

### Tips for Success

- Only run post-hoc tests if ANOVA is significant

- Focus on comparisons that make theoretical sense

- Consider practical significance, not just statistical

- Report both the difference and its confidence interval

- Remember: non-significant doesn't mean "exactly equal"

## References

Tukey, J. W. (1949). Comparing individual means in the analysis of
variance. Biometrics, 5(2), 99-114.

Kramer, C. Y. (1956). Extension of multiple range tests to group means
with unequal numbers of replications. Biometrics, 12(3), 307-310.

## See also

[`oneway_anova`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)
for performing ANOVA tests.

[`TukeyHSD`](https://rdrr.io/r/stats/TukeyHSD.html) for the base R Tukey
HSD function.

[`levene_test`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md)
for testing homogeneity of variances.

Other posthoc:
[`dunn_test()`](https://YannickDiehl.github.io/mariposa/reference/dunn_test.md),
[`levene_test()`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md),
[`pairwise_wilcoxon()`](https://YannickDiehl.github.io/mariposa/reference/pairwise_wilcoxon.md),
[`scheffe_test()`](https://YannickDiehl.github.io/mariposa/reference/scheffe_test.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Perform ANOVA followed by Tukey post-hoc test
anova_result <- survey_data %>%
  oneway_anova(life_satisfaction, group = education)

# Tukey post-hoc comparisons
anova_result %>% tukey_test()
#> Tukey HSD Post-Hoc Test by education
#>   life_satisfaction: 6 comparisons, 5 significant (p < .05)
#> Use summary() for the full comparison table.

# With weights
anova_weighted <- survey_data %>%
  oneway_anova(life_satisfaction, group = education, weights = sampling_weight)

anova_weighted %>% tukey_test()
#> Tukey HSD Post-Hoc Test by education [Weighted]
#>   life_satisfaction: 6 comparisons, 5 significant (p < .05)
#> Use summary() for the full comparison table.

# Multiple variables
anova_multi <- survey_data %>%
  oneway_anova(trust_government, trust_science, group = education)

anova_multi %>% tukey_test()
#> Tukey HSD Post-Hoc Test by education
#>   trust_government: 6 comparisons, 0 significant (p < .05)
#>   trust_science: 6 comparisons, 0 significant (p < .05)
#> Use summary() for the full comparison table.

# Grouped analysis
anova_grouped <- survey_data %>%
  group_by(region) %>%
  oneway_anova(life_satisfaction, group = education)

anova_grouped %>% tukey_test()
#> Tukey HSD Post-Hoc Test by education
#> [region = East]
#>   life_satisfaction: 6 comparisons, 2 significant (p < .05)
#> [region = West]
#>   life_satisfaction: 6 comparisons, 5 significant (p < .05)
#> Use summary() for the full comparison table.
```
