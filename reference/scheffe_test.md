# Compare All Groups More Conservatively After ANOVA

`scheffe_test()` tells you which groups differ after ANOVA, using the
most conservative approach. It's like Tukey's test but even more careful
about avoiding false positives.

Think of it as:

- The most cautious post-hoc test available

- A way to compare groups when sample sizes are very unequal

- Insurance against finding differences that aren't real

## Usage

``` r
scheffe_test(x, conf.level = 0.95, ...)

# Default S3 method
scheffe_test(x, conf.level = 0.95, ...)
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

- Adjusted p-values (extra conservative)

- Confidence intervals for each difference

## Details

### Understanding the Results

**Adjusted P-values**: Extra conservative to prevent false positives

- p \< 0.05: Groups are significantly different (you can be very
  confident)

- p ≥ 0.05: No significant difference between these groups

- Scheffe adjustments are stricter than other methods

**Confidence Intervals**: Wider than Tukey's

- Do not include 0: Groups differ significantly

- Include 0: No significant difference

- Wider intervals reflect extra caution

### When to Use Scheffe Test

Use Scheffe test when:

- Your ANOVA shows significant differences (p \< 0.05)

- Group sizes are very unequal

- You want to be extra cautious about false positives

- You might test complex comparisons (not just pairs)

- Sample sizes are small

### Scheffe vs. Tukey

**Scheffe Test:**

- Most conservative (hardest to find differences)

- Best for unequal group sizes

- Protects against all possible comparisons

- Wider confidence intervals

**Tukey Test:**

- Less conservative (easier to find differences)

- Best for equal group sizes

- Protects only pairwise comparisons

- Narrower confidence intervals

### Reading the Output

Example: "Group A - Group B: Diff = 3.2, p = 0.082"

- Group A's average is 3.2 units higher than Group B's

- This difference is NOT significant with Scheffe (p \> 0.05)

- It might be significant with less conservative tests

### Tips for Success

- Scheffe may not find differences even when ANOVA does

- This is normal - it's being extra careful

- Consider Tukey if group sizes are similar

- Report which post-hoc test you used and why

- Focus on confidence intervals, not just p-values

## References

Scheffe, H. (1953). A method for judging all contrasts in the analysis
of variance. Biometrika, 40(1-2), 87-110.

Scheffe, H. (1959). The Analysis of Variance. New York: Wiley.

## See also

[`oneway_anova`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)
for performing ANOVA tests.

[`tukey_test`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md)
for Tukey HSD post-hoc tests.

[`levene_test`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md)
for testing homogeneity of variances.

Other posthoc:
[`dunn_test()`](https://YannickDiehl.github.io/mariposa/reference/dunn_test.md),
[`levene_test()`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md),
[`pairwise_wilcoxon()`](https://YannickDiehl.github.io/mariposa/reference/pairwise_wilcoxon.md),
[`tukey_test()`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Perform ANOVA followed by Scheffe post-hoc test
anova_result <- survey_data %>%
  oneway_anova(life_satisfaction, group = education)

# Scheffe post-hoc comparisons
anova_result %>% scheffe_test()
#> Scheffe Post-Hoc Test by education
#>   life_satisfaction: 6 comparisons, 4 significant (p < .05)
#> Use summary() for the full comparison table.

# Multiple variables
anova_result_multi <- survey_data %>%
  oneway_anova(life_satisfaction, income, group = education)

anova_result_multi %>% scheffe_test()
#> Scheffe Post-Hoc Test by education
#>   life_satisfaction: 6 comparisons, 4 significant (p < .05)
#>   income: 6 comparisons, 6 significant (p < .05)
#> Use summary() for the full comparison table.

# Weighted analysis
anova_weighted <- survey_data %>%
  oneway_anova(life_satisfaction, group = education, weights = sampling_weight)

anova_weighted %>% scheffe_test()
#> Scheffe Post-Hoc Test by education [Weighted]
#>   life_satisfaction: 6 comparisons, 4 significant (p < .05)
#> Use summary() for the full comparison table.

# Grouped analysis
anova_grouped <- survey_data %>%
  group_by(region) %>%
  oneway_anova(life_satisfaction, group = education)

anova_grouped %>% scheffe_test()
#> Scheffe Post-Hoc Test by education
#> [region = East]
#>   life_satisfaction: 6 comparisons, 2 significant (p < .05)
#> [region = West]
#>   life_satisfaction: 6 comparisons, 4 significant (p < .05)
#> Use summary() for the full comparison table.

# Custom confidence level (99%)
anova_result %>% scheffe_test(conf.level = 0.99)
#> Scheffe Post-Hoc Test by education
#>   life_satisfaction: 6 comparisons, 4 significant (p < .05)
#> Use summary() for the full comparison table.
```
