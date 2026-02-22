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
[`levene_test()`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md),
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
#> ── Scheffe Post-Hoc Test Results ───────────────────────────────────────────────
#> 
#> Dependent Variable: life_satisfaction
#> Grouping Variable: education
#> Confidence level: 95.0%
#> Family-wise error rate controlled using Scheffe's method
#> Note: Most conservative post-hoc test (widest confidence intervals)
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Scheffe Results:
#> ------------------------------------------------------------------------------------ 
#>                                   Comparison Difference Lower CI Upper CI
#>     Basic Secondary - Intermediate Secondary     -0.497   -0.662   -0.331
#>         Basic Secondary - Academic Secondary     -0.649   -0.816   -0.483
#>                 Basic Secondary - University     -0.843   -1.034   -0.651
#>  Intermediate Secondary - Academic Secondary     -0.153   -0.330    0.024
#>          Intermediate Secondary - University     -0.346   -0.547   -0.145
#>              Academic Secondary - University     -0.193   -0.395    0.009
#>  p-value Sig
#>    <.001 ***
#>    <.001 ***
#>    <.001 ***
#>    0.121    
#>    <.001 ***
#>    0.067    
#> ------------------------------------------------------------------------------------ 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - Positive differences: First group > Second group
#> - Negative differences: First group < Second group
#> - Confidence intervals not containing 0 indicate significant differences
#> - p-values are adjusted for all possible contrasts (most conservative)
#> - Scheffe test has wider CIs than Tukey HSD

# Multiple variables
anova_result_multi <- survey_data %>%
  oneway_anova(life_satisfaction, income, group = education)

anova_result_multi %>% scheffe_test()
#> ── Scheffe Post-Hoc Test Results ───────────────────────────────────────────────
#> Grouping Variable: education
#> Confidence level: 95.0%
#> Family-wise error rate controlled using Scheffe's method
#> Note: Most conservative post-hoc test (widest confidence intervals)
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Scheffe Results:
#> ------------------------------------------------------------------------------------ 
#>                                   Comparison Difference Lower CI Upper CI
#>     Basic Secondary - Intermediate Secondary     -0.497   -0.662   -0.331
#>         Basic Secondary - Academic Secondary     -0.649   -0.816   -0.483
#>                 Basic Secondary - University     -0.843   -1.034   -0.651
#>  Intermediate Secondary - Academic Secondary     -0.153   -0.330    0.024
#>          Intermediate Secondary - University     -0.346   -0.547   -0.145
#>              Academic Secondary - University     -0.193   -0.395    0.009
#>  p-value Sig
#>    <.001 ***
#>    <.001 ***
#>    <.001 ***
#>    0.121    
#>    <.001 ***
#>    0.067    
#> ------------------------------------------------------------------------------------ 
#> 
#> 
#> --- income ---
#> 
#> Scheffe Results:
#> -------------------------------------------------------------------------------------- 
#>                                   Comparison Difference  Lower CI  Upper CI
#>     Basic Secondary - Intermediate Secondary   -833.471 -1010.178  -656.763
#>         Basic Secondary - Academic Secondary  -1465.040 -1641.748 -1288.332
#>                 Basic Secondary - University  -2578.135 -2780.499 -2375.772
#>  Intermediate Secondary - Academic Secondary   -631.569  -820.717  -442.422
#>          Intermediate Secondary - University  -1744.665 -1957.977 -1531.353
#>              Academic Secondary - University  -1113.096 -1326.408  -899.783
#>  p-value Sig
#>    <.001 ***
#>    <.001 ***
#>    <.001 ***
#>    <.001 ***
#>    <.001 ***
#>    <.001 ***
#> -------------------------------------------------------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - Positive differences: First group > Second group
#> - Negative differences: First group < Second group
#> - Confidence intervals not containing 0 indicate significant differences
#> - p-values are adjusted for all possible contrasts (most conservative)
#> - Scheffe test has wider CIs than Tukey HSD

# Weighted analysis
anova_weighted <- survey_data %>%
  oneway_anova(life_satisfaction, group = education, weights = sampling_weight)

anova_weighted %>% scheffe_test()
#> ── Weighted Scheffe Post-Hoc Test Results ──────────────────────────────────────
#> 
#> Dependent Variable: life_satisfaction
#> Grouping Variable: education
#> Weights Variable: sampling_weight
#> Confidence level: 95.0%
#> Family-wise error rate controlled using Scheffe's method
#> Note: Most conservative post-hoc test (widest confidence intervals)
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Weighted Scheffe Results:
#> ------------------------------------------------------------------------------------ 
#>                                   Comparison Difference Lower CI Upper CI
#>     Basic Secondary - Intermediate Secondary     -0.490   -0.655   -0.325
#>         Basic Secondary - Academic Secondary     -0.643   -0.808   -0.477
#>                 Basic Secondary - University     -0.832   -1.026   -0.638
#>  Intermediate Secondary - Academic Secondary     -0.153   -0.329    0.023
#>          Intermediate Secondary - University     -0.342   -0.545   -0.140
#>              Academic Secondary - University     -0.189   -0.393    0.014
#>  p-value Sig
#>    <.001 ***
#>    <.001 ***
#>    <.001 ***
#>    0.115    
#>    <.001 ***
#>    0.079    
#> ------------------------------------------------------------------------------------ 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - Positive differences: First group > Second group
#> - Negative differences: First group < Second group
#> - Confidence intervals not containing 0 indicate significant differences
#> - p-values are adjusted for all possible contrasts (most conservative)
#> - Scheffe test has wider CIs than Tukey HSD

# Grouped analysis
anova_grouped <- survey_data %>%
  group_by(region) %>%
  oneway_anova(life_satisfaction, group = education)

anova_grouped %>% scheffe_test()
#> ── Scheffe Post-Hoc Test Results ───────────────────────────────────────────────
#> 
#> Grouping Variable: education
#> Confidence level: 95.0%
#> Family-wise error rate controlled using Scheffe's method
#> Note: Most conservative post-hoc test (widest confidence intervals)
#> 
#> 
#> Group: region = East
#> 
#> --- life_satisfaction ---
#> 
#> Scheffe Results:
#> ------------------------------------------------------------------------------------ 
#>                                   Comparison Difference Lower CI Upper CI
#>     Basic Secondary - Intermediate Secondary     -0.334   -0.736    0.067
#>         Basic Secondary - Academic Secondary     -0.532   -0.943   -0.121
#>                 Basic Secondary - University     -0.642   -1.107   -0.178
#>  Intermediate Secondary - Academic Secondary     -0.198   -0.637    0.242
#>          Intermediate Secondary - University     -0.308   -0.798    0.182
#>              Academic Secondary - University     -0.110   -0.608    0.387
#>  p-value Sig
#>    0.143    
#>    0.005  **
#>    0.002  **
#>    0.661    
#>    0.376    
#>    0.943    
#> ------------------------------------------------------------------------------------ 
#> 
#> 
#> Group: region = West
#> 
#> --- life_satisfaction ---
#> 
#> Scheffe Results:
#> ------------------------------------------------------------------------------------ 
#>                                   Comparison Difference Lower CI Upper CI
#>     Basic Secondary - Intermediate Secondary     -0.536   -0.718   -0.355
#>         Basic Secondary - Academic Secondary     -0.678   -0.860   -0.496
#>                 Basic Secondary - University     -0.892   -1.102   -0.681
#>  Intermediate Secondary - Academic Secondary     -0.142   -0.335    0.052
#>          Intermediate Secondary - University     -0.355   -0.575   -0.135
#>              Academic Secondary - University     -0.213   -0.434    0.007
#>  p-value Sig
#>    <.001 ***
#>    <.001 ***
#>    <.001 ***
#>     0.24    
#>    <.001 ***
#>    0.062    
#> ------------------------------------------------------------------------------------ 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - Positive differences: First group > Second group
#> - Negative differences: First group < Second group
#> - Confidence intervals not containing 0 indicate significant differences
#> - p-values are adjusted for all possible contrasts (most conservative)
#> - Scheffe test has wider CIs than Tukey HSD

# Custom confidence level (99%)
anova_result %>% scheffe_test(conf.level = 0.99)
#> ── Scheffe Post-Hoc Test Results ───────────────────────────────────────────────
#> 
#> Dependent Variable: life_satisfaction
#> Grouping Variable: education
#> Confidence level: 99.0%
#> Family-wise error rate controlled using Scheffe's method
#> Note: Most conservative post-hoc test (widest confidence intervals)
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Scheffe Results:
#> ------------------------------------------------------------------------------------ 
#>                                   Comparison Difference Lower CI Upper CI
#>     Basic Secondary - Intermediate Secondary     -0.497   -0.696   -0.297
#>         Basic Secondary - Academic Secondary     -0.649   -0.850   -0.449
#>                 Basic Secondary - University     -0.843   -1.074   -0.612
#>  Intermediate Secondary - Academic Secondary     -0.153   -0.366    0.061
#>          Intermediate Secondary - University     -0.346   -0.588   -0.104
#>              Academic Secondary - University     -0.193   -0.436    0.050
#>  p-value Sig
#>    <.001 ***
#>    <.001 ***
#>    <.001 ***
#>    0.121    
#>    <.001 ***
#>    0.067    
#> ------------------------------------------------------------------------------------ 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - Positive differences: First group > Second group
#> - Negative differences: First group < Second group
#> - Confidence intervals not containing 0 indicate significant differences
#> - p-values are adjusted for all possible contrasts (most conservative)
#> - Scheffe test has wider CIs than Tukey HSD
```
