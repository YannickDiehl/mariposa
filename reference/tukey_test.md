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
[`levene_test()`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md),
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
#> ── Tukey HSD Post-Hoc Test Results ─────────────────────────────────────────────
#> 
#> Dependent Variable: life_satisfaction
#> Grouping Variable: education
#> Confidence level: 95.0%
#> Family-wise error rate controlled using Tukey HSD
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Tukey Results:
#> ---------------------------------------------------------------------------------- 
#>                                 Comparison Difference Lower CI Upper CI p-value
#>     Intermediate Secondary-Basic Secondary      0.497    0.344    0.649   <.001
#>         Academic Secondary-Basic Secondary      0.649    0.496    0.802   <.001
#>                 University-Basic Secondary      0.843    0.666    1.019   <.001
#>  Academic Secondary-Intermediate Secondary      0.153   -0.010    0.316   0.075
#>          University-Intermediate Secondary      0.346    0.161    0.531   <.001
#>              University-Academic Secondary      0.193    0.008    0.379   0.037
#>  Sig
#>  ***
#>  ***
#>  ***
#>     
#>  ***
#>    *
#> ---------------------------------------------------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - Positive differences: First group > Second group
#> - Negative differences: First group < Second group
#> - Confidence intervals not containing 0 indicate significant differences
#> - p-values are adjusted for multiple comparisons (family-wise error control)

# With weights
anova_weighted <- survey_data %>%
  oneway_anova(life_satisfaction, group = education, weights = sampling_weight)

anova_weighted %>% tukey_test()
#> ── Weighted Tukey HSD Post-Hoc Test Results ────────────────────────────────────
#> 
#> Dependent Variable: life_satisfaction
#> Grouping Variable: education
#> Weights Variable: sampling_weight
#> Confidence level: 95.0%
#> Family-wise error rate controlled using Tukey HSD
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Weighted Tukey Results:
#> ------------------------------------------------------------------------------------ 
#>                                   Comparison Difference Lower CI Upper CI
#>     Basic Secondary - Intermediate Secondary     -0.490   -0.641   -0.339
#>         Basic Secondary - Academic Secondary     -0.643   -0.795   -0.491
#>                 Basic Secondary - University     -0.832   -1.011   -0.654
#>  Intermediate Secondary - Academic Secondary     -0.153   -0.314    0.008
#>          Intermediate Secondary - University     -0.342   -0.529   -0.156
#>              Academic Secondary - University     -0.189   -0.376   -0.003
#>  p-value Sig
#>    <.001 ***
#>    <.001 ***
#>    <.001 ***
#>    0.071    
#>    <.001 ***
#>    0.046   *
#> ------------------------------------------------------------------------------------ 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - Positive differences: First group > Second group
#> - Negative differences: First group < Second group
#> - Confidence intervals not containing 0 indicate significant differences
#> - p-values are adjusted for multiple comparisons (family-wise error control)

# Multiple variables
anova_multi <- survey_data %>%
  oneway_anova(trust_government, trust_companies, group = education)
#> Error in oneway_anova(., trust_government, trust_companies, group = education): Can't select columns that don't exist.
#> ✖ Column `trust_companies` doesn't exist.

anova_multi %>% tukey_test()
#> Error: object 'anova_multi' not found

# Grouped analysis
anova_grouped <- survey_data %>%
  group_by(region) %>%
  oneway_anova(life_satisfaction, group = education)

anova_grouped %>% tukey_test()
#> ── Tukey HSD Post-Hoc Test Results ─────────────────────────────────────────────
#> 
#> Grouping Variable: education
#> Confidence level: 95.0%
#> Family-wise error rate controlled using Tukey HSD
#> 
#> 
#> Group: region = East
#> 
#> --- life_satisfaction ---
#> 
#> Tukey Results:
#> ---------------------------------------------------------------------------------- 
#>                                 Comparison Difference Lower CI Upper CI p-value
#>     Intermediate Secondary-Basic Secondary      0.334   -0.035    0.703   0.092
#>         Academic Secondary-Basic Secondary      0.532    0.154    0.910   0.002
#>                 University-Basic Secondary      0.642    0.215    1.069   <.001
#>  Academic Secondary-Intermediate Secondary      0.198   -0.206    0.602   0.587
#>          University-Intermediate Secondary      0.308   -0.142    0.758   0.292
#>              University-Academic Secondary      0.110   -0.347    0.568   0.925
#>  Sig
#>     
#>   **
#>  ***
#>     
#>     
#>     
#> ---------------------------------------------------------------------------------- 
#> 
#> 
#> Group: region = West
#> 
#> --- life_satisfaction ---
#> 
#> Tukey Results:
#> ---------------------------------------------------------------------------------- 
#>                                 Comparison Difference Lower CI Upper CI p-value
#>     Intermediate Secondary-Basic Secondary      0.536    0.369    0.703   <.001
#>         Academic Secondary-Basic Secondary      0.678    0.511    0.845   <.001
#>                 University-Basic Secondary      0.892    0.698    1.085   <.001
#>  Academic Secondary-Intermediate Secondary      0.142   -0.036    0.319    0.17
#>          University-Intermediate Secondary      0.355    0.153    0.557   <.001
#>              University-Academic Secondary      0.213    0.011    0.416   0.034
#>  Sig
#>  ***
#>  ***
#>  ***
#>     
#>  ***
#>    *
#> ---------------------------------------------------------------------------------- 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - Positive differences: First group > Second group
#> - Negative differences: First group < Second group
#> - Confidence intervals not containing 0 indicate significant differences
#> - p-values are adjusted for multiple comparisons (family-wise error control)
```
