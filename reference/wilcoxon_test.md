# Compare Two Related Measurements Without Assuming Normality

`wilcoxon_test()` compares two paired measurements from the same
subjects when your data isn't normally distributed. It's the
non-parametric alternative to the paired t-test.

Think of it as:

- A way to test whether scores changed between two time points

- Comparing ratings of two items from the same respondents

- A robust paired comparison that works with any data shape

The test tells you:

- Whether scores are significantly different between the two
  measurements

- How many subjects increased, decreased, or stayed the same

- The strength of the effect (effect size r)

## Usage

``` r
wilcoxon_test(data, x, y, weights = NULL, conf.level = 0.95)
```

## Arguments

- data:

  Your survey data (a data frame or tibble) in wide format, with one row
  per subject and the two measurements in separate columns

- x:

  The first measurement variable (e.g., pre-test, trust in government)

- y:

  The second measurement variable (e.g., post-test, trust in media). The
  difference is computed as `y - x`

- weights:

  Optional survey weights for population-representative results

- conf.level:

  Confidence level for intervals (Default: 0.95 = 95 percent)

## Value

Test results showing whether the two measurements differ, including:

- Z statistic (standardized test statistic, normal approximation)

- P-value (is there a significant difference?)

- Effect size r (how strong is the difference?)

- Rank statistics (negative ranks, positive ranks, ties)

## Details

### Understanding the Results

**P-value**: If p \< 0.05, the two measurements are significantly
different

- p \< 0.001: Very strong evidence of a difference

- p \< 0.01: Strong evidence of a difference

- p \< 0.05: Moderate evidence of a difference

- p \> 0.05: No significant difference found

**Effect Size r** (How strong is the difference?):

- \< 0.1: Negligible effect

- 0.1 - 0.3: Small effect

- 0.3 - 0.5: Medium effect

- 0.5 or higher: Large effect

**Rank Categories**:

- Negative Ranks: subjects where y \< x (decreased)

- Positive Ranks: subjects where y \> x (increased)

- Ties: subjects where y = x (no change)

### When to Use This

Use Wilcoxon signed-rank test when:

- Comparing two related measurements from the same subjects

- Your data is not normally distributed

- You have ordinal data (ratings, rankings)

- Sample size is small

- You want a robust alternative to the paired t-test

### Relationship to Other Tests

- For normally distributed paired data: Use paired t-test instead

- For independent groups: Use
  [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md)
  instead

### Weighted variants

SPSS `NPAR TESTS` ignores `WEIGHT BY`, so weighted results have no SPSS
reference. The weighted variant is an R-only frequency-weight extension
that reduces exactly to the unweighted test when all weights equal 1
(enforced by an internal invariance suite); see
[`vignette("spss-compatibility")`](https://YannickDiehl.github.io/mariposa/articles/spss-compatibility.md)
for validation status.

- For 3+ related measurements: Use
  [`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md)
  instead

## References

Wilcoxon, F. (1945). Individual comparisons by ranking methods.
Biometrics Bulletin, 1(6), 80-83.

Fritz, C. O., Morris, P. E., & Richler, J. J. (2012). Effect size
estimates: current use, calculations, and interpretation. Journal of
Experimental Psychology: General, 141(1), 2.

## See also

[`wilcox.test`](https://rdrr.io/r/stats/wilcox.test.html) for the base R
Wilcoxon test.

[`mann_whitney`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md)
for comparing two independent groups.

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
[`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Compare trust in government vs trust in media
survey_data %>%
  wilcoxon_test(x = trust_government, y = trust_media)
#> Wilcoxon Signed-Rank Test: trust_media - trust_government
#>   Z = -5.097, p < 0.001 ***, r = 0.123 (small), N = 2227
#> Use summary() for detailed output.

# Weighted analysis
survey_data %>%
  wilcoxon_test(x = trust_government, y = trust_media,
                weights = sampling_weight)
#> Wilcoxon Signed-Rank Test: trust_media - trust_government [Weighted]
#>   Z = -5.035, p < 0.001 ***, r = 0.121 (small), N = 2242
#> Use summary() for detailed output.

# Grouped analysis (separate test per region)
survey_data %>%
  group_by(region) %>%
  wilcoxon_test(x = trust_government, y = trust_media)
#> [region = 1]
#> Wilcoxon Signed-Rank Test: trust_media - trust_government
#>   Z = -2.727, p = 0.006 **, r = 0.147 (small), N = 435
#> [region = 2]
#> Wilcoxon Signed-Rank Test: trust_media - trust_government
#>   Z = -4.346, p < 0.001 ***, r = 0.117 (small), N = 1792
#> Use summary() for detailed output.
```
