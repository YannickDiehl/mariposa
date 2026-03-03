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
  [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/dev/reference/mann_whitney.md)
  instead

- For 3+ related measurements: Use
  [`friedman_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/friedman_test.md)
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

[`mann_whitney`](https://YannickDiehl.github.io/mariposa/dev/reference/mann_whitney.md)
for comparing two independent groups.

Other hypothesis_tests:
[`ancova()`](https://YannickDiehl.github.io/mariposa/dev/reference/ancova.md),
[`binomial_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/binomial_test.md),
[`chi_square()`](https://YannickDiehl.github.io/mariposa/dev/reference/chi_square.md),
[`chisq_gof()`](https://YannickDiehl.github.io/mariposa/dev/reference/chisq_gof.md),
[`factorial_anova()`](https://YannickDiehl.github.io/mariposa/dev/reference/factorial_anova.md),
[`fisher_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/fisher_test.md),
[`friedman_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/friedman_test.md),
[`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/dev/reference/kruskal_wallis.md),
[`mann_whitney()`](https://YannickDiehl.github.io/mariposa/dev/reference/mann_whitney.md),
[`mcnemar_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/mcnemar_test.md),
[`oneway_anova()`](https://YannickDiehl.github.io/mariposa/dev/reference/oneway_anova.md),
[`t_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/t_test.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Compare trust in government vs trust in media
survey_data %>%
  wilcoxon_test(x = trust_government, y = trust_media)
#> 
#> Wilcoxon Signed-Rank Test Results
#> ---------------------------------
#> 
#> - Pair: trust_media vs trust_government
#> 
#> trust_media - trust_government
#> ------------------------------
#>   Ranks:
#>   -------------------------------------------
#>                      N Mean Rank Sum of Ranks
#>    Negative Ranks  955    887.53     847592.5
#>    Positive Ranks  770    832.57     641082.5
#>              Ties  502        NA           NA
#>             Total 2227        NA           NA
#>   -------------------------------------------
#> 
#>   a trust_media < trust_government
#>   b trust_media > trust_government
#>   c trust_media = trust_government
#> 
#>   Test Statistics:
#>   ----------------------------
#>         Z p value Effect r sig
#>    -5.097       0    0.123 ***
#>   ----------------------------
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (r):
#> - Small effect: 0.1 - 0.3
#> - Medium effect: 0.3 - 0.5
#> - Large effect: > 0.5

# Weighted analysis
survey_data %>%
  wilcoxon_test(x = trust_government, y = trust_media,
                weights = sampling_weight)
#> 
#> Weighted Wilcoxon Signed-Rank Test Results
#> ------------------------------------------
#> 
#> - Pair: trust_media vs trust_government
#> - Weights variable: sampling_weight
#> 
#> trust_media - trust_government
#> ------------------------------
#>   Ranks:
#>   -------------------------------------------
#>                      N Mean Rank Sum of Ranks
#>    Negative Ranks  960    891.43     855464.0
#>    Positive Ranks  775    838.58     649788.2
#>              Ties  508        NA           NA
#>             Total 2242        NA           NA
#>   -------------------------------------------
#> 
#>   a trust_media < trust_government
#>   b trust_media > trust_government
#>   c trust_media = trust_government
#> 
#>   Weighted Test Statistics:
#>   ----------------------------
#>         Z p value Effect r sig
#>    -5.033       0    0.121 ***
#>   ----------------------------
#> 
#> Note: Weighted analysis uses frequency-weighted ranks.
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (r):
#> - Small effect: 0.1 - 0.3
#> - Medium effect: 0.3 - 0.5
#> - Large effect: > 0.5

# Grouped analysis (separate test per region)
survey_data %>%
  group_by(region) %>%
  wilcoxon_test(x = trust_government, y = trust_media)
#> 
#> Wilcoxon Signed-Rank Test Results
#> ---------------------------------
#> 
#> - Pair: trust_media vs trust_government
#> 
#> 
#> Group: region = East
#> --------------------
#> 
#> trust_media - trust_government
#> ------------------------------
#>   Ranks:
#>   ------------------------------------------
#>                     N Mean Rank Sum of Ranks
#>    Negative Ranks 191    183.16      34983.5
#>    Positive Ranks 155    161.60      25047.5
#>              Ties  89        NA           NA
#>             Total 435        NA           NA
#>   ------------------------------------------
#> 
#>   a trust_media < trust_government
#>   b trust_media > trust_government
#>   c trust_media = trust_government
#> 
#>   Test Statistics:
#>   ----------------------------
#>         Z p value Effect r sig
#>    -2.727   0.006    0.147  **
#>   ----------------------------
#> 
#> 
#> Group: region = West
#> --------------------
#> 
#> trust_media - trust_government
#> ------------------------------
#>   Ranks:
#>   -------------------------------------------
#>                      N Mean Rank Sum of Ranks
#>    Negative Ranks  764    705.10     538697.5
#>    Positive Ranks  615    671.24     412812.5
#>              Ties  413        NA           NA
#>             Total 1792        NA           NA
#>   -------------------------------------------
#> 
#>   a trust_media < trust_government
#>   b trust_media > trust_government
#>   c trust_media = trust_government
#> 
#>   Test Statistics:
#>   ----------------------------
#>         Z p value Effect r sig
#>    -4.346       0    0.117 ***
#>   ----------------------------
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (r):
#> - Small effect: 0.1 - 0.3
#> - Medium effect: 0.3 - 0.5
#> - Large effect: > 0.5
```
