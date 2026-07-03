# Compare Three or More Related Measurements Without Assuming Normality

`friedman_test()` compares three or more related measurements from the
same subjects when your data isn't normally distributed. It's the
non-parametric alternative to repeated-measures ANOVA.

Think of it as:

- Comparing ratings of multiple items by the same respondents

- Testing whether scores change across three or more time points

- A robust repeated-measures comparison that works with any data shape

The test tells you:

- Whether at least one measurement is significantly different from the
  others

- Which measurements tend to be rated higher or lower (via mean ranks)

- The strength of the overall effect (Kendall's W)

## Usage

``` r
friedman_test(data, ..., weights = NULL, conf.level = 0.95)
```

## Arguments

- data:

  Your survey data (a data frame or tibble) in wide format, with one row
  per subject and each measurement in a separate column

- ...:

  The measurement variables to compare (at least 3). You can list them
  individually or use helpers like `starts_with("trust_")`

- weights:

  Optional survey weights for population-representative results

- conf.level:

  Confidence level for intervals (Default: 0.95 = 95 percent)

## Value

Test results showing whether the measurements differ, including:

- Chi-Square statistic (`chi_squared`, the Friedman test statistic)

- Degrees of freedom (number of measurements minus 1)

- P-value (are measurements different?)

- Kendall's W (effect size: how strong is the overall pattern?)

- Mean rank for each measurement (which measurements are higher/lower?)

## Details

### Understanding the Results

**P-value**: If p \< 0.05, at least one measurement is significantly
different

- p \< 0.001: Very strong evidence of differences

- p \< 0.01: Strong evidence of differences

- p \< 0.05: Moderate evidence of differences

- p \> 0.05: No significant differences found

**Kendall's W** (Effect size: How consistent is the pattern?):

- \< 0.1: Negligible agreement/effect

- 0.1 - 0.3: Weak agreement

- 0.3 - 0.5: Moderate agreement

- 0.5 or higher: Strong agreement

**Mean Ranks**:

- Higher mean rank = measurement tends to have higher values

- Lower mean rank = measurement tends to have lower values

- Compare mean ranks to see which measurements stand out

### When to Use This

Use the Friedman test when:

- You have 3 or more related measurements from the same subjects

- Your data is not normally distributed

- You have ordinal data (ratings, rankings)

- You want a robust alternative to repeated-measures ANOVA

- You're comparing multiple ratings by the same respondents

### Relationship to Other Tests

- For 2 related measurements: Use
  [`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/reference/wilcoxon_test.md)
  instead

- For normally distributed repeated measures: Use repeated-measures
  ANOVA

### Weighted variants

SPSS `NPAR TESTS` ignores `WEIGHT BY`, so weighted results have no SPSS
reference. The weighted variant is an R-only frequency-weight extension
that reduces exactly to the unweighted test when all weights equal 1
(enforced by an internal invariance suite); see
[`vignette("spss-compatibility")`](https://YannickDiehl.github.io/mariposa/articles/spss-compatibility.md)
for validation status.

- For independent groups: Use
  [`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md)
  instead

## References

Friedman, M. (1937). The use of ranks to avoid the assumption of
normality implicit in the analysis of variance. Journal of the American
Statistical Association, 32(200), 675-701.

Kendall, M. G., & Babington Smith, B. (1939). The problem of m rankings.
The Annals of Mathematical Statistics, 10(3), 275-287.

## See also

[`friedman.test`](https://rdrr.io/r/stats/friedman.test.html) for the
base R Friedman test.

[`wilcoxon_test`](https://YannickDiehl.github.io/mariposa/reference/wilcoxon_test.md)
for comparing two related measurements.

[`kruskal_wallis`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md)
for comparing independent groups.

Other hypothesis_tests:
[`ancova()`](https://YannickDiehl.github.io/mariposa/reference/ancova.md),
[`binomial_test()`](https://YannickDiehl.github.io/mariposa/reference/binomial_test.md),
[`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md),
[`chisq_gof()`](https://YannickDiehl.github.io/mariposa/reference/chisq_gof.md),
[`factorial_anova()`](https://YannickDiehl.github.io/mariposa/reference/factorial_anova.md),
[`fisher_test()`](https://YannickDiehl.github.io/mariposa/reference/fisher_test.md),
[`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md),
[`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md),
[`mcnemar_test()`](https://YannickDiehl.github.io/mariposa/reference/mcnemar_test.md),
[`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md),
[`t_test()`](https://YannickDiehl.github.io/mariposa/reference/t_test.md),
[`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/reference/wilcoxon_test.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Compare three trust items (rated by same respondents)
survey_data %>%
  friedman_test(trust_government, trust_media, trust_science)
#> Friedman Test: trust_government, trust_media, trust_science
#>   chi2(2) = 1009.035, p < 0.001 ***, W = 0.236, N = 2135
#> Use summary() for detailed output.

# Using tidyselect helpers
survey_data %>%
  friedman_test(starts_with("trust_"))
#> Friedman Test: trust_government, trust_media, trust_science
#>   chi2(2) = 1009.035, p < 0.001 ***, W = 0.236, N = 2135
#> Use summary() for detailed output.

# Weighted analysis
survey_data %>%
  friedman_test(trust_government, trust_media, trust_science,
                weights = sampling_weight)
#> Friedman Test: trust_government, trust_media, trust_science [Weighted]
#>   chi2(2) = 1012.084, p < 0.001 ***, W = 0.235, N = 2150
#> Use summary() for detailed output.

# Grouped analysis (separate test per region)
survey_data %>%
  group_by(region) %>%
  friedman_test(trust_government, trust_media, trust_science)
#> [region = 1]
#> Friedman Test: trust_government, trust_media, trust_science
#>   chi2(2) = 217.100, p < 0.001 ***, W = 0.257, N = 422
#> [region = 2]
#> Friedman Test: trust_government, trust_media, trust_science
#>   chi2(2) = 792.344, p < 0.001 ***, W = 0.231, N = 1713
#> Use summary() for detailed output.
```
