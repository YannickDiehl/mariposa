# Compare Multiple Groups Without Assuming Normal Data

`kruskal_wallis()` compares three or more groups when your data isn't
normally distributed or when you have ordinal data (like ratings or
rankings). It's the non-parametric alternative to one-way ANOVA.

Think of it as:

- An extension of the Mann-Whitney test for more than two groups

- A robust way to compare groups that works with any data shape

- Perfect for Likert scales, rankings, or skewed distributions

The test tells you:

- Whether at least one group is different from the others

- How strong the overall group effect is (effect size)

- Which groups tend to have higher or lower values (via mean ranks)

## Usage

``` r
kruskal_wallis(data, ..., group, weights = NULL, conf.level = 0.95)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- ...:

  The variables you want to compare between groups. You can list
  multiple variables or use helpers like `starts_with("satisfaction")`

- group:

  The categorical variable that defines your groups (e.g., education,
  employment). Must have at least 2 groups (3+ for meaningful use).

- weights:

  Optional survey weights for population-representative results

- conf.level:

  Confidence level for intervals (Default: 0.95 = 95%)

## Value

Test results showing whether groups differ, including:

- H statistic (Kruskal-Wallis chi-square test statistic)

- Degrees of freedom (number of groups minus 1)

- P-value (are groups different?)

- Effect size epsilon-squared (how big is the group effect?)

- Mean rank for each group (which groups are higher/lower?)

## Details

### Understanding the Results

**P-value**: If p \< 0.05, at least one group is significantly different

- p \< 0.001: Very strong evidence of group differences

- p \< 0.01: Strong evidence of group differences

- p \< 0.05: Moderate evidence of group differences

- p \> 0.05: No significant group differences found

**Effect Size Epsilon-squared** (How much do groups matter?):

- \< 0.01: Negligible effect

- 0.01-0.06: Small effect

- 0.06-0.14: Medium effect

- 0.14 or higher: Large effect

**Mean Ranks**:

- Higher mean rank = group tends to have higher values

- Lower mean rank = group tends to have lower values

- Compare mean ranks to see the pattern of group differences

### When to Use This

Use Kruskal-Wallis test when:

- Your data is not normally distributed (skewed, outliers)

- You have ordinal data (rankings, Likert scales)

- Sample sizes are small or very unequal across groups

- You want a robust alternative to one-way ANOVA

- You're comparing satisfaction ratings, income, or other skewed
  variables

### What Comes Next?

If the Kruskal-Wallis test is significant:

1.  Look at mean ranks to see the pattern

2.  Use pairwise Mann-Whitney tests with Bonferroni correction to find
    which specific groups differ

3.  Consider effect sizes to judge practical importance

### Relationship to Other Tests

- For 2 groups: Use
  [`mann_whitney()`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md)
  instead

- For normally distributed data: Use
  [`oneway_anova()`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)
  instead

### Weighted variants

SPSS `NPAR TESTS` ignores `WEIGHT BY`, so weighted results have no SPSS
reference. The weighted variant is an R-only frequency-weight extension
that reduces exactly to the unweighted test when all weights equal 1
(enforced by an internal invariance suite); see
[`vignette("spss-compatibility")`](https://YannickDiehl.github.io/mariposa/articles/spss-compatibility.md)
for validation status.

- For repeated measures (same subjects): Use
  [`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md)
  instead

## References

Kruskal, W. H., & Wallis, W. A. (1952). Use of ranks in one-criterion
variance analysis. Journal of the American Statistical Association,
47(260), 583-621.

Tomczak, M., & Tomczak, E. (2014). The need to report effect size
estimates revisited. An overview of some recommended measures of effect
size. Trends in Sport Sciences, 1(21), 19-25.

## See also

[`kruskal.test`](https://rdrr.io/r/stats/kruskal.test.html) for the base
R Kruskal-Wallis test.

[`mann_whitney`](https://YannickDiehl.github.io/mariposa/reference/mann_whitney.md)
for comparing exactly two groups.

[`oneway_anova`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)
for parametric one-way ANOVA.

Other hypothesis_tests:
[`ancova()`](https://YannickDiehl.github.io/mariposa/reference/ancova.md),
[`binomial_test()`](https://YannickDiehl.github.io/mariposa/reference/binomial_test.md),
[`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md),
[`chisq_gof()`](https://YannickDiehl.github.io/mariposa/reference/chisq_gof.md),
[`factorial_anova()`](https://YannickDiehl.github.io/mariposa/reference/factorial_anova.md),
[`fisher_test()`](https://YannickDiehl.github.io/mariposa/reference/fisher_test.md),
[`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md),
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

# Basic Kruskal-Wallis test (comparing across education levels)
survey_data %>%
  kruskal_wallis(life_satisfaction, group = education)
#> Kruskal-Wallis Test: life_satisfaction by education
#>   H(3) = 171.178, p < 0.001 ***, eps2 = 0.071, N = 2421
#> Use summary() for detailed output.

# Multiple variables
survey_data %>%
  kruskal_wallis(life_satisfaction, income, trust_government,
                 group = education)
#> Kruskal-Wallis Test: life_satisfaction by education
#>   H(3) = 171.178, p < 0.001 ***, eps2 = 0.071, N = 2421
#> Kruskal-Wallis Test: income by education
#>   H(3) = 814.174, p < 0.001 ***, eps2 = 0.373, N = 2186
#> Kruskal-Wallis Test: trust_government by education
#>   H(3) = 1.235, p = 0.745 , eps2 = 0.001, N = 2354
#> Use summary() for detailed output.

# Using tidyselect helpers
survey_data %>%
  kruskal_wallis(starts_with("trust_"), group = education)
#> Kruskal-Wallis Test: trust_government by education
#>   H(3) = 1.235, p = 0.745 , eps2 = 0.001, N = 2354
#> Kruskal-Wallis Test: trust_media by education
#>   H(3) = 2.709, p = 0.439 , eps2 = 0.001, N = 2367
#> Kruskal-Wallis Test: trust_science by education
#>   H(3) = 3.047, p = 0.384 , eps2 = 0.001, N = 2398
#> Use summary() for detailed output.

# Weighted analysis
survey_data %>%
  kruskal_wallis(life_satisfaction, group = education,
                 weights = sampling_weight)
#> Kruskal-Wallis Test: life_satisfaction by education [Weighted]
#>   H(3) = 167.075, p < 0.001 ***, eps2 = 0.069, N = 2437
#> Use summary() for detailed output.

# Grouped analysis (separate test for each region)
survey_data %>%
  group_by(region) %>%
  kruskal_wallis(life_satisfaction, group = education)
#> [region = 1]
#> Kruskal-Wallis Test: life_satisfaction by education
#>   H(3) = 17.105, p < 0.001 ***, eps2 = 0.037, N = 465
#> [region = 2]
#> Kruskal-Wallis Test: life_satisfaction by education
#>   H(3) = 158.807, p < 0.001 ***, eps2 = 0.081, N = 1956
#> Use summary() for detailed output.

# Compare across employment status (5 groups)
survey_data %>%
  kruskal_wallis(income, group = employment)
#> Kruskal-Wallis Test: income by employment
#>   H(4) = 28.026, p < 0.001 ***, eps2 = 0.013, N = 2186
#> Use summary() for detailed output.
```
