# Test Whether a Proportion Matches an Expected Value

`binomial_test()` tests whether the observed proportion of a binary
variable differs from a hypothesized proportion. It uses the exact
binomial test, making it valid for any sample size.

Think of it as:

- Testing whether a coin is fair (proportion of heads = 50 percent)

- Checking if your sample's gender ratio matches the population

- Verifying if satisfaction rates meet a target proportion

The test tells you:

- Whether the observed proportion differs significantly from the
  expected

- The exact p-value (based on the binomial distribution)

- A confidence interval for the true proportion

## Usage

``` r
binomial_test(data, ..., p = 0.5, weights = NULL, conf.level = 0.95)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- ...:

  One or more binary variables to test. Each must have exactly 2
  categories (e.g., Yes/No, Male/Female, 0/1, TRUE/FALSE)

- p:

  The hypothesized proportion to test against (Default: 0.50 = 50
  percent). This refers to the proportion of the first category (first
  factor level, or the lower numeric value).

- weights:

  Optional survey weights for population-representative results

- conf.level:

  Confidence level for intervals (Default: 0.95 = 95 percent)

## Value

Test results showing whether the proportion differs from expected,
including:

- Category counts and observed proportions

- Test proportion (null hypothesis)

- Exact p-value (two-sided)

- Confidence interval for the true proportion

## Details

### Understanding the Results

**P-value**: If p \< 0.05, the proportion differs significantly from the
test proportion

- p \< 0.001: Very strong evidence the proportion differs

- p \< 0.01: Strong evidence the proportion differs

- p \< 0.05: Moderate evidence the proportion differs

- p \> 0.05: No significant difference from expected proportion

**Observed Proportion**: The actual proportion in your data. Compare
this with the test proportion to see the direction of any difference.

**Confidence Interval**: The range likely to contain the true population
proportion. If the test proportion falls outside this range, the result
is significant.

### When to Use This

Use the binomial test when:

- You want to compare an observed proportion to a known value

- Your variable has exactly 2 categories

- You need an exact test (not relying on normal approximation)

- Sample size is small (where chi-square may not be reliable)

### Relationship to Other Tests

- For testing association between two categorical variables: Use
  [`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
  instead

- For comparing proportions between groups: Use chi-square or z-test for
  proportions

- For larger samples with normal approximation:

### Weighted variants

SPSS `NPAR TESTS` ignores `WEIGHT BY`, so weighted results have no SPSS
reference. The weighted variant is an R-only frequency-weight extension
that reduces exactly to the unweighted test when all weights equal 1
(enforced by an internal invariance suite); see
[`vignette("spss-compatibility")`](https://YannickDiehl.github.io/mariposa/articles/spss-compatibility.md)
for validation status.

Results will be very similar to a one-sample z-test for proportions

## References

Conover, W. J. (1999). Practical nonparametric statistics (3rd ed.).
John Wiley & Sons.

## See also

[`binom.test`](https://rdrr.io/r/stats/binom.test.html) for the base R
exact binomial test.

[`chi_square`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md)
for testing associations between categorical variables.

Other hypothesis_tests:
[`ancova()`](https://YannickDiehl.github.io/mariposa/reference/ancova.md),
[`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md),
[`chisq_gof()`](https://YannickDiehl.github.io/mariposa/reference/chisq_gof.md),
[`factorial_anova()`](https://YannickDiehl.github.io/mariposa/reference/factorial_anova.md),
[`fisher_test()`](https://YannickDiehl.github.io/mariposa/reference/fisher_test.md),
[`friedman_test()`](https://YannickDiehl.github.io/mariposa/reference/friedman_test.md),
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

# Test whether gender split is 50/50
survey_data %>%
  binomial_test(gender, p = 0.50)
#> Binomial Test: gender
#>   Group 1 (Male): prop = 0.478 vs 0.500, p = 0.026 *, N = 2500
#> Use summary() for detailed output.

# Test whether East region proportion is 50%
survey_data %>%
  binomial_test(region, p = 0.50)
#> Binomial Test: region
#>   Group 1 (East): prop = 0.194 vs 0.500, p < 0.001 ***, N = 2500
#> Use summary() for detailed output.

# Multiple variables at once
survey_data %>%
  binomial_test(gender, region, p = 0.50)
#> Binomial Test: gender
#>   Group 1 (Male): prop = 0.478 vs 0.500, p = 0.026 *, N = 2500
#> Binomial Test: region
#>   Group 1 (East): prop = 0.194 vs 0.500, p < 0.001 ***, N = 2500
#> Use summary() for detailed output.

# Weighted analysis
survey_data %>%
  binomial_test(gender, p = 0.50, weights = sampling_weight)
#> Binomial Test: gender [Weighted]
#>   Group 1 (Male): prop = 0.478 vs 0.500, p = 0.026 *, N = 2500
#> Use summary() for detailed output.

# Grouped analysis (separate test per region)
survey_data %>%
  group_by(region) %>%
  binomial_test(gender, p = 0.50)
#> [region = 1]
#> Binomial Test: gender
#>   Group 1 (Male): prop = 0.491 vs 0.500, p = 0.716 , N = 485
#> [region = 2]
#> Binomial Test: gender
#>   Group 1 (Male): prop = 0.474 vs 0.500, p = 0.023 *, N = 2015
#> Use summary() for detailed output.
```
