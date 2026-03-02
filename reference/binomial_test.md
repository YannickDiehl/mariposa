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

- For larger samples with normal approximation: Results will be very
  similar to a one-sample z-test for proportions

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
#> 
#> Binomial Test Results
#> ---------------------
#> 
#> - Test proportion: 0.5
#> - Confidence level: 95.0%
#> 
#> ── gender ──────────────────────────────────────────────────────────────────────
#> 
#>   Categories:
#>   ------------------------------------
#>                       N Observed Prop.
#>      Group 1: Male 1194          0.478
#>    Group 2: Female 1306          0.522
#>              Total 2500          1.000
#>   ------------------------------------
#> 
#>   Test Statistics:
#>   -----------------------------------------
#>    Test Prop. p value CI lower CI upper sig
#>           0.5   0.026    0.458    0.497   *
#>   -----------------------------------------
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05

# Test whether East region proportion is 50%
survey_data %>%
  binomial_test(region, p = 0.50)
#> 
#> Binomial Test Results
#> ---------------------
#> 
#> - Test proportion: 0.5
#> - Confidence level: 95.0%
#> 
#> ── region ──────────────────────────────────────────────────────────────────────
#> 
#>   Categories:
#>   ----------------------------------
#>                     N Observed Prop.
#>    Group 1: East  485          0.194
#>    Group 2: West 2015          0.806
#>            Total 2500          1.000
#>   ----------------------------------
#> 
#>   Test Statistics:
#>   -----------------------------------------
#>    Test Prop. p value CI lower CI upper sig
#>           0.5       0    0.179     0.21 ***
#>   -----------------------------------------
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05

# Multiple variables at once
survey_data %>%
  binomial_test(gender, region, p = 0.50)
#> 
#> Binomial Test Results
#> ---------------------
#> 
#> - Test proportion: 0.5
#> - Confidence level: 95.0%
#> 
#> ── gender ──────────────────────────────────────────────────────────────────────
#> 
#>   Categories:
#>   ------------------------------------
#>                       N Observed Prop.
#>      Group 1: Male 1194          0.478
#>    Group 2: Female 1306          0.522
#>              Total 2500          1.000
#>   ------------------------------------
#> 
#>   Test Statistics:
#>   -----------------------------------------
#>    Test Prop. p value CI lower CI upper sig
#>           0.5   0.026    0.458    0.497   *
#>   -----------------------------------------
#> 
#> ── region ──────────────────────────────────────────────────────────────────────
#> 
#>   Categories:
#>   ----------------------------------
#>                     N Observed Prop.
#>    Group 1: East  485          0.194
#>    Group 2: West 2015          0.806
#>            Total 2500          1.000
#>   ----------------------------------
#> 
#>   Test Statistics:
#>   -----------------------------------------
#>    Test Prop. p value CI lower CI upper sig
#>           0.5       0    0.179     0.21 ***
#>   -----------------------------------------
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05

# Weighted analysis
survey_data %>%
  binomial_test(gender, p = 0.50, weights = sampling_weight)
#> 
#> Weighted Binomial Test Results
#> ------------------------------
#> 
#> - Test proportion: 0.5
#> - Confidence level: 95.0%
#> - Weights variable: sampling_weight
#> 
#> ── gender ──────────────────────────────────────────────────────────────────────
#> 
#>   Categories:
#>   ------------------------------------
#>                       N Observed Prop.
#>      Group 1: Male 1194          0.478
#>    Group 2: Female 1306          0.522
#>              Total 2500          1.000
#>   ------------------------------------
#> 
#>   Weighted Test Statistics:
#>   -----------------------------------------
#>    Test Prop. p value CI lower CI upper sig
#>           0.5   0.026    0.458    0.497   *
#>   -----------------------------------------
#> 
#> Note: Weighted analysis uses rounded frequency weights.
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05

# Grouped analysis (separate test per region)
survey_data %>%
  group_by(region) %>%
  binomial_test(gender, p = 0.50)
#> 
#> Binomial Test Results
#> ---------------------
#> 
#> - Test proportion: 0.5
#> - Confidence level: 95.0%
#> 
#> 
#> Group: region = East
#> --------------------
#> 
#> ── gender ──────────────────────────────────────────────────────────────────────
#> 
#>   Categories:
#>   -----------------------------------
#>                      N Observed Prop.
#>      Group 1: Male 238          0.491
#>    Group 2: Female 247          0.509
#>              Total 485          1.000
#>   -----------------------------------
#> 
#>   Test Statistics:
#>   -----------------------------------------
#>    Test Prop. p value CI lower CI upper sig
#>           0.5   0.716    0.445    0.536    
#>   -----------------------------------------
#> 
#> 
#> Group: region = West
#> --------------------
#> 
#> ── gender ──────────────────────────────────────────────────────────────────────
#> 
#>   Categories:
#>   ------------------------------------
#>                       N Observed Prop.
#>      Group 1: Male  956          0.474
#>    Group 2: Female 1059          0.526
#>              Total 2015          1.000
#>   ------------------------------------
#> 
#>   Test Statistics:
#>   -----------------------------------------
#>    Test Prop. p value CI lower CI upper sig
#>           0.5   0.023    0.452    0.497   *
#>   -----------------------------------------
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
