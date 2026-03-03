# McNemar's Test for Paired Proportions

`mcnemar_test()` tests whether paired proportions have changed between
two dichotomous measurements. Use this for before/after comparisons of
categorical outcomes.

Think of it as:

- A paired comparison test for binary (yes/no) data

- The categorical equivalent of a paired t-test

- Tests whether the proportion of "changers" is symmetric

The test tells you:

- Whether paired proportions changed significantly

- Both asymptotic and exact p-values for maximum reliability

- The number of discordant pairs (who actually changed)

## Usage

``` r
mcnemar_test(data, var1, var2, weights = NULL, correct = TRUE, ...)
```

## Arguments

- data:

  Your survey data (data frame or tibble)

- var1:

  First dichotomous variable (0/1 or two-level factor)

- var2:

  Second dichotomous variable (0/1 or two-level factor)

- weights:

  Optional survey weights for population-representative results

- correct:

  Logical, whether to apply continuity correction (default: TRUE)

- ...:

  Additional arguments (currently unused)

## Value

Test results showing whether paired proportions changed, including:

- McNemar chi-square statistic (with continuity correction)

- Asymptotic p-value

- Exact binomial p-value (two-sided)

- 2x2 contingency table

- Discordant pair counts (b and c)

## Details

### Understanding the Results

**P-value**: If p \< 0.05, the paired proportions are significantly
different

- p \< 0.001: Very strong evidence of change

- p \< 0.01: Strong evidence of change

- p \< 0.05: Moderate evidence of change

- p \>= 0.05: No significant change found

**Exact vs Asymptotic**: The exact binomial p-value is more reliable for
small samples. For large samples, both p-values will be very similar.

**Discordant Pairs**: Only pairs where the two measurements differ (b
and c) contribute to the test. If b approximately equals c, there is no
evidence of systematic change.

### When to Use This

Use McNemar's test when:

- You have paired or matched binary data

- You're comparing before/after proportions

- Both variables must be dichotomous (exactly 2 levels)

### The McNemar Statistic

For a 2x2 table with discordant cells b and c: \$\$\chi^2 = \frac{(\|b -
c\| - 1)^2}{b + c}\$\$ (with continuity correction)

The exact test uses a binomial test on the discordant pairs.

### Relationship to Other Tests

- For unpaired categorical data: Use
  [`chi_square()`](https://YannickDiehl.github.io/mariposa/dev/reference/chi_square.md)
  instead

- For paired ordinal/continuous data: Use
  [`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/wilcoxon_test.md)
  instead

- For paired data with more than 2 levels: Consider the Bowker test of
  symmetry

### SPSS Equivalent

SPSS: `CROSSTABS /STATISTICS=MCNEMAR`

## References

McNemar, Q. (1947). Note on the sampling error of the difference between
correlated proportions or percentages. *Psychometrika*, 12(2), 153-157.

## See also

[`chi_square`](https://YannickDiehl.github.io/mariposa/dev/reference/chi_square.md)
for independence tests.

[`wilcoxon_test`](https://YannickDiehl.github.io/mariposa/dev/reference/wilcoxon_test.md)
for paired non-parametric tests on ordinal data.

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
[`oneway_anova()`](https://YannickDiehl.github.io/mariposa/dev/reference/oneway_anova.md),
[`t_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/t_test.md),
[`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/wilcoxon_test.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Create dichotomous variables
test_data <- survey_data %>%
  mutate(
    trust_gov_high = as.integer(trust_government >= 4),
    trust_media_high = as.integer(trust_media >= 4)
  )

# McNemar test
test_data %>%
  mcnemar_test(var1 = trust_gov_high, var2 = trust_media_high)
#> McNemar Test Results
#> --------------------
#> 
#> - Variable 1: trust_gov_high
#> - Variable 2: trust_media_high
#> 
#> 2x2 Contingency Table:
#> ---------------------------------------- 
#>    v2
#> v1     0    1
#>   0 1342  338
#>   1  448   99
#> ---------------------------------------- 
#> 
#> Test Results:
#> ----------------------------------------- 
#>  Chi-Sq (cc) p (asymp) p (exact)    N Sig 
#>       15.116     <.001     <.001 2227 *** 
#> ----------------------------------------- 
#> 
#> Discordant pairs: b = 338, c = 448
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05

# Grouped analysis
test_data %>%
  group_by(region) %>%
  mcnemar_test(var1 = trust_gov_high, var2 = trust_media_high)
#> McNemar Test Results
#> --------------------
#> 
#> - Variable 1: trust_gov_high
#> - Variable 2: trust_media_high
#> 
#> 
#> Group: region = East
#> --------------------
#> ----------------------------------- 
#>  Chi-Sq p (asymp) p (exact)   N Sig 
#>   8.255    0.0041    0.0039 435  ** 
#> ----------------------------------- 
#> 
#> 
#> Group: region = West
#> --------------------
#> ------------------------------------ 
#>  Chi-Sq p (asymp) p (exact)    N Sig 
#>   8.242    0.0041    0.0041 1792  ** 
#> ------------------------------------ 
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
