# Test If Two Categories Are Related

`chi_square()` helps you discover if two categorical variables are
related or independent. For example, is education level related to
voting preference? Or are they independent of each other?

The test tells you:

- Whether the relationship is statistically significant

- How strong the relationship is (effect sizes)

- What patterns exist in your data

## Usage

``` r
chi_square(data, ..., weights = NULL, correct = FALSE)

phi(data, ..., weights = NULL, correct = FALSE)

cramers_v(data, ..., weights = NULL, correct = FALSE)

goodman_gamma(data, ..., weights = NULL, correct = FALSE)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- ...:

  Two categorical variables to test (e.g., gender, region)

- weights:

  Optional survey weights for population-representative results

- correct:

  Apply continuity correction for small samples? (Default: FALSE)

## Value

Test results showing whether the variables are related, including:

- Chi-squared statistic and p-value

- Observed vs expected frequencies

- Effect sizes to measure relationship strength Use
  [`summary()`](https://rdrr.io/r/base/summary.html) for the full
  SPSS-style output with toggleable sections.

## Details

### Understanding the Results

**P-value**: If p \< 0.05, the variables are likely related (not
independent)

- p \< 0.001: Very strong evidence of relationship

- p \< 0.01: Strong evidence of relationship

- p \< 0.05: Moderate evidence of relationship

- p ≥ 0.05: No significant relationship found

**Effect Sizes** (How strong is the relationship?):

- **Cramer's V**: Works for any table size (0 = no relationship, 1 =
  perfect relationship)

  - \< 0.1: Negligible relationship

  - 0.1-0.3: Small relationship

  - 0.3-0.5: Medium relationship

  - 0.5 or higher: Large relationship

- **Phi**: Only for 2x2 tables (similar interpretation as Cramer's V)

- **Gamma**: For ordinal data (-1 to +1, shows direction of
  relationship)

### When to Use This

Use chi-squared test when:

- Both variables are categorical (gender, region, education level, etc.)

- You want to know if they're related or independent

- You have at least 5 observations in most cells

### Reading the Frequency Tables

- **Observed**: What you actually found in your data

- **Expected**: What you'd expect if variables were independent

- Large differences suggest a relationship exists

### Tips for Success

- Check that most cells have at least 5 observations

- Use weights for population estimates

- Look at both significance (p-value) and strength (effect sizes)

- Consider using crosstab() for detailed percentage breakdowns

## References

Pearson, K. (1900). On the criterion that a given system of deviations
from the probable in the case of a correlated system of variables is
such that it can be reasonably supposed to have arisen from random
sampling. *Philosophical Magazine*, 50(302), 157–175.

Cramer, H. (1946). *Mathematical Methods of Statistics*. Princeton
University Press.

IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.

## See also

[`chisq.test`](https://rdrr.io/r/stats/chisq.test.html) for the base R
chi-squared test.

[`crosstab`](https://YannickDiehl.github.io/mariposa/reference/crosstab.md)
for detailed cross-tabulation tables.

[`frequency`](https://YannickDiehl.github.io/mariposa/reference/frequency.md)
for single-variable frequency tables.

[`summary.chi_square`](https://YannickDiehl.github.io/mariposa/reference/summary.chi_square.md)
for detailed output with toggleable sections.

Other hypothesis_tests:
[`ancova()`](https://YannickDiehl.github.io/mariposa/reference/ancova.md),
[`binomial_test()`](https://YannickDiehl.github.io/mariposa/reference/binomial_test.md),
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

# Basic chi-squared test for independence
survey_data %>% chi_square(gender, region)
#> Chi-Squared Test: gender × region
#>   chi2(1) = 0.415, p = 0.519 , V = 0.013 (neglig.), N = 2500

# With weights
survey_data %>% chi_square(gender, education, weights = sampling_weight)
#> Chi-Squared Test: gender × education [Weighted]
#>   chi2(3) = 4.403, p = 0.221 , V = 0.042 (neglig.), N = 2517

# Grouped analysis
survey_data %>% 
  group_by(region) %>% 
  chi_square(gender, employment)
#> [region = 1]
#> Chi-Squared Test: gender × employment
#>   chi2(4) = 5.970, p = 0.201 , V = 0.111 (small), N = 485
#> [region = 2]
#> Chi-Squared Test: gender × employment
#>   chi2(4) = 4.166, p = 0.384 , V = 0.045 (neglig.), N = 2015

# With continuity correction
survey_data %>% chi_square(gender, region, correct = TRUE)
#> Chi-Squared Test: gender × region
#>   chi2(1) = 0.353, p = 0.553 , V = 0.012 (neglig.), N = 2500

# --- Three-layer output ---
result <- chi_square(survey_data, gender, education)
result              # compact one-line overview
#> Chi-Squared Test: gender × education
#>   chi2(3) = 3.470, p = 0.325 , V = 0.037 (neglig.), N = 2500
summary(result)     # full detailed output with all sections
#> 
#> Chi-Squared Test of Independence 
#> ---------------------------------
#> 
#> - Variables: gender × education
#> 
#> Observed Frequencies:
#>         education
#> gender   Basic Secondary Intermediate Seco... Academic Secondary University
#>   Male               401                  289                320        184
#>   Female             440                  340                311        215
#> 
#> Expected Frequencies:
#>         education
#> gender   Basic Secondary Intermediate Seco... Academic Secondary University
#>   Male           401.662               300.41            301.366    190.562
#>   Female         439.338               328.59            329.634    208.438
#> 
#> Chi-Squared Test Results:
#> -------------------------------------------------- 
#>  Chi_squared df p_value sig
#>         3.47  3   0.325    
#> -------------------------------------------------- 
#> 
#> Effect Sizes:
#> ---------------------------------------------------------------------- 
#>     Measure  Value p_value sig Interpretation
#>  Cramer's V  0.037   0.325            Neglig.
#>       Gamma -0.008   0.850               Weak
#> ---------------------------------------------------------------------- 
#> Table size: 2×4 | N = 2500
#> Note: Phi coefficient only shown for 2x2 tables
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
summary(result, cross_tabulation = FALSE)  # hide cross-tabulation
#> 
#> Chi-Squared Test of Independence 
#> ---------------------------------
#> 
#> - Variables: gender × education
#> 
#> Observed Frequencies:
#>         education
#> gender   Basic Secondary Intermediate Seco... Academic Secondary University
#>   Male               401                  289                320        184
#>   Female             440                  340                311        215
#> 
#> Expected Frequencies:
#>         education
#> gender   Basic Secondary Intermediate Seco... Academic Secondary University
#>   Male           401.662               300.41            301.366    190.562
#>   Female         439.338               328.59            329.634    208.438
#> 
#> Chi-Squared Test Results:
#> -------------------------------------------------- 
#>  Chi_squared df p_value sig
#>         3.47  3   0.325    
#> -------------------------------------------------- 
#> 
#> Effect Sizes:
#> ---------------------------------------------------------------------- 
#>     Measure  Value p_value sig Interpretation
#>  Cramer's V  0.037   0.325            Neglig.
#>       Gamma -0.008   0.850               Weak
#> ---------------------------------------------------------------------- 
#> Table size: 2×4 | N = 2500
#> Note: Phi coefficient only shown for 2x2 tables
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
