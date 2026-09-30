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
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- ...:

  Two categorical variables to test (e.g., gender, region)

- weights:

  Optional survey weights for population-representative results. Give a
  column name (unquoted or as a string), an expression such as
  `sampling_weight * 2`, or a numeric vector with one weight per row.

- correct:

  Apply Yates' continuity correction to a 2x2 table? (Default: FALSE).
  The corrected statistic is reported as `chi_squared`; as in SPSS
  (which prints "Pearson Chi-Square" and "Continuity Correction" side by
  side), the uncorrected Pearson value is kept in `pearson_chi_squared`
  and Phi / Cramer's V are always computed from it.

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

- p \>= 0.05: No significant relationship found

**Effect Sizes** (How strong is the relationship?):

- **Cramer's V**: Works for any table size (0 = no relationship, 1 =
  perfect relationship)

  - \< 0.1: Negligible relationship

  - 0.1-0.3: Small relationship

  - 0.3-0.5: Medium relationship

  - 0.5 or higher: Large relationship

- **Phi**: sqrt(chi-squared / N). Reported for every table, as SPSS
  does; in a 2x2 table it equals Cramer's V, in larger tables it can
  exceed 1 (use Cramer's V there)

- **Gamma**: For two ordinal variables (-1 to +1, shows the direction of
  the relationship). Shown by
  [`summary()`](https://rdrr.io/r/base/summary.html) only when both
  variables are ordered factors or numeric; for nominal variables its
  sign depends on the arbitrary category order.
  [`goodman_gamma()`](https://YannickDiehl.github.io/mariposa/reference/phi.md)
  always computes it.

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
#> Chi-Squared Test: gender x region
#>   chi2(1) = 0.415, p = 0.519, V = 0.013 (negligible), N = 2500
#> Use summary() for detailed output.

# With weights
survey_data %>% chi_square(gender, education, weights = sampling_weight)
#> Chi-Squared Test: gender x education [Weighted]
#>   chi2(3) = 4.403, p = 0.221, V = 0.042 (negligible), N = 2517
#> Use summary() for detailed output.

# Grouped analysis
survey_data %>% 
  group_by(region) %>% 
  chi_square(gender, employment)
#> [region = East]
#> Chi-Squared Test: gender x employment
#>   chi2(4) = 5.970, p = 0.201, V = 0.111 (small), N = 485
#> [region = West]
#> Chi-Squared Test: gender x employment
#>   chi2(4) = 4.166, p = 0.384, V = 0.045 (negligible), N = 2015
#> Use summary() for detailed output.

# With continuity correction
survey_data %>% chi_square(gender, region, correct = TRUE)
#> Chi-Squared Test: gender x region
#>   chi2(1) = 0.353 (continuity-corrected), p = 0.553, V = 0.013 (negligible), N = 2500
#> Use summary() for detailed output.

# --- Three-layer output ---
result <- chi_square(survey_data, gender, education)
result              # compact one-line overview
#> Chi-Squared Test: gender x education
#>   chi2(3) = 3.470, p = 0.325, V = 0.037 (negligible), N = 2500
#> Use summary() for detailed output.
summary(result)     # full detailed output with all sections
#> 
#> Chi-Squared Test of Independence
#> --------------------------------
#> 
#> - Variables: gender x education
#> 
#> Observed Frequencies:
#>         education
#> gender   Basic Secondary Intermediate Secondary Academic Secondary University
#>   Male               401                    289                320        184
#>   Female             440                    340                311        215
#> 
#> Expected Frequencies:
#>         education
#> gender   Basic Secondary Intermediate Secondary Academic Secondary University
#>   Male           401.662                300.410            301.366    190.562
#>   Female         439.338                328.590            329.634    208.438
#> 
#> Chi-Squared Test Results:
#> -----------------------------------------
#>                     Value  df  p value   
#> -----------------------------------------
#> Pearson Chi-Square  3.470   3     .325   
#> -----------------------------------------
#> 
#> Effect Sizes:
#> ---------------------------------------------
#> Measure     Value  p value     Interpretation
#> ---------------------------------------------
#> Phi         0.037     .325                   
#> Cramer's V  0.037     .325         Negligible
#> ---------------------------------------------
#> Table size: 2 x 4 | N = 2500
#> Note: Gamma is shown for two ordinal variables (ordered factor or numeric) only.
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
summary(result, cross_tabulation = FALSE)  # hide cross-tabulation
#> 
#> Chi-Squared Test of Independence
#> --------------------------------
#> 
#> - Variables: gender x education
#> 
#> Observed Frequencies:
#>         education
#> gender   Basic Secondary Intermediate Secondary Academic Secondary University
#>   Male               401                    289                320        184
#>   Female             440                    340                311        215
#> 
#> Expected Frequencies:
#>         education
#> gender   Basic Secondary Intermediate Secondary Academic Secondary University
#>   Male           401.662                300.410            301.366    190.562
#>   Female         439.338                328.590            329.634    208.438
#> 
#> Chi-Squared Test Results:
#> -----------------------------------------
#>                     Value  df  p value   
#> -----------------------------------------
#> Pearson Chi-Square  3.470   3     .325   
#> -----------------------------------------
#> 
#> Effect Sizes:
#> ---------------------------------------------
#> Measure     Value  p value     Interpretation
#> ---------------------------------------------
#> Phi         0.037     .325                   
#> Cramer's V  0.037     .325         Negligible
#> ---------------------------------------------
#> Table size: 2 x 4 | N = 2500
#> Note: Gamma is shown for two ordinal variables (ordered factor or numeric) only.
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
