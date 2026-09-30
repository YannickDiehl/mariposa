# Compare Groups Across Multiple Factors: Factorial ANOVA

`factorial_anova()` tests whether group means differ across two or more
factors simultaneously, including their interactions. It performs a
factorial (two-way, three-way) ANOVA using Type III Sum of Squares,
matching SPSS UNIANOVA output.

Think of it as:

- Testing multiple grouping variables at once

- Detecting interaction effects (do factor combinations matter?)

- An extension of one-way ANOVA to multiple factors

The test tells you:

- Whether each factor has a main effect on the outcome

- Whether factors interact (the effect of one depends on the other)

- How much variance each factor explains (partial eta squared)

## Usage

``` r
factorial_anova(data, dv, between, weights = NULL, ss_type = 3)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- dv:

  The numeric dependent variable to analyze (unquoted)

- between:

  Character vector or unquoted variable names specifying the
  between-subjects factors (2-3 factors). These must be categorical
  variables (factor, character, or labelled numeric).

- weights:

  Optional survey weights for population-representative results. Give a
  column name (unquoted or as a string), an expression such as
  `sampling_weight * 2`, or a numeric vector with one weight per row.

- ss_type:

  Deprecated; only 3 (Type III, the SPSS default) is implemented.
  Passing 2 issues a warning and computes Type III.

## Value

An object of class `"factorial_anova"` containing:

- anova_table:

  Tibble with Source, SS, df, MS, F, p, Partial Eta Squared

- descriptives:

  Tibble with cell means, SDs, and Ns for each factor combination

- levene_test:

  Tibble with Levene's test results (F, df1, df2, p)

- r_squared:

  R-squared and Adjusted R-squared

- model:

  The underlying model object for S3 dispatch

- call_info:

  List with metadata (dv, factors, weighted, n_total, n_missing)

Use [`summary()`](https://rdrr.io/r/base/summary.html) for the full
SPSS-style output with toggleable sections. For data grouped with
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html), one
ANOVA is computed per group: the tables carry the group keys as leading
columns, `group_results` holds the complete result of each group
(`NULL`, with a warning, for a group that cannot be analysed), and
[`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html),
[`tukey_test()`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md),
[`scheffe_test()`](https://YannickDiehl.github.io/mariposa/reference/scheffe_test.md)
and
[`levene_test()`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md)
report per group.

## Details

### Understanding the Results

**Main Effects**: Does each factor independently affect the outcome?

- Significant main effect = group means differ for that factor

- Example: Education affects income regardless of gender

**Interaction Effects**: Does the effect of one factor depend on
another?

- Significant interaction = the pattern differs across factor
  combinations

- Example: The gender gap in income varies by education level

**Partial Eta Squared** (Effect Size):

- Less than 0.01: Negligible

- 0.01 to 0.06: Small

- 0.06 to 0.14: Medium

- 0.14 or greater: Large

### Type III Sum of Squares

Type III SS tests each effect after adjusting for all other effects.
This is the standard in SPSS and recommended for unbalanced designs
(unequal cell sizes). It uses orthogonal contrasts (contr.sum)
internally.

### When to Use This

Use factorial ANOVA when:

- You have one numeric outcome variable

- You have 2-3 categorical grouping factors

- You want to test main effects AND interactions

- Your data is approximately normally distributed within cells

### What Comes Next?

If the ANOVA is significant:

1.  Check which effects are significant (main effects vs. interactions)

2.  Use
    [`tukey_test()`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md)
    for post-hoc comparisons on main effects

3.  Examine cell means to interpret interaction patterns

4.  Consider effect sizes for practical significance

## References

Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences
(2nd ed.). Lawrence Erlbaum Associates.

IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.

Maxwell, S. E., & Delaney, H. D. (2004). Designing Experiments and
Analyzing Data (2nd ed.). Lawrence Erlbaum Associates.

## See also

[`oneway_anova`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)
for single-factor ANOVA.

[`tukey_test`](https://YannickDiehl.github.io/mariposa/reference/tukey_test.md)
for post-hoc pairwise comparisons.

[`levene_test`](https://YannickDiehl.github.io/mariposa/reference/levene_test.md)
for testing homogeneity of variances.

[`ancova`](https://YannickDiehl.github.io/mariposa/reference/ancova.md)
for ANOVA with covariates.

[`summary.factorial_anova`](https://YannickDiehl.github.io/mariposa/reference/summary.factorial_anova.md)
for detailed output with toggleable sections.

Other hypothesis_tests:
[`ancova()`](https://YannickDiehl.github.io/mariposa/reference/ancova.md),
[`binomial_test()`](https://YannickDiehl.github.io/mariposa/reference/binomial_test.md),
[`chi_square()`](https://YannickDiehl.github.io/mariposa/reference/chi_square.md),
[`chisq_gof()`](https://YannickDiehl.github.io/mariposa/reference/chisq_gof.md),
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

# Two-way ANOVA: income by gender and education
survey_data %>%
  factorial_anova(dv = income, between = c(gender, education))
#> Factorial ANOVA (2-Way): income by gender, education, N = 2186
#>   gender:           F(1, 2178) = 0.098, p = 0.755, eta2p = 0.000
#>   education:        F(3, 2178) = 463.521, p < 0.001 ***, eta2p = 0.390
#>   gender:education: F(3, 2178) = 0.399, p = 0.754, eta2p = 0.001
#> Use summary() for detailed output.

# Two-way ANOVA with weights
survey_data %>%
  factorial_anova(dv = life_satisfaction, between = c(gender, region),
                  weights = sampling_weight)
#> Factorial ANOVA (2-Way): life_satisfaction by gender, region [Weighted], N = 2421
#>   gender:        F(1, 2417) = 0.008, p = 0.930, eta2p = 0.000
#>   region:        F(1, 2417) = 0.001, p = 0.979, eta2p = 0.000
#>   gender:region: F(1, 2417) = 1.642, p = 0.200, eta2p = 0.001
#> Use summary() for detailed output.

# Separate ANOVA for each region
survey_data %>%
  group_by(region) %>%
  factorial_anova(dv = life_satisfaction, between = c(gender, education))
#> Factorial ANOVA (2-Way): life_satisfaction by gender, education
#> [region = East] N = 465
#>   gender:           F(1, 457) = 0.340, p = 0.560, eta2p = 0.001
#>   education:        F(3, 457) = 6.877, p < 0.001 ***, eta2p = 0.043
#>   gender:education: F(3, 457) = 0.060, p = 0.981, eta2p = 0.000
#> [region = West] N = 1956
#>   gender:           F(1, 1948) = 3.631, p = 0.057, eta2p = 0.002
#>   education:        F(3, 1948) = 61.144, p < 0.001 ***, eta2p = 0.086
#>   gender:education: F(3, 1948) = 1.107, p = 0.345, eta2p = 0.002
#> Use summary() for detailed output.

# Three-way ANOVA
survey_data %>%
  factorial_anova(dv = income, between = c(gender, region, education))
#> Factorial ANOVA (3-Way): income by gender, region, education, N = 2186
#>   gender:                  F(1, 2170) = 2.976, p = 0.085, eta2p = 0.001
#>   region:                  F(1, 2170) = 0.056, p = 0.812, eta2p = 0.000
#>   education:               F(3, 2170) = 279.309, p < 0.001 ***, eta2p = 0.279
#>   gender:region:           F(1, 2170) = 5.769, p = 0.016 *, eta2p = 0.003
#>   gender:education:        F(3, 2170) = 0.597, p = 0.617, eta2p = 0.001
#>   region:education:        F(3, 2170) = 0.990, p = 0.396, eta2p = 0.001
#>   gender:region:education: F(3, 2170) = 3.889, p = 0.009 **, eta2p = 0.005
#> Use summary() for detailed output.

# Follow up with post-hoc tests
result <- survey_data %>%
  factorial_anova(dv = income, between = c(gender, education))
result %>% tukey_test()
#> Tukey HSD Post-Hoc Test by gender x education
#>   gender: 1 comparison, 0 significant (p < .05)
#>   education: 6 comparisons, 6 significant (p < .05)
#> Use summary() for the full comparison table.
result %>% levene_test()
#> Levene's Test: income by gender * education
#>   F(7, 2178) = 44.988, p < 0.001 ***, variances unequal
#> Use summary() for detailed output.

# --- Three-layer output ---
result              # compact overview
#> Factorial ANOVA (2-Way): income by gender, education, N = 2186
#>   gender:           F(1, 2178) = 0.098, p = 0.755, eta2p = 0.000
#>   education:        F(3, 2178) = 463.521, p < 0.001 ***, eta2p = 0.390
#>   gender:education: F(3, 2178) = 0.399, p = 0.754, eta2p = 0.001
#> Use summary() for detailed output.
summary(result)     # full detailed output with all sections
#> Factorial ANOVA (2-Way ANOVA) Results
#> -------------------------------------
#> 
#> - Dependent variable: income
#> - Factors: gender x education
#> - Sum of squares: Type III
#> - N (complete cases): 2186
#> - Missing: 314
#> 
#> Tests of Between-Subjects Effects
#>   --------------------------------------------------------------------------------------------------------------
#>   Source              Type III Sum of Squares    df      Mean Square          F    Sig  Partial Eta Squared     
#>   --------------------------------------------------------------------------------------------------------------
#>   Corrected Model              1754652069.847     7    250664581.407    199.909  <.001                0.391  ***
#>   Intercept                   32212607064.510     1  32212607064.510  25690.075  <.001                0.922  ***
#>   gender                           122637.601     1       122637.601      0.098   .755                0.000     
#>   education                    1743617622.524     3    581205874.175    463.521  <.001                0.390  ***
#>   gender * education              1499440.759     3       499813.586      0.399   .754                0.001     
#>   Error                        2730979096.667  2178      1253893.066                                            
#>   Total                       35290790000.000  2186                                                             
#>   Corrected Total              4485631166.514  2185                                                             
#>   --------------------------------------------------------------------------------------------------------------
#> R Squared = 0.391 (Adjusted R Squared = 0.389)
#> 
#> Descriptive Statistics
#>   -------------------------------------------------------------
#>   gender  education                   Mean  Std. Deviation    N
#>   -------------------------------------------------------------
#>   Male    Basic Secondary         2803.429         774.959  350
#>   Male    Intermediate Secondary  3574.089         996.649  247
#>   Male    Academic Secondary      4246.454        1180.779  282
#>   Male    University              5318.563        1718.805  167
#>   Female  Basic Secondary         2718.701         795.831  385
#>   Female  Intermediate Secondary  3607.641         996.214  301
#>   Female  Academic Secondary      4200.376        1178.118  266
#>   Female  University              5353.723        1612.265  188
#>   -------------------------------------------------------------
#> 
#> Levene's Test of Equality of Error Variances
#>   F(7, 2178) = 44.988, p < 0.001 ***
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
summary(result, descriptives = FALSE)  # hide the cell descriptives
#> Factorial ANOVA (2-Way ANOVA) Results
#> -------------------------------------
#> 
#> - Dependent variable: income
#> - Factors: gender x education
#> - Sum of squares: Type III
#> - N (complete cases): 2186
#> - Missing: 314
#> 
#> Tests of Between-Subjects Effects
#>   --------------------------------------------------------------------------------------------------------------
#>   Source              Type III Sum of Squares    df      Mean Square          F    Sig  Partial Eta Squared     
#>   --------------------------------------------------------------------------------------------------------------
#>   Corrected Model              1754652069.847     7    250664581.407    199.909  <.001                0.391  ***
#>   Intercept                   32212607064.510     1  32212607064.510  25690.075  <.001                0.922  ***
#>   gender                           122637.601     1       122637.601      0.098   .755                0.000     
#>   education                    1743617622.524     3    581205874.175    463.521  <.001                0.390  ***
#>   gender * education              1499440.759     3       499813.586      0.399   .754                0.001     
#>   Error                        2730979096.667  2178      1253893.066                                            
#>   Total                       35290790000.000  2186                                                             
#>   Corrected Total              4485631166.514  2185                                                             
#>   --------------------------------------------------------------------------------------------------------------
#> R Squared = 0.391 (Adjusted R Squared = 0.389)
#> 
#> Levene's Test of Equality of Error Variances
#>   F(7, 2178) = 44.988, p < 0.001 ***
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
