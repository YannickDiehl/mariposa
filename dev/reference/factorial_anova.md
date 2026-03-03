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

  Optional survey weights for population-representative results
  (unquoted variable name)

- ss_type:

  Type of Sum of Squares: 3 (default, SPSS standard) or 2. Type III is
  recommended for unbalanced designs.

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
SPSS-style output with toggleable sections.

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
    [`tukey_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/tukey_test.md)
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

[`oneway_anova`](https://YannickDiehl.github.io/mariposa/dev/reference/oneway_anova.md)
for single-factor ANOVA.

[`tukey_test`](https://YannickDiehl.github.io/mariposa/dev/reference/tukey_test.md)
for post-hoc pairwise comparisons.

[`levene_test`](https://YannickDiehl.github.io/mariposa/dev/reference/levene_test.md)
for testing homogeneity of variances.

[`ancova`](https://YannickDiehl.github.io/mariposa/dev/reference/ancova.md)
for ANOVA with covariates.

[`summary.factorial_anova`](https://YannickDiehl.github.io/mariposa/dev/reference/summary.factorial_anova.md)
for detailed output with toggleable sections.

Other hypothesis_tests:
[`ancova()`](https://YannickDiehl.github.io/mariposa/dev/reference/ancova.md),
[`binomial_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/binomial_test.md),
[`chi_square()`](https://YannickDiehl.github.io/mariposa/dev/reference/chi_square.md),
[`chisq_gof()`](https://YannickDiehl.github.io/mariposa/dev/reference/chisq_gof.md),
[`fisher_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/fisher_test.md),
[`friedman_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/friedman_test.md),
[`kruskal_wallis()`](https://YannickDiehl.github.io/mariposa/dev/reference/kruskal_wallis.md),
[`mann_whitney()`](https://YannickDiehl.github.io/mariposa/dev/reference/mann_whitney.md),
[`mcnemar_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/mcnemar_test.md),
[`oneway_anova()`](https://YannickDiehl.github.io/mariposa/dev/reference/oneway_anova.md),
[`t_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/t_test.md),
[`wilcoxon_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/wilcoxon_test.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Two-way ANOVA: income by gender and education
survey_data %>%
  factorial_anova(dv = income, between = c(gender, education))
#> Factorial ANOVA (2-Way): income by gender, education
#>   gender:           F(1, 2178) = 0.098, p = 0.755 , eta2p = 0.000
#>   education:        F(3, 2178) = 463.521, p < 0.001 ***, eta2p = 0.390
#>   gender:education: F(3, 2178) = 0.399, p = 0.754 , eta2p = 0.001, N = 2186

# Two-way ANOVA with weights
survey_data %>%
  factorial_anova(dv = life_satisfaction, between = c(gender, region),
                  weights = sampling_weight)
#> Factorial ANOVA (2-Way): life_satisfaction by gender, region [Weighted]
#>   gender:        F(1, 2417) = 0.008, p = 0.930 , eta2p = 0.000
#>   region:        F(1, 2417) = 0.001, p = 0.979 , eta2p = 0.000
#>   gender:region: F(1, 2417) = 1.642, p = 0.200 , eta2p = 0.001, N = 2421

# Three-way ANOVA
survey_data %>%
  factorial_anova(dv = income, between = c(gender, region, education))
#> Factorial ANOVA (3-Way): income by gender, region, education
#>   gender:                  F(1, 2170) = 2.976, p = 0.085 , eta2p = 0.001
#>   region:                  F(1, 2170) = 0.056, p = 0.812 , eta2p = 0.000
#>   education:               F(3, 2170) = 279.309, p < 0.001 ***, eta2p = 0.279
#>   gender:region:           F(1, 2170) = 5.769, p = 0.016 *, eta2p = 0.003
#>   gender:education:        F(3, 2170) = 0.597, p = 0.617 , eta2p = 0.001
#>   region:education:        F(3, 2170) = 0.990, p = 0.396 , eta2p = 0.001
#>   gender:region:education: F(3, 2170) = 3.889, p = 0.009 **, eta2p = 0.005, N = 2186

# Follow up with post-hoc tests
result <- survey_data %>%
  factorial_anova(dv = income, between = c(gender, education))
result %>% tukey_test()
#> Tukey HSD Post-Hoc Test Results
#> -------------------------------
#> 
#> - Dependent variable: income
#> - Factors: gender x education
#> - Confidence level: 95.0%
#>   Family-wise error rate controlled using Tukey HSD
#> 
#> 
#> --- Factor: gender ---
#> 
#> ---------------------------------------------------- 
#>   Comparison Difference Lower CI Upper CI p-value Sig
#>  Female-Male     -42.32 -162.639   77.999    0.49    
#> ---------------------------------------------------- 
#> 
#> --- Factor: education ---
#> 
#> ---------------------------------------------------------------------------------- 
#>                                 Comparison Difference Lower CI Upper CI p-value
#>     Intermediate Secondary-Basic Secondary    833.471  671.079  995.862   <.001
#>         Academic Secondary-Basic Secondary   1465.040 1302.648 1627.432   <.001
#>                 University-Basic Secondary   2578.135 2392.167 2764.104   <.001
#>  Academic Secondary-Intermediate Secondary    631.569  457.746  805.393   <.001
#>          University-Intermediate Secondary   1744.665 1548.634 1940.695   <.001
#>              University-Academic Secondary   1113.096  917.065 1309.126   <.001
#>  Sig
#>  ***
#>  ***
#>  ***
#>  ***
#>  ***
#>  ***
#> ---------------------------------------------------------------------------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - Positive differences: First group > Second group
#> - Negative differences: First group < Second group
#> - Confidence intervals not containing 0 indicate significant differences
#> - p-values are adjusted for multiple comparisons (family-wise error control)
result %>% levene_test()
#> 
#> Levene's Test for Homogeneity of Variance 
#> ------------------------------------------
#> 
#> - Grouping variable: gender * education
#> - Center: mean
#> 
#> 
#> --- income ---
#> 
#> Levene's Test Results:
#> ----------------------------------------------------------- 
#>  Variable F_statistic df1  df2 p_value sig        Conclusion
#>    income      44.988   7 2178       0 *** Variances unequal
#> ----------------------------------------------------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - p > 0.05: Variances are homogeneous (equal variances assumed)
#> - p <= 0.05: Variances are heterogeneous (equal variances NOT assumed)
#> 
#> Recommendation based on Levene test:
#> - Use Welch's t-test (unequal variances)

# --- Three-layer output ---
result              # compact overview
#> Factorial ANOVA (2-Way): income by gender, education
#>   gender:           F(1, 2178) = 0.098, p = 0.755 , eta2p = 0.000
#>   education:        F(3, 2178) = 463.521, p < 0.001 ***, eta2p = 0.390
#>   gender:education: F(3, 2178) = 0.399, p = 0.754 , eta2p = 0.001, N = 2186
summary(result)     # full detailed output with all sections
#> Factorial ANOVA (2-Way ANOVA) Results
#> -------------------------------------
#> 
#> - Dependent variable: income
#> - Factors: gender x education
#> - Type III Sum of Squares: Type 3
#> - N (complete cases): 2186
#> - Missing: 314
#> 
#> Tests of Between-Subjects Effects
#> ------------------------------------------------------------------ 
#>  Source             Type III SS  df   Mean Square  F         Sig. 
#>  Corrected Model    1.754652e+09    7 2.506646e+08   199.909 <.001
#>  Intercept          3.221261e+10    1 3.221261e+10 25690.075 <.001
#>  gender             1.226376e+05    1 1.226376e+05     0.098 0.755
#>  education          1.743618e+09    3 5.812059e+08   463.521 <.001
#>  gender * education 1.499441e+06    3 4.998136e+05     0.399 0.754
#>  Error              2.730979e+09 2178 1.253893e+06                
#>  Total              3.529079e+10 2186                             
#>  Corrected Total    4.485631e+09 2185                             
#>  Partial Eta Sq    
#>  0.391          ***
#>  0.922          ***
#>  0.000             
#>  0.390          ***
#>  0.001             
#>                    
#>                    
#>                    
#> ------------------------------------------------------------------ 
#> R Squared = 0.391 (Adjusted R Squared = 0.389)
#> 
#> Descriptive Statistics
#> ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#>  gender education              Mean    Std. Deviation N  
#>  Male   Basic Secondary        2803.43  774.959       350
#>  Male   Intermediate Secondary 3574.09  996.649       247
#>  Male   Academic Secondary     4246.45 1180.779       282
#>  Male   University             5318.56 1718.805       167
#>  Female Basic Secondary        2718.70  795.831       385
#>  Female Intermediate Secondary 3607.64  996.214       301
#>  Female Academic Secondary     4200.38 1178.118       266
#>  Female University             5353.72 1612.265       188
#> ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#> 
#> Levene's Test of Equality of Error Variances
#>   F(7, 2178) = 44.988, p = <.001
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
summary(result, marginal_means = FALSE)  # hide estimated marginal means
#> Factorial ANOVA (2-Way ANOVA) Results
#> -------------------------------------
#> 
#> - Dependent variable: income
#> - Factors: gender x education
#> - Type III Sum of Squares: Type 3
#> - N (complete cases): 2186
#> - Missing: 314
#> 
#> Tests of Between-Subjects Effects
#> ------------------------------------------------------------------ 
#>  Source             Type III SS  df   Mean Square  F         Sig. 
#>  Corrected Model    1.754652e+09    7 2.506646e+08   199.909 <.001
#>  Intercept          3.221261e+10    1 3.221261e+10 25690.075 <.001
#>  gender             1.226376e+05    1 1.226376e+05     0.098 0.755
#>  education          1.743618e+09    3 5.812059e+08   463.521 <.001
#>  gender * education 1.499441e+06    3 4.998136e+05     0.399 0.754
#>  Error              2.730979e+09 2178 1.253893e+06                
#>  Total              3.529079e+10 2186                             
#>  Corrected Total    4.485631e+09 2185                             
#>  Partial Eta Sq    
#>  0.391          ***
#>  0.922          ***
#>  0.000             
#>  0.390          ***
#>  0.001             
#>                    
#>                    
#>                    
#> ------------------------------------------------------------------ 
#> R Squared = 0.391 (Adjusted R Squared = 0.389)
#> 
#> Descriptive Statistics
#> ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#>  gender education              Mean    Std. Deviation N  
#>  Male   Basic Secondary        2803.43  774.959       350
#>  Male   Intermediate Secondary 3574.09  996.649       247
#>  Male   Academic Secondary     4246.45 1180.779       282
#>  Male   University             5318.56 1718.805       167
#>  Female Basic Secondary        2718.70  795.831       385
#>  Female Intermediate Secondary 3607.64  996.214       301
#>  Female Academic Secondary     4200.38 1178.118       266
#>  Female University             5353.72 1612.265       188
#> ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#> 
#> Levene's Test of Equality of Error Variances
#>   F(7, 2178) = 44.988, p = <.001
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
