# Analysis of Covariance: ANCOVA

`ancova()` tests whether group means differ after controlling for one or
more continuous covariates. It performs a factorial ANCOVA using Type
III Sum of Squares, matching SPSS UNIANOVA output with the WITH keyword.

Think of it as:

- An ANOVA that removes the effect of confounding variables first

- Testing group differences on "adjusted" means

- Combining regression (covariates) and ANOVA (factors) in one model

The test tells you:

- Whether each factor has a significant effect AFTER controlling for
  covariates

- The relationship between each covariate and the outcome

- Effect sizes for factors and covariates (partial eta squared)

- Estimated marginal means (group means adjusted for covariates)

## Usage

``` r
ancova(data, dv, between, covariate, weights = NULL, ss_type = 3)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- dv:

  The numeric dependent variable to analyze (unquoted)

- between:

  Character vector or unquoted variable names specifying the
  between-subjects factors (1-3 factors). These must be categorical
  variables (factor, character, or labelled numeric).

- covariate:

  Unquoted variable names of the continuous covariates to control for.
  Use `c(age, income)` for multiple covariates.

- weights:

  Optional survey weights for population-representative results. Give a
  column name (unquoted or as a string), an expression such as
  `sampling_weight * 2`, or a numeric vector with one weight per row.

- ss_type:

  Deprecated; only 3 (Type III, the SPSS default) is implemented.
  Passing 2 issues a warning and computes Type III.

## Value

An object of class `"ancova"` containing:

- anova_table:

  Tibble with Source, SS, df, MS, F, p, Partial Eta Squared

- parameter_estimates:

  Tibble with regression coefficients (B, SE, t, p, CI, partial eta
  squared) in SPSS coding: one row per category, the last category of
  each factor is the reference and reported as a redundant 0
  (`redundant = TRUE`)

- descriptives:

  Tibble with unadjusted cell means, SDs, and Ns

- estimated_marginal_means:

  Tibble with adjusted cell means (covariates at grand mean)

- emm_main_effects:

  For 2+ factors: named list of tibbles with the adjusted main-effect
  means (unweighted average of the cell means, as SPSS
  `/EMMEANS=TABLES(factor)`); NULL for one factor

- levene_test:

  Tibble with Levene's test of equality of error variances (f, df1, df2,
  p): as in SPSS UNIANOVA, a one-way ANOVA of the absolute residuals of
  the ANCOVA model across the cells of the design. Also available as
  `levene_test(result)`.

- r_squared:

  R-squared and Adjusted R-squared

- model:

  The underlying lm model object

- call_info:

  List with metadata (dv, factors, covariates, weighted, etc.)

Use [`summary()`](https://rdrr.io/r/base/summary.html) for the full
SPSS-style output with toggleable sections. For data grouped with
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html), one
ANCOVA is computed per group: the tables carry the group keys as leading
columns and `group_results` holds the complete result of each group
(`NULL`, with a warning, for a group that cannot be analysed).

## Details

### Understanding the Results

**Adjusted Means (Estimated Marginal Means)**: These are the group means
after statistically removing the effect of the covariate(s). They
answer: "What would the group means be if all groups had the same
covariate values?"

**Covariate Effects**: The covariate row in the ANOVA table shows
whether the covariate significantly predicts the DV after adjusting for
the factors.

**Factor Effects**: These show whether the factor affects the DV after
controlling for the covariate. This is the primary test of interest.

**Partial Eta Squared** (Effect Size):

- Less than 0.01: Negligible

- 0.01 to 0.06: Small

- 0.06 to 0.14: Medium

- 0.14 or greater: Large

### When to Use This

Use ANCOVA when:

- You have group comparisons (ANOVA) but want to control for a confound

- Your covariate is continuous and linearly related to the DV

- You want to increase statistical power by removing known variance
  sources

- The covariate's relationship with the DV is the same across groups
  (homogeneity of regression slopes assumption)

## References

Huitema, B. E. (2011). The Analysis of Covariance and Alternatives (2nd
ed.). Wiley.

IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.

## See also

[`factorial_anova`](https://YannickDiehl.github.io/mariposa/reference/factorial_anova.md)
for ANOVA without covariates.

[`linear_regression`](https://YannickDiehl.github.io/mariposa/reference/linear_regression.md)
for regression analysis.

[`oneway_anova`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)
for single-factor ANOVA.

[`summary.ancova`](https://YannickDiehl.github.io/mariposa/reference/summary.ancova.md)
for detailed output with toggleable sections.

Other hypothesis_tests:
[`binomial_test()`](https://YannickDiehl.github.io/mariposa/reference/binomial_test.md),
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
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
data(survey_data)

# One-way ANCOVA: income by education, controlling for age
survey_data %>%
  ancova(dv = income, between = c(education), covariate = c(age))
#> ANCOVA: income by education, covariate: age, N = 2186
#>   age (covariate): F(1, 2181) = 0.030, p = 0.862, eta2p = 0.000
#>   education:       F(3, 2181) = 466.246, p < 0.001 ***, eta2p = 0.391
#> Use summary() for detailed output.

# Two-way ANCOVA with weights
survey_data %>%
  ancova(dv = income, between = c(gender, education),
         covariate = c(age), weights = sampling_weight)
#> ANCOVA: income by gender, education, covariate: age [Weighted], N = 2186
#>   age (covariate):  F(1, 2177) = 0.013, p = 0.911, eta2p = 0.000
#>   gender:           F(1, 2177) = 0.115, p = 0.735, eta2p = 0.000
#>   education:        F(3, 2177) = 455.614, p < 0.001 ***, eta2p = 0.386
#>   gender:education: F(3, 2177) = 0.298, p = 0.827, eta2p = 0.000
#> Use summary() for detailed output.

# Multiple covariates
survey_data %>%
  ancova(dv = income, between = c(education),
         covariate = c(age, political_orientation))
#> ANCOVA: income by education, covariate: age, political_orientation, N = 2008
#>   age (covariate):                   F(1, 2002) = 0.018, p = 0.895, eta2p = 0.000
#>   political_orientation (covariate): F(1, 2002) = 1.366, p = 0.243, eta2p = 0.001
#>   education:                         F(3, 2002) = 419.350, p < 0.001 ***, eta2p = 0.386
#> Use summary() for detailed output.

# --- Three-layer output ---
result <- ancova(survey_data, dv = income, between = c(education),
                 covariate = c(age))
result              # compact overview
#> ANCOVA: income by education, covariate: age, N = 2186
#>   age (covariate): F(1, 2181) = 0.030, p = 0.862, eta2p = 0.000
#>   education:       F(3, 2181) = 466.246, p < 0.001 ***, eta2p = 0.391
#> Use summary() for detailed output.
summary(result)     # full detailed output with all sections
#> ANCOVA (One-Way ANCOVA) Results
#> -------------------------------
#> 
#> - Dependent variable: income
#> - Factor(s): education
#> - Covariate(s): age
#> - Sum of squares: Type III
#> - N (complete cases): 2186
#> - Missing: 314
#> 
#> Tests of Between-Subjects Effects
#>   ---------------------------------------------------------------------------------------------------------
#>   Source           Type III Sum of Squares    df     Mean Square         F    Sig  Partial Eta Squared     
#>   ---------------------------------------------------------------------------------------------------------
#>   Corrected Model           1752821002.875     4   438205250.719   349.723  <.001                0.391  ***
#>   Intercept                 3472400779.183     1  3472400779.183  2771.252  <.001                0.560  ***
#>   age                            37721.406     1       37721.406     0.030   .862                0.000     
#>   education                 1752630664.965     3   584210221.655   466.246  <.001                0.391  ***
#>   Error                     2732810163.639  2181     1253007.870                                           
#>   Total                    35290790000.000  2186                                                           
#>   Corrected Total           4485631166.514  2185                                                           
#>   ---------------------------------------------------------------------------------------------------------
#> R Squared = 0.391 (Adjusted R Squared = 0.390)
#> 
#> Parameter Estimates
#>   --------------------------------------------------------------------------------------------------------------------------
#>   Parameter                                   B  Std. Error        t    Sig  95% CI Lower  95% CI Upper  Partial Eta Squared
#>   --------------------------------------------------------------------------------------------------------------------------
#>   Intercept                            5349.320      91.774   58.288  <.001      5169.346      5529.293                0.609
#>   age                                    -0.245       1.412   -0.174   .862        -3.014         2.524                0.000
#>   [education=Basic Secondary]         -2577.931      72.359  -35.627  <.001     -2719.830     -2436.032                0.368
#>   [education=Intermediate Secondary]  -1744.236      76.304  -22.859  <.001     -1893.871     -1594.600                0.193
#>   [education=Academic Secondary]      -1112.550      76.328  -14.576  <.001     -1262.233      -962.866                0.089
#>   [education=University]                  0 (a)                                                                             
#>   --------------------------------------------------------------------------------------------------------------------------
#> (a) This parameter is set to zero because it is redundant (SPSS coding: the
#>     last category of each factor is the reference).
#> 
#> Estimated Marginal Means
#> (Evaluated at covariate means)
#>   ------------------------------------------------------------------------
#>   education                   Mean  Std. Error  95% CI Lower  95% CI Upper
#>   ------------------------------------------------------------------------
#>   Basic Secondary         2758.939      41.294      2677.960      2839.918
#>   Intermediate Secondary  3592.634      47.822      3498.852      3686.416
#>   Academic Secondary      4224.320      47.836      4130.511      4318.130
#>   University              5336.870      59.438      5220.309      5453.431
#>   ------------------------------------------------------------------------
#> 
#> Levene's Test of Equality of Error Variances
#>   F(3, 2182) = 103.953, p < 0.001 ***
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
summary(result, marginal_means = FALSE)  # hide estimated marginal means
#> ANCOVA (One-Way ANCOVA) Results
#> -------------------------------
#> 
#> - Dependent variable: income
#> - Factor(s): education
#> - Covariate(s): age
#> - Sum of squares: Type III
#> - N (complete cases): 2186
#> - Missing: 314
#> 
#> Tests of Between-Subjects Effects
#>   ---------------------------------------------------------------------------------------------------------
#>   Source           Type III Sum of Squares    df     Mean Square         F    Sig  Partial Eta Squared     
#>   ---------------------------------------------------------------------------------------------------------
#>   Corrected Model           1752821002.875     4   438205250.719   349.723  <.001                0.391  ***
#>   Intercept                 3472400779.183     1  3472400779.183  2771.252  <.001                0.560  ***
#>   age                            37721.406     1       37721.406     0.030   .862                0.000     
#>   education                 1752630664.965     3   584210221.655   466.246  <.001                0.391  ***
#>   Error                     2732810163.639  2181     1253007.870                                           
#>   Total                    35290790000.000  2186                                                           
#>   Corrected Total           4485631166.514  2185                                                           
#>   ---------------------------------------------------------------------------------------------------------
#> R Squared = 0.391 (Adjusted R Squared = 0.390)
#> 
#> Parameter Estimates
#>   --------------------------------------------------------------------------------------------------------------------------
#>   Parameter                                   B  Std. Error        t    Sig  95% CI Lower  95% CI Upper  Partial Eta Squared
#>   --------------------------------------------------------------------------------------------------------------------------
#>   Intercept                            5349.320      91.774   58.288  <.001      5169.346      5529.293                0.609
#>   age                                    -0.245       1.412   -0.174   .862        -3.014         2.524                0.000
#>   [education=Basic Secondary]         -2577.931      72.359  -35.627  <.001     -2719.830     -2436.032                0.368
#>   [education=Intermediate Secondary]  -1744.236      76.304  -22.859  <.001     -1893.871     -1594.600                0.193
#>   [education=Academic Secondary]      -1112.550      76.328  -14.576  <.001     -1262.233      -962.866                0.089
#>   [education=University]                  0 (a)                                                                             
#>   --------------------------------------------------------------------------------------------------------------------------
#> (a) This parameter is set to zero because it is redundant (SPSS coding: the
#>     last category of each factor is the reference).
#> 
#> Levene's Test of Equality of Error Variances
#>   F(3, 2182) = 103.953, p < 0.001 ***
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
