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

  Optional survey weights for population-representative results
  (unquoted variable name)

- ss_type:

  Type of Sum of Squares: 3 (default, SPSS standard) or 2. Type III is
  recommended for unbalanced designs.

## Value

An object of class `"ancova"` containing:

- anova_table:

  Tibble with Source, SS, df, MS, F, p, Partial Eta Squared

- parameter_estimates:

  Tibble with regression coefficients (B, SE, t, p)

- descriptives:

  Tibble with unadjusted cell means, SDs, and Ns

- estimated_marginal_means:

  Tibble with adjusted means (covariates at grand mean)

- levene_test:

  Tibble with Levene's test results

- r_squared:

  R-squared and Adjusted R-squared

- model:

  The underlying lm model object

- call_info:

  List with metadata (dv, factors, covariates, weighted, etc.)

Use [`summary()`](https://rdrr.io/r/base/summary.html) for the full
SPSS-style output with toggleable sections.

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

[`factorial_anova`](https://YannickDiehl.github.io/mariposa/dev/reference/factorial_anova.md)
for ANOVA without covariates.

[`linear_regression`](https://YannickDiehl.github.io/mariposa/dev/reference/linear_regression.md)
for regression analysis.

[`oneway_anova`](https://YannickDiehl.github.io/mariposa/dev/reference/oneway_anova.md)
for single-factor ANOVA.

[`summary.ancova`](https://YannickDiehl.github.io/mariposa/dev/reference/summary.ancova.md)
for detailed output with toggleable sections.

Other hypothesis_tests:
[`binomial_test()`](https://YannickDiehl.github.io/mariposa/dev/reference/binomial_test.md),
[`chi_square()`](https://YannickDiehl.github.io/mariposa/dev/reference/chi_square.md),
[`chisq_gof()`](https://YannickDiehl.github.io/mariposa/dev/reference/chisq_gof.md),
[`factorial_anova()`](https://YannickDiehl.github.io/mariposa/dev/reference/factorial_anova.md),
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
#> ANCOVA: income by education, covariate: age
#>   age (covariate): F(1, 2181) = 0.030, p = 0.862 , eta2p = 0.000
#>   education:       F(3, 2181) = 466.246, p < 0.001 ***, eta2p = 0.391, N = 2186

# Two-way ANCOVA with weights
survey_data %>%
  ancova(dv = income, between = c(gender, education),
         covariate = c(age), weights = sampling_weight)
#> ANCOVA: income by gender, education, covariate: age [Weighted]
#>   age (covariate):  F(1, 2177) = 0.013, p = 0.911 , eta2p = 0.000
#>   gender:           F(1, 2177) = 0.115, p = 0.735 , eta2p = 0.000
#>   education:        F(3, 2177) = 455.614, p < 0.001 ***, eta2p = 0.386
#>   gender:education: F(3, 2177) = 0.298, p = 0.827 , eta2p = 0.000, N = 2186

# Multiple covariates
survey_data %>%
  ancova(dv = income, between = c(education),
         covariate = c(age, political_orientation))
#> ANCOVA: income by education, covariate: age, political_orientation
#>   age (covariate):                   F(1, 2002) = 0.018, p = 0.895 , eta2p = 0.000
#>   political_orientation (covariate): F(1, 2002) = 1.366, p = 0.243 , eta2p = 0.001
#>   education:                         F(3, 2002) = 419.350, p < 0.001 ***, eta2p = 0.386, N = 2008

# --- Three-layer output ---
result <- ancova(survey_data, dv = income, between = c(education),
                 covariate = c(age))
result              # compact overview
#> ANCOVA: income by education, covariate: age
#>   age (covariate): F(1, 2181) = 0.030, p = 0.862 , eta2p = 0.000
#>   education:       F(3, 2181) = 466.246, p < 0.001 ***, eta2p = 0.391, N = 2186
summary(result)     # full detailed output with all sections
#> ANCOVA (One-Way ANCOVA) Results
#> -------------------------------
#> 
#> - Dependent variable: income
#> - Factor(s): education
#> - Covariate(s): age
#> - Type III Sum of Squares: Type 3
#> - N (complete cases): 2186
#> - Missing: 314
#> 
#> Tests of Between-Subjects Effects
#> ----------------------------------------------------------------------------- 
#>  Source          Type III SS  df   Mean Square  F        Sig.  Partial Eta Sq
#>  Corrected Model 1.752821e+09    4 4.382053e+08  349.723 <.001 0.391         
#>  Intercept       3.472401e+09    1 3.472401e+09 2771.252 <.001 0.560         
#>  age             3.772141e+04    1 3.772141e+04    0.030 0.862 0.000         
#>  education       1.752631e+09    3 5.842102e+08  466.246 <.001 0.391         
#>  Error           2.732810e+09 2181 1.253008e+06                              
#>  Total           3.529079e+10 2186                                           
#>  Corrected Total 4.485631e+09 2185                                           
#>     
#>  ***
#>  ***
#>     
#>  ***
#>     
#>     
#>     
#> ----------------------------------------------------------------------------- 
#> R Squared = 0.391 (Adjusted R Squared = 0.390)
#> 
#> Parameter Estimates
#> --------------------------------------------------------------------- 
#>  Parameter   B        Std. Error t      Sig.  Lower Bound Upper Bound
#>  (Intercept) 3990.641 75.806     52.643 <.001 3841.981    4139.301   
#>  age           -0.245  1.412     -0.174 0.862   -3.014       2.524   
#>  education.L 1870.578 50.838     36.795 <.001 1770.881    1970.274   
#>  education.Q  139.427 49.566      2.813 0.005   42.226     236.629   
#>  education.C  152.695 48.167      3.170 0.002   58.236     247.154   
#>  Partial Eta Sq
#>  0.560         
#>  0.000         
#>  0.383         
#>  0.004         
#>  0.005         
#> --------------------------------------------------------------------- 
#> 
#> Estimated Marginal Means
#> (Evaluated at covariate means)
#> ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#>  education              Mean     Std. Error Lower Bound Upper Bound
#>  Basic Secondary        2758.939 41.294     2677.960    2839.918   
#>  Intermediate Secondary 3592.634 47.822     3498.852    3686.416   
#>  Academic Secondary     4224.320 47.836     4130.511    4318.130   
#>  University             5336.870 59.438     5220.309    5453.431   
#> ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#> 
#> Levene's Test of Equality of Error Variances
#>   F(3, 2182) = 103.901, p = <.001
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
summary(result, marginal_means = FALSE)  # hide estimated marginal means
#> ANCOVA (One-Way ANCOVA) Results
#> -------------------------------
#> 
#> - Dependent variable: income
#> - Factor(s): education
#> - Covariate(s): age
#> - Type III Sum of Squares: Type 3
#> - N (complete cases): 2186
#> - Missing: 314
#> 
#> Tests of Between-Subjects Effects
#> ----------------------------------------------------------------------------- 
#>  Source          Type III SS  df   Mean Square  F        Sig.  Partial Eta Sq
#>  Corrected Model 1.752821e+09    4 4.382053e+08  349.723 <.001 0.391         
#>  Intercept       3.472401e+09    1 3.472401e+09 2771.252 <.001 0.560         
#>  age             3.772141e+04    1 3.772141e+04    0.030 0.862 0.000         
#>  education       1.752631e+09    3 5.842102e+08  466.246 <.001 0.391         
#>  Error           2.732810e+09 2181 1.253008e+06                              
#>  Total           3.529079e+10 2186                                           
#>  Corrected Total 4.485631e+09 2185                                           
#>     
#>  ***
#>  ***
#>     
#>  ***
#>     
#>     
#>     
#> ----------------------------------------------------------------------------- 
#> R Squared = 0.391 (Adjusted R Squared = 0.390)
#> 
#> Parameter Estimates
#> --------------------------------------------------------------------- 
#>  Parameter   B        Std. Error t      Sig.  Lower Bound Upper Bound
#>  (Intercept) 3990.641 75.806     52.643 <.001 3841.981    4139.301   
#>  age           -0.245  1.412     -0.174 0.862   -3.014       2.524   
#>  education.L 1870.578 50.838     36.795 <.001 1770.881    1970.274   
#>  education.Q  139.427 49.566      2.813 0.005   42.226     236.629   
#>  education.C  152.695 48.167      3.170 0.002   58.236     247.154   
#>  Partial Eta Sq
#>  0.560         
#>  0.000         
#>  0.383         
#>  0.004         
#>  0.005         
#> --------------------------------------------------------------------- 
#> 
#> Levene's Test of Equality of Error Variances
#>   F(3, 2182) = 103.901, p = <.001
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
