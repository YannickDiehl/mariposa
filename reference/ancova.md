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
#> 
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

# Two-way ANCOVA with weights
survey_data %>%
  ancova(dv = income, between = c(gender, education),
         covariate = c(age), weights = sampling_weight)
#> 
#> Weighted ANCOVA (2-Way ANCOVA) Results
#> --------------------------------------
#> 
#> - Dependent variable: income
#> - Factor(s): gender x education
#> - Covariate(s): age
#> - Type III Sum of Squares: Type 3
#> - Weights variable: sampling_weight
#> - N (complete cases): 2186
#> - Missing: 314
#> 
#> Tests of Between-Subjects Effects
#> ----------------------------------------------------------------- 
#>  Source             Type III SS  df   Mean Square  F        Sig. 
#>  Corrected Model    1.727833e+09    8 2.159791e+08  172.044 <.001
#>  Intercept          3.512177e+09    1 3.512177e+09 2797.728 <.001
#>  age                1.571502e+04    1 1.571502e+04    0.013 0.911
#>  gender             1.437527e+05    1 1.437527e+05    0.115 0.735
#>  education          1.715890e+09    3 5.719633e+08  455.614 <.001
#>  gender * education 1.122626e+06    3 3.742086e+05    0.298 0.827
#>  Error              2.732936e+09 2177 1.255368e+06               
#>  Total              3.529768e+10 2186                            
#>  Corrected Total    4.460769e+09 2185                            
#>  Partial Eta Sq    
#>  0.387          ***
#>  0.562          ***
#>  0.000             
#>  0.000             
#>  0.386          ***
#>  0.000             
#>                    
#>                    
#>                    
#> ----------------------------------------------------------------- 
#> R Squared = 0.387 (Adjusted R Squared = 0.385)
#> 
#> Parameter Estimates
#> ----------------------------------------------------------------------------- 
#>  Parameter           B        Std. Error t      Sig.  Lower Bound Upper Bound
#>  (Intercept)         3984.324 75.327     52.894 <.001 3836.603    4132.045   
#>  age                   -0.157  1.402     -0.112 0.911   -2.906       2.593   
#>  gender1                8.417 24.873      0.338 0.735  -40.360      57.194   
#>  education.L         1865.352 51.415     36.280 <.001 1764.525    1966.179   
#>  education.Q          138.678 49.789      2.785 0.005   41.039     236.317   
#>  education.C          147.993 48.027      3.081 0.002   53.809     242.178   
#>  gender1:education.L  -30.796 51.414     -0.599 0.549 -131.621      70.029   
#>  gender1:education.Q    6.990 49.766      0.140 0.888  -90.604     104.584   
#>  gender1:education.C  -32.104 48.024     -0.668 0.504 -126.282      62.074   
#>  Partial Eta Sq
#>  0.562         
#>  0.000         
#>  0.000         
#>  0.377         
#>  0.004         
#>  0.004         
#>  0.000         
#>  0.000         
#>  0.000         
#> ----------------------------------------------------------------------------- 
#> 
#> Estimated Marginal Means
#> (Evaluated at covariate means)
#> -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#>  gender education              Mean     Std. Error Lower Bound Upper Bound
#>  Male   Basic Secondary        2801.036 59.824     2683.718    2918.354   
#>  Male   Intermediate Secondary 3579.461 71.117     3439.997    3718.925   
#>  Male   Academic Secondary     4244.417 66.034     4114.920    4373.914   
#>  Male   University             5314.178 88.494     5140.637    5487.720   
#>  Female Basic Secondary        2721.537 56.737     2610.274    2832.801   
#>  Female Intermediate Secondary 3598.917 63.638     3474.119    3723.714   
#>  Female Academic Secondary     4205.275 68.184     4071.562    4338.987   
#>  Female University             5346.029 82.845     5183.566    5508.492   
#> -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#> 
#> Levene's Test of Equality of Error Variances
#>   F(7, 2178) = 41.244, p = <.001
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05

# Multiple covariates
survey_data %>%
  ancova(dv = income, between = c(education),
         covariate = c(age, political_orientation))
#> 
#> ANCOVA (One-Way ANCOVA) Results
#> -------------------------------
#> 
#> - Dependent variable: income
#> - Factor(s): education
#> - Covariate(s): age, political_orientation
#> - Type III Sum of Squares: Type 3
#> - N (complete cases): 2008
#> - Missing: 492
#> 
#> Tests of Between-Subjects Effects
#> -------------------------------------------------------------------- 
#>  Source                Type III SS  df   Mean Square  F        Sig. 
#>  Corrected Model       1.582047e+09    5 3.164093e+08  252.422 <.001
#>  Intercept             1.958785e+09    1 1.958785e+09 1562.659 <.001
#>  age                   2.194524e+04    1 2.194524e+04    0.018 0.895
#>  political_orientation 1.712167e+06    1 1.712167e+06    1.366 0.243
#>  education             1.576958e+09    3 5.256526e+08  419.350 <.001
#>  Error                 2.509497e+09 2002 1.253495e+06               
#>  Total                 3.214036e+10 2008                            
#>  Corrected Total       4.091544e+09 2007                            
#>  Partial Eta Sq    
#>  0.387          ***
#>  0.438          ***
#>  0.000             
#>  0.001             
#>  0.386          ***
#>                    
#>                    
#>                    
#> -------------------------------------------------------------------- 
#> R Squared = 0.387 (Adjusted R Squared = 0.385)
#> 
#> Parameter Estimates
#> ------------------------------------------------------------------------------- 
#>  Parameter             B        Std. Error t      Sig.  Lower Bound Upper Bound
#>  (Intercept)           4040.829 102.221    39.530 <.001 3840.359    4241.299   
#>  age                     -0.196   1.479    -0.132 0.895   -3.096       2.704   
#>  political_orientation  -26.771  22.906    -1.169 0.243  -71.693      18.151   
#>  education.L           1847.837  53.119    34.787 <.001 1743.662    1952.011   
#>  education.Q            103.244  51.756     1.995 0.046    1.744     204.745   
#>  education.C            154.449  50.186     3.078 0.002   56.027     252.871   
#>  Partial Eta Sq
#>  0.438         
#>  0.000         
#>  0.001         
#>  0.377         
#>  0.002         
#>  0.005         
#> ------------------------------------------------------------------------------- 
#> 
#> Estimated Marginal Means
#> (Evaluated at covariate means)
#> ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#>  education              Mean     Std. Error Lower Bound Upper Bound
#>  Basic Secondary        2735.624 43.231     2650.841    2820.408   
#>  Intermediate Secondary 3596.901 49.594     3499.640    3694.163   
#>  Academic Secondary     4216.064 50.054     4117.900    4314.228   
#>  University             5283.829 62.076     5162.090    5405.569   
#> ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#> 
#> Levene's Test of Equality of Error Variances
#>   F(3, 2004) = 97.179, p = <.001
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
