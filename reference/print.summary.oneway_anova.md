# Print summary of one-way ANOVA results (detailed output)

Displays the detailed SPSS-style output for a one-way ANOVA, with
sections controlled by the boolean parameters passed to
[`summary.oneway_anova`](https://YannickDiehl.github.io/mariposa/reference/summary.oneway_anova.md).
Sections include group descriptives, ANOVA table, homogeneity of
variances (Levene's test), and effect sizes.

## Usage

``` r
# S3 method for class 'summary.oneway_anova'
print(x, ...)
```

## Arguments

- x:

  A `summary.oneway_anova` object created by
  [`summary.oneway_anova`](https://YannickDiehl.github.io/mariposa/reference/summary.oneway_anova.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`oneway_anova`](https://YannickDiehl.github.io/mariposa/reference/oneway_anova.md)
for the main analysis,
[`summary.oneway_anova`](https://YannickDiehl.github.io/mariposa/reference/summary.oneway_anova.md)
for summary options.

## Examples

``` r
result <- oneway_anova(survey_data, life_satisfaction, group = education)
summary(result)                        # all sections
#> One-Way ANOVA Results
#> ---------------------
#> 
#> - Dependent variable: life_satisfaction
#> - Grouping variable: education
#> - Confidence level: 95.0%
#>   Null hypothesis: All group means are equal
#>   Alternative hypothesis: At least one group mean differs
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Descriptive Statistics:
#>   ------------------------------------------------------------------------------------------
#>   education                 N   Mean  Std. Deviation  Std. Error  95% CI Lower  95% CI Upper
#>   ------------------------------------------------------------------------------------------
#>   Basic Secondary         809  3.204           1.243       0.044         3.118         3.290
#>   Intermediate Secondary  618  3.701           1.112       0.045         3.613         3.789
#>   Academic Secondary      607  3.853           0.998       0.041         3.774         3.933
#>   University              387  4.047           0.957       0.049         3.951         4.142
#>   ------------------------------------------------------------------------------------------
#> 
#> ANOVA Results:
#>   ---------------------------------------------------------------------
#>                   Sum of Squares    df  Mean Square       F    Sig     
#>   ---------------------------------------------------------------------
#>   Between Groups         247.347     3       82.449  67.096  <.001  ***
#>   Within Groups         2970.080  2417        1.229                    
#>   Total                 3217.428  2420                                 
#>   ---------------------------------------------------------------------
#> 
#> Robust Tests of Equality of Means:
#>   -------------------------------------------
#>          Statistic  df1       df2    Sig     
#>   -------------------------------------------
#>   Welch     64.489    3  1229.456  <.001  ***
#>   -------------------------------------------
#> 
#> Effect Sizes:
#>   -------------------------------------------------------------------------
#>   Variable           Eta Squared  Epsilon Squared  Omega Squared  Magnitude
#>   -------------------------------------------------------------------------
#>   life_satisfaction        0.077            0.076          0.076     medium
#>   -------------------------------------------------------------------------
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation:
#> - Eta-squared: Proportion of variance explained (biased upward)
#> - Epsilon-squared: Less biased than eta-squared
#> - Omega-squared: Unbiased estimate (preferred for publication)
#> - Small effect: eta-squared ~ 0.01, Medium effect: eta-squared ~ 0.06, Large effect: eta-squared ~ 0.14
#> 
#> Post-hoc tests: Use tukey_test() for pairwise comparisons
summary(result, descriptives = FALSE)  # hide group statistics
#> One-Way ANOVA Results
#> ---------------------
#> 
#> - Dependent variable: life_satisfaction
#> - Grouping variable: education
#> - Confidence level: 95.0%
#>   Null hypothesis: All group means are equal
#>   Alternative hypothesis: At least one group mean differs
#> 
#> 
#> --- life_satisfaction ---
#> 
#> 
#> ANOVA Results:
#>   ---------------------------------------------------------------------
#>                   Sum of Squares    df  Mean Square       F    Sig     
#>   ---------------------------------------------------------------------
#>   Between Groups         247.347     3       82.449  67.096  <.001  ***
#>   Within Groups         2970.080  2417        1.229                    
#>   Total                 3217.428  2420                                 
#>   ---------------------------------------------------------------------
#> 
#> Robust Tests of Equality of Means:
#>   -------------------------------------------
#>          Statistic  df1       df2    Sig     
#>   -------------------------------------------
#>   Welch     64.489    3  1229.456  <.001  ***
#>   -------------------------------------------
#> 
#> Effect Sizes:
#>   -------------------------------------------------------------------------
#>   Variable           Eta Squared  Epsilon Squared  Omega Squared  Magnitude
#>   -------------------------------------------------------------------------
#>   life_satisfaction        0.077            0.076          0.076     medium
#>   -------------------------------------------------------------------------
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation:
#> - Eta-squared: Proportion of variance explained (biased upward)
#> - Epsilon-squared: Less biased than eta-squared
#> - Omega-squared: Unbiased estimate (preferred for publication)
#> - Small effect: eta-squared ~ 0.01, Medium effect: eta-squared ~ 0.06, Large effect: eta-squared ~ 0.14
#> 
#> Post-hoc tests: Use tukey_test() for pairwise comparisons
```
