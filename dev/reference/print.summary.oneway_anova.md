# Print summary of one-way ANOVA results (detailed output)

Displays the detailed SPSS-style output for a one-way ANOVA, with
sections controlled by the boolean parameters passed to
[`summary.oneway_anova`](https://YannickDiehl.github.io/mariposa/dev/reference/summary.oneway_anova.md).
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
  [`summary.oneway_anova`](https://YannickDiehl.github.io/mariposa/dev/reference/summary.oneway_anova.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`oneway_anova`](https://YannickDiehl.github.io/mariposa/dev/reference/oneway_anova.md)
for the main analysis,
[`summary.oneway_anova`](https://YannickDiehl.github.io/mariposa/dev/reference/summary.oneway_anova.md)
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
#> Descriptive Statistics by Group:
#>   Basic Secondary: mean = 3.204, sd = 1.243, n = 809
#>   Intermediate Secondary: mean = 3.701, sd = 1.112, n = 618
#>   Academic Secondary: mean = 3.853, sd = 0.998, n = 607
#>   University: mean = 4.047, sd = 0.957, n = 387
#> 
#> ANOVA Results:
#> -------------------------------------------------------------------------------- 
#>          Source Sum_Squares   df Mean_Square      F p_value sig
#>  Between Groups     247.347    3      82.449 67.096   <.001   1
#>   Within Groups    2970.080 2417       1.229                   
#>           Total    3217.428 2420                               
#> -------------------------------------------------------------------------------- 
#> 
#> Assumption Tests:
#> ---------------- 
#>  Assumption Statistic df1  df2 p_value sig
#>       Welch    64.489   3 1229   <.001 ***
#> 
#> Effect Sizes:
#> ------------ 
#>           Variable Eta_Squared Epsilon_Squared Omega_Squared Effect_Size
#>  life_satisfaction       0.077           0.076         0.076      medium
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
#> -------------------------------------------------------------------------------- 
#>          Source Sum_Squares   df Mean_Square      F p_value sig
#>  Between Groups     247.347    3      82.449 67.096   <.001   1
#>   Within Groups    2970.080 2417       1.229                   
#>           Total    3217.428 2420                               
#> -------------------------------------------------------------------------------- 
#> 
#> Assumption Tests:
#> ---------------- 
#>  Assumption Statistic df1  df2 p_value sig
#>       Welch    64.489   3 1229   <.001 ***
#> 
#> Effect Sizes:
#> ------------ 
#>           Variable Eta_Squared Epsilon_Squared Omega_Squared Effect_Size
#>  life_satisfaction       0.077           0.076         0.076      medium
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
