# Print ANOVA test results (compact)

Compact print method for objects of class `"oneway_anova"`. Shows a
one-line summary per variable with F statistic, p-value, effect size,
and sample size.

For the full detailed output, use
[`summary()`](https://rdrr.io/r/base/summary.html).

## Usage

``` r
# S3 method for class 'oneway_anova'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An object of class `"oneway_anova"` returned by
  [`oneway_anova`](https://YannickDiehl.github.io/mariposa/dev/reference/oneway_anova.md).

- digits:

  Number of decimal places to display (default: 3).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## Examples

``` r
result <- oneway_anova(survey_data, life_satisfaction,
                       group = education)
result              # compact one-line overview
#> One-Way ANOVA: life_satisfaction by education
#>   F(3, 2417) = 67.096, p < 0.001 ***, eta2 = 0.077 (medium), N = 2421
summary(result)     # full detailed output
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
```
