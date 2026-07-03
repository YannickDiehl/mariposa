# Print Levene test results (compact)

Compact print method for objects of class `"levene_test"`. Shows a
one-line summary per variable with the F statistic, degrees of freedom,
p-value, and the equal/unequal variances conclusion.

For the full detailed output (results tables, interpretation,
recommendation), use [`summary()`](https://rdrr.io/r/base/summary.html).

## Usage

``` r
# S3 method for class 'levene_test'
print(x, digits = 3, ...)
```

## Arguments

- x:

  A levene_test object

- digits:

  Number of decimal places to display (default: 3)

- ...:

  Additional arguments (not used)

## Value

Invisibly returns the input object `x`.

## Examples

``` r
result <- levene_test(survey_data, life_satisfaction, group = education)
result              # compact one-line overview
#> Levene's Test: life_satisfaction by education
#>   F(3, 2417) = 31.634, p < 0.001 ***, variances unequal
#> Use summary() for detailed output.
summary(result)     # full detailed output
#> Levene's Test for Homogeneity of Variance 
#> ------------------------------------------
#> 
#> - Grouping variable: education
#> - Center: mean
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Levene's Test Results:
#> -------------------------------------------------------------------- 
#>           Variable F_statistic df1  df2 p_value sig        Conclusion
#>  life_satisfaction      31.634   3 2417       0 *** Variances unequal
#> -------------------------------------------------------------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation:
#> - p > 0.05: Variances are homogeneous (equal variances assumed)
#> - p <= 0.05: Variances are heterogeneous (equal variances NOT assumed)
```
