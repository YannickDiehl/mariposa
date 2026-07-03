# Print Kruskal-Wallis test results (compact)

Compact print method for objects of class `"kruskal_wallis"`. Shows a
one-line summary per variable with the H statistic, p-value, effect size
(epsilon-squared), and sample size.

For the full detailed output (rank tables, test statistics), use
[`summary()`](https://rdrr.io/r/base/summary.html).

## Usage

``` r
# S3 method for class 'kruskal_wallis'
print(x, digits = 3, ...)
```

## Arguments

- x:

  A kruskal_wallis object

- digits:

  Number of decimal places to display (default: 3)

- ...:

  Additional arguments (not used)

## Value

Invisibly returns the input object `x`.

## Examples

``` r
result <- kruskal_wallis(survey_data, life_satisfaction, group = education)
result              # compact one-line overview
#> Kruskal-Wallis Test: life_satisfaction by education
#>   H(3) = 171.178, p < 0.001 ***, eps2 = 0.071, N = 2421
#> Use summary() for detailed output.
summary(result)     # full detailed output
#> Kruskal-Wallis Test Results
#> ---------------------------
#> 
#> - Grouping variable: education
#> - Groups: Basic Secondary, Intermediate Secondary, Academic Secondary, University
#> 
#> life_satisfaction
#> -----------------
#>   Ranks:
#>   --------------------------------------
#>                     Group    N Mean Rank
#>           Basic Secondary  809    974.29
#>    Intermediate Secondary  618   1250.73
#>        Academic Secondary  607   1329.56
#>                University  387   1456.42
#>                     Total 2421        NA
#>   --------------------------------------
#> 
#>   Test Statistics:
#>   ------------------------------------------------
#>    Kruskal-Wallis H df p value Epsilon-squared sig
#>             171.178  3       0           0.071 ***
#>   ------------------------------------------------
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation (Epsilon-squared):
#> - Small effect: 0.01 - 0.06
#> - Medium effect: 0.06 - 0.14
#> - Large effect: > 0.14
```
