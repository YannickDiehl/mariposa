# Summary method for Kruskal-Wallis test results

Creates a summary object that produces detailed output when printed,
including rank statistics per group and the test statistics table with
H, degrees of freedom, p-value, and epsilon-squared.

## Usage

``` r
# S3 method for class 'kruskal_wallis'
summary(object, ranks = TRUE, results = TRUE, digits = 3, ...)
```

## Arguments

- object:

  A `kruskal_wallis` result object.

- ranks:

  Logical. Show rank statistics per group? (Default: TRUE)

- results:

  Logical. Show test statistics table? (Default: TRUE)

- digits:

  Number of decimal places for formatting (Default: 3).

- ...:

  Additional arguments (not used).

## Value

A `summary.kruskal_wallis` object.

## See also

[`kruskal_wallis`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md)
for the main analysis function.

## Examples

``` r
result <- kruskal_wallis(survey_data, life_satisfaction, group = education)
summary(result)
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
summary(result, ranks = FALSE)
#> Kruskal-Wallis Test Results
#> ---------------------------
#> 
#> - Grouping variable: education
#> - Groups: Basic Secondary, Intermediate Secondary, Academic Secondary, University
#> 
#> life_satisfaction
#> -----------------
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
