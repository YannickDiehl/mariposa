# Print summary of Kruskal-Wallis test results (detailed output)

Displays the detailed SPSS-style output for a Kruskal-Wallis test, with
sections controlled by the boolean parameters passed to
[`summary.kruskal_wallis`](https://YannickDiehl.github.io/mariposa/reference/summary.kruskal_wallis.md).
Sections include the per-group rank table and the test statistics table
with effect size.

## Usage

``` r
# S3 method for class 'summary.kruskal_wallis'
print(x, ...)
```

## Arguments

- x:

  A `summary.kruskal_wallis` object created by
  [`summary.kruskal_wallis`](https://YannickDiehl.github.io/mariposa/reference/summary.kruskal_wallis.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`kruskal_wallis`](https://YannickDiehl.github.io/mariposa/reference/kruskal_wallis.md)
for the main analysis,
[`summary.kruskal_wallis`](https://YannickDiehl.github.io/mariposa/reference/summary.kruskal_wallis.md)
for summary options.

## Examples

``` r
result <- kruskal_wallis(survey_data, life_satisfaction, group = education)
summary(result)                # all sections
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
summary(result, ranks = FALSE) # hide rank tables
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
