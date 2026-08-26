# Summary method for partial correlation results

Creates a summary object that produces detailed output when printed: the
pairwise partial-correlation table (with zero-order comparison, df, t,
and p) and — for three or more variables — the partial-correlation
matrix.

## Usage

``` r
# S3 method for class 'partial_cor'
summary(object, pairwise = TRUE, matrix = TRUE, digits = 3, ...)
```

## Arguments

- object:

  A `partial_cor` result object.

- pairwise:

  Logical. Show the pairwise results table? (Default: TRUE)

- matrix:

  Logical. Show the partial-correlation matrix (three or more analysis
  variables)? (Default: TRUE)

- digits:

  Number of decimal places for formatting (Default: 3).

- ...:

  Additional arguments (not used).

## Value

A `summary.partial_cor` object.

## See also

[`partial_cor`](https://YannickDiehl.github.io/mariposa/reference/partial_cor.md)
for the main analysis function.

## Examples

``` r
result <- partial_cor(survey_data, trust_government, trust_media,
                      trust_science, controls = age)
summary(result)
#> 
#> Partial Correlation Results
#> ---------------------------
#> - Variables: trust_government, trust_media, trust_science
#> - Controlling for: age
#> - Missing: Listwise deletion
#> 
#> Partial Correlation Matrix:
#> --------------------------- 
#>                  trust_government trust_media trust_science
#> trust_government            1.000       0.014         0.020
#> trust_media                 0.014       1.000         0.015
#> trust_science               0.020       0.015         1.000
#> --------------------------- 
#> 
#> Pairwise Results:
#>   -------------------------------------------------------------------------------------- 
#>   Variable 1           Variable 2  Partial r  Zero-order r    df      t     p     n  sig 
#>   -------------------------------------------------------------------------------------- 
#>   trust_government    trust_media      0.014         0.014  2132  0.645  .519  2135      
#>   trust_government  trust_science      0.020         0.020  2132  0.938  .348  2135      
#>   trust_media       trust_science      0.015         0.015  2132  0.687  .492  2135      
#>   -------------------------------------------------------------------------------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
summary(result, matrix = FALSE)
#> 
#> Partial Correlation Results
#> ---------------------------
#> - Variables: trust_government, trust_media, trust_science
#> - Controlling for: age
#> - Missing: Listwise deletion
#> 
#> Pairwise Results:
#>   -------------------------------------------------------------------------------------- 
#>   Variable 1           Variable 2  Partial r  Zero-order r    df      t     p     n  sig 
#>   -------------------------------------------------------------------------------------- 
#>   trust_government    trust_media      0.014         0.014  2132  0.645  .519  2135      
#>   trust_government  trust_science      0.020         0.020  2132  0.938  .348  2135      
#>   trust_media       trust_science      0.015         0.015  2132  0.687  .492  2135      
#>   -------------------------------------------------------------------------------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
