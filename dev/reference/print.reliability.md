# Print reliability results (compact)

Compact print method for objects of class `"reliability"`. Shows
Cronbach's Alpha with quality interpretation and item count in a single
line per group.

For the full detailed output including item statistics, inter-item
correlations, and item-total statistics, use
[`summary()`](https://rdrr.io/r/base/summary.html).

## Usage

``` r
# S3 method for class 'reliability'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An object of class `"reliability"` returned by
  [`reliability`](https://YannickDiehl.github.io/mariposa/dev/reference/reliability.md).

- digits:

  Number of decimal places to display. Default is `3`.

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## Examples

``` r
result <- reliability(survey_data, trust_government, trust_media, trust_science)
result              # compact one-line overview
#> Reliability Analysis: 3 items
#>   Cronbach's Alpha = 0.047 (Poor), N = 2135
summary(result)     # full detailed output
#> 
#> Reliability Analysis Results
#> ----------------------------
#> - Items: trust_government, trust_media, trust_science
#> - N of Items: 3
#> 
#> Reliability Statistics
#> ---------------------------------------- 
#>   Cronbach's Alpha:              0.047
#>   Alpha (standardized):          0.048
#>   N of Items:                    3
#>   N (listwise):                  2135
#> 
#> Item Statistics
#> ---------------------------------------- 
#>              item  mean    sd    n
#>  trust_government 2.621 1.162 2135
#>       trust_media 2.430 1.156 2135
#>     trust_science 3.624 1.034 2135
#> 
#> Inter-Item Correlation Matrix:
#> ------------------------------ 
#>                  trust_government trust_media trust_science
#> trust_government            1.000       0.014         0.020
#> trust_media                 0.014       1.000         0.015
#> trust_science               0.020       0.015         1.000
#> ------------------------------ 
#> 
#> Item-Total Statistics
#> ---------------------------------------- 
#>              item scale_mean_deleted scale_var_deleted corrected_r
#>  trust_government               6.05             2.440       0.024
#>       trust_media               6.25             2.467       0.020
#>     trust_science               5.05             2.723       0.025
#>  alpha_deleted
#>          0.029
#>          0.040
#>          0.027
```
