# Print summary of reliability analysis results (detailed output)

Displays the detailed SPSS-style output for a reliability analysis, with
sections controlled by the boolean parameters passed to
[`summary.reliability`](https://YannickDiehl.github.io/mariposa/reference/summary.reliability.md).
Sections include reliability statistics, item statistics, inter-item
correlations, and item-total statistics.

## Usage

``` r
# S3 method for class 'summary.reliability'
print(x, ...)
```

## Arguments

- x:

  A `summary.reliability` object created by
  [`summary.reliability`](https://YannickDiehl.github.io/mariposa/reference/summary.reliability.md).

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object `x`.

## See also

[`reliability`](https://YannickDiehl.github.io/mariposa/reference/reliability.md)
for the main analysis,
[`summary.reliability`](https://YannickDiehl.github.io/mariposa/reference/summary.reliability.md)
for summary options.

## Examples

``` r
result <- reliability(survey_data, trust_government, trust_media, trust_science)
summary(result)                                  # all sections
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
summary(result, inter_item_correlations = FALSE)  # hide correlations
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
