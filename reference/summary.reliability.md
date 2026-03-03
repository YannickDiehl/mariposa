# Summarize a reliability analysis

Creates a detailed summary of a reliability analysis result. All
sections are shown by default; set individual toggles to `FALSE` to
suppress specific sections.

## Usage

``` r
# S3 method for class 'reliability'
summary(
  object,
  reliability_statistics = TRUE,
  item_statistics = TRUE,
  inter_item_correlations = TRUE,
  item_total_statistics = TRUE,
  digits = 3,
  ...
)
```

## Arguments

- object:

  A `reliability` result object

- reliability_statistics:

  Show Cronbach's Alpha statistics? (Default: TRUE)

- item_statistics:

  Show per-item means and SDs? (Default: TRUE)

- inter_item_correlations:

  Show inter-item correlation matrix? (Default: TRUE)

- item_total_statistics:

  Show item-total statistics? (Default: TRUE)

- digits:

  Number of decimal places (Default: 3)

- ...:

  Additional arguments (ignored)

## Value

A `summary.reliability` object (list with `$show` toggles)

## See also

[`reliability`](https://YannickDiehl.github.io/mariposa/reference/reliability.md)
for the main analysis function.

## Examples

``` r
result <- reliability(survey_data, trust_government, trust_media, trust_science)
summary(result)
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
summary(result, inter_item_correlations = FALSE)
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
