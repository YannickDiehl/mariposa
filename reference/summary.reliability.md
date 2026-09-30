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

  Show Cronbach's Alpha and McDonald's Omega statistics? (Default: TRUE)

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
#> - Items:
#>     trust_government  Trust in government (1=none, 5=complete)
#>     trust_media       Trust in media (1=none, 5=complete)
#>     trust_science     Trust in science (1=none, 5=complete)
#> - N of Items: 3
#> 
#> Reliability Statistics
#> ---------------------------------------- 
#>   Cronbach's Alpha:              0.047
#>   Alpha (standardized):          0.048
#>   McDonald's Omega:              0.047
#>   Omega (standardized):          0.048
#>   N of Items:                    3
#>   N (listwise):                  2135
#> 
#> Item Statistics
#>   ----------------------------------------------------------------------
#>   Item                                        Mean  Std. Deviation     N
#>   ----------------------------------------------------------------------
#>   trust_government  Trust in government ...  2.621           1.162  2135
#>   trust_media       Trust in media (1=no...  2.430           1.156  2135
#>   trust_science     Trust in science (1=...  3.624           1.034  2135
#>   ----------------------------------------------------------------------
#> 
#> Inter-Item Correlation Matrix
#>   -----------------------------------------
#>                           (1)    (2)    (3)
#>   -----------------------------------------
#>   (1) trust_government  1.000  0.014  0.020
#>   (2) trust_media       0.014  1.000  0.015
#>   (3) trust_science     0.020  0.015  1.000
#>   -----------------------------------------
#> 
#> Item-Total Statistics
#>   ------------------------------------------------------------------------
#>                     Scale Mean  Scale Var.   Corrected  Alpha if  Omega if
#>   Item              if Deleted  if Deleted  Item-Total   Deleted   Deleted
#>   ------------------------------------------------------------------------
#>   trust_government       6.054       2.440       0.024     0.029          
#>   trust_media            6.245       2.467       0.020     0.040          
#>   trust_science          5.051       2.723       0.025     0.027          
#>   ------------------------------------------------------------------------
#> Note: Omega if item deleted requires at least 4 items
#> (a one-factor model on the remaining 2 items is not identified).
summary(result, inter_item_correlations = FALSE)
#> 
#> Reliability Analysis Results
#> ----------------------------
#> - Items:
#>     trust_government  Trust in government (1=none, 5=complete)
#>     trust_media       Trust in media (1=none, 5=complete)
#>     trust_science     Trust in science (1=none, 5=complete)
#> - N of Items: 3
#> 
#> Reliability Statistics
#> ---------------------------------------- 
#>   Cronbach's Alpha:              0.047
#>   Alpha (standardized):          0.048
#>   McDonald's Omega:              0.047
#>   Omega (standardized):          0.048
#>   N of Items:                    3
#>   N (listwise):                  2135
#> 
#> Item Statistics
#>   ----------------------------------------------------------------------
#>   Item                                        Mean  Std. Deviation     N
#>   ----------------------------------------------------------------------
#>   trust_government  Trust in government ...  2.621           1.162  2135
#>   trust_media       Trust in media (1=no...  2.430           1.156  2135
#>   trust_science     Trust in science (1=...  3.624           1.034  2135
#>   ----------------------------------------------------------------------
#> 
#> Item-Total Statistics
#>   ------------------------------------------------------------------------
#>                     Scale Mean  Scale Var.   Corrected  Alpha if  Omega if
#>   Item              if Deleted  if Deleted  Item-Total   Deleted   Deleted
#>   ------------------------------------------------------------------------
#>   trust_government       6.054       2.440       0.024     0.029          
#>   trust_media            6.245       2.467       0.020     0.040          
#>   trust_science          5.051       2.723       0.025     0.027          
#>   ------------------------------------------------------------------------
#> Note: Omega if item deleted requires at least 4 items
#> (a one-factor model on the remaining 2 items is not identified).
```
