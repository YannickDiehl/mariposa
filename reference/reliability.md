# Check How Reliably Your Scale Measures a Concept

`reliability()` calculates Cronbach's Alpha and detailed item statistics
to evaluate whether your survey items form a reliable scale. This is the
R equivalent of SPSS's
`RELIABILITY /MODEL=ALPHA /STATISTICS=DESCRIPTIVE CORR /SUMMARY=TOTAL`.

For example, if you have 3 items measuring "trust", reliability analysis
tells you whether these items consistently measure the same concept.

## Usage

``` r
reliability(data, ..., weights = NULL, na.rm = TRUE)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- ...:

  The items to analyze. Use bare column names separated by commas, or
  tidyselect helpers like `starts_with("trust")`.

- weights:

  Optional survey weights for population-representative results

- na.rm:

  Remove missing values before calculating? (Default: TRUE). Uses
  listwise deletion (only complete cases across all items).

## Value

A reliability result object containing:

- alpha:

  Cronbach's Alpha (unstandardized)

- alpha_standardized:

  Cronbach's Alpha based on standardized items

- n_items:

  Number of items in the scale

- item_statistics:

  Mean, SD, and N for each item

- item_total:

  Corrected Item-Total Correlation and Alpha if Item Deleted

- inter_item_cor:

  Inter-item correlation matrix

- n:

  Sample size (listwise)

## Details

### Understanding the Results

**Cronbach's Alpha** tells you how internally consistent your scale is:

- Alpha \> 0.90: Excellent reliability

- Alpha 0.80 - 0.90: Good reliability

- Alpha 0.70 - 0.80: Acceptable reliability

- Alpha 0.60 - 0.70: Questionable reliability

- Alpha \< 0.60: Poor reliability - reconsider items

**Item-Total Correlation** shows how well each item fits the scale:

- Values \> 0.40: Item fits well

- Values 0.20 - 0.40: Item may need review

- Values \< 0.20: Consider removing the item

**Alpha if Item Deleted** shows what happens if you remove an item:

- If alpha increases when removing an item, that item hurts reliability

- If alpha decreases, the item contributes to the scale

### When to Use This

Run `reliability()` before creating scale scores with
[`scale_index`](https://YannickDiehl.github.io/mariposa/reference/scale_index.md):

1.  Select your items

2.  Check reliability

3.  If acceptable (alpha \> .70), create the index

4.  If not, review items and consider removing problematic ones

## See also

[`scale_index`](https://YannickDiehl.github.io/mariposa/reference/scale_index.md)
for creating mean indices after checking reliability.

[`pearson_cor`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md)
for bivariate correlations.

Other scale:
[`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md),
[`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md),
[`scale_index()`](https://YannickDiehl.github.io/mariposa/reference/scale_index.md)

## Examples

``` r
library(dplyr)
data(survey_data)

# Check reliability of trust items
reliability(survey_data, trust_government, trust_media, trust_science)
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

# With survey weights
reliability(survey_data, trust_government, trust_media, trust_science,
            weights = sampling_weight)
#> 
#> Weighted Reliability Analysis Results
#> -------------------------------------
#> - Items: trust_government, trust_media, trust_science
#> - N of Items: 3
#> - Weights: sampling_weight
#> 
#> Reliability Statistics
#> ---------------------------------------- 
#>   Cronbach's Alpha:              0.052
#>   Alpha (standardized):          0.053
#>   N of Items:                    3
#>   N (listwise):                  2150.20 (weighted)
#> 
#> Item Statistics
#> ---------------------------------------- 
#>              item  mean    sd      n
#>  trust_government 2.621 1.162 2150.2
#>       trust_media 2.434 1.158 2150.2
#>     trust_science 3.624 1.033 2150.2
#> 
#> Inter-Item Correlation Matrix:
#> ------------------------------ 
#>                  trust_government trust_media trust_science
#> trust_government            1.000       0.017         0.021
#> trust_media                 0.017       1.000         0.017
#> trust_science               0.021       0.017         1.000
#> ------------------------------ 
#> 
#> Item-Total Statistics
#> ---------------------------------------- 
#>              item scale_mean_deleted scale_var_deleted corrected_r
#>  trust_government               6.06             2.451       0.026
#>       trust_media               6.25             2.469       0.023
#>     trust_science               5.05             2.737       0.027
#>  alpha_deleted
#>          0.033
#>          0.041
#>          0.033

# Using tidyselect helpers
reliability(survey_data, starts_with("trust"))
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

# Grouped by region
survey_data %>%
  group_by(region) %>%
  reliability(trust_government, trust_media, trust_science)
#> 
#> Reliability Analysis Results
#> ----------------------------
#> 
#> Group: region = 1
#> -----------------
#> - Items: trust_government, trust_media, trust_science
#> - N of Items: 3
#> 
#> Reliability Statistics
#> ---------------------------------------- 
#>   Cronbach's Alpha:              0.037
#>   Alpha (standardized):          0.042
#>   N of Items:                    3
#>   N (listwise):                  422
#> 
#> Item Statistics
#> ---------------------------------------- 
#>              item  mean    sd   n
#>  trust_government 2.618 1.165 422
#>       trust_media 2.396 1.091 422
#>     trust_science 3.661 1.009 422
#> 
#> Inter-Item Correlation Matrix:
#> ------------------------------ 
#>                  trust_government trust_media trust_science
#> trust_government            1.000      -0.017         0.001
#> trust_media                -0.017       1.000         0.060
#> trust_science               0.001       0.060         1.000
#> ------------------------------ 
#> 
#> Item-Total Statistics
#> ---------------------------------------- 
#>              item scale_mean_deleted scale_var_deleted corrected_r
#>  trust_government               6.06             2.339      -0.012
#>       trust_media               6.28             2.378       0.026
#>     trust_science               5.01             2.503       0.042
#>  alpha_deleted
#>          0.112
#>          0.002
#>         -0.035
#> 
#> Group: region = 2
#> -----------------
#> - Items: trust_government, trust_media, trust_science
#> - N of Items: 3
#> 
#> Reliability Statistics
#> ---------------------------------------- 
#>   Cronbach's Alpha:              0.050
#>   Alpha (standardized):          0.050
#>   N of Items:                    3
#>   N (listwise):                  1713
#> 
#> Item Statistics
#> ---------------------------------------- 
#>              item  mean    sd    n
#>  trust_government 2.622 1.161 1713
#>       trust_media 2.438 1.172 1713
#>     trust_science 3.615 1.040 1713
#> 
#> Inter-Item Correlation Matrix:
#> ------------------------------ 
#>                  trust_government trust_media trust_science
#> trust_government            1.000       0.021         0.025
#> trust_media                 0.021       1.000         0.005
#> trust_science               0.025       0.005         1.000
#> ------------------------------ 
#> 
#> Item-Total Statistics
#> ---------------------------------------- 
#>              item scale_mean_deleted scale_var_deleted corrected_r
#>  trust_government               6.05             2.467       0.032
#>       trust_media               6.24             2.491       0.019
#>     trust_science               5.06             2.779       0.021
#>  alpha_deleted
#>          0.010
#>          0.049
#>          0.041
```
