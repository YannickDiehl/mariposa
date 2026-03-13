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

Use [`summary()`](https://rdrr.io/r/base/summary.html) for the full
SPSS-style output with toggleable sections.

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
[`row_means`](https://YannickDiehl.github.io/mariposa/reference/row_means.md):

1.  Select your items

2.  Check reliability

3.  If acceptable (alpha \> .70), create the index

4.  If not, review items and consider removing problematic ones

## See also

[`row_means`](https://YannickDiehl.github.io/mariposa/reference/row_means.md)
for creating mean indices after checking reliability.

[`pearson_cor`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md)
for bivariate correlations.

[`summary.reliability`](https://YannickDiehl.github.io/mariposa/reference/summary.reliability.md)
for detailed output with toggleable sections.

Other scale:
[`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md),
[`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md),
[`row_count()`](https://YannickDiehl.github.io/mariposa/reference/row_count.md),
[`row_means()`](https://YannickDiehl.github.io/mariposa/reference/row_means.md),
[`row_sums()`](https://YannickDiehl.github.io/mariposa/reference/row_sums.md)

## Examples

``` r
library(dplyr)
data(survey_data)

# Check reliability of trust items
reliability(survey_data, trust_government, trust_media, trust_science)
#> Reliability Analysis: 3 items
#>   Cronbach's Alpha = 0.047 (Poor), N = 2135

# With survey weights
reliability(survey_data, trust_government, trust_media, trust_science,
            weights = sampling_weight)
#> Reliability Analysis: 3 items [Weighted]
#>   Cronbach's Alpha = 0.052 (Poor), N = 2150

# Using tidyselect helpers
reliability(survey_data, starts_with("trust"))
#> Reliability Analysis: 3 items
#>   Cronbach's Alpha = 0.047 (Poor), N = 2135

# Grouped by region
survey_data %>%
  group_by(region) %>%
  reliability(trust_government, trust_media, trust_science)
#> [region = 1]
#> Reliability Analysis: 3 items
#>   Cronbach's Alpha = 0.037 (Poor), N = 422
#> [region = 2]
#> Reliability Analysis: 3 items
#>   Cronbach's Alpha = 0.050 (Poor), N = 1713

# --- Three-layer output ---
result <- reliability(survey_data, trust_government, trust_media, trust_science)
result              # compact one-line overview
#> Reliability Analysis: 3 items
#>   Cronbach's Alpha = 0.047 (Poor), N = 2135
summary(result)     # full detailed output with all sections
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
