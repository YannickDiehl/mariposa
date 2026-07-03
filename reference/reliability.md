# Check How Reliably Your Scale Measures a Concept

`reliability()` calculates Cronbach's Alpha, McDonald's Omega, and
detailed item statistics to evaluate whether your survey items form a
reliable scale. This is the R equivalent of SPSS's
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

- omega:

  McDonald's Omega (raw/total, one-factor ML model; NA for fewer than 3
  items)

- omega_std:

  Standardized Omega (correlation metric)

- n_items:

  Number of items in the scale

- item_statistics:

  Mean, SD, and N for each item

- item_total:

  Corrected Item-Total Correlation, Alpha if Item Deleted, and Omega if
  Item Deleted

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

### McDonald's Omega

**McDonald's Omega** (omega total) is a factor-model-based reliability
coefficient. Where alpha assumes every item measures the construct
equally well (tau-equivalence), omega fits a one-factor model and lets
each item carry its own loading. The two agree when items are roughly
tau-equivalent; omega is typically slightly higher (and the more
accurate estimate) when loadings differ across items, which is the
common case in survey scales (Hayes & Coutts, 2020).

`reliability()` reports two variants, mirroring the two alpha variants:
`omega` from the covariance metric (analogous to raw alpha) and
`omega_std` from the correlation metric (analogous to standardized
alpha). **Omega if Item Deleted** refits the one-factor model without
each item, mirroring Alpha if Item Deleted.

A one-factor model needs at least 3 items to be identified: with fewer
than 3 items the omega fields are `NA` (alpha is still computed), and
Omega if Item Deleted is `NA` whenever the reduced scale would fall
below 3 items.

### Weighted variants and validation status

Cronbach's alpha and the item statistics are validated against SPSS v29
`RELIABILITY` output (weighted and unweighted). McDonald's omega is
currently an R-only statistic (Tier 4 per the Validation Charter): SPSS
offers omega from v27 onward, but IBM's algorithm documentation for it
is not publicly retrievable and no SPSS v29 reference run exists yet.
mariposa computes omega from a one-factor maximum-likelihood solution
(the same estimator family as
[`efa`](https://YannickDiehl.github.io/mariposa/reference/efa.md) with
`fm = "ml"`). The weighted omega uses the same weighted correlation and
covariance matrices as the weighted alpha and reduces exactly to the
unweighted omega when all weights equal 1 (enforced by an internal
invariance suite); see
[`vignette("spss-compatibility")`](https://YannickDiehl.github.io/mariposa/articles/spss-compatibility.md)
for validation status.

### When to Use This

Run `reliability()` before creating scale scores with
[`row_means`](https://YannickDiehl.github.io/mariposa/reference/row_means.md):

1.  Select your items

2.  Check reliability

3.  If acceptable (alpha \> .70), create the index

4.  If not, review items and consider removing problematic ones

## References

McDonald, R. P. (1999). *Test Theory: A Unified Treatment*. Mahwah, NJ:
Lawrence Erlbaum.

Hayes, A. F., & Coutts, J. J. (2020). Use omega rather than Cronbach's
alpha for estimating reliability. But... *Communication Methods and
Measures*, 14(1), 1-24.

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
#>   Cronbach's Alpha = 0.047 (Poor), McDonald's Omega = 0.047, N = 2135

# With survey weights
reliability(survey_data, trust_government, trust_media, trust_science,
            weights = sampling_weight)
#> Reliability Analysis: 3 items [Weighted]
#>   Cronbach's Alpha = 0.052 (Poor), McDonald's Omega = 0.053, N = 2150

# Using tidyselect helpers
reliability(survey_data, starts_with("trust"))
#> Reliability Analysis: 3 items
#>   Cronbach's Alpha = 0.047 (Poor), McDonald's Omega = 0.047, N = 2135

# Grouped by region
survey_data %>%
  group_by(region) %>%
  reliability(trust_government, trust_media, trust_science)
#> [region = 1]
#> Reliability Analysis: 3 items
#>   Cronbach's Alpha = 0.037 (Poor), McDonald's Omega = 0.349, N = 422
#> [region = 2]
#> Reliability Analysis: 3 items
#>   Cronbach's Alpha = 0.050 (Poor), McDonald's Omega = 0.071, N = 1713

# --- Three-layer output ---
result <- reliability(survey_data, trust_government, trust_media, trust_science)
result              # compact one-line overview
#> Reliability Analysis: 3 items
#>   Cronbach's Alpha = 0.047 (Poor), McDonald's Omega = 0.047, N = 2135
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
#>   McDonald's Omega:              0.047
#>   Omega (standardized):          0.048
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
#>  alpha_deleted omega_deleted
#>          0.029            NA
#>          0.040            NA
#>          0.027            NA
#> Note: Omega if item deleted requires at least 4 items
#> (a one-factor model on the remaining 2 items is not identified).
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
#>   McDonald's Omega:              0.047
#>   Omega (standardized):          0.048
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
#>  alpha_deleted omega_deleted
#>          0.029            NA
#>          0.040            NA
#>          0.027            NA
#> Note: Omega if item deleted requires at least 4 items
#> (a one-factor model on the remaining 2 items is not identified).
```
