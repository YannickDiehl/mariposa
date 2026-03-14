# Scale Analysis

``` r
library(mariposa)
library(dplyr)
data(survey_data)
```

## Overview

Scale analysis combines multiple survey items into a single score that
reliably measures a concept. The typical workflow is:

1.  **Check reliability** with
    [`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md)
    — do the items measure the same thing?
2.  **Explore structure** with
    [`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md)
    — how do items group into dimensions?
3.  **Create scores** with
    [`row_means()`](https://YannickDiehl.github.io/mariposa/reference/row_means.md)
    — compute mean indices
4.  **Standardize** with
    [`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md)
    — transform to a comparable 0–100 scale

This guide uses the trust items from `survey_data` (trust in government,
media, and science).

| Function                                                                            | Purpose                                          |
|-------------------------------------------------------------------------------------|--------------------------------------------------|
| [`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md) | Internal consistency (Cronbach’s Alpha)          |
| [`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md)                 | Discover underlying dimensions (factor analysis) |
| [`row_means()`](https://YannickDiehl.github.io/mariposa/reference/row_means.md)     | Row-wise mean indices                            |
| [`row_sums()`](https://YannickDiehl.github.io/mariposa/reference/row_sums.md)       | Row-wise sums                                    |
| [`row_count()`](https://YannickDiehl.github.io/mariposa/reference/row_count.md)     | Count specific values per row                    |
| [`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md)             | Percent of Maximum Possible Scores (0–100)       |

## Reliability Analysis

### Basic Usage

``` r
reliability(survey_data, trust_government, trust_media, trust_science)
#> Reliability Analysis: 3 items
#>   Cronbach's Alpha = 0.047 (Poor), N = 2135
```

### Detailed Output

``` r
rel <- reliability(survey_data, trust_government, trust_media, trust_science)
summary(rel)
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

The detailed output shows item-total correlations,
alpha-if-item-deleted, and inter-item correlations — matching SPSS
RELIABILITY.

### Interpreting Cronbach’s Alpha

- Alpha \> 0.90: Excellent
- 0.80 – 0.90: Good
- 0.70 – 0.80: Acceptable
- 0.60 – 0.70: Questionable
- Below 0.60: Reconsider your items

**Item-Total Correlation** shows how well each item fits the scale.
Values above 0.40 indicate good fit; below 0.20 suggests the item does
not belong.

**Alpha if Item Deleted** shows what happens without each item. If alpha
increases when you remove an item, that item weakens the scale.

### With Survey Weights

``` r
reliability(survey_data, trust_government, trust_media, trust_science,
            weights = sampling_weight)
#> Reliability Analysis: 3 items [Weighted]
#>   Cronbach's Alpha = 0.052 (Poor), N = 2150
```

### Using tidyselect

``` r
reliability(survey_data, starts_with("trust"))
#> Reliability Analysis: 3 items
#>   Cronbach's Alpha = 0.047 (Poor), N = 2135
```

### Grouped Analysis

Check whether reliability holds across subgroups:

``` r
survey_data %>%
  group_by(region) %>%
  reliability(trust_government, trust_media, trust_science)
#> [region = 1]
#> Reliability Analysis: 3 items
#>   Cronbach's Alpha = 0.037 (Poor), N = 422
#> [region = 2]
#> Reliability Analysis: 3 items
#>   Cronbach's Alpha = 0.050 (Poor), N = 1713
```

A scale that works well overall might be unreliable in specific
subgroups. Always check when your sample spans diverse populations.

## Exploratory Factor Analysis

### Basic Usage

When you have many items,
[`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md)
reveals how they group into underlying dimensions:

``` r
efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science)
#> Exploratory Factor Analysis: 6 items, 3 components (PCA/Varimax)
#>   KMO = 0.505 (Miserable), Variance explained: 61.0%
```

### Detailed Output

``` r
efa_result <- efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science)

summary(efa_result)
#> 
#> Exploratory Factor Analysis (PCA, Varimax) Results
#> --------------------------------------------------
#> - Variables: political_orientation, environmental_concern, life_satisfaction, trust_government, trust_media, trust_science
#> - Extraction: Principal Component Analysis
#> - Rotation: Varimax with Kaiser Normalization
#> - N of Factors: 3
#> 
#> KMO and Bartlett's Test
#> ---------------------------------------- 
#>   Kaiser-Meyer-Olkin Measure:     0.505
#>   Bartlett's Chi-Square:          932.068
#>   df:                             15
#>   Sig.:                           0.000
#> 
#> Communalities
#> ---------------------------------------- 
#>               variable initial extraction
#>  political_orientation       1      0.786
#>  environmental_concern       1      0.783
#>      life_satisfaction       1      0.668
#>       trust_government       1      0.347
#>            trust_media       1      0.475
#>          trust_science       1      0.598
#> Extraction Method: Principal Component Analysis.
#> 
#> Total Variance Explained
#> ---------------------------------------- 
#>   PC1  Eigenvalue: 1.600  Variance: 26.666%  Cumulative: 26.666%
#>   PC2  Eigenvalue: 1.041  Variance: 17.358%  Cumulative: 44.024%
#>   PC3  Eigenvalue: 1.017  Variance: 16.955%  Cumulative: 60.979%
#>   PC4  Eigenvalue: 0.980  Variance: 16.334%  Cumulative: 77.313%
#>   PC5  Eigenvalue: 0.949  Variance: 15.814%  Cumulative: 93.127%
#>   PC6  Eigenvalue: 0.412  Variance: 6.873%  Cumulative: 100.000%
#> 
#> Rotation Sums of Squared Loadings
#> ---------------------------------------- 
#>   PC1  SS Loading: 1.598  Variance: 26.634%  Cumulative: 26.634%
#>   PC2  SS Loading: 1.039  Variance: 17.325%  Cumulative: 43.959%
#>   PC3  SS Loading: 1.021  Variance: 17.020%  Cumulative: 60.979%
#> 
#> Component Matrix (unrotated)
#> ---------------------------------------- 
#>                           PC1     PC2     PC3
#> political_orientation   0.885                
#> environmental_concern  -0.885                
#> trust_science                  -0.672        
#> trust_government               -0.547        
#> trust_media                    -0.524  -0.448
#> life_satisfaction                      -0.809
#> Extraction Method: Principal Component Analysis.
#> 
#> Rotated Component Matrix
#> ---------------------------------------- 
#>                           PC1     PC2     PC3
#> political_orientation   0.887                
#> environmental_concern  -0.884                
#> trust_science                  -0.762        
#> trust_government               -0.566        
#> life_satisfaction                      -0.789
#> trust_media                            -0.620
#> Extraction Method: Principal Component Analysis.
#> Rotation Method: Varimax with Kaiser Normalization.
```

### Understanding the Output

**KMO (Kaiser-Meyer-Olkin)** measures sampling adequacy for factor
analysis:

- Above 0.80: Good to excellent
- 0.60 – 0.80: Acceptable
- Below 0.60: Factor analysis may not be appropriate

**Bartlett’s Test** should be significant ($p < .05$), confirming that
meaningful correlations exist.

**Eigenvalues** show variance explained per component. By default,
components with eigenvalue \> 1 are retained (Kaiser criterion).

**Factor Loadings** show item-component associations:

- Above 0.70: Strong
- 0.40 – 0.70: Moderate
- Below 0.40: Suppressed by default

### Rotation Methods

**Varimax** (default) — assumes uncorrelated factors:

``` r
efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science,
    rotation = "varimax")
#> Exploratory Factor Analysis: 6 items, 3 components (PCA/Varimax)
#>   KMO = 0.505 (Miserable), Variance explained: 61.0%
```

**Promax** — allows correlated factors, produces Pattern and Structure
matrices:

``` r
efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science,
    rotation = "promax")
#> Exploratory Factor Analysis: 6 items, 3 components (PCA/Promax)
#>   KMO = 0.505 (Miserable), Variance explained: 61.0%
```

**Oblimin** — another oblique rotation, common in psychology:

``` r
# Requires GPArotation package
efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science,
    rotation = "oblimin")
```

### Extraction Methods

By default,
[`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md) uses
PCA (Principal Component Analysis). For a true factor analysis model,
use Maximum Likelihood:

``` r
efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science,
    extraction = "ml")
#> Exploratory Factor Analysis: 6 items, 3 components (ML/Varimax)
#>   KMO = 0.505 (Miserable), Variance explained: 61.0%
```

ML extraction provides a goodness-of-fit test and uses SMC (squared
multiple correlations) as initial communalities. Combine any extraction
with any rotation:

``` r
efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science,
    extraction = "ml", rotation = "promax")
#> Exploratory Factor Analysis: 6 items, 3 components (ML/Promax)
#>   KMO = 0.505 (Miserable), Variance explained: 61.0%
```

### Fixing the Number of Factors

``` r
efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science,
    n_factors = 2)
#> Exploratory Factor Analysis: 6 items, 2 components (PCA/Varimax)
#>   KMO = 0.505 (Miserable), Variance explained: 44.0%
```

### With Survey Weights

``` r
efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science,
    weights = sampling_weight)
#> Exploratory Factor Analysis: 6 items, 3 components (PCA/Varimax) [Weighted]
#>   KMO = 0.505 (Miserable), Variance explained: 61.0%
```

## Creating Scale Scores

After confirming reliability, create scores using the row operation
functions. For details on
[`row_means()`](https://YannickDiehl.github.io/mariposa/reference/row_means.md),
[`row_sums()`](https://YannickDiehl.github.io/mariposa/reference/row_sums.md),
[`row_count()`](https://YannickDiehl.github.io/mariposa/reference/row_count.md),
and
[`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md),
see
[`vignette("data-transformation")`](https://YannickDiehl.github.io/mariposa/articles/data-transformation.md).

### Quick Scale Construction

``` r
# Create mean index
survey_data <- survey_data %>%
  mutate(m_trust = row_means(., trust_government, trust_media, trust_science,
                             min_valid = 2))

# Transform to 0-100 scale
survey_data <- survey_data %>%
  mutate(trust_pomps = pomps(m_trust, scale_min = 1, scale_max = 5))

# Check the result
survey_data %>%
  describe(m_trust, trust_pomps)
#> 
#> Descriptive Statistics
#> ----------------------
#>     Variable   Mean Median     SD Range IQR Skewness    N Missing
#>      m_trust  2.916      3  0.691     4   1     0.02 2484      16
#>  trust_pomps 47.892     50 17.283   100  25     0.02 2484      16
#> ----------------------------------------
```

### Using the Scale in Analysis

``` r
# Group comparison
survey_data %>%
  t_test(m_trust, group = gender, weights = sampling_weight)
#> t-Test: m_trust by gender [Weighted]
#>   t(2457.2) = -2.362, p = 0.018 *, g = -0.095 (negligible), N = 2499
```

``` r
# As a predictor in regression
survey_data %>%
  linear_regression(life_satisfaction ~ m_trust + age + income,
                    weights = sampling_weight)
#> Linear Regression: life_satisfaction ~ m_trust + age + income [Weighted]
#>   R2 = 0.201, adj.R2 = 0.200, F(3, 2109) = 177.00, p < 0.001 ***, N = 2113
```

## Complete Example

``` r
# 1. Check reliability
rel <- reliability(survey_data, trust_government, trust_media, trust_science)
rel
#> Reliability Analysis: 3 items
#>   Cronbach's Alpha = 0.047 (Poor), N = 2135
summary(rel)
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

# 2. Explore factor structure
efa_result <- efa(survey_data, trust_government, trust_media, trust_science)
efa_result
#> Exploratory Factor Analysis: 3 items, 1 component (PCA/Unrotated)
#>   KMO = 0.506 (Miserable), Variance explained: 34.7%

# 3. Create mean index (Alpha was acceptable)
survey_data <- survey_data %>%
  mutate(m_trust = row_means(., trust_government, trust_media, trust_science,
                             min_valid = 2))

# 4. Transform to POMPS
survey_data <- survey_data %>%
  mutate(trust_pomps = pomps(m_trust, scale_min = 1, scale_max = 5))

# 5. Use in further analysis
survey_data %>%
  group_by(education) %>%
  describe(m_trust, trust_pomps, weights = sampling_weight)
#> 
#> Weighted Descriptive Statistics
#> -------------------------------
#> 
#> Group: education = Basic Secondary
#> ----------------------------------
#> ----------------------------------------
#>     Variable   Mean Median     SD Range IQR Skewness Effective_N
#>      m_trust  2.924      3  0.696     4   1    0.023       829.7
#>  trust_pomps 48.093     50 17.390   100  25    0.023       829.7
#> ----------------------------------------
#> 
#> Group: education = Intermediate Secondary
#> -----------------------------------------
#> ----------------------------------------
#>     Variable   Mean Median     SD Range IQR Skewness Effective_N
#>      m_trust  2.920      3  0.697     4   1    0.035       617.7
#>  trust_pomps 48.002     50 17.421   100  25    0.035       617.7
#> ----------------------------------------
#> 
#> Group: education = Academic Secondary
#> -------------------------------------
#> ----------------------------------------
#>     Variable   Mean Median     SD Range    IQR Skewness Effective_N
#>      m_trust  2.922      3  0.691     4  0.833   -0.006       616.4
#>  trust_pomps 48.053     50 17.267   100 20.833   -0.006       616.4
#> ----------------------------------------
#> 
#> Group: education = University
#> -----------------------------
#> ----------------------------------------
#>     Variable   Mean Median     SD Range IQR Skewness Effective_N
#>      m_trust  2.885      3  0.686     4   1    0.062       389.6
#>  trust_pomps 47.132     50 17.157   100  25    0.062       389.6
#> ----------------------------------------
```

## Practical Tips

1.  **Always check reliability first.** A mean index from unreliable
    items produces meaningless results. Aim for Alpha \> .70.

2.  **Use `min_valid` wisely.** For 3–5 item scales, `min_valid = 2` is
    a reasonable compromise. For longer scales, require at least half
    the items.

3.  **Specify theoretical `scale_min` / `scale_max` in
    [`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md).**
    Using observed values makes scores sample-dependent and
    non-comparable.

4.  **Run EFA when you have 6+ items.** Factor analysis can reveal
    unexpected dimensions before you create indices.

5.  **Check grouped reliability.** A scale that works well overall may
    be unreliable in specific subgroups (e.g., different regions or
    education levels).

## Summary

1.  [`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md)
    checks whether items form a consistent scale (Cronbach’s Alpha)
2.  [`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md)
    discovers underlying dimensions (PCA or ML extraction,
    Varimax/Oblimin/Promax rotation)
3.  [`row_means()`](https://YannickDiehl.github.io/mariposa/reference/row_means.md)
    creates mean indices;
    [`row_sums()`](https://YannickDiehl.github.io/mariposa/reference/row_sums.md)
    and
    [`row_count()`](https://YannickDiehl.github.io/mariposa/reference/row_count.md)
    provide alternatives
4.  [`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md)
    transforms scores to a comparable 0–100 scale
5.  Always validate reliability **before** creating scale scores

## Next Steps

- Learn about row operations and transformations — see
  [`vignette("data-transformation")`](https://YannickDiehl.github.io/mariposa/articles/data-transformation.md)
- Use scales in regression models — see
  [`vignette("regression-analysis")`](https://YannickDiehl.github.io/mariposa/articles/regression-analysis.md)
- Compare scale scores across groups — see
  [`vignette("hypothesis-testing")`](https://YannickDiehl.github.io/mariposa/articles/hypothesis-testing.md)
- Apply survey weights — see
  [`vignette("survey-weights")`](https://YannickDiehl.github.io/mariposa/articles/survey-weights.md)
