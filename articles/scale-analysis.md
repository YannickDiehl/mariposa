# Scale Analysis

``` r
library(mariposa)
library(dplyr)
data(survey_data)
```

## Overview

Scale analysis is the process of combining multiple survey items into a
single score that reliably measures a concept. In mariposa, the typical
workflow is:

1.  Check reliability with
    [`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md)
    – do the items measure the same thing?
2.  Explore structure with
    [`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md)
    – how do items group into dimensions?
3.  Create scores with
    [`scale_index()`](https://YannickDiehl.github.io/mariposa/reference/scale_index.md)
    – compute mean indices
4.  Standardize with
    [`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md)
    – transform to comparable 0-100 scale

This guide walks through each step using the trust items from
`survey_data` (trust in government, media, and science).

| Function                                                                            | Purpose                                          |
|-------------------------------------------------------------------------------------|--------------------------------------------------|
| [`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md) | Check internal consistency (Cronbach’s Alpha)    |
| [`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md)                 | Discover underlying dimensions (factor analysis) |
| [`scale_index()`](https://YannickDiehl.github.io/mariposa/reference/scale_index.md) | Create mean indices across items                 |
| [`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md)             | Transform to Percent of Maximum Possible Scores  |

## Reliability Analysis

### Basic Usage

Before creating a scale score, check whether your items measure the same
concept consistently:

``` r
reliability(survey_data, trust_government, trust_media, trust_science)
#> Reliability Analysis: 3 items
#>   Cronbach's Alpha = 0.047 (Poor), N = 2135
```

The output shows Cronbach’s Alpha, item-total correlations, and
alpha-if-item-deleted – matching SPSS RELIABILITY output.

### Understanding the Output

**Cronbach’s Alpha** tells you how internally consistent your scale is:

- Alpha \> 0.90: Excellent
- 0.80 – 0.90: Good
- 0.70 – 0.80: Acceptable
- 0.60 – 0.70: Questionable
- Below 0.60: Reconsider your items

**Item-Total Correlation** shows how well each item fits:

- Above 0.40: Good fit
- 0.20 – 0.40: Needs review
- Below 0.20: Consider removing

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

When your items share a naming pattern, select them efficiently:

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

The output includes KMO measure, Bartlett’s test, eigenvalues, and the
rotated component matrix – matching SPSS FACTOR output.

### Understanding the Output

**KMO (Kaiser-Meyer-Olkin)** measures whether your data is suitable for
factor analysis:

- Above 0.80: Good to excellent
- 0.60 – 0.80: Acceptable
- Below 0.60: Factor analysis may not be appropriate

**Bartlett’s Test** should be significant ($p < .05$), confirming that
correlations exist in your data.

**Eigenvalues** show how much variance each component explains. By
default, components with eigenvalue \> 1 are retained (Kaiser
criterion).

**Factor Loadings** show item-component associations:

- Above 0.70: Strong
- 0.40 – 0.70: Moderate
- Below 0.40: Suppressed by default

### Choosing a Rotation

**Varimax** (default) assumes uncorrelated factors – simpler to
interpret:

``` r
efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science,
    rotation = "varimax")
#> Exploratory Factor Analysis: 6 items, 3 components (PCA/Varimax)
#>   KMO = 0.505 (Miserable), Variance explained: 61.0%
```

**Oblimin** allows correlated factors – often more realistic for social
science:

``` r
# Requires GPArotation package
efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science,
    rotation = "oblimin")
```

With oblimin, you get a Pattern Matrix (unique contributions) and
Structure Matrix (total correlations).

**Promax** is another oblique rotation, common in SPSS. Like oblimin, it
produces Pattern and Structure matrices:

``` r
efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science,
    rotation = "promax")
#> Exploratory Factor Analysis: 6 items, 3 components (PCA/Promax)
#>   KMO = 0.505 (Miserable), Variance explained: 61.0%
```

### Choosing an Extraction Method

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

ML extraction provides a **goodness-of-fit test** that evaluates whether
your factor model adequately reproduces the observed correlations. A
non-significant chi-square ($p > .05$) suggests good fit. ML also
reports initial communalities as SMC (squared multiple correlations)
instead of 1.0.

You can combine any extraction with any rotation:

``` r
# ML extraction with Promax rotation
efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science,
    extraction = "ml", rotation = "promax")
#> Exploratory Factor Analysis: 6 items, 3 components (ML/Promax)
#>   KMO = 0.505 (Miserable), Variance explained: 61.0%
```

### Fixing the Number of Factors

If you have a theoretical reason for a specific number of components:

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

### scale_index(): Mean Indices

After confirming reliability, create a mean index:

``` r
survey_data <- survey_data %>%
  mutate(m_trust = scale_index(., trust_government, trust_media, trust_science))

# Check the result
survey_data %>%
  describe(m_trust)
#> 
#> Descriptive Statistics
#> ----------------------
#>  Variable  Mean Median  SD Range IQR Skewness    N Missing
#>   m_trust 2.915      3 0.7     4   1    0.015 2500       0
#> ----------------------------------------
```

[`scale_index()`](https://YannickDiehl.github.io/mariposa/reference/scale_index.md)
works inside
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
computing the row-wise mean across items.

### Using tidyselect

``` r
survey_data <- survey_data %>%
  mutate(m_trust2 = scale_index(., starts_with("trust")))
```

### Using pick()

The [`pick()`](https://dplyr.tidyverse.org/reference/pick.html) function
works with both `%>%` and `|>` pipes:

``` r
survey_data <- survey_data %>%
  mutate(m_trust3 = scale_index(
    pick(trust_government, trust_media, trust_science)
  ))
```

### Minimum Valid Items

Require a minimum number of valid responses per respondent, matching
SPSS `MEAN.2()` syntax:

``` r
survey_data <- survey_data %>%
  mutate(m_trust_strict = scale_index(
    ., trust_government, trust_media, trust_science,
    min_valid = 2
  ))
```

If a respondent answered fewer than 2 items, they receive `NA` instead
of a potentially unreliable score.

### pomps(): Standardize to 0-100

Transform scores to a common 0-100 scale for cross-scale comparison:

``` r
survey_data <- survey_data %>%
  mutate(trust_pomps = pomps(m_trust, scale_min = 1, scale_max = 5))

survey_data %>%
  describe(trust_pomps)
#> 
#> Descriptive Statistics
#> ----------------------
#>     Variable   Mean Median   SD Range IQR Skewness    N Missing
#>  trust_pomps 47.885     50 17.5   100  25    0.015 2500       0
#> ----------------------------------------
```

A score of 0 means the minimum possible, 100 means the maximum. Always
specify the theoretical scale range for consistent results.

### Multiple Variables at Once

``` r
survey_data <- survey_data %>%
  mutate(across(
    c(trust_government, trust_media, trust_science),
    ~ pomps(.x, scale_min = 1, scale_max = 5),
    .names = "{.col}_pomps"
  ))
```

## Complete Example

A typical scale analysis workflow from start to finish:

``` r
# 1. Check reliability
rel <- reliability(survey_data, trust_government, trust_media, trust_science)
rel                # compact: Alpha + interpretation
#> Reliability Analysis: 3 items
#>   Cronbach's Alpha = 0.047 (Poor), N = 2135
summary(rel)       # detailed: item statistics, inter-item correlations
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
efa_result         # compact: KMO + variance explained
#> Exploratory Factor Analysis: 3 items, 1 component (PCA/Unrotated)
#>   KMO = 0.506 (Miserable), Variance explained: 34.7%
summary(efa_result, communalities = FALSE)  # detailed, skip communalities
#> 
#> Exploratory Factor Analysis (PCA, Unrotated) Results
#> ----------------------------------------------------
#> - Variables: trust_government, trust_media, trust_science
#> - Extraction: Principal Component Analysis
#> - Rotation: None
#> - N of Factors: 1
#> 
#> KMO and Bartlett's Test
#> ---------------------------------------- 
#>   Kaiser-Meyer-Olkin Measure:     0.506
#>   Bartlett's Chi-Square:          3.203
#>   df:                             3
#>   Sig.:                           0.361
#> 
#> Total Variance Explained
#> ---------------------------------------- 
#>   PC1  Eigenvalue: 1.042  Variance: 34.719%  Cumulative: 34.719%
#>   PC2  Eigenvalue: 0.992  Variance: 33.056%  Cumulative: 67.775%
#>   PC3  Eigenvalue: 0.967  Variance: 32.225%  Cumulative: 100.000%
#> 
#> Component Matrix (unrotated)
#> ---------------------------------------- 
#>                      PC1
#> trust_science     -0.678
#> trust_government  -0.597
#> trust_media       -0.475
#> Extraction Method: Principal Component Analysis.

# 3. Create mean index (Alpha was acceptable)
survey_data <- survey_data %>%
  mutate(m_trust = scale_index(., trust_government, trust_media, trust_science))

# 4. Transform to POMPS for reporting
survey_data <- survey_data %>%
  mutate(trust_pomps = pomps(m_trust, scale_min = 1, scale_max = 5))

# 5. Use in further analyses
survey_data %>%
  t_test(m_trust, group = region, weights = sampling_weight)
#> t-Test: m_trust by region [Weighted]
#>   t(799.2) = -0.406, p = 0.685 , g = -0.020 (negligible), N = 2516
```

## Practical Tips

1.  **Always check reliability first.** A mean index from unreliable
    items produces meaningless results. Aim for Alpha \> .70.

2.  **Use `min_valid` wisely.** For scales with 3-5 items,
    `min_valid = 2` is a reasonable compromise between data loss and
    score quality.

3.  **Specify `scale_min` and `scale_max` in
    [`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md).**
    Using observed values instead of theoretical range makes scores
    incomparable across samples.

4.  **Run EFA when you have many items.** With 6+ items, factor analysis
    can reveal unexpected dimensions before you create indices.

5.  **Check grouped reliability.** Alpha may differ substantially across
    subgroups (e.g., regions, education levels). A scale that works well
    overall might be unreliable in specific groups.

## Summary

1.  [`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md)
    checks whether items form a consistent scale (Cronbach’s Alpha)
2.  [`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md)
    discovers underlying dimensions in your items (Principal Component
    Analysis)
3.  [`scale_index()`](https://YannickDiehl.github.io/mariposa/reference/scale_index.md)
    computes mean indices for use in further analyses
4.  [`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md)
    transforms scores to a comparable 0-100 scale

## Next Steps

- Use your new scale scores in group comparisons – see
  [`vignette("hypothesis-testing")`](https://YannickDiehl.github.io/mariposa/articles/hypothesis-testing.md)
- Explore relationships between scales – see
  [`vignette("correlation-analysis")`](https://YannickDiehl.github.io/mariposa/articles/correlation-analysis.md)
- Predict outcomes with regression – see
  [`vignette("regression-analysis")`](https://YannickDiehl.github.io/mariposa/articles/regression-analysis.md)
- Apply survey weights to all analyses – see
  [`vignette("survey-weights")`](https://YannickDiehl.github.io/mariposa/articles/survey-weights.md)
