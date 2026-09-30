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

| Function | Purpose |
|----|----|
| [`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md) | Internal consistency (Cronbach’s Alpha, McDonald’s Omega) |
| [`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md) | Discover underlying dimensions (factor analysis) |
| [`row_means()`](https://YannickDiehl.github.io/mariposa/reference/row_means.md) | Row-wise mean indices |
| [`row_sums()`](https://YannickDiehl.github.io/mariposa/reference/row_sums.md) | Row-wise sums |
| [`row_count()`](https://YannickDiehl.github.io/mariposa/reference/row_count.md) | Count specific values per row |
| [`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md) | Percent of Maximum Possible Scores (0–100) |

## Reliability Analysis

### Basic Usage

``` r

reliability(survey_data, trust_government, trust_media, trust_science)
#> Reliability Analysis: 3 items
#>   Cronbach's Alpha = 0.047 (Poor), McDonald's Omega = 0.047, N = 2135
#> Use summary() for detailed output.
```

### Detailed Output

``` r

rel <- reliability(survey_data, trust_government, trust_media, trust_science)
summary(rel)
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
#>   ---------------------------------------------------------------------------------------
#>   Item                                                         Mean  Std. Deviation     N
#>   ---------------------------------------------------------------------------------------
#>   trust_government  Trust in government (1=none, 5=complete)  2.621           1.162  2135
#>   trust_media       Trust in media (1=none, 5=complete)       2.430           1.156  2135
#>   trust_science     Trust in science (1=none, 5=complete)     3.624           1.034  2135
#>   ---------------------------------------------------------------------------------------
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

### McDonald’s Omega

Alongside alpha,
[`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md)
reports **McDonald’s Omega**, a factor-model-based reliability
coefficient. Alpha assumes all items measure the construct equally well;
omega fits a one-factor model and lets each item carry its own loading,
which usually makes it the more accurate estimate when item loadings
differ. The same thresholds as for alpha are commonly applied. Omega
needs at least 3 items (with 2 items it is reported as `NA`), and
**Omega if Item Deleted** appears in the item-total table for scales of
4 or more items.

Note: omega is currently an R-only statistic (no SPSS reference run
yet); see
[`vignette("spss-compatibility")`](https://YannickDiehl.github.io/mariposa/articles/spss-compatibility.md)
for its validation status.

### With Survey Weights

``` r

reliability(survey_data, trust_government, trust_media, trust_science,
            weights = sampling_weight)
#> Reliability Analysis: 3 items [Weighted]
#>   Cronbach's Alpha = 0.052 (Poor), McDonald's Omega = 0.053, N = 2150
#> Use summary() for detailed output.
```

### Using tidyselect

``` r

reliability(survey_data, starts_with("trust"))
#> Reliability Analysis: 3 items
#>   Cronbach's Alpha = 0.047 (Poor), McDonald's Omega = 0.047, N = 2135
#> Use summary() for detailed output.
```

### Grouped Analysis

Check whether reliability holds across subgroups:

``` r

survey_data %>%
  group_by(region) %>%
  reliability(trust_government, trust_media, trust_science)
#> [region = East]
#> Reliability Analysis: 3 items
#>   Cronbach's Alpha = 0.037 (Poor), McDonald's Omega = not computed, N = 422
#> [region = West]
#> Reliability Analysis: 3 items
#>   Cronbach's Alpha = 0.050 (Poor), McDonald's Omega = 0.071, N = 1713
#> Use summary() for detailed output.
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
#>   KMO = 0.505 (Miserable), Variance explained: 61.0%, N = 2168 (smallest pairwise)
#> Use summary() for detailed output.
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
#> - Variables:
#>     political_orientation  Political orientation (1=left, 5=right)
#>     environmental_concern  Environmental concern (1=low, 5=high)
#>     life_satisfaction      Life satisfaction (1=dissatisfied, 5=satisfied)
#>     trust_government       Trust in government (1=none, 5=complete)
#>     trust_media            Trust in media (1=none, 5=complete)
#>     trust_science          Trust in science (1=none, 5=complete)
#> - Extraction: Principal Component Analysis
#> - Rotation: Varimax with Kaiser Normalization
#> - N of Components: 3
#> - N (smallest pairwise): 2168
#> 
#> Descriptive Statistics
#>   --------------------------------------------------------------------------------------------------------------------
#>   Variable                                                                 Mean  Std. Deviation  Analysis N  Missing N
#>   --------------------------------------------------------------------------------------------------------------------
#>   political_orientation  Political orientation (1=left, 5=right)          2.722           1.086        2299        201
#>   environmental_concern  Environmental concern (1=low, 5=high)            3.573           1.194        2400        100
#>   life_satisfaction      Life satisfaction (1=dissatisfied, 5=satisfied)  3.628           1.153        2421         79
#>   trust_government       Trust in government (1=none, 5=complete)         2.621           1.163        2354        146
#>   trust_media            Trust in media (1=none, 5=complete)              2.452           1.163        2367        133
#>   trust_science          Trust in science (1=none, 5=complete)            3.641           1.028        2398        102
#>   --------------------------------------------------------------------------------------------------------------------
#> 
#> KMO and Bartlett's Test
#> ---------------------------------------- 
#>   Kaiser-Meyer-Olkin Measure:     0.505
#>   Bartlett's Chi-Square:          932.068
#>   df:                             15
#>   Sig.:                           <.001
#> 
#> Communalities
#>   -------------------------------------------------------------------------------------------
#>   Variable                                                                Initial  Extraction
#>   -------------------------------------------------------------------------------------------
#>   political_orientation  Political orientation (1=left, 5=right)            1.000       0.786
#>   environmental_concern  Environmental concern (1=low, 5=high)              1.000       0.783
#>   life_satisfaction      Life satisfaction (1=dissatisfied, 5=satisfied)    1.000       0.668
#>   trust_government       Trust in government (1=none, 5=complete)           1.000       0.347
#>   trust_media            Trust in media (1=none, 5=complete)                1.000       0.475
#>   trust_science          Trust in science (1=none, 5=complete)              1.000       0.598
#>   -------------------------------------------------------------------------------------------
#> Extraction Method: Principal Component Analysis.
#> 
#> Total Variance Explained
#>   -------------------------------------------------------------------------
#>              Initial Eigenvalues   Extraction Sums      Rotation Sums
#>   Component  Total % Var.  Cum. %  Total % Var. Cum. %  Total % Var. Cum. %
#>   -------------------------------------------------------------------------
#>           1  1.600 26.666  26.666  1.600 26.666 26.666  1.598 26.635 26.635
#>           2  1.041 17.358  44.024  1.041 17.358 44.024  1.039 17.324 43.959
#>           3  1.017 16.955  60.979  1.017 16.955 60.979  1.021 17.020 60.979
#>           4  0.980 16.334  77.313
#>           5  0.949 15.814  93.127
#>           6  0.412  6.873 100.000
#>   -------------------------------------------------------------------------
#> Sums = sums of squared loadings.
#> Extraction Method: Principal Component Analysis.
#> 
#> Component Matrix (unrotated)
#> ----------------------------------------
#>                                                                            PC1
#> political_orientation  Political orientation (1=left, 5=right)          -0.885
#> environmental_concern  Environmental concern (1=low, 5=high)             0.885
#> trust_science          Trust in science (1=none, 5=complete)                  
#> trust_government       Trust in government (1=none, 5=complete)               
#> trust_media            Trust in media (1=none, 5=complete)                    
#> life_satisfaction      Life satisfaction (1=dissatisfied, 5=satisfied)        
#>                                                                            PC2
#> political_orientation  Political orientation (1=left, 5=right)                
#> environmental_concern  Environmental concern (1=low, 5=high)                  
#> trust_science          Trust in science (1=none, 5=complete)             0.672
#> trust_government       Trust in government (1=none, 5=complete)          0.547
#> trust_media            Trust in media (1=none, 5=complete)               0.524
#> life_satisfaction      Life satisfaction (1=dissatisfied, 5=satisfied)        
#>                                                                            PC3
#> political_orientation  Political orientation (1=left, 5=right)                
#> environmental_concern  Environmental concern (1=low, 5=high)                  
#> trust_science          Trust in science (1=none, 5=complete)                  
#> trust_government       Trust in government (1=none, 5=complete)               
#> trust_media            Trust in media (1=none, 5=complete)               0.448
#> life_satisfaction      Life satisfaction (1=dissatisfied, 5=satisfied)   0.809
#> Extraction Method: Principal Component Analysis.
#> 
#> Rotated Component Matrix
#> ----------------------------------------
#>                                                                            PC1
#> political_orientation  Political orientation (1=left, 5=right)          -0.887
#> environmental_concern  Environmental concern (1=low, 5=high)             0.884
#> trust_science          Trust in science (1=none, 5=complete)                  
#> trust_government       Trust in government (1=none, 5=complete)               
#> life_satisfaction      Life satisfaction (1=dissatisfied, 5=satisfied)        
#> trust_media            Trust in media (1=none, 5=complete)                    
#>                                                                            PC2
#> political_orientation  Political orientation (1=left, 5=right)                
#> environmental_concern  Environmental concern (1=low, 5=high)                  
#> trust_science          Trust in science (1=none, 5=complete)             0.762
#> trust_government       Trust in government (1=none, 5=complete)          0.566
#> life_satisfaction      Life satisfaction (1=dissatisfied, 5=satisfied)        
#> trust_media            Trust in media (1=none, 5=complete)                    
#>                                                                            PC3
#> political_orientation  Political orientation (1=left, 5=right)                
#> environmental_concern  Environmental concern (1=low, 5=high)                  
#> trust_science          Trust in science (1=none, 5=complete)                  
#> trust_government       Trust in government (1=none, 5=complete)               
#> life_satisfaction      Life satisfaction (1=dissatisfied, 5=satisfied)   0.789
#> trust_media            Trust in media (1=none, 5=complete)               0.620
#> Extraction Method: Principal Component Analysis.
#> Rotation Method: Varimax with Kaiser Normalization.
#> Rotation converged in 4 iterations.
```

### Understanding the Output

**KMO (Kaiser-Meyer-Olkin)** measures sampling adequacy for factor
analysis:

- Above 0.80: Good to excellent
- 0.60 – 0.80: Acceptable
- Below 0.60: Factor analysis may not be appropriate

**Bartlett’s Test** should be significant (p \< .05), confirming that
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
#>   KMO = 0.505 (Miserable), Variance explained: 61.0%, N = 2168 (smallest pairwise)
#> Use summary() for detailed output.
```

**Promax** — allows correlated factors, produces Pattern and Structure
matrices:

``` r

efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science,
    rotation = "promax")
#> Exploratory Factor Analysis: 6 items, 3 components (PCA/Promax)
#>   KMO = 0.505 (Miserable), Variance explained: 61.0%, N = 2168 (smallest pairwise)
#> Use summary() for detailed output.
```

**Oblimin** — another oblique rotation, common in psychology:

``` r

efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science,
    rotation = "oblimin")
#> Exploratory Factor Analysis: 6 items, 3 components (PCA/Oblimin)
#>   KMO = 0.505 (Miserable), Variance explained: 61.0%, N = 2168 (smallest pairwise)
#> Use summary() for detailed output.
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
#> Exploratory Factor Analysis: 6 items, 3 factors (ML/Varimax)
#>   KMO = 0.505 (Miserable), Variance explained: 24.1%, N = 2168 (smallest pairwise)
#> Use summary() for detailed output.
```

ML extraction provides a goodness-of-fit test and uses SMC (squared
multiple correlations) as initial communalities. Combine any extraction
with any rotation:

``` r

efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science,
    extraction = "ml", rotation = "promax")
#> Exploratory Factor Analysis: 6 items, 3 factors (ML/Promax)
#>   KMO = 0.505 (Miserable), Variance explained: 24.1%, N = 2168 (smallest pairwise)
#> Use summary() for detailed output.
```

### Fixing the Number of Factors

``` r

efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science,
    n_factors = 2)
#> Exploratory Factor Analysis: 6 items, 2 components (PCA/Varimax)
#>   KMO = 0.505 (Miserable), Variance explained: 44.0%, N = 2168 (smallest pairwise)
#> Use summary() for detailed output.
```

### With Survey Weights

``` r

efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science,
    weights = sampling_weight)
#> Exploratory Factor Analysis: 6 items, 3 components (PCA/Varimax) [Weighted]
#>   KMO = 0.505 (Miserable), Variance explained: 61.0%, N = 2182 (smallest pairwise)
#> Use summary() for detailed output.
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
#> 
#>   -----------------------------------------------------------------------------
#>   Variable       Mean  Median      SD    Range     IQR  Skewness     N  Missing
#>   -----------------------------------------------------------------------------
#>   m_trust       2.916   3.000   0.691    4.000   1.000     0.020  2484       16
#>   trust_pomps  47.892  50.000  17.283  100.000  25.000     0.020  2484       16
#>   -----------------------------------------------------------------------------
```

### Using the Scale in Analysis

``` r

# Group comparison
survey_data %>%
  t_test(m_trust, group = gender, weights = sampling_weight)
#> t-Test: m_trust by gender [Weighted]
#>   t(2457.6) = -2.362, p = 0.018 *, g = -0.095 (negligible), N = 2499
#> Use summary() for detailed output.
```

``` r

# As a predictor in regression
survey_data %>%
  linear_regression(life_satisfaction ~ m_trust + age + income,
                    weights = sampling_weight)
#> Linear Regression: life_satisfaction ~ m_trust + age + income [Weighted]
#>   R2 = 0.201, adj.R2 = 0.200, F(3, 2109) = 177.02, p < 0.001 ***, N = 2113
#> Use summary() for detailed output.
```

## Complete Example

``` r

# 1. Check reliability
rel <- reliability(survey_data, trust_government, trust_media, trust_science)
rel
#> Reliability Analysis: 3 items
#>   Cronbach's Alpha = 0.047 (Poor), McDonald's Omega = 0.047, N = 2135
#> Use summary() for detailed output.
summary(rel)
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
#>   ---------------------------------------------------------------------------------------
#>   Item                                                         Mean  Std. Deviation     N
#>   ---------------------------------------------------------------------------------------
#>   trust_government  Trust in government (1=none, 5=complete)  2.621           1.162  2135
#>   trust_media       Trust in media (1=none, 5=complete)       2.430           1.156  2135
#>   trust_science     Trust in science (1=none, 5=complete)     3.624           1.034  2135
#>   ---------------------------------------------------------------------------------------
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

# 2. Explore factor structure
efa_result <- efa(survey_data, trust_government, trust_media, trust_science)
efa_result
#> Exploratory Factor Analysis: 3 items, 1 component (PCA/Unrotated)
#>   KMO = 0.506 (Miserable), Variance explained: 34.7%, N = 2227 (smallest pairwise)
#>   Only one component was extracted. The solution cannot be rotated.
#> Use summary() for detailed output.

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
#> 
#>   ----------------------------------------------------------------------------
#>   Variable       Mean  Median      SD    Range     IQR  Skewness    N  Missing
#>   ----------------------------------------------------------------------------
#>   m_trust       2.924   3.000   0.696    4.000   1.000     0.024  845        3
#>   trust_pomps  48.093  50.000  17.390  100.000  25.000     0.024  845        3
#>   ----------------------------------------------------------------------------
#> 
#> Group: education = Intermediate Secondary
#> -----------------------------------------
#> 
#>   ----------------------------------------------------------------------------
#>   Variable       Mean  Median      SD    Range     IQR  Skewness    N  Missing
#>   ----------------------------------------------------------------------------
#>   m_trust       2.920   3.000   0.697    4.000   1.000     0.035  636        5
#>   trust_pomps  48.002  50.000  17.421  100.000  25.000     0.035  636        5
#>   ----------------------------------------------------------------------------
#> 
#> Group: education = Academic Secondary
#> -------------------------------------
#> 
#>   ----------------------------------------------------------------------------
#>   Variable       Mean  Median      SD    Range     IQR  Skewness    N  Missing
#>   ----------------------------------------------------------------------------
#>   m_trust       2.922   3.000   0.691    4.000   0.833    -0.006  634        8
#>   trust_pomps  48.053  50.000  17.267  100.000  20.833    -0.006  634        8
#>   ----------------------------------------------------------------------------
#> 
#> Group: education = University
#> -----------------------------
#> 
#>   ----------------------------------------------------------------------------
#>   Variable       Mean  Median      SD    Range     IQR  Skewness    N  Missing
#>   ----------------------------------------------------------------------------
#>   m_trust       2.885   3.000   0.686    4.000   1.000     0.063  385        0
#>   trust_pomps  47.132  50.000  17.157  100.000  25.000     0.063  385        0
#>   ----------------------------------------------------------------------------
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
