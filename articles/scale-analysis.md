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
```

### Using tidyselect

When your items share a naming pattern, select them efficiently:

``` r
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
```

### Grouped Analysis

Check whether reliability holds across subgroups:

``` r
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

## Exploratory Factor Analysis

### Basic Usage

When you have many items,
[`efa()`](https://YannickDiehl.github.io/mariposa/reference/efa.md)
reveals how they group into underlying dimensions:

``` r
efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science)
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
#> 
#> Exploratory Factor Analysis (PCA, Promax) Results
#> -------------------------------------------------
#> - Variables: political_orientation, environmental_concern, life_satisfaction, trust_government, trust_media, trust_science
#> - Extraction: Principal Component Analysis
#> - Rotation: Promax with Kaiser Normalization
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
#>   PC1  SS Loading: 1.604
#>   PC2  SS Loading: 1.065
#>   PC3  SS Loading: 1.045
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
#> Pattern Matrix
#> ---------------------------------------- 
#>                           PC1     PC2     PC3
#> political_orientation   0.888                
#> environmental_concern  -0.886                
#> trust_science                  -0.781        
#> trust_government               -0.558        
#> life_satisfaction                      -0.817
#> trust_media                            -0.593
#> Extraction Method: Principal Component Analysis.
#> Rotation Method: Promax with Kaiser Normalization.
#> 
#> Structure Matrix
#> ---------------------------------------- 
#>                           PC1     PC2     PC3
#> political_orientation   0.885                
#> environmental_concern  -0.885                
#> trust_science                  -0.751        
#> trust_government               -0.571        
#> life_satisfaction                      -0.777
#> trust_media                            -0.635
#> 
#> Component Correlation Matrix
#> ---------------------------------------- 
#>       PC1   PC2   PC3
#> PC1 1.000 0.055 0.024
#> PC2 0.055 1.000 0.155
#> PC3 0.024 0.155 1.000
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
#> 
#> Exploratory Factor Analysis (Maximum Likelihood, Varimax) Results
#> -----------------------------------------------------------------
#> - Variables: political_orientation, environmental_concern, life_satisfaction, trust_government, trust_media, trust_science
#> - Extraction: Maximum Likelihood
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
#>  political_orientation   0.346      0.616
#>  environmental_concern   0.345      0.565
#>      life_satisfaction   0.001      0.144
#>       trust_government   0.005      0.018
#>            trust_media   0.001      0.009
#>          trust_science   0.003      0.096
#> Extraction Method: Maximum Likelihood.
#> 
#> Total Variance Explained
#> ---------------------------------------- 
#>   Factor1  Eigenvalue: 1.600  Variance: 26.666%  Cumulative: 26.666%
#>   Factor2  Eigenvalue: 1.041  Variance: 17.358%  Cumulative: 44.024%
#>   Factor3  Eigenvalue: 1.017  Variance: 16.955%  Cumulative: 60.979%
#>   Factor4  Eigenvalue: 0.980  Variance: 16.334%  Cumulative: 77.313%
#>   Factor5  Eigenvalue: 0.949  Variance: 15.814%  Cumulative: 93.127%
#>   Factor6  Eigenvalue: 0.412  Variance: 6.873%  Cumulative: 100.000%
#> 
#> Rotation Sums of Squared Loadings
#> ---------------------------------------- 
#>   Factor1  SS Loading: 1.185  Variance: 19.745%  Cumulative: 19.745%
#>   Factor2  SS Loading: 0.149  Variance: 2.488%  Cumulative: 22.233%
#>   Factor3  SS Loading: 0.115  Variance: 1.911%  Cumulative: 24.144%
#> 
#> Factor Matrix (unrotated)
#> ---------------------------------------- 
#>                       Factor1 Factor2 Factor3
#> political_orientation  -0.784                
#> environmental_concern   0.751                
#> life_satisfaction                            
#> trust_science                                
#> trust_government                             
#> trust_media                                  
#> Extraction Method: Maximum Likelihood.
#> 
#> Rotated Factor Matrix
#> ---------------------------------------- 
#>                       Factor1 Factor2 Factor3
#> political_orientation  -0.785                
#> environmental_concern   0.750                
#> life_satisfaction                            
#> trust_science                                
#> trust_government                             
#> trust_media                                  
#> Extraction Method: Maximum Likelihood.
#> Rotation Method: Varimax with Kaiser Normalization.
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
#> 
#> Exploratory Factor Analysis (Maximum Likelihood, Promax) Results
#> ----------------------------------------------------------------
#> - Variables: political_orientation, environmental_concern, life_satisfaction, trust_government, trust_media, trust_science
#> - Extraction: Maximum Likelihood
#> - Rotation: Promax with Kaiser Normalization
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
#>  political_orientation   0.346      0.616
#>  environmental_concern   0.345      0.565
#>      life_satisfaction   0.001      0.144
#>       trust_government   0.005      0.018
#>            trust_media   0.001      0.009
#>          trust_science   0.003      0.096
#> Extraction Method: Maximum Likelihood.
#> 
#> Total Variance Explained
#> ---------------------------------------- 
#>   Factor1  Eigenvalue: 1.600  Variance: 26.666%  Cumulative: 26.666%
#>   Factor2  Eigenvalue: 1.041  Variance: 17.358%  Cumulative: 44.024%
#>   Factor3  Eigenvalue: 1.017  Variance: 16.955%  Cumulative: 60.979%
#>   Factor4  Eigenvalue: 0.980  Variance: 16.334%  Cumulative: 77.313%
#>   Factor5  Eigenvalue: 0.949  Variance: 15.814%  Cumulative: 93.127%
#>   Factor6  Eigenvalue: 0.412  Variance: 6.873%  Cumulative: 100.000%
#> 
#> Rotation Sums of Squared Loadings
#> ---------------------------------------- 
#>   Factor1  SS Loading: 1.186
#>   Factor2  SS Loading: 0.148
#>   Factor3  SS Loading: 0.115
#> 
#> Factor Matrix (unrotated)
#> ---------------------------------------- 
#>                       Factor1 Factor2 Factor3
#> political_orientation  -0.784                
#> environmental_concern   0.751                
#> life_satisfaction                            
#> trust_science                                
#> trust_government                             
#> trust_media                                  
#> Extraction Method: Maximum Likelihood.
#> 
#> Pattern Matrix
#> ---------------------------------------- 
#>                       Factor1 Factor2 Factor3
#> political_orientation  -0.785                
#> environmental_concern   0.750                
#> life_satisfaction                            
#> trust_science                                
#> trust_government                             
#> trust_media                                  
#> Extraction Method: Maximum Likelihood.
#> Rotation Method: Promax with Kaiser Normalization.
#> 
#> Structure Matrix
#> ---------------------------------------- 
#>                       Factor1 Factor2 Factor3
#> political_orientation  -0.784                
#> environmental_concern   0.751                
#> life_satisfaction                            
#> trust_science                                
#> trust_government                             
#> trust_media                                  
#> 
#> Factor Correlation Matrix
#> ---------------------------------------- 
#>         Factor1 Factor2 Factor3
#> Factor1   1.000   0.013   0.030
#> Factor2   0.013   1.000   0.002
#> Factor3   0.030   0.002   1.000
```

### Fixing the Number of Factors

If you have a theoretical reason for a specific number of components:

``` r
efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science,
    n_factors = 2)
#> 
#> Exploratory Factor Analysis (PCA, Varimax) Results
#> --------------------------------------------------
#> - Variables: political_orientation, environmental_concern, life_satisfaction, trust_government, trust_media, trust_science
#> - Extraction: Principal Component Analysis
#> - Rotation: Varimax with Kaiser Normalization
#> - N of Factors: 2
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
#>      life_satisfaction       1      0.014
#>       trust_government       1      0.327
#>            trust_media       1      0.275
#>          trust_science       1      0.456
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
#>   PC1  SS Loading: 1.599  Variance: 26.653%  Cumulative: 26.653%
#>   PC2  SS Loading: 1.042  Variance: 17.371%  Cumulative: 44.024%
#> 
#> Component Matrix (unrotated)
#> ---------------------------------------- 
#>                           PC1     PC2
#> political_orientation   0.885        
#> environmental_concern  -0.885        
#> trust_science                  -0.672
#> trust_government               -0.547
#> trust_media                    -0.524
#> life_satisfaction                    
#> Extraction Method: Principal Component Analysis.
#> 
#> Rotated Component Matrix
#> ---------------------------------------- 
#>                           PC1     PC2
#> political_orientation   0.887        
#> environmental_concern  -0.885        
#> trust_science                  -0.669
#> trust_government               -0.552
#> trust_media                    -0.524
#> life_satisfaction                    
#> Extraction Method: Principal Component Analysis.
#> Rotation Method: Varimax with Kaiser Normalization.
```

### With Survey Weights

``` r
efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science,
    weights = sampling_weight)
#> 
#> Weighted Exploratory Factor Analysis (PCA, Varimax) Results
#> -----------------------------------------------------------
#> - Variables: political_orientation, environmental_concern, life_satisfaction, trust_government, trust_media, trust_science
#> - Extraction: Principal Component Analysis
#> - Rotation: Varimax with Kaiser Normalization
#> - N of Factors: 3
#> - Weights: sampling_weight
#> 
#> KMO and Bartlett's Test
#> ---------------------------------------- 
#>   Kaiser-Meyer-Olkin Measure:     0.505
#>   Bartlett's Chi-Square:          930.012
#>   df:                             15
#>   Sig.:                           0.000
#> 
#> Communalities
#> ---------------------------------------- 
#>               variable initial extraction
#>  political_orientation       1      0.784
#>  environmental_concern       1      0.782
#>      life_satisfaction       1      0.712
#>       trust_government       1      0.332
#>            trust_media       1      0.442
#>          trust_science       1      0.610
#> Extraction Method: Principal Component Analysis.
#> 
#> Total Variance Explained
#> ---------------------------------------- 
#>   PC1  Eigenvalue: 1.599  Variance: 26.644%  Cumulative: 26.644%
#>   PC2  Eigenvalue: 1.045  Variance: 17.412%  Cumulative: 44.056%
#>   PC3  Eigenvalue: 1.018  Variance: 16.964%  Cumulative: 61.021%
#>   PC4  Eigenvalue: 0.980  Variance: 16.329%  Cumulative: 77.349%
#>   PC5  Eigenvalue: 0.944  Variance: 15.735%  Cumulative: 93.085%
#>   PC6  Eigenvalue: 0.415  Variance: 6.915%  Cumulative: 100.000%
#> 
#> Rotation Sums of Squared Loadings
#> ---------------------------------------- 
#>   PC1  SS Loading: 1.597  Variance: 26.614%  Cumulative: 26.614%
#>   PC2  SS Loading: 1.044  Variance: 17.396%  Cumulative: 44.011%
#>   PC3  SS Loading: 1.021  Variance: 17.010%  Cumulative: 61.021%
#> 
#> Component Matrix (unrotated)
#> ---------------------------------------- 
#>                           PC1     PC2     PC3
#> political_orientation   0.884                
#> environmental_concern  -0.884                
#> trust_science                  -0.663   0.406
#> trust_government               -0.547        
#> trust_media                    -0.542        
#> life_satisfaction                      -0.838
#> Extraction Method: Principal Component Analysis.
#> 
#> Rotated Component Matrix
#> ---------------------------------------- 
#>                           PC1     PC2     PC3
#> political_orientation   0.885                
#> environmental_concern  -0.883                
#> trust_science                  -0.752        
#> trust_government               -0.542        
#> life_satisfaction                      -0.828
#> trust_media                            -0.534
#> Extraction Method: Principal Component Analysis.
#> Rotation Method: Varimax with Kaiser Normalization.
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

# 2. Explore factor structure
efa(survey_data, trust_government, trust_media, trust_science)
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
#> Communalities
#> ---------------------------------------- 
#>          variable initial extraction
#>  trust_government       1      0.356
#>       trust_media       1      0.225
#>     trust_science       1      0.460
#> Extraction Method: Principal Component Analysis.
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
#> Weighted t-Test Results
#> -----------------------
#> 
#> - Grouping variable: region
#> - Groups compared: East vs. West
#> - Weights variable: sampling_weight
#> - Confidence level: 95.0%
#> - Alternative hypothesis: two.sided
#> - Null hypothesis (mu): 0.000
#> 
#> 
#> --- m_trust ---
#> 
#>   East: mean = 2.905, n = 509.0
#>   West: mean = 2.919, n = 2007.0
#> 
#> Weighted t-test Results:
#> -------------------------------------------------------------------------------- 
#>         Assumption t_stat       df p_value mean_diff        conf_int sig
#>    Equal variances -0.400 2514.000   0.689    -0.014 [-0.082, 0.054]    
#>  Unequal variances -0.406  799.225   0.685    -0.014 [-0.081, 0.053]    
#> -------------------------------------------------------------------------------- 
#> 
#> Effect Sizes:
#> ------------ 
#>  Variable Cohens_d Hedges_g Glass_Delta Effect_Size
#>   m_trust    -0.02    -0.02       -0.02  negligible
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation:
#> - Cohen's d: pooled standard deviation (classic)
#> - Hedges' g: bias-corrected Cohen's d (preferred)
#> - Glass' Delta: control group standard deviation only
#> - Small effect: |effect| ~ 0.2
#> - Medium effect: |effect| ~ 0.5
#> - Large effect: |effect| ~ 0.8
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
