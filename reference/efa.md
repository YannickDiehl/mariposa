# Explore the Structure Behind Your Survey Items

`efa()` performs Exploratory Factor Analysis (EFA) to discover
underlying patterns in your survey items. Supports both Principal
Component Analysis (PCA) and Maximum Likelihood (ML) extraction. This is
the R equivalent of SPSS's `FACTOR` procedure.

For example, if you have 6 items measuring different attitudes, EFA can
reveal whether they group into 2-3 underlying dimensions (factors or
components).

## Usage

``` r
efa(
  data,
  ...,
  n_factors = NULL,
  rotation = "varimax",
  extraction = "pca",
  weights = NULL,
  use = "pairwise",
  sort = TRUE,
  blank = 0.4,
  na.rm = TRUE
)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- ...:

  The items to analyze. Use bare column names separated by commas, or
  tidyselect helpers like `starts_with("trust")`.

- n_factors:

  Number of components to extract. Default `NULL` uses the Kaiser
  criterion (eigenvalue \> 1).

- rotation:

  Rotation method: `"varimax"` (default, orthogonal), `"oblimin"`
  (oblique, allows correlated factors), `"promax"` (oblique,
  power-based), or `"none"`.

- extraction:

  Extraction method: `"pca"` (default, Principal Component Analysis) or
  `"ml"` (Maximum Likelihood, enables goodness-of-fit testing, assumes
  multivariate normality).

- weights:

  Optional survey weights for population-representative results.

- use:

  How to handle missing data for correlation computation: `"pairwise"`
  (default, matches SPSS) or `"complete"` (listwise).

- sort:

  Logical. Sort loadings by size within each component? Default `TRUE`.

- blank:

  Numeric. Suppress (hide) loadings with absolute value below this
  threshold in the print output. Default `0.40` (matches SPSS
  BLANK(.40)). Set to `0` to show all loadings.

- na.rm:

  Logical. Remove missing values? Default `TRUE`.

## Value

An `efa` result object containing:

- loadings:

  Component loading matrix (rotated if rotation applied)

- unrotated_loadings:

  Unrotated component matrix

- eigenvalues:

  All eigenvalues from the correlation matrix

- variance_explained:

  Tibble with Total, % of Variance, Cumulative %

- rotation_variance:

  Tibble with rotation sums of squared loadings

- communalities:

  Extraction communalities for each variable

- kmo:

  List with overall KMO and per-item MSA values

- bartlett:

  List with chi_sq, df, and p_value

- rotation:

  Rotation method used

- extraction:

  Extraction method used

- n_factors:

  Number of components extracted

- correlation_matrix:

  Correlation matrix used for analysis

- initial_communalities:

  Initial communalities (1.0 for PCA, SMC for ML)

- goodness_of_fit:

  Goodness-of-fit test (ML only): chi_sq, df, p_value. NULL for PCA.

- uniquenesses:

  Unique variances per variable (ML only, NULL for PCA)

- pattern_matrix:

  Pattern matrix (oblimin/promax only, NULL otherwise)

- structure_matrix:

  Structure matrix (oblimin/promax only, NULL otherwise)

- factor_correlations:

  Factor correlation matrix (oblimin/promax only, NULL otherwise)

- variables:

  Character vector of variable names

- weights:

  Weights variable name or NULL

- n:

  Sample size

- col_prefix:

  Column name prefix: `"PC"` for PCA, `"Factor"` for ML

- sort:

  Whether loadings are sorted

- blank:

  Suppression threshold

## Details

### Understanding the Results

**KMO (Kaiser-Meyer-Olkin)** measures sampling adequacy:

- KMO \> 0.90: Marvelous

- KMO 0.80 - 0.90: Meritorious

- KMO 0.70 - 0.80: Middling

- KMO 0.60 - 0.70: Mediocre

- KMO 0.50 - 0.60: Miserable

- KMO \< 0.50: Unacceptable - don't use factor analysis

**Bartlett's Test of Sphericity** tests whether correlations are
significantly different from zero. A significant result (p \< .05) means
the correlation matrix is suitable for factor analysis.

**Eigenvalues** indicate how much variance each component explains. The
Kaiser criterion retains components with eigenvalue \> 1.

**Factor Loadings** show how strongly each item relates to each
component:

- \|loading\| \> 0.70: Strong association

- \|loading\| 0.40 - 0.70: Moderate association

- \|loading\| \< 0.40: Weak (suppressed by default)

**Communalities** show how much of each item's variance is explained by
the extracted components. Low communalities (\< 0.40) suggest the item
doesn't fit well with the others.

### Choosing an Extraction Method

- **PCA** (default): Extracts components explaining maximum total
  variance. Simple and robust. Does not assume normality.

- **ML**: Extracts factors explaining shared variance only. Assumes
  multivariate normality. Provides a goodness-of-fit test to evaluate
  model fit. A non-significant chi-square (p \> .05) suggests adequate
  fit.

### Choosing a Rotation

- **Varimax** (default): Assumes factors are uncorrelated. Produces
  simpler, easier-to-interpret results.

- **Oblimin**: Allows factors to be correlated. More realistic for
  social science data. Produces both a Pattern Matrix (unique
  contributions) and Structure Matrix (total correlations).

- **Promax**: Oblique rotation based on a power transformation of
  Varimax results. Like Oblimin, produces Pattern and Structure
  matrices. Common alternative to Oblimin in SPSS.

- **None**: No rotation. Rarely useful for interpretation.

## See also

[`reliability`](https://YannickDiehl.github.io/mariposa/reference/reliability.md)
for checking scale reliability before creating indices.

[`scale_index`](https://YannickDiehl.github.io/mariposa/reference/scale_index.md)
for creating mean indices after identifying factors.

Other scale:
[`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md),
[`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md),
[`scale_index()`](https://YannickDiehl.github.io/mariposa/reference/scale_index.md)

## Examples

``` r
library(dplyr)
data(survey_data)

# Basic EFA with Varimax rotation
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

# With Oblimin rotation (allows correlated factors)
efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science,
    rotation = "oblimin")
#> 
#> Exploratory Factor Analysis (PCA, Oblimin) Results
#> --------------------------------------------------
#> - Variables: political_orientation, environmental_concern, life_satisfaction, trust_government, trust_media, trust_science
#> - Extraction: Principal Component Analysis
#> - Rotation: Oblimin with Kaiser Normalization
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
#>   PC1  SS Loading: 1.600
#>   PC2  SS Loading: 1.043
#>   PC3  SS Loading: 1.025
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
#> political_orientation   0.887                
#> environmental_concern  -0.884                
#> trust_science                  -0.769        
#> trust_government               -0.561        
#> life_satisfaction                      -0.796
#> trust_media                            -0.613
#> Extraction Method: Principal Component Analysis.
#> Rotation Method: Oblimin with Kaiser Normalization.
#> 
#> Structure Matrix
#> ---------------------------------------- 
#>                           PC1     PC2     PC3
#> political_orientation   0.886                
#> environmental_concern  -0.884                
#> trust_science                  -0.757        
#> trust_government               -0.571        
#> life_satisfaction                      -0.781
#> trust_media                            -0.630
#> 
#> Component Correlation Matrix
#> ---------------------------------------- 
#>       PC1   PC2   PC3
#> PC1 1.000 0.041 0.025
#> PC2 0.041 1.000 0.063
#> PC3 0.025 0.063 1.000

# Maximum Likelihood extraction
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

# Promax rotation (oblique)
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

# Fix number of factors
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

# With survey weights
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
#> trust_science                   0.663   0.406
#> trust_government                0.547        
#> trust_media                     0.542        
#> life_satisfaction                      -0.838
#> Extraction Method: Principal Component Analysis.
#> 
#> Rotated Component Matrix
#> ---------------------------------------- 
#>                           PC1     PC2     PC3
#> political_orientation   0.885                
#> environmental_concern  -0.883                
#> trust_science                   0.752        
#> trust_government                0.542        
#> life_satisfaction                      -0.828
#> trust_media                            -0.534
#> Extraction Method: Principal Component Analysis.
#> Rotation Method: Varimax with Kaiser Normalization.

# Grouped by region
survey_data %>%
  group_by(region) %>%
  efa(political_orientation, environmental_concern, life_satisfaction,
      trust_government, trust_media, trust_science)
#> 
#> Exploratory Factor Analysis (PCA, Varimax) Results
#> --------------------------------------------------
#> 
#> Group: region = 1
#> -----------------
#> - Variables: political_orientation, environmental_concern, life_satisfaction, trust_government, trust_media, trust_science
#> - Extraction: Principal Component Analysis
#> - Rotation: Varimax with Kaiser Normalization
#> - N of Factors: 3
#> 
#> KMO and Bartlett's Test
#> ---------------------------------------- 
#>   Kaiser-Meyer-Olkin Measure:     0.475
#>   Bartlett's Chi-Square:          202.026
#>   df:                             15
#>   Sig.:                           0.000
#> 
#> Communalities
#> ---------------------------------------- 
#>               variable initial extraction
#>  political_orientation       1      0.803
#>  environmental_concern       1      0.801
#>      life_satisfaction       1      0.324
#>       trust_government       1      0.939
#>            trust_media       1      0.473
#>          trust_science       1      0.388
#> Extraction Method: Principal Component Analysis.
#> 
#> Total Variance Explained
#> ---------------------------------------- 
#>   PC1  Eigenvalue: 1.607  Variance: 26.786%  Cumulative: 26.786%
#>   PC2  Eigenvalue: 1.115  Variance: 18.582%  Cumulative: 45.368%
#>   PC3  Eigenvalue: 1.007  Variance: 16.779%  Cumulative: 62.147%
#>   PC4  Eigenvalue: 0.956  Variance: 15.938%  Cumulative: 78.085%
#>   PC5  Eigenvalue: 0.933  Variance: 15.549%  Cumulative: 93.634%
#>   PC6  Eigenvalue: 0.382  Variance: 6.366%  Cumulative: 100.000%
#> 
#> Rotation Sums of Squared Loadings
#> ---------------------------------------- 
#>   PC1  SS Loading: 1.603  Variance: 26.710%  Cumulative: 26.710%
#>   PC2  SS Loading: 1.119  Variance: 18.643%  Cumulative: 45.353%
#>   PC3  SS Loading: 1.008  Variance: 16.794%  Cumulative: 62.147%
#> 
#> Component Matrix (unrotated)
#> ---------------------------------------- 
#>                           PC1     PC2     PC3
#> political_orientation   0.894                
#> environmental_concern  -0.889                
#> trust_media                     0.664        
#> trust_science                   0.585        
#> life_satisfaction              -0.566        
#> trust_government                       -0.968
#> Extraction Method: Principal Component Analysis.
#> 
#> Rotated Component Matrix
#> ---------------------------------------- 
#>                           PC1     PC2     PC3
#> political_orientation   0.895                
#> environmental_concern  -0.894                
#> trust_media                     0.673        
#> trust_science                   0.584        
#> life_satisfaction              -0.567        
#> trust_government                       -0.969
#> Extraction Method: Principal Component Analysis.
#> Rotation Method: Varimax with Kaiser Normalization.
#> 
#> Group: region = 2
#> -----------------
#> - Variables: political_orientation, environmental_concern, life_satisfaction, trust_government, trust_media, trust_science
#> - Extraction: Principal Component Analysis
#> - Rotation: Varimax with Kaiser Normalization
#> - N of Factors: 3
#> 
#> KMO and Bartlett's Test
#> ---------------------------------------- 
#>   Kaiser-Meyer-Olkin Measure:     0.505
#>   Bartlett's Chi-Square:          748.803
#>   df:                             15
#>   Sig.:                           0.000
#> 
#> Communalities
#> ---------------------------------------- 
#>               variable initial extraction
#>  political_orientation       1      0.786
#>  environmental_concern       1      0.777
#>      life_satisfaction       1      0.554
#>       trust_government       1      0.428
#>            trust_media       1      0.499
#>          trust_science       1      0.633
#> Extraction Method: Principal Component Analysis.
#> 
#> Total Variance Explained
#> ---------------------------------------- 
#>   PC1  Eigenvalue: 1.601  Variance: 26.684%  Cumulative: 26.684%
#>   PC2  Eigenvalue: 1.050  Variance: 17.498%  Cumulative: 44.182%
#>   PC3  Eigenvalue: 1.026  Variance: 17.105%  Cumulative: 61.288%
#>   PC4  Eigenvalue: 0.960  Variance: 16.003%  Cumulative: 77.291%
#>   PC5  Eigenvalue: 0.948  Variance: 15.797%  Cumulative: 93.088%
#>   PC6  Eigenvalue: 0.415  Variance: 6.912%  Cumulative: 100.000%
#> 
#> Rotation Sums of Squared Loadings
#> ---------------------------------------- 
#>   PC1  SS Loading: 1.598  Variance: 26.629%  Cumulative: 26.629%
#>   PC2  SS Loading: 1.042  Variance: 17.371%  Cumulative: 44.000%
#>   PC3  SS Loading: 1.037  Variance: 17.288%  Cumulative: 61.288%
#> 
#> Component Matrix (unrotated)
#> ---------------------------------------- 
#>                           PC1     PC2     PC3
#> political_orientation   0.884                
#> environmental_concern  -0.880                
#> trust_media                    -0.641        
#> life_satisfaction              -0.542  -0.510
#> trust_science                           0.689
#> trust_government               -0.432   0.449
#> Extraction Method: Principal Component Analysis.
#> 
#> Rotated Component Matrix
#> ---------------------------------------- 
#>                           PC1     PC2     PC3
#> political_orientation   0.886                
#> environmental_concern  -0.880                
#> life_satisfaction              -0.737        
#> trust_media                    -0.694        
#> trust_science                           0.782
#> trust_government                        0.629
#> Extraction Method: Principal Component Analysis.
#> Rotation Method: Varimax with Kaiser Normalization.
```
