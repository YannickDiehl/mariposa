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

Use [`summary()`](https://rdrr.io/r/base/summary.html) for the full
SPSS-style output with toggleable sections.

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

[`row_means`](https://YannickDiehl.github.io/mariposa/reference/row_means.md)
for creating mean indices after identifying factors.

[`summary.efa`](https://YannickDiehl.github.io/mariposa/reference/summary.efa.md)
for detailed output with toggleable sections.

Other scale:
[`pomps()`](https://YannickDiehl.github.io/mariposa/reference/pomps.md),
[`reliability()`](https://YannickDiehl.github.io/mariposa/reference/reliability.md),
[`row_count()`](https://YannickDiehl.github.io/mariposa/reference/row_count.md),
[`row_means()`](https://YannickDiehl.github.io/mariposa/reference/row_means.md),
[`row_sums()`](https://YannickDiehl.github.io/mariposa/reference/row_sums.md)

## Examples

``` r
library(dplyr)
data(survey_data)

# Basic EFA with Varimax rotation
efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science)
#> Exploratory Factor Analysis: 6 items, 3 components (PCA/Varimax)
#>   KMO = 0.505 (Miserable), Variance explained: 61.0%

# With Oblimin rotation (requires GPArotation package)
# \donttest{
efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science,
    rotation = "oblimin")
#> Exploratory Factor Analysis: 6 items, 3 components (PCA/Oblimin)
#>   KMO = 0.505 (Miserable), Variance explained: 61.0%
# }

# Maximum Likelihood extraction
efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science,
    extraction = "ml")
#> Exploratory Factor Analysis: 6 items, 3 components (ML/Varimax)
#>   KMO = 0.505 (Miserable), Variance explained: 61.0%

# Promax rotation (oblique)
efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science,
    rotation = "promax")
#> Exploratory Factor Analysis: 6 items, 3 components (PCA/Promax)
#>   KMO = 0.505 (Miserable), Variance explained: 61.0%

# Fix number of factors
efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science,
    n_factors = 2)
#> Exploratory Factor Analysis: 6 items, 2 components (PCA/Varimax)
#>   KMO = 0.505 (Miserable), Variance explained: 44.0%

# With survey weights
efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science,
    weights = sampling_weight)
#> Exploratory Factor Analysis: 6 items, 3 components (PCA/Varimax) [Weighted]
#>   KMO = 0.505 (Miserable), Variance explained: 61.0%

# Grouped by region
survey_data %>%
  group_by(region) %>%
  efa(political_orientation, environmental_concern, life_satisfaction,
      trust_government, trust_media, trust_science)
#> [region = 1]
#> Exploratory Factor Analysis: 6 items, 3 components (PCA/Varimax)
#>   KMO = 0.475 (Unacceptable), Variance explained: 62.1%
#> [region = 2]
#> Exploratory Factor Analysis: 6 items, 3 components (PCA/Varimax)
#>   KMO = 0.505 (Miserable), Variance explained: 61.3%

# --- Three-layer output ---
result <- efa(survey_data, political_orientation, environmental_concern,
              life_satisfaction, trust_government, trust_media, trust_science)
result              # compact overview
#> Exploratory Factor Analysis: 6 items, 3 components (PCA/Varimax)
#>   KMO = 0.505 (Miserable), Variance explained: 61.0%
summary(result)     # full detailed output with all sections
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
summary(result, communalities = FALSE)  # hide communalities table
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
