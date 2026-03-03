# Summarize an exploratory factor analysis

Creates a detailed summary of an EFA result. All sections are shown by
default; set individual toggles to `FALSE` to suppress specific
sections.

## Usage

``` r
# S3 method for class 'efa'
summary(
  object,
  kmo_bartlett = TRUE,
  communalities = TRUE,
  variance_explained = TRUE,
  unrotated_matrix = TRUE,
  rotated_matrix = TRUE,
  pattern_matrix = TRUE,
  structure_matrix = TRUE,
  factor_correlations = TRUE,
  digits = 3,
  ...
)
```

## Arguments

- object:

  An `efa` result object

- kmo_bartlett:

  Show KMO and Bartlett's test? (Default: TRUE)

- communalities:

  Show communalities table? (Default: TRUE)

- variance_explained:

  Show total variance explained? (Default: TRUE)

- unrotated_matrix:

  Show unrotated component/factor matrix? (Default: TRUE)

- rotated_matrix:

  Show rotated matrix (varimax)? (Default: TRUE)

- pattern_matrix:

  Show pattern matrix (oblimin/promax)? (Default: TRUE)

- structure_matrix:

  Show structure matrix (oblimin/promax)? (Default: TRUE)

- factor_correlations:

  Show factor correlation matrix (oblimin/promax)? (Default: TRUE)

- digits:

  Number of decimal places (Default: 3)

- ...:

  Additional arguments (ignored)

## Value

A `summary.efa` object (list with `$show` toggles)

## See also

[`efa`](https://YannickDiehl.github.io/mariposa/dev/reference/efa.md)
for the main analysis function.

## Examples

``` r
result <- efa(survey_data, political_orientation, environmental_concern,
             life_satisfaction, trust_government, trust_media, trust_science)
summary(result)
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
summary(result, communalities = FALSE)
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
