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
  descriptives = TRUE,
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

- descriptives:

  Show the per-item descriptive statistics (mean, SD, analysis N,
  missing N; SPSS `/PRINT UNIVARIATE`)? (Default: TRUE)

- digits:

  Number of decimal places (Default: 3)

- ...:

  Additional arguments (ignored)

## Value

A `summary.efa` object (list with `$show` toggles)

## See also

[`efa`](https://YannickDiehl.github.io/mariposa/reference/efa.md) for
the main analysis function.

## Examples

``` r
result <- efa(survey_data, political_orientation, environmental_concern,
             life_satisfaction, trust_government, trust_media, trust_science)
summary(result)
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
#>   -------------------------------------------------------------------
#>   Variable                Mean  Std. Deviation  Analysis N  Missing N
#>   -------------------------------------------------------------------
#>   political_orientation  2.722           1.086        2299        201
#>   environmental_concern  3.573           1.194        2400        100
#>   life_satisfaction      3.628           1.153        2421         79
#>   trust_government       2.621           1.163        2354        146
#>   trust_media            2.452           1.163        2367        133
#>   trust_science          3.641           1.028        2398        102
#>   -------------------------------------------------------------------
#> 
#> KMO and Bartlett's Test
#> ---------------------------------------- 
#>   Kaiser-Meyer-Olkin Measure:     0.505
#>   Bartlett's Chi-Square:          932.068
#>   df:                             15
#>   Sig.:                           <.001
#> 
#> Communalities
#>   ---------------------------------------------------------------------
#>   Variable                                          Initial  Extraction
#>   ---------------------------------------------------------------------
#>   political_orientation  Political orientation ...    1.000       0.786
#>   environmental_concern  Environmental concern ...    1.000       0.783
#>   life_satisfaction      Life satisfaction (1=d...    1.000       0.668
#>   trust_government       Trust in government (1...    1.000       0.347
#>   trust_media            Trust in media (1=none...    1.000       0.475
#>   trust_science          Trust in science (1=no...    1.000       0.598
#>   ---------------------------------------------------------------------
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
#>                                                            PC1     PC2     PC3
#> political_orientation  Political orientation (1=lef...  -0.885                
#> environmental_concern  Environmental concern (1=low...   0.885                
#> trust_science          Trust in science (1=none, 5=...           0.672        
#> trust_government       Trust in government (1=none,...           0.547        
#> trust_media            Trust in media (1=none, 5=co...           0.524   0.448
#> life_satisfaction      Life satisfaction (1=dissati...                   0.809
#> Extraction Method: Principal Component Analysis.
#> 
#> Rotated Component Matrix
#> ---------------------------------------- 
#>                                                            PC1     PC2     PC3
#> political_orientation  Political orientation (1=lef...  -0.887                
#> environmental_concern  Environmental concern (1=low...   0.884                
#> trust_science          Trust in science (1=none, 5=...           0.762        
#> trust_government       Trust in government (1=none,...           0.566        
#> life_satisfaction      Life satisfaction (1=dissati...                   0.789
#> trust_media            Trust in media (1=none, 5=co...                   0.620
#> Extraction Method: Principal Component Analysis.
#> Rotation Method: Varimax with Kaiser Normalization.
#> Rotation converged in 4 iterations.
summary(result, communalities = FALSE)
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
#>   -------------------------------------------------------------------
#>   Variable                Mean  Std. Deviation  Analysis N  Missing N
#>   -------------------------------------------------------------------
#>   political_orientation  2.722           1.086        2299        201
#>   environmental_concern  3.573           1.194        2400        100
#>   life_satisfaction      3.628           1.153        2421         79
#>   trust_government       2.621           1.163        2354        146
#>   trust_media            2.452           1.163        2367        133
#>   trust_science          3.641           1.028        2398        102
#>   -------------------------------------------------------------------
#> 
#> KMO and Bartlett's Test
#> ---------------------------------------- 
#>   Kaiser-Meyer-Olkin Measure:     0.505
#>   Bartlett's Chi-Square:          932.068
#>   df:                             15
#>   Sig.:                           <.001
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
#>                                                            PC1     PC2     PC3
#> political_orientation  Political orientation (1=lef...  -0.885                
#> environmental_concern  Environmental concern (1=low...   0.885                
#> trust_science          Trust in science (1=none, 5=...           0.672        
#> trust_government       Trust in government (1=none,...           0.547        
#> trust_media            Trust in media (1=none, 5=co...           0.524   0.448
#> life_satisfaction      Life satisfaction (1=dissati...                   0.809
#> Extraction Method: Principal Component Analysis.
#> 
#> Rotated Component Matrix
#> ---------------------------------------- 
#>                                                            PC1     PC2     PC3
#> political_orientation  Political orientation (1=lef...  -0.887                
#> environmental_concern  Environmental concern (1=low...   0.884                
#> trust_science          Trust in science (1=none, 5=...           0.762        
#> trust_government       Trust in government (1=none,...           0.566        
#> life_satisfaction      Life satisfaction (1=dissati...                   0.789
#> trust_media            Trust in media (1=none, 5=co...                   0.620
#> Extraction Method: Principal Component Analysis.
#> Rotation Method: Varimax with Kaiser Normalization.
#> Rotation converged in 4 iterations.
```
