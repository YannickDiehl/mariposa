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

  Number of components to extract (a whole number). Default `NULL` uses
  the Kaiser criterion (eigenvalue \> 1). A single component cannot be
  rotated; it is shown unrotated with SPSS's note.

- rotation:

  Rotation method: `"varimax"` (default, orthogonal), `"oblimin"`
  (oblique direct oblimin, delta = 0, allows correlated factors),
  `"promax"` (oblique, power 4), or `"none"`. All rotations use Kaiser
  normalization and SPSS FACTOR's own algorithms and stopping rules (at
  most 25 iterations, as SPSS `/CRITERIA ITERATE(25)`), so they
  reproduce SPSS's rotated matrices; they differ slightly from
  [`stats::varimax()`](https://rdrr.io/r/stats/varimax.html),
  [`stats::promax()`](https://rdrr.io/r/stats/varimax.html) and
  [`GPArotation::oblimin()`](https://rdrr.io/pkg/GPArotation/man/rotations.html).

- extraction:

  Extraction method: `"pca"` (default, Principal Component Analysis) or
  `"ml"` (Maximum Likelihood, enables goodness-of-fit testing, assumes
  multivariate normality). ML uses SPSS's starting values, bounds a
  Heywood variable's communality at .999 as SPSS does and keeps SPSS's
  factor order.

- weights:

  Optional survey weights for population-representative results. Give a
  column name (unquoted or as a string), an expression such as
  `sampling_weight * 2`, or a numeric vector with one weight per row.

- use:

  How to handle missing data for correlation computation: `"pairwise"`
  (default, matches SPSS) or `"complete"` (listwise; `"listwise"` is
  accepted as an alias).

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

  Tibble with the initial eigenvalues: Total, % of Variance, Cumulative
  % (one row per variable)

- extraction_variance:

  Tibble with the extraction sums of squared loadings of the extracted
  components/factors (SPSS "Extraction Sums of Squared Loadings"). For
  ML this is the variance the common factors explain - the figure the
  compact print reports.

- rotation_variance:

  Tibble with rotation sums of squared loadings (for the oblique
  rotations the column sums of squares of the structure matrix, as SPSS
  reports them)

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

- rotation_iterations, rotation_converged:

  Iterations of the rotation as SPSS counts them ("Rotation converged in
  4 iterations"; for promax those of its varimax step) and whether it
  converged; NULL without rotation

- variables:

  Character vector of variable names

- variable_labels:

  Named character vector with the variable labels (`NA` where a variable
  has none); [`summary()`](https://rdrr.io/r/base/summary.html) shows
  them next to the names, shortened to the console width in the tables

- weights:

  Weights variable name or NULL

- item_statistics:

  Tibble with mean, SD, analysis N and missing N per item. With
  `use = "pairwise"` each item uses its own valid cases; with
  `use = "complete"` all items use the complete cases.

- n:

  Sample size: the smallest pairwise N (`use = "pairwise"`, the N of
  Bartlett's test as in SPSS) or the number of complete cases
  (`use = "complete"`); the sum of weights when weighted

- use:

  The missing-data handling used (`"pairwise"` or `"complete"`)

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
#>   KMO = 0.505 (Miserable), Variance explained: 61.0%, N = 2168 (smallest pairwise)
#> Use summary() for detailed output.

# With Oblimin rotation
efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science,
    rotation = "oblimin")
#> Exploratory Factor Analysis: 6 items, 3 components (PCA/Oblimin)
#>   KMO = 0.505 (Miserable), Variance explained: 61.0%, N = 2168 (smallest pairwise)
#> Use summary() for detailed output.

# Maximum Likelihood extraction
efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science,
    extraction = "ml")
#> Exploratory Factor Analysis: 6 items, 3 factors (ML/Varimax)
#>   KMO = 0.505 (Miserable), Variance explained: 24.1%, N = 2168 (smallest pairwise)
#> Use summary() for detailed output.

# Promax rotation (oblique)
efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science,
    rotation = "promax")
#> Exploratory Factor Analysis: 6 items, 3 components (PCA/Promax)
#>   KMO = 0.505 (Miserable), Variance explained: 61.0%, N = 2168 (smallest pairwise)
#> Use summary() for detailed output.

# Fix number of factors
efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science,
    n_factors = 2)
#> Exploratory Factor Analysis: 6 items, 2 components (PCA/Varimax)
#>   KMO = 0.505 (Miserable), Variance explained: 44.0%, N = 2168 (smallest pairwise)
#> Use summary() for detailed output.

# With survey weights
efa(survey_data,
    political_orientation, environmental_concern, life_satisfaction,
    trust_government, trust_media, trust_science,
    weights = sampling_weight)
#> Exploratory Factor Analysis: 6 items, 3 components (PCA/Varimax) [Weighted]
#>   KMO = 0.505 (Miserable), Variance explained: 61.0%, N = 2182 (smallest pairwise)
#> Use summary() for detailed output.

# Grouped by region
survey_data %>%
  group_by(region) %>%
  efa(political_orientation, environmental_concern, life_satisfaction,
      trust_government, trust_media, trust_science)
#> [region = East]
#> Exploratory Factor Analysis: 6 items, 3 components (PCA/Varimax)
#>   KMO = 0.475 (Unacceptable), Variance explained: 62.1%, N = 419 (smallest pairwise)
#> [region = West]
#> Exploratory Factor Analysis: 6 items, 3 components (PCA/Varimax)
#>   KMO = 0.505 (Miserable), Variance explained: 61.3%, N = 1749 (smallest pairwise)
#> Use summary() for detailed output.

# --- Three-layer output ---
result <- efa(survey_data, political_orientation, environmental_concern,
              life_satisfaction, trust_government, trust_media, trust_science)
result              # compact overview
#> Exploratory Factor Analysis: 6 items, 3 components (PCA/Varimax)
#>   KMO = 0.505 (Miserable), Variance explained: 61.0%, N = 2168 (smallest pairwise)
#> Use summary() for detailed output.
summary(result)     # full detailed output with all sections
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
summary(result, communalities = FALSE)  # hide communalities table
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
