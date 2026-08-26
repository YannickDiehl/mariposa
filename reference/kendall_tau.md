# Kendall's Tau Correlation Analysis

Calculates Kendall's tau-b rank correlation coefficients between
variables with support for weighted correlations, grouped data, and
multiple variable pairs. Provides significance testing and
SPSS-compatible output formatting.

The function computes tau-b, which is adjusted for ties and is
particularly suitable for ordinal data or when the assumptions of
Pearson correlation are not met. For weighted correlations, it uses
survey-weighted rank calculations.

## Usage

``` r
kendall_tau(
  data,
  ...,
  weights = NULL,
  alternative = c("two.sided", "less", "greater"),
  use = c("pairwise", "listwise"),
  na.rm = NULL
)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- ...:

  The variables you want to correlate. List two for a single correlation
  or more for a correlation matrix. You can use helpers like
  `starts_with("trust")`.

- weights:

  Optional survey weights for population-representative results.

- alternative:

  Direction of the test:

  - `"two.sided"` (default): Two-tailed test

  - `"less"`: One-tailed test (negative correlation)

  - `"greater"`: One-tailed test (positive correlation)

- use:

  How to handle missing values:

  - `"pairwise"` (default): Pairwise deletion - each correlation uses
    all available cases

  - `"listwise"`: Listwise deletion - only complete cases across all
    variables

- na.rm:

  Deprecated. Use `use` instead.

## Value

Correlation results showing rank-based relationships between variables,
including the tau-b coefficient, p-value, z-score, and sample size for
each pair. For multiple variables, correlation, significance, and sample
size matrices are also provided. Use
[`summary()`](https://rdrr.io/r/base/summary.html) for the full
SPSS-style output with toggleable sections.

## Details

### Understanding the Results

Kendall's tau measures how often pairs of observations are in the same
order (concordant) versus different order (discordant). It is
particularly useful for ordinal data, data with outliers, small sample
sizes, and non-linear but monotonic relationships.

The tau value ranges from -1 to +1:

- **Strong positive** (0.5 to 1.0): High values of one variable tend to
  go with high values of the other

- **Moderate positive** (0.3 to 0.5): Some tendency for values to
  increase together

- **Weak positive** (0 to 0.3): Slight tendency for values to increase
  together

- **No correlation** (near 0): No relationship between the variables

- **Negative values**: As one variable increases, the other tends to
  decrease

The output also provides:

- **p-value**: Probability of seeing this correlation by chance (smaller
  = stronger evidence)

- **n**: Number of observations used

- **significance stars**: Quick visual indicator of statistical
  significance

### When to Use This

Choose Kendall's tau when:

- Your data is ordinal (ranked categories)

- You have a small sample size (\< 30 observations)

- Your data has outliers that might affect Pearson correlation

### Weighted variants

The weighted tau-b is an exact frequency-weighted computation (it
reproduces the unweighted tau when all weights equal 1, enforced by an
internal invariance suite), but its z statistic and p-value use a
no-ties normal approximation. SPSS `NONPAR CORR` ignores `WEIGHT BY`, so
weighted results have no SPSS reference; see
[`vignette("spss-compatibility")`](https://YannickDiehl.github.io/mariposa/articles/spss-compatibility.md)
for validation status.

- You want a more conservative measure than Spearman's rho

## References

Kendall, M. G. (1938). A new measure of rank correlation. *Biometrika*,
30(1/2), 81–93.

Kendall, M. G. (1945). The treatment of ties in ranking problems.
*Biometrika*, 33(3), 239–251.

Agresti, A. (2010). *Analysis of Ordinal Categorical Data* (2nd ed.).
John Wiley & Sons.

## See also

[`cor`](https://rdrr.io/r/stats/cor.html) with `method = "kendall"` for
the base R implementation.

[`spearman_rho`](https://YannickDiehl.github.io/mariposa/reference/spearman_rho.md)
for Spearman's rank correlation.

[`pearson_cor`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md)
for Pearson correlation analysis.

[`summary.kendall_tau`](https://YannickDiehl.github.io/mariposa/reference/summary.kendall_tau.md)
for detailed output with toggleable sections.

Other correlation:
[`partial_cor()`](https://YannickDiehl.github.io/mariposa/reference/partial_cor.md),
[`pearson_cor()`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md),
[`spearman_rho()`](https://YannickDiehl.github.io/mariposa/reference/spearman_rho.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Kendall's tau is O(n^2); the examples run on a subset for speed.
# On your own data, simply drop the subsetting.
mini_survey <- survey_data[1:300, ]

# Basic correlation between two variables
mini_survey %>%
  kendall_tau(life_satisfaction, political_orientation)
#> Kendall's Tau: life_satisfaction x political_orientation
#>   tau = -0.028, p = 0.582 , N = 260

# Correlation matrix for multiple variables
mini_survey %>%
  kendall_tau(life_satisfaction, political_orientation, trust_media)
#> Kendall's Tau: 3 variables
#>   life_satisfaction x political_orientation: tau = -0.028, p = 0.582  
#>   life_satisfaction x trust_media: tau = 0.026, p = 0.608  
#>   political_orientation x trust_media: tau = -0.037, p = 0.471  
#>   0/3 pairs significant (p < .05), N = 260

# Weighted correlations
mini_survey %>%
  kendall_tau(age, income, weights = sampling_weight)
#> Kendall's Tau: age x income [Weighted]
#>   tau = -0.036, p = 0.374 , N = 271

# Listwise deletion for missing data
mini_survey %>%
  kendall_tau(age, income, use = "listwise")
#> Kendall's Tau: age x income
#>   tau = -0.038, p = 0.370 , N = 267

# One-tailed test
mini_survey %>%
  kendall_tau(age, income, alternative = "greater")
#> Kendall's Tau: age x income
#>   tau = -0.038, p = 0.815 , N = 267

# \donttest{
# Grouped correlations
mini_survey %>%
  group_by(region) %>%
  kendall_tau(age, income, life_satisfaction)
#> [region = 1]
#> Kendall's Tau: 3 variables
#>   age x income:                  tau = 0.102, p = 0.265  
#>   age x life_satisfaction:       tau = 0.061, p = 0.532  
#>   income x life_satisfaction:    tau = 0.315, p = 0.002 ** 
#>   1/3 pairs significant (p < .05), N = 58
#> [region = 2]
#> Kendall's Tau: 3 variables
#>   age x income:                  tau = -0.068, p = 0.149  
#>   age x life_satisfaction:       tau = -0.034, p = 0.491  
#>   income x life_satisfaction:    tau = 0.361, p < 0.001 *** 
#>   1/3 pairs significant (p < .05), N = 209

# Using tidyselect helpers for ordinal variables
mini_survey %>%
  kendall_tau(starts_with("trust"), weights = sampling_weight)
#> Kendall's Tau: 3 variables [Weighted]
#>   trust_government x trust_media: tau = 0.084, p = 0.037 * 
#>   trust_government x trust_science: tau = -0.028, p = 0.493  
#>   trust_media x trust_science:   tau = 0.007, p = 0.870  
#>   1/3 pairs significant (p < .05), N = 274

# --- Three-layer output ---
result <- mini_survey %>%
  kendall_tau(life_satisfaction, political_orientation, trust_media,
              weights = sampling_weight)
result              # compact one-line overview
#> Kendall's Tau: 3 variables [Weighted]
#>   life_satisfaction x political_orientation: tau = -0.026, p = 0.535  
#>   life_satisfaction x trust_media: tau = 0.025, p = 0.534  
#>   political_orientation x trust_media: tau = -0.039, p = 0.350  
#>   0/3 pairs significant (p < .05), N = 262
summary(result)     # full correlation, p-value, and N matrices
#> 
#> Weighted Kendall's Tau-b Correlation 
#> -------------------------------------
#> 
#> - Weights variable: sampling_weight
#> - Missing data handling: pairwise deletion
#> - Alternative hypothesis: two.sided
#> 
#> 
#> Kendall's Tau-b Matrix:
#> ----------------------- 
#>                       life_satisfaction political_orientation trust_media
#> life_satisfaction                 1.000                -0.026       0.025
#> political_orientation            -0.026                 1.000      -0.039
#> trust_media                       0.025                -0.039       1.000
#> ----------------------- 
#> 
#> Significance Matrix (p-values, 2-tailed):
#> ----------------------------------------- 
#>                       life_satisfaction political_orientation trust_media
#> life_satisfaction                0.0000                0.5346      0.5345
#> political_orientation            0.5346                0.0000      0.3497
#> trust_media                      0.5345                0.3497      0.0000
#> ----------------------------------------- 
#> 
#> Sample Size Matrix:
#> ------------------- 
#>                       life_satisfaction political_orientation trust_media
#> life_satisfaction                   290                   262         277
#> political_orientation               262                   272         259
#> trust_media                         277                   259         289
#> ------------------- 
#> 
#> Pairwise Results:
#> ---------------- 
#>                                       Pair  tau_b      z      p   n sig
#>  life_satisfaction × political_orientation -0.026 -0.621 0.5346 262    
#>            life_satisfaction × trust_media  0.025  0.621 0.5345 277    
#>        political_orientation × trust_media -0.039 -0.935 0.3497 259    
#> ---------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
summary(result, pvalue_matrix = FALSE)  # hide p-values
#> 
#> Weighted Kendall's Tau-b Correlation 
#> -------------------------------------
#> 
#> - Weights variable: sampling_weight
#> - Missing data handling: pairwise deletion
#> - Alternative hypothesis: two.sided
#> 
#> 
#> Kendall's Tau-b Matrix:
#> ----------------------- 
#>                       life_satisfaction political_orientation trust_media
#> life_satisfaction                 1.000                -0.026       0.025
#> political_orientation            -0.026                 1.000      -0.039
#> trust_media                       0.025                -0.039       1.000
#> ----------------------- 
#> 
#> Sample Size Matrix:
#> ------------------- 
#>                       life_satisfaction political_orientation trust_media
#> life_satisfaction                   290                   262         277
#> political_orientation               262                   272         259
#> trust_media                         277                   259         289
#> ------------------- 
#> 
#> Pairwise Results:
#> ---------------- 
#>                                       Pair  tau_b      z      p   n sig
#>  life_satisfaction × political_orientation -0.026 -0.621 0.5346 262    
#>            life_satisfaction × trust_media  0.025  0.621 0.5345 277    
#>        political_orientation × trust_media -0.039 -0.935 0.3497 259    
#> ---------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
# }
```
