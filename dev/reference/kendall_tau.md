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

[`spearman_rho`](https://YannickDiehl.github.io/mariposa/dev/reference/spearman_rho.md)
for Spearman's rank correlation.

[`pearson_cor`](https://YannickDiehl.github.io/mariposa/dev/reference/pearson_cor.md)
for Pearson correlation analysis.

[`summary.kendall_tau`](https://YannickDiehl.github.io/mariposa/dev/reference/summary.kendall_tau.md)
for detailed output with toggleable sections.

Other correlation:
[`pearson_cor()`](https://YannickDiehl.github.io/mariposa/dev/reference/pearson_cor.md),
[`spearman_rho()`](https://YannickDiehl.github.io/mariposa/dev/reference/spearman_rho.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic correlation between two variables
survey_data %>%
  kendall_tau(life_satisfaction, political_orientation)
#> Kendall's Tau: life_satisfaction x political_orientation
#>   tau = -0.004, p = 0.832 , N = 2228

# Correlation matrix for multiple variables
survey_data %>%
  kendall_tau(life_satisfaction, political_orientation, trust_media)
#> Kendall's Tau: 3 variables
#>   life_satisfaction x political_orientation: tau = -0.004, p = 0.832  
#>   life_satisfaction x trust_media: tau = 0.023, p = 0.175  
#>   political_orientation x trust_media: tau = 0.003, p = 0.882  
#>   0/3 pairs significant (p < .05), N = 2228

# Weighted correlations
survey_data %>%
  kendall_tau(age, income, weights = sampling_weight)
#> Kendall's Tau: age x income [Weighted]
#>   tau = 0.003, p = 0.840 , N = 2201

# Listwise deletion for missing data
survey_data %>%
  kendall_tau(age, income, use = "listwise")
#> Kendall's Tau: age x income
#>   tau = 0.002, p = 0.867 , N = 2186

# One-tailed test
survey_data %>%
  kendall_tau(age, income, alternative = "greater")
#> Kendall's Tau: age x income
#>   tau = 0.002, p = 0.433 , N = 2186

# \donttest{
# Grouped correlations
survey_data %>%
  group_by(region) %>%
  kendall_tau(age, income, life_satisfaction)
#> [region = 1]
#> Kendall's Tau: 3 variables
#>   age x income:                  tau = 0.040, p = 0.227  
#>   age x life_satisfaction:       tau = -0.030, p = 0.380  
#>   income x life_satisfaction:    tau = 0.338, p < 0.001 *** 
#>   1/3 pairs significant (p < .05), N = 429
#> [region = 2]
#> Kendall's Tau: 3 variables
#>   age x income:                  tau = -0.006, p = 0.726  
#>   age x life_satisfaction:       tau = -0.015, p = 0.377  
#>   income x life_satisfaction:    tau = 0.357, p < 0.001 *** 
#>   1/3 pairs significant (p < .05), N = 1757

# Using tidyselect helpers for ordinal variables
survey_data %>%
  kendall_tau(starts_with("trust"), weights = sampling_weight)
#> Kendall's Tau: 3 variables [Weighted]
#>   trust_government x trust_media: tau = 0.007, p = 0.640  
#>   trust_government x trust_science: tau = 0.021, p = 0.143  
#>   trust_media x trust_science:   tau = 0.013, p = 0.354  
#>   0/3 pairs significant (p < .05), N = 2242

# --- Three-layer output ---
result <- survey_data %>%
  kendall_tau(life_satisfaction, political_orientation, trust_media,
              weights = sampling_weight)
result              # compact one-line overview
#> Kendall's Tau: 3 variables [Weighted]
#>   life_satisfaction x political_orientation: tau = -0.004, p = 0.766  
#>   life_satisfaction x trust_media: tau = 0.021, p = 0.132  
#>   political_orientation x trust_media: tau = 0.003, p = 0.812  
#>   0/3 pairs significant (p < .05), N = 2241
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
#> life_satisfaction                 1.000                -0.004       0.021
#> political_orientation            -0.004                 1.000       0.003
#> trust_media                       0.021                 0.003       1.000
#> ----------------------- 
#> 
#> Significance Matrix (p-values, 2-tailed):
#> ----------------------------------------- 
#>                       life_satisfaction political_orientation trust_media
#> life_satisfaction                0.0000                0.7659      0.1323
#> political_orientation            0.7659                0.0000      0.8118
#> trust_media                      0.1323                0.8118      0.0000
#> ----------------------------------------- 
#> 
#> Sample Size Matrix:
#> ------------------- 
#>                       life_satisfaction political_orientation trust_media
#> life_satisfaction                  2437                  2241        2305
#> political_orientation              2241                  2312        2190
#> trust_media                        2305                  2190        2382
#> ------------------- 
#> 
#> Pairwise Results:
#> --------------------------------------------------------------------- 
#>                                       Pair  tau_b      z      p    n sig
#>  life_satisfaction × political_orientation -0.004 -0.298 0.7659 2241    
#>            life_satisfaction × trust_media  0.021  1.505 0.1323 2305    
#>        political_orientation × trust_media  0.003  0.238 0.8118 2190    
#> --------------------------------------------------------------------- 
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
#> life_satisfaction                 1.000                -0.004       0.021
#> political_orientation            -0.004                 1.000       0.003
#> trust_media                       0.021                 0.003       1.000
#> ----------------------- 
#> 
#> Sample Size Matrix:
#> ------------------- 
#>                       life_satisfaction political_orientation trust_media
#> life_satisfaction                  2437                  2241        2305
#> political_orientation              2241                  2312        2190
#> trust_media                        2305                  2190        2382
#> ------------------- 
#> 
#> Pairwise Results:
#> --------------------------------------------------------------------- 
#>                                       Pair  tau_b      z      p    n sig
#>  life_satisfaction × political_orientation -0.004 -0.298 0.7659 2241    
#>            life_satisfaction × trust_media  0.021  1.505 0.1323 2305    
#>        political_orientation × trust_media  0.003  0.238 0.8118 2190    
#> --------------------------------------------------------------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
# }
```
