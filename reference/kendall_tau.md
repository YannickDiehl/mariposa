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
  alternative = "two.sided",
  na.rm = "pairwise"
)
```

## Arguments

- data:

  A data frame or tibble containing the variables to analyze

- ...:

  \<\code{\link\[dplyr\]{dplyr_tidy_select}}\> Variables for correlation
  analysis. Supports all tidyselect helpers. If more than two variables
  are selected, a correlation matrix is computed.

- weights:

  \<\code{\link\[dplyr\]{dplyr_data_masking}}\> Optional sampling
  weights for weighted correlations. Should be a numeric variable with
  positive values.

- alternative:

  Character string specifying the alternative hypothesis:

  - `"two.sided"` (default): Two-tailed test

  - `"less"`: One-tailed test (negative correlation)

  - `"greater"`: One-tailed test (positive correlation)

- na.rm:

  Character string specifying missing data handling:

  - `"pairwise"` (default): Pairwise deletion - each correlation uses
    all available cases

  - `"listwise"`: Listwise deletion - only complete cases across all
    variables

## Value

An object of class `"kendall_tau"` containing:

- correlations:

  Data frame with tau coefficients, p-values, and z-scores

- n_obs:

  Matrix of sample sizes for each correlation

- variables:

  Character vector of analyzed variable names

- weights:

  Name of the weights variable (if used)

- alternative:

  Alternative hypothesis used

- is_grouped:

  Logical indicating if data was grouped

- groups:

  Grouping variables (if any)

## Details

### What Kendall's Tau Measures

Kendall's tau measures how often pairs of observations are in the same
order (concordant) versus different order (discordant). It's
particularly useful for:

- Ordinal data (like rating scales: strongly disagree to strongly agree)

- Data with outliers (more robust than Pearson correlation)

- Small sample sizes

- Non-linear but monotonic relationships

### Interpreting Results

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

### When to Use Kendall's Tau

Choose Kendall's tau when:

- Your data is ordinal (ranked categories)

- You have a small sample size (\< 30 observations)

- Your data has outliers that might affect Pearson correlation

- You want a more conservative measure than Spearman's rho

### Understanding the Output

The function provides:

- **tau**: The correlation coefficient

- **p-value**: Probability of seeing this correlation by chance (smaller
  = stronger evidence)

- **n**: Number of observations used

- **significance stars**: Quick visual indicator of statistical
  significance

## References

Kendall, M.G. (1938). A new measure of rank correlation. Biometrika,
30(1/2), 81-93.

Kendall, M.G. (1945). The treatment of ties in ranking problems.
Biometrika, 33(3), 239-251.

Agresti, A. (2010). Analysis of Ordinal Categorical Data (2nd ed.). John
Wiley & Sons.

## See also

[`cor`](https://rdrr.io/r/stats/cor.html) with method="kendall" for base
R implementation [`cor.test`](https://rdrr.io/r/stats/cor.test.html)
with method="kendall" for significance testing
[`pearson_cor`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md)
for Pearson correlation analysis

Other correlation:
[`pearson_cor()`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md),
[`spearman_rho()`](https://YannickDiehl.github.io/mariposa/reference/spearman_rho.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic correlation between two variables
survey_data %>%
  kendall_tau(life_satisfaction, political_orientation)
#> 
#> ── Kendall's Tau-b Correlation  ────────────────────────────────────────────────
#> 
#> • Missing data handling: pairwise deletion
#> • Alternative hypothesis: two.sided
#> 
#> 
#> ┌─ life_satisfaction × political_orientation ─┐
#> 
#>   Kendall's tau-b: τ = -0.004
#>   Sample size: n = 2228
#>   z-score: -0.212
#>   p-value (2-tailed): 0.8321
#>   Significance: ns
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation: weak negative correlation

# Correlation matrix for multiple variables
survey_data %>%
  kendall_tau(life_satisfaction, political_orientation, trust_media)
#> 
#> ── Kendall's Tau-b Correlation  ────────────────────────────────────────────────
#> 
#> • Missing data handling: pairwise deletion
#> • Alternative hypothesis: two.sided
#> 
#> 
#> Kendall's Tau-b Matrix:
#> ----------------------- 
#>                       life_satisfaction political_orientation trust_media
#> life_satisfaction                 1.000                -0.004       0.023
#> political_orientation            -0.004                 1.000       0.003
#> trust_media                       0.023                 0.003       1.000
#> ----------------------- 
#> 
#> Significance Matrix (p-values, 2-tailed):
#> ----------------------------------------- 
#>                       life_satisfaction political_orientation trust_media
#> life_satisfaction                0.0000                0.8321      0.1751
#> political_orientation            0.8321                0.0000      0.8823
#> trust_media                      0.1751                0.8823      0.0000
#> ----------------------------------------- 
#> 
#> Sample Size Matrix:
#> ------------------- 
#>                       life_satisfaction political_orientation trust_media
#> life_satisfaction                  2421                  2228        2291
#> political_orientation              2228                  2299        2177
#> trust_media                        2291                  2177        2367
#> ------------------- 
#> 
#> Pairwise Results:
#> ─────────────────────────────────────────────────────────────────────
#>                                       Pair  tau_b      z      p    n sig
#>  life_satisfaction × political_orientation -0.004 -0.212 0.8321 2228    
#>            life_satisfaction × trust_media  0.023  1.356 0.1751 2291    
#>        political_orientation × trust_media  0.003  0.148 0.8823 2177    
#> ─────────────────────────────────────────────────────────────────────
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05

# Weighted correlations
survey_data %>%
  kendall_tau(age, income, weights = sampling_weight)
#> 
#> ── Weighted Kendall's Tau-b Correlation  ───────────────────────────────────────
#> 
#> • Weights variable: sampling_weight
#> • Missing data handling: pairwise deletion
#> • Alternative hypothesis: two.sided
#> 
#> 
#> ┌─ age × income ─┐
#> 
#>   Kendall's tau-b: τ = 0.003
#>   Sample size: n = 2201
#>   z-score: 0.202
#>   p-value (2-tailed): 0.8397
#>   Significance: ns
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation: weak positive correlation

# Grouped correlations
survey_data %>%
  group_by(region) %>%
  kendall_tau(age, income, life_satisfaction)
#> 
#> ── Kendall's Tau-b Correlation  ────────────────────────────────────────────────
#> 
#> • Missing data handling: pairwise deletion
#> • Alternative hypothesis: two.sided
#> 
#> 
#> 
#> ── Group: region = East ──
#> 
#> 
#> Kendall's Tau-b Matrix:
#> ----------------------- 
#>                       age  income life_satisfaction
#> age                 1.000   0.040            -0.030
#> income              0.040   1.000             0.338
#> life_satisfaction  -0.030   0.338             1.000
#> ----------------------- 
#> 
#> Significance Matrix (p-values, 2-tailed):
#> ----------------------------------------- 
#>                       age  income life_satisfaction
#> age                0.0000  0.2272            0.3799
#> income             0.2272  0.0000            0.0000
#> life_satisfaction  0.3799  0.0000            0.0000
#> ----------------------------------------- 
#> 
#> Sample Size Matrix:
#> ------------------- 
#>                    age income life_satisfaction
#> age                485    429               465
#> income             429    429               410
#> life_satisfaction  465    410               465
#> ------------------- 
#> 
#> Pairwise Results:
#> ─────────────────────────────────────────────────────────────────────
#>                        Pair  tau_b      z      p   n sig
#>                age × income  0.040  1.208 0.2272 429    
#>     age × life_satisfaction -0.030 -0.878 0.3799 465    
#>  income × life_satisfaction  0.338  9.099 0.0000 410 ***
#> ─────────────────────────────────────────────────────────────────────
#> 
#> 
#> ── Group: region = West ──
#> 
#> 
#> Kendall's Tau-b Matrix:
#> ----------------------- 
#>                       age  income life_satisfaction
#> age                 1.000  -0.006            -0.015
#> income             -0.006   1.000             0.357
#> life_satisfaction  -0.015   0.357             1.000
#> ----------------------- 
#> 
#> Significance Matrix (p-values, 2-tailed):
#> ----------------------------------------- 
#>                       age  income life_satisfaction
#> age                0.0000  0.7257            0.3766
#> income             0.7257  0.0000            0.0000
#> life_satisfaction  0.3766  0.0000            0.0000
#> ----------------------------------------- 
#> 
#> Sample Size Matrix:
#> ------------------- 
#>                     age income life_satisfaction
#> age                2015   1757              1956
#> income             1757   1757              1705
#> life_satisfaction  1956   1705              1956
#> ------------------- 
#> 
#> Pairwise Results:
#> ─────────────────────────────────────────────────────────────────────
#>                        Pair  tau_b      z      p    n sig
#>                age × income -0.006 -0.351 0.7257 1757    
#>     age × life_satisfaction -0.015 -0.884 0.3766 1956    
#>  income × life_satisfaction  0.357 19.639 0.0000 1705 ***
#> ─────────────────────────────────────────────────────────────────────
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05

# Using tidyselect helpers for ordinal variables
survey_data %>%
  kendall_tau(starts_with("trust"), weights = sampling_weight)
#> 
#> ── Weighted Kendall's Tau-b Correlation  ───────────────────────────────────────
#> 
#> • Weights variable: sampling_weight
#> • Missing data handling: pairwise deletion
#> • Alternative hypothesis: two.sided
#> 
#> 
#> Kendall's Tau-b Matrix:
#> ----------------------- 
#>                  trust_government trust_media trust_science
#> trust_government            1.000       0.007         0.021
#> trust_media                 0.007       1.000         0.013
#> trust_science               0.021       0.013         1.000
#> ----------------------- 
#> 
#> Significance Matrix (p-values, 2-tailed):
#> ----------------------------------------- 
#>                  trust_government trust_media trust_science
#> trust_government           0.0000      0.6405        0.1429
#> trust_media                0.6405      0.0000        0.3537
#> trust_science              0.1429      0.3537        0.0000
#> ----------------------------------------- 
#> 
#> Sample Size Matrix:
#> ------------------- 
#>                  trust_government trust_media trust_science
#> trust_government             2371        2242          2271
#> trust_media                  2242        2382          2286
#> trust_science                2271        2286          2414
#> ------------------- 
#> 
#> Pairwise Results:
#> ─────────────────────────────────────────────────────────────────────
#>                              Pair tau_b     z      p    n sig
#>    trust_government × trust_media 0.007 0.467 0.6405 2242    
#>  trust_government × trust_science 0.021 1.465 0.1429 2271    
#>       trust_media × trust_science 0.013 0.927 0.3537 2286    
#> ─────────────────────────────────────────────────────────────────────
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05

# Listwise deletion for missing data
survey_data %>%
  kendall_tau(age, income, na.rm = "listwise")
#> 
#> ── Kendall's Tau-b Correlation  ────────────────────────────────────────────────
#> 
#> • Missing data handling: listwise deletion
#> • Alternative hypothesis: two.sided
#> 
#> 
#> ┌─ age × income ─┐
#> 
#>   Kendall's tau-b: τ = 0.002
#>   Sample size: n = 2186
#>   z-score: 0.168
#>   p-value (2-tailed): 0.8667
#>   Significance: ns
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation: weak positive correlation

# One-tailed test
survey_data %>%
  kendall_tau(age, income, alternative = "greater")
#> 
#> ── Kendall's Tau-b Correlation  ────────────────────────────────────────────────
#> 
#> • Missing data handling: pairwise deletion
#> • Alternative hypothesis: greater
#> 
#> 
#> ┌─ age × income ─┐
#> 
#>   Kendall's tau-b: τ = 0.002
#>   Sample size: n = 2186
#>   z-score: 0.168
#>   p-value (1-tailed): 0.4334
#>   Significance: ns
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation: weak positive correlation

# Store results for further analysis
result <- survey_data %>%
  kendall_tau(life_satisfaction, political_orientation, trust_media,
              weights = sampling_weight)
print(result)
#> 
#> ── Weighted Kendall's Tau-b Correlation  ───────────────────────────────────────
#> 
#> • Weights variable: sampling_weight
#> • Missing data handling: pairwise deletion
#> • Alternative hypothesis: two.sided
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
#> ─────────────────────────────────────────────────────────────────────
#>                                       Pair  tau_b      z      p    n sig
#>  life_satisfaction × political_orientation -0.004 -0.298 0.7659 2241    
#>            life_satisfaction × trust_media  0.021  1.505 0.1323 2305    
#>        political_orientation × trust_media  0.003  0.238 0.8118 2190    
#> ─────────────────────────────────────────────────────────────────────
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
