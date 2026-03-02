# Spearman's Rank Correlation Analysis

Calculates Spearman's rank correlation coefficients (rho) between
variables with support for weighted correlations, grouped data, and
multiple variable pairs. Provides significance testing and
SPSS-compatible output formatting.

Spearman's rho is a non-parametric measure of rank correlation that
assesses monotonic relationships between variables. It is particularly
suitable for ordinal data or when the assumptions of Pearson correlation
are not met.

## Usage

``` r
spearman_rho(
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

  Optional survey weights for population-representative results. Note:
  SPSS may not apply weights to Spearman's rho; our implementation uses
  weighted ranks for mathematically correct survey analysis.

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
including the rho coefficient, p-value, t-statistic, and sample size for
each pair. For multiple variables, correlation, significance, and sample
size matrices are also provided.

## Details

### Understanding the Results

Spearman's rho measures the strength and direction of a monotonic
relationship between two variables. Unlike Pearson correlation, it does
not require a linear relationship – it just needs one variable to
consistently increase (or decrease) as the other increases. Think of it
as ranking your data first, then checking if the ranks tend to go
together.

The rho value ranges from -1 to +1:

- **Strong positive** (0.7 to 1.0): High ranks in one variable go with
  high ranks in the other

- **Moderate positive** (0.3 to 0.7): Moderate tendency for ranks to
  increase together

- **Weak positive** (0 to 0.3): Slight tendency for ranks to increase
  together

- **No correlation** (near 0): No relationship between the variables

- **Negative values**: As one variable's rank increases, the other's
  tends to decrease

The output also provides:

- **p-value**: Probability of seeing this correlation by chance

- **n**: Number of observation pairs used

- **significance stars**: Visual indicator (\*\*\* very strong, \*\*
  strong, \* moderate evidence)

### When to Use This

Choose Spearman's rho when:

- Your relationship is monotonic but not necessarily linear

- Your data has outliers (they have less impact on ranks)

- Your variables are ordinal (ordered categories)

- You are not sure if your data meets Pearson correlation assumptions

- You want to detect any monotonic trend, not just linear ones

### Spearman vs. Kendall

- **Spearman's rho** is usually larger in magnitude than Kendall's tau

- **Spearman's rho** is better for detecting linear relationships in
  ranks

- **Kendall's tau** is more robust and has better statistical properties

- **Spearman's rho** is more commonly reported in research

## References

Spearman, C. (1904). The proof and measurement of association between
two things. *American Journal of Psychology*, 15(1), 72–101.

Lehmann, E. L. (1975). *Nonparametrics: Statistical Methods Based on
Ranks*. Holden-Day.

## See also

[`cor`](https://rdrr.io/r/stats/cor.html) with `method = "spearman"` for
the base R implementation.

[`kendall_tau`](https://YannickDiehl.github.io/mariposa/reference/kendall_tau.md)
for Kendall's rank correlation.

[`pearson_cor`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md)
for Pearson correlation analysis.

Other correlation:
[`kendall_tau()`](https://YannickDiehl.github.io/mariposa/reference/kendall_tau.md),
[`pearson_cor()`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic correlation between two variables
survey_data %>%
  spearman_rho(life_satisfaction, political_orientation)
#> 
#> ── Spearman's Rank Correlation Analysis  ───────────────────────────────────────
#> 
#> • Method: Spearman's rho (rank correlation)
#> • Variables: life_satisfaction, political_orientation
#> • Missing data handling: pairwise deletion
#> • Alternative hypothesis: two.sided
#> 
#> --- life_satisfaction × political_orientation ---
#> 
#>   Spearman's rho: ρ = -0.004
#>   t-statistic: -0.210
#>   Sample size: n = 2228
#>   p-value (2-tailed): 0.8334
#>   Significance: ns
#> 
#> Signif. codes: 0 *** 0.001 ** 0.01 * 0.05
#> 
#> Interpretation: weak negative monotonic relationship

# Correlation matrix for multiple variables
survey_data %>%
  spearman_rho(life_satisfaction, political_orientation, trust_media)
#> 
#> ── Spearman's Rank Correlation Analysis  ───────────────────────────────────────
#> 
#> • Method: Spearman's rho (rank correlation)
#> • Variables: life_satisfaction, political_orientation, trust_media
#> • Missing data handling: pairwise deletion
#> • Alternative hypothesis: two.sided
#> 
#> 
#> Spearman's Rho Matrix:
#> --------------------------------------------------------------------- 
#>               Variable life_satisfaction political_orientation trust_media
#>      life_satisfaction             1.000                -0.004       0.028
#>  political_orientation            -0.004                 1.000       0.003
#>            trust_media             0.028                 0.003       1.000
#> 
#> Significance Matrix (p-values, 2-tailed):
#> --------------------------------------------------------------------- 
#>               Variable life_satisfaction political_orientation trust_media
#>      life_satisfaction               -                  0.8334      0.1805
#>  political_orientation            0.8334                   -        0.8846
#>            trust_media            0.1805                0.8846         -  
#> 
#> Sample Size Matrix:
#> --------------------------------------------------------------------- 
#>               Variable life_satisfaction political_orientation trust_media
#>      life_satisfaction              2421                  2228        2291
#>  political_orientation              2228                  2299        2177
#>            trust_media              2291                  2177        2367
#> 
#> Pairwise Results:
#> --------------------------------------------------------------------- 
#>                                       Pair    rho      t      p    n sig
#>  life_satisfaction × political_orientation -0.004 -0.210 0.8334 2228    
#>            life_satisfaction × trust_media  0.028  1.340 0.1805 2291    
#>        political_orientation × trust_media  0.003  0.145 0.8846 2177    
#> --------------------------------------------------------------------- 
#> 
#> Signif. codes: 0 *** 0.001 ** 0.01 * 0.05

# Weighted correlations (mathematically correct, though SPSS may not apply weights)
survey_data %>%
  spearman_rho(age, income, weights = sampling_weight)
#> 
#> ── Weighted Spearman's Rank Correlation Analysis  ──────────────────────────────
#> 
#> • Method: Spearman's rho (rank correlation)
#> • Variables: age, income
#> • Weights variable: sampling_weight
#> • Missing data handling: pairwise deletion
#> • Alternative hypothesis: two.sided
#> 
#> --- age × income ---
#> 
#>   Spearman's rho: ρ = 0.003
#>   t-statistic: 0.163
#>   Sample size: n = 2186
#>   p-value (2-tailed): 0.8703
#>   Significance: ns
#> 
#> Signif. codes: 0 *** 0.001 ** 0.01 * 0.05
#> 
#> Interpretation: weak positive monotonic relationship

# Grouped correlations
survey_data %>%
  group_by(region) %>%
  spearman_rho(age, income, life_satisfaction)
#> 
#> ── Spearman's Rank Correlation Analysis  ───────────────────────────────────────
#> 
#> • Method: Spearman's rho (rank correlation)
#> • Variables: age, income, life_satisfaction
#> • Missing data handling: pairwise deletion
#> • Alternative hypothesis: two.sided
#> 
#> 
#> ── Group: region = East ──
#> 
#> 
#> Spearman's Rho Matrix:
#> --------------------------------------------------------------------- 
#>           Variable    age income life_satisfaction
#>                age  1.000  0.058            -0.040
#>             income  0.058  1.000             0.440
#>  life_satisfaction -0.040  0.440             1.000
#> 
#> Significance Matrix (p-values, 2-tailed):
#> --------------------------------------------------------------------- 
#>           Variable    age income life_satisfaction
#>                age    -   0.2342            0.3913
#>             income 0.2342    -              0.0000
#>  life_satisfaction 0.3913 0.0000               -  
#> 
#> Sample Size Matrix:
#> --------------------------------------------------------------------- 
#>           Variable   age income life_satisfaction
#>                age   485    429               465
#>             income   429    429               410
#>  life_satisfaction   465    410               465
#> 
#> Pairwise Results:
#> --------------------------------------------------------------------- 
#>                        Pair    rho      t      p   n sig
#>                age × income  0.058  1.191 0.2342 429    
#>     age × life_satisfaction -0.040 -0.858 0.3913 465    
#>  income × life_satisfaction  0.440  9.886 0.0000 410 ***
#> --------------------------------------------------------------------- 
#> 
#> 
#> ── Group: region = West ──
#> 
#> 
#> Spearman's Rho Matrix:
#> --------------------------------------------------------------------- 
#>           Variable    age income life_satisfaction
#>                age  1.000 -0.008            -0.020
#>             income -0.008  1.000             0.470
#>  life_satisfaction -0.020  0.470             1.000
#> 
#> Significance Matrix (p-values, 2-tailed):
#> --------------------------------------------------------------------- 
#>           Variable    age income life_satisfaction
#>                age    -   0.7251            0.3819
#>             income 0.7251    -              0.0000
#>  life_satisfaction 0.3819 0.0000               -  
#> 
#> Sample Size Matrix:
#> --------------------------------------------------------------------- 
#>           Variable   age income life_satisfaction
#>                age  2015   1757              1956
#>             income  1757   1757              1705
#>  life_satisfaction  1956   1705              1956
#> 
#> Pairwise Results:
#> --------------------------------------------------------------------- 
#>                        Pair    rho      t      p    n sig
#>                age × income -0.008 -0.352 0.7251 1757    
#>     age × life_satisfaction -0.020 -0.875 0.3819 1956    
#>  income × life_satisfaction  0.470 21.981 0.0000 1705 ***
#> --------------------------------------------------------------------- 
#> 
#> Signif. codes: 0 *** 0.001 ** 0.01 * 0.05

# Using tidyselect helpers
survey_data %>%
  spearman_rho(starts_with("trust"), weights = sampling_weight)
#> 
#> ── Weighted Spearman's Rank Correlation Analysis  ──────────────────────────────
#> 
#> • Method: Spearman's rho (rank correlation)
#> • Variables: trust_government, trust_media, trust_science
#> • Weights variable: sampling_weight
#> • Missing data handling: pairwise deletion
#> • Alternative hypothesis: two.sided
#> 
#> 
#> Spearman's Rho Matrix:
#> --------------------------------------------------------------------- 
#>          Variable trust_government trust_media trust_science
#>  trust_government            1.000       0.008         0.027
#>       trust_media            0.008       1.000         0.016
#>     trust_science            0.027       0.016         1.000
#> 
#> Significance Matrix (p-values, 2-tailed):
#> --------------------------------------------------------------------- 
#>          Variable trust_government trust_media trust_science
#>  trust_government              -        0.7227        0.2070
#>       trust_media           0.7227         -          0.4533
#>     trust_science           0.2070      0.4533           -  
#> 
#> Sample Size Matrix:
#> --------------------------------------------------------------------- 
#>          Variable trust_government trust_media trust_science
#>  trust_government             2354        2227          2255
#>       trust_media             2227        2367          2272
#>     trust_science             2255        2272          2398
#> 
#> Pairwise Results:
#> --------------------------------------------------------------------- 
#>                              Pair   rho     t      p    n sig
#>    trust_government × trust_media 0.008 0.355 0.7227 2227    
#>  trust_government × trust_science 0.027 1.262 0.2070 2255    
#>       trust_media × trust_science 0.016 0.750 0.4533 2272    
#> --------------------------------------------------------------------- 
#> 
#> Signif. codes: 0 *** 0.001 ** 0.01 * 0.05

# Listwise deletion for missing data
survey_data %>%
  spearman_rho(age, income, use = "listwise")
#> 
#> ── Spearman's Rank Correlation Analysis  ───────────────────────────────────────
#> 
#> • Method: Spearman's rho (rank correlation)
#> • Variables: age, income
#> • Missing data handling: listwise deletion
#> • Alternative hypothesis: two.sided
#> 
#> --- age × income ---
#> 
#>   Spearman's rho: ρ = 0.003
#>   t-statistic: 0.163
#>   Sample size: n = 2186
#>   p-value (2-tailed): 0.8703
#>   Significance: ns
#> 
#> Signif. codes: 0 *** 0.001 ** 0.01 * 0.05
#> 
#> Interpretation: weak positive monotonic relationship

# One-tailed test
survey_data %>%
  spearman_rho(age, income, alternative = "greater")
#> 
#> ── Spearman's Rank Correlation Analysis  ───────────────────────────────────────
#> 
#> • Method: Spearman's rho (rank correlation)
#> • Variables: age, income
#> • Missing data handling: pairwise deletion
#> • Alternative hypothesis: greater
#> 
#> --- age × income ---
#> 
#>   Spearman's rho: ρ = 0.003
#>   t-statistic: 0.163
#>   Sample size: n = 2186
#>   p-value (1-tailed): 0.4351
#>   Significance: ns
#> 
#> Signif. codes: 0 *** 0.001 ** 0.01 * 0.05
#> 
#> Interpretation: weak positive monotonic relationship
```
