# Spearman's Rank Correlation Analysis

Calculates Spearman's rank correlation coefficients (rho) between
variables with support for grouped data and multiple variable pairs.
Provides significance testing and SPSS-compatible output formatting.
Survey weights only select cases (see `weights`); the coefficient itself
is unweighted, as in SPSS NONPAR CORR.

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
  `starts_with("trust")`. Numeric variables or ordered factors (ranked
  by their level order).

- weights:

  Optional survey weights. Following the SPSS NONPAR CORR convention,
  weights are used only to filter cases (rows with weight \<= 0 or NA
  are dropped); the rank correlation itself is computed unweighted on
  the remaining sample. For design-based weighted Spearman use
  [`survey::svyolr()`](https://rdrr.io/pkg/survey/man/svyolr.html) or
  `wCorr::weightedCorr(method = "Spearman")`. Give a column name
  (unquoted or as a string), an expression such as
  `sampling_weight * 2`, or a numeric vector with one weight per row.

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
size matrices are also provided. Use
[`summary()`](https://rdrr.io/r/base/summary.html) for the full
SPSS-style output with toggleable sections.

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

[`summary.spearman_rho`](https://YannickDiehl.github.io/mariposa/reference/summary.spearman_rho.md)
for detailed output with toggleable sections.

Other correlation:
[`kendall_tau()`](https://YannickDiehl.github.io/mariposa/reference/kendall_tau.md),
[`partial_cor()`](https://YannickDiehl.github.io/mariposa/reference/partial_cor.md),
[`pearson_cor()`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic correlation between two variables
survey_data %>%
  spearman_rho(life_satisfaction, political_orientation)
#> Spearman Correlation: life_satisfaction x political_orientation
#>   rho = -0.004, p = 0.833, N = 2228
#> Use summary() for detailed output.

# Correlation matrix for multiple variables
survey_data %>%
  spearman_rho(life_satisfaction, political_orientation, trust_media)
#> Spearman Correlation: 3 variables
#>   life_satisfaction x political_orientation: rho = -0.004, p = 0.833
#>   life_satisfaction x trust_media:           rho = 0.028, p = 0.181
#>   political_orientation x trust_media:       rho = 0.003, p = 0.885
#>   0/3 pairs significant (p < .05), N = 2177-2291
#> Use summary() for detailed output.

# Weights only filter cases (weight <= 0 or NA dropped); rho itself is
# unweighted, matching SPSS NONPAR CORR
survey_data %>%
  spearman_rho(age, income, weights = sampling_weight)
#> Spearman Correlation: age x income
#>   rho = 0.003, p = 0.870, N = 2186
#> Use summary() for detailed output.

# Grouped correlations
survey_data %>%
  group_by(region) %>%
  spearman_rho(age, income, life_satisfaction)
#> [region = East]
#> Spearman Correlation: 3 variables
#>   age x income:               rho = 0.058, p = 0.234
#>   age x life_satisfaction:    rho = -0.040, p = 0.391
#>   income x life_satisfaction: rho = 0.440, p < 0.001 ***
#>   1/3 pairs significant (p < .05), N = 410-465
#> [region = West]
#> Spearman Correlation: 3 variables
#>   age x income:               rho = -0.008, p = 0.725
#>   age x life_satisfaction:    rho = -0.020, p = 0.382
#>   income x life_satisfaction: rho = 0.470, p < 0.001 ***
#>   1/3 pairs significant (p < .05), N = 1705-1956
#> Use summary() for detailed output.

# Using tidyselect helpers
survey_data %>%
  spearman_rho(starts_with("trust"), weights = sampling_weight)
#> Spearman Correlation: 3 variables
#>   trust_government x trust_media:   rho = 0.008, p = 0.723
#>   trust_government x trust_science: rho = 0.027, p = 0.207
#>   trust_media x trust_science:      rho = 0.016, p = 0.453
#>   0/3 pairs significant (p < .05), N = 2227-2272
#> Use summary() for detailed output.

# Listwise deletion for missing data
survey_data %>%
  spearman_rho(age, income, use = "listwise")
#> Spearman Correlation: age x income
#>   rho = 0.003, p = 0.870, N = 2186
#> Use summary() for detailed output.

# One-tailed test
survey_data %>%
  spearman_rho(age, income, alternative = "greater")
#> Spearman Correlation: age x income [one-sided: greater]
#>   rho = 0.003, p = 0.435, N = 2186
#> Use summary() for detailed output.

# --- Three-layer output ---
result <- spearman_rho(survey_data, age, income, life_satisfaction)
result              # compact one-line overview
#> Spearman Correlation: 3 variables
#>   age x income:               rho = 0.003, p = 0.870
#>   age x life_satisfaction:    rho = -0.024, p = 0.238
#>   income x life_satisfaction: rho = 0.464, p < 0.001 ***
#>   1/3 pairs significant (p < .05), N = 2115-2421
#> Use summary() for detailed output.
summary(result)     # full correlation, p-value, and N matrices
#> 
#> Spearman's Rank Correlation Analysis
#> ------------------------------------
#> 
#> - Method: Spearman's rho (rank correlation)
#> - Variables: age, income, life_satisfaction
#> - Missing data handling: pairwise deletion
#> - Alternative hypothesis: two.sided
#> 
#> 
#> Spearman's Rho Matrix:
#> ----------------------
#>                       age     income     life_satisfaction   
#> age                     1      0.003                -0.024   
#> income              0.003          1                 0.464***
#> life_satisfaction  -0.024      0.464***                  1   
#> ----------------------
#> 
#> Significance Matrix (p-values, 2-tailed):
#> -----------------------------------------
#>                     age  income  life_satisfaction
#> age                        .870               .238
#> income             .870                      <.001
#> life_satisfaction  .238   <.001                   
#> -----------------------------------------
#> 
#> Sample Size Matrix:
#> -------------------
#>                     age  income  life_satisfaction
#> age                2500    2186               2421
#> income             2186    2186               2115
#> life_satisfaction  2421    2115               2421
#> -------------------
#> 
#> Pairwise Results:
#>   ------------------------------------------------------------
#>   Pair                           rho       t      p     N     
#>   ------------------------------------------------------------
#>   age x income                 0.003   0.163   .870  2186     
#>   age x life_satisfaction     -0.024  -1.180   .238  2421     
#>   income x life_satisfaction   0.464  24.073  <.001  2115  ***
#>   ------------------------------------------------------------
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
summary(result, pvalue_matrix = FALSE)  # hide p-values
#> 
#> Spearman's Rank Correlation Analysis
#> ------------------------------------
#> 
#> - Method: Spearman's rho (rank correlation)
#> - Variables: age, income, life_satisfaction
#> - Missing data handling: pairwise deletion
#> - Alternative hypothesis: two.sided
#> 
#> 
#> Spearman's Rho Matrix:
#> ----------------------
#>                       age     income     life_satisfaction   
#> age                     1      0.003                -0.024   
#> income              0.003          1                 0.464***
#> life_satisfaction  -0.024      0.464***                  1   
#> ----------------------
#> 
#> Sample Size Matrix:
#> -------------------
#>                     age  income  life_satisfaction
#> age                2500    2186               2421
#> income             2186    2186               2115
#> life_satisfaction  2421    2115               2421
#> -------------------
#> 
#> Pairwise Results:
#>   ------------------------------------------------------------
#>   Pair                           rho       t      p     N     
#>   ------------------------------------------------------------
#>   age x income                 0.003   0.163   .870  2186     
#>   age x life_satisfaction     -0.024  -1.180   .238  2421     
#>   income x life_satisfaction   0.464  24.073  <.001  2115  ***
#>   ------------------------------------------------------------
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
