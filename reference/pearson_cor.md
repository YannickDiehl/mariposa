# Measure How Strongly Variables Are Related

`pearson_cor()` shows you how strongly numeric variables are related to
each other. For example, is age related to income? Does satisfaction
increase with experience? This helps you understand patterns in your
data.

The correlation tells you:

- **Direction**: Positive (both increase together) or negative (one
  increases as other decreases)

- **Strength**: How closely the variables move together (from 0 = no
  relationship to 1 = perfect relationship)

- **Significance**: Whether the relationship is real or could be due to
  chance

## Usage

``` r
pearson_cor(
  data,
  ...,
  weights = NULL,
  conf.level = 0.95,
  alternative = c("two.sided", "less", "greater"),
  use = c("pairwise", "listwise"),
  na.rm = NULL
)
```

## Arguments

- data:

  Your survey data (a data frame or tibble)

- ...:

  The numeric variables you want to correlate. List two for a single
  correlation or more for a correlation matrix.

- weights:

  Optional survey weights for population-representative results.
  Following SPSS CORRELATIONS, the weighted degrees of freedom use
  `n = sum(w)` (not Kish's effective sample size). This is appropriate
  for normalized survey weights with mean \\\approx 1\\. For raw
  expansion weights (e.g., summing to millions of population units), the
  resulting standard errors and confidence intervals will be drastically
  too narrow — in that case, normalize weights so that `sum(w) == n`, or
  use the survey package for design-based inference. Give a column name
  (unquoted or as a string), an expression such as
  `sampling_weight * 2`, or a numeric vector with one weight per row.

- conf.level:

  Confidence level for intervals (Default: 0.95 = 95%)

- alternative:

  Direction of the test: `"two.sided"` (default), `"less"`, or
  `"greater"`. A one-sided test gets the matching one-sided confidence
  interval (`[-1, upper]` for `"less"`, `[lower, 1]` for `"greater"`),
  as [`stats::cor.test()`](https://rdrr.io/r/stats/cor.test.html).

- use:

  How to handle missing values:

  - `"pairwise"` (default): Use all available data for each pair

  - `"listwise"`: Only use complete cases across all variables

- na.rm:

  Deprecated. Use `use` instead.

## Value

Correlation results showing relationships between variables, including:

- Correlation coefficient (r): Strength and direction of relationship

- P-value: Whether the relationship is statistically significant

- Confidence interval: Range of plausible correlation values

- Sample size: Number of observations used Use
  [`summary()`](https://rdrr.io/r/base/summary.html) for the full
  SPSS-style output with toggleable sections.

## Details

### Understanding the Results

**Correlation coefficient (r)** ranges from -1 to +1:

- **+1**: Perfect positive relationship (as one goes up, the other
  always goes up)

- **0**: No linear relationship

- **-1**: Perfect negative relationship (as one goes up, the other
  always goes down)

**Interpreting strength** (absolute value of r):

- 0.00 - 0.10: Negligible relationship

- 0.10 - 0.30: Weak relationship

- 0.30 - 0.50: Moderate relationship

- 0.50 - 0.70: Strong relationship

- 0.70 - 0.90: Very strong relationship

- 0.90 - 1.00: Extremely strong relationship

**P-value interpretation**:

- p \< 0.001: Very strong evidence of a relationship

- p \< 0.01: Strong evidence of a relationship

- p \< 0.05: Moderate evidence of a relationship

- p \>= 0.05: No significant relationship found

A correlation of 0.65 with p \< 0.001 means:

- Strong positive relationship (r = 0.65)

- As one variable increases, the other tends to increase

- Very unlikely to be due to chance (p \< 0.001)

- About 42% of variation is shared (r-squared = 0.65 squared = 0.42)

### When to Use This

Use Pearson correlation when:

- Both variables are numeric and continuous

- You expect a linear relationship

- Data is roughly normally distributed

- You want to measure strength of linear association

Don't use when:

- Data has extreme outliers (consider Spearman instead)

- Relationship is curved/non-linear

- Variables are categorical (use chi-squared test)

- You need to establish causation (correlation does not imply causation)

### Tips for Success

- Always plot your data first to check for non-linear patterns

- Consider both statistical significance (p-value) and practical
  importance (r value)

- Remember: correlation does not imply causation

- Check for outliers that might inflate or deflate correlations

- Use Spearman correlation for ordinal data or non-normal distributions

## References

Cohen, J. (1988). *Statistical Power Analysis for the Behavioral
Sciences* (2nd ed.). Lawrence Erlbaum Associates.

Fisher, R. A. (1915). Frequency distribution of the values of the
correlation coefficient in samples from an indefinitely large
population. *Biometrika*, 10(4), 507–521.

## See also

[`cor`](https://rdrr.io/r/stats/cor.html) for the base R correlation
function.

[`cor.test`](https://rdrr.io/r/stats/cor.test.html) for correlation
significance testing.

[`spearman_rho`](https://YannickDiehl.github.io/mariposa/reference/spearman_rho.md)
for rank-based correlation (robust to outliers).

[`kendall_tau`](https://YannickDiehl.github.io/mariposa/reference/kendall_tau.md)
for ordinal correlation.

[`summary.pearson_cor`](https://YannickDiehl.github.io/mariposa/reference/summary.pearson_cor.md)
for detailed output with toggleable sections.

Other correlation:
[`kendall_tau()`](https://YannickDiehl.github.io/mariposa/reference/kendall_tau.md),
[`partial_cor()`](https://YannickDiehl.github.io/mariposa/reference/partial_cor.md),
[`spearman_rho()`](https://YannickDiehl.github.io/mariposa/reference/spearman_rho.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic correlation between two variables
survey_data %>%
  pearson_cor(age, income)
#> Pearson Correlation: age x income
#>   r = -0.007, p = 0.761, N = 2186
#> Use summary() for detailed output.

# Correlation matrix for multiple variables
survey_data %>%
  pearson_cor(age, income, life_satisfaction)
#> Pearson Correlation: 3 variables
#>   age x income:               r = -0.007, p = 0.761
#>   age x life_satisfaction:    r = -0.029, p = 0.158
#>   income x life_satisfaction: r = 0.448, p < 0.001 ***
#>   1/3 pairs significant (p < .05), N = 2115-2421
#> Use summary() for detailed output.

# Weighted correlations
survey_data %>%
  pearson_cor(age, income, weights = sampling_weight)
#> Pearson Correlation: age x income [Weighted]
#>   r = -0.005, p = 0.828, N = 2201
#> Use summary() for detailed output.

# Grouped correlations
survey_data %>%
  group_by(region) %>%
  pearson_cor(age, income, life_satisfaction)
#> [region = East]
#> Pearson Correlation: 3 variables
#>   age x income:               r = 0.039, p = 0.415
#>   age x life_satisfaction:    r = -0.043, p = 0.350
#>   income x life_satisfaction: r = 0.448, p < 0.001 ***
#>   1/3 pairs significant (p < .05), N = 410-465
#> [region = West]
#> Pearson Correlation: 3 variables
#>   age x income:               r = -0.018, p = 0.462
#>   age x life_satisfaction:    r = -0.025, p = 0.274
#>   income x life_satisfaction: r = 0.449, p < 0.001 ***
#>   1/3 pairs significant (p < .05), N = 1705-1956
#> Use summary() for detailed output.

# Using tidyselect helpers
survey_data %>%
  pearson_cor(where(is.numeric), weights = sampling_weight)
#> Pearson Correlation: 10 variables [Weighted]
#>   political_orientation x environmental_concern: r = -0.584, p < 0.001 ***
#>   income x life_satisfaction:                    r = 0.450, p < 0.001 ***
#>   environmental_concern x trust_government:      r = 0.064, p = 0.002 **
#>   political_orientation x trust_government:      r = -0.057, p = 0.008 **
#>   (Shown: the 4 strongest significant pairs; summary() shows all 45)
#>   4/45 pairs significant (p < .05), N = 2020-2516
#> Use summary() for detailed output.

# Listwise deletion for missing data
survey_data %>%
  pearson_cor(age, income, use = "listwise")
#> Pearson Correlation: age x income
#>   r = -0.007, p = 0.761, N = 2186
#> Use summary() for detailed output.

# --- Three-layer output ---
result <- survey_data %>%
  pearson_cor(age, income, life_satisfaction, weights = sampling_weight)
result              # compact one-line overview
#> Pearson Correlation: 3 variables [Weighted]
#>   age x income:               r = -0.005, p = 0.828
#>   age x life_satisfaction:    r = -0.029, p = 0.150
#>   income x life_satisfaction: r = 0.450, p < 0.001 ***
#>   1/3 pairs significant (p < .05), N = 2130-2437
#> Use summary() for detailed output.
summary(result)     # full correlation, p-value, and N matrices
#> 
#> Weighted Pearson Correlation
#> ----------------------------
#> 
#> - Weights variable: sampling_weight
#> - Missing data handling: pairwise deletion
#> - Confidence level: 95.0%
#> - Alternative hypothesis: two.sided
#> 
#> 
#> Correlation Matrix:
#> -------------------
#>                       age     income     life_satisfaction   
#> age                     1     -0.005                -0.029   
#> income             -0.005          1                 0.450***
#> life_satisfaction  -0.029      0.450***                  1   
#> -------------------
#> 
#> Significance Matrix (p-values, 2-tailed):
#> -----------------------------------------
#>                     age  income  life_satisfaction
#> age                        .828               .150
#> income             .828                      <.001
#> life_satisfaction  .150   <.001                   
#> -----------------------------------------
#> 
#> Sample Size Matrix:
#> -------------------
#>                     age  income  life_satisfaction
#> age                2516    2201               2437
#> income             2201    2201               2130
#> life_satisfaction  2437    2130               2437
#> -------------------
#> 
#> Pairwise Results:
#>   ---------------------------------------------------------------------
#>   Pair                             r      p           95% CI     N     
#>   ---------------------------------------------------------------------
#>   age x income                -0.005   .828  [-0.046, 0.037]  2201     
#>   age x life_satisfaction     -0.029   .150  [-0.069, 0.011]  2437     
#>   income x life_satisfaction   0.450  <.001   [0.416, 0.483]  2130  ***
#>   ---------------------------------------------------------------------
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
summary(result, pvalue_matrix = FALSE)  # hide p-values
#> 
#> Weighted Pearson Correlation
#> ----------------------------
#> 
#> - Weights variable: sampling_weight
#> - Missing data handling: pairwise deletion
#> - Confidence level: 95.0%
#> - Alternative hypothesis: two.sided
#> 
#> 
#> Correlation Matrix:
#> -------------------
#>                       age     income     life_satisfaction   
#> age                     1     -0.005                -0.029   
#> income             -0.005          1                 0.450***
#> life_satisfaction  -0.029      0.450***                  1   
#> -------------------
#> 
#> Sample Size Matrix:
#> -------------------
#>                     age  income  life_satisfaction
#> age                2516    2201               2437
#> income             2201    2201               2130
#> life_satisfaction  2437    2130               2437
#> -------------------
#> 
#> Pairwise Results:
#>   ---------------------------------------------------------------------
#>   Pair                             r      p           95% CI     N     
#>   ---------------------------------------------------------------------
#>   age x income                -0.005   .828  [-0.046, 0.037]  2201     
#>   age x life_satisfaction     -0.029   .150  [-0.069, 0.011]  2437     
#>   income x life_satisfaction   0.450  <.001   [0.416, 0.483]  2130  ***
#>   ---------------------------------------------------------------------
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
