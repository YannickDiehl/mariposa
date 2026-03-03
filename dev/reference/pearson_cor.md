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

  Optional survey weights for population-representative results

- conf.level:

  Confidence level for intervals (Default: 0.95 = 95%)

- alternative:

  Direction of the test: `"two.sided"` (default), `"less"`, or
  `"greater"`.

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

[`spearman_rho`](https://YannickDiehl.github.io/mariposa/dev/reference/spearman_rho.md)
for rank-based correlation (robust to outliers).

[`kendall_tau`](https://YannickDiehl.github.io/mariposa/dev/reference/kendall_tau.md)
for ordinal correlation.

[`summary.pearson_cor`](https://YannickDiehl.github.io/mariposa/dev/reference/summary.pearson_cor.md)
for detailed output with toggleable sections.

Other correlation:
[`kendall_tau()`](https://YannickDiehl.github.io/mariposa/dev/reference/kendall_tau.md),
[`spearman_rho()`](https://YannickDiehl.github.io/mariposa/dev/reference/spearman_rho.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic correlation between two variables
survey_data %>% 
  pearson_cor(age, income)
#> Pearson Correlation: age x income
#>   r = -0.007, p = 0.761 , N = 2186

# Correlation matrix for multiple variables
survey_data %>% 
  pearson_cor(age, income, life_satisfaction)
#> Pearson Correlation: 3 variables
#>   age x income:                  r = -0.007, p = 0.761  
#>   age x life_satisfaction:       r = -0.029, p = 0.158  
#>   income x life_satisfaction:    r = 0.448, p < 0.001 *** 
#>   1/3 pairs significant (p < .05), N = 2186

# Weighted correlations
survey_data %>% 
  pearson_cor(age, income, weights = sampling_weight)
#> Pearson Correlation: age x income [Weighted]
#>   r = -0.005, p = 0.828 , N = 2201

# Grouped correlations
survey_data %>% 
  group_by(region) %>% 
  pearson_cor(age, income, life_satisfaction)
#> [region = 1]
#> Pearson Correlation: 3 variables
#>   age x income:                  r = 0.039, p = 0.415  
#>   age x life_satisfaction:       r = -0.043, p = 0.350  
#>   income x life_satisfaction:    r = 0.448, p < 0.001 *** 
#>   1/3 pairs significant (p < .05), N = 429
#> [region = 2]
#> Pearson Correlation: 3 variables
#>   age x income:                  r = -0.018, p = 0.462  
#>   age x life_satisfaction:       r = -0.025, p = 0.274  
#>   income x life_satisfaction:    r = 0.449, p < 0.001 *** 
#>   1/3 pairs significant (p < .05), N = 1757

# Using tidyselect helpers
survey_data %>% 
  pearson_cor(where(is.numeric), weights = sampling_weight)
#> Pearson Correlation: 10 variables [Weighted]
#>   id x age:                      r = 0.009, p = 0.648  
#>   id x income:                   r = 0.027, p = 0.206  
#>   id x political_orientation:    r = -0.023, p = 0.271  
#>   id x environmental_concern:    r = -0.006, p = 0.763  
#>   id x life_satisfaction:        r = 0.011, p = 0.587  
#>   id x trust_government:         r = 0.021, p = 0.310  
#>   id x trust_media:              r = -0.008, p = 0.714  
#>   id x trust_science:            r = 0.016, p = 0.439  
#>   id x sampling_weight:          r = -0.016, p = 0.436  
#>   age x income:                  r = -0.005, p = 0.828  
#>   age x political_orientation:   r = -0.029, p = 0.168  
#>   age x environmental_concern:   r = 0.024, p = 0.244  
#>   age x life_satisfaction:       r = -0.029, p = 0.150  
#>   age x trust_government:        r = 0.005, p = 0.804  
#>   age x trust_media:             r = 0.005, p = 0.820  
#>   age x trust_science:           r = 0.003, p = 0.902  
#>   age x sampling_weight:         r = -0.009, p = 0.645  
#>   income x political_orientation: r = -0.034, p = 0.125  
#>   income x environmental_concern: r = 0.015, p = 0.503  
#>   income x life_satisfaction:    r = 0.450, p < 0.001 *** 
#>   income x trust_government:     r = -0.001, p = 0.975  
#>   income x trust_media:          r = -0.011, p = 0.629  
#>   income x trust_science:        r = -0.024, p = 0.270  
#>   income x sampling_weight:      r = -0.040, p = 0.058  
#>   political_orientation x environmental_concern: r = -0.584, p < 0.001 *** 
#>   political_orientation x life_satisfaction: r = -0.004, p = 0.836  
#>   political_orientation x trust_government: r = -0.057, p = 0.008 ** 
#>   political_orientation x trust_media: r = 0.004, p = 0.835  
#>   political_orientation x trust_science: r = 0.040, p = 0.059  
#>   political_orientation x sampling_weight: r = 0.011, p = 0.590  
#>   environmental_concern x life_satisfaction: r = -0.003, p = 0.866  
#>   environmental_concern x trust_government: r = 0.064, p = 0.002 ** 
#>   environmental_concern x trust_media: r = 0.002, p = 0.907  
#>   environmental_concern x trust_science: r = -0.014, p = 0.507  
#>   environmental_concern x sampling_weight: r = 0.020, p = 0.328  
#>   life_satisfaction x trust_government: r = 0.011, p = 0.604  
#>   life_satisfaction x trust_media: r = 0.020, p = 0.330  
#>   life_satisfaction x trust_science: r = -0.019, p = 0.371  
#>   life_satisfaction x sampling_weight: r = -0.019, p = 0.359  
#>   trust_government x trust_media: r = 0.012, p = 0.582  
#>   trust_government x trust_science: r = 0.031, p = 0.145  
#>   trust_government x sampling_weight: r = -0.009, p = 0.679  
#>   trust_media x trust_science:   r = 0.024, p = 0.259  
#>   trust_media x sampling_weight: r = 0.022, p = 0.281  
#>   trust_science x sampling_weight: r = 0.001, p = 0.961  
#>   4/45 pairs significant (p < .05), N = 2516

# Listwise deletion for missing data
survey_data %>% 
  pearson_cor(age, income, use = "listwise")
#> Pearson Correlation: age x income
#>   r = -0.007, p = 0.761 , N = 2186

# --- Three-layer output ---
result <- survey_data %>%
  pearson_cor(age, income, life_satisfaction, weights = sampling_weight)
result              # compact one-line overview
#> Pearson Correlation: 3 variables [Weighted]
#>   age x income:                  r = -0.005, p = 0.828  
#>   age x life_satisfaction:       r = -0.029, p = 0.150  
#>   income x life_satisfaction:    r = 0.450, p < 0.001 *** 
#>   1/3 pairs significant (p < .05), N = 2201
summary(result)     # full correlation, p-value, and N matrices
#> 
#> Weighted Pearson Correlation 
#> -----------------------------
#> 
#> - Weights variable: sampling_weight
#> - Missing data handling: pairwise deletion
#> - Confidence level: 95.0%
#> 
#> 
#> Correlation Matrix:
#> ------------------- 
#>                       age  income life_satisfaction
#> age                 1.000  -0.005            -0.029
#> income             -0.005   1.000             0.450
#> life_satisfaction  -0.029   0.450             1.000
#> ------------------- 
#> 
#> Significance Matrix (p-values):
#> ------------------------------- 
#>                       age  income life_satisfaction
#> age                0.0000  0.8276            0.1496
#> income             0.8276  0.0000            0.0000
#> life_satisfaction  0.1496  0.0000            0.0000
#> ------------------------------- 
#> 
#> Sample Size Matrix:
#> ------------------- 
#>                     age income life_satisfaction
#> age                2516   2201              2437
#> income             2201   2201              2130
#> life_satisfaction  2437   2130              2437
#> ------------------- 
#> 
#> Pairwise Results:
#> ---------------- 
#>               Variable_Pair      r r_squared p_value           CI_95    n sig
#>                age × income -0.005     0.000  0.8276 [-0.046, 0.037] 2201    
#>     age × life_satisfaction -0.029     0.001  0.1496 [-0.069, 0.011] 2437    
#>  income × life_satisfaction  0.450     0.203  0.0000  [0.416, 0.483] 2130 ***
#> ---------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
summary(result, pvalue_matrix = FALSE)  # hide p-values
#> 
#> Weighted Pearson Correlation 
#> -----------------------------
#> 
#> - Weights variable: sampling_weight
#> - Missing data handling: pairwise deletion
#> - Confidence level: 95.0%
#> 
#> 
#> Correlation Matrix:
#> ------------------- 
#>                       age  income life_satisfaction
#> age                 1.000  -0.005            -0.029
#> income             -0.005   1.000             0.450
#> life_satisfaction  -0.029   0.450             1.000
#> ------------------- 
#> 
#> Sample Size Matrix:
#> ------------------- 
#>                     age income life_satisfaction
#> age                2516   2201              2437
#> income             2201   2201              2130
#> life_satisfaction  2437   2130              2437
#> ------------------- 
#> 
#> Pairwise Results:
#> ---------------- 
#>               Variable_Pair      r r_squared p_value           CI_95    n sig
#>                age × income -0.005     0.000  0.8276 [-0.046, 0.037] 2201    
#>     age × life_satisfaction -0.029     0.001  0.1496 [-0.069, 0.011] 2437    
#>  income × life_satisfaction  0.450     0.203  0.0000  [0.416, 0.483] 2130 ***
#> ---------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```
