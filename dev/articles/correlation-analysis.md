# Understanding Relationships: Correlation Analysis

``` r
library(mariposa)
library(dplyr)
data(survey_data)
```

## Overview

Correlation measures how two variables move together. When one goes up,
does the other tend to go up (positive) or down (negative)?

mariposa provides three correlation methods for different situations:

| Method            | Function                                                                                  | Best for                                                     |
|-------------------|-------------------------------------------------------------------------------------------|--------------------------------------------------------------|
| Pearson’s *r*     | [`pearson_cor()`](https://YannickDiehl.github.io/mariposa/dev/reference/pearson_cor.md)   | Linear relationships between continuous variables            |
| Spearman’s $\rho$ | [`spearman_rho()`](https://YannickDiehl.github.io/mariposa/dev/reference/spearman_rho.md) | Monotonic relationships, ordinal data, or data with outliers |
| Kendall’s $\tau$  | [`kendall_tau()`](https://YannickDiehl.github.io/mariposa/dev/reference/kendall_tau.md)   | Ordinal data, small samples, or many tied values             |

## Pearson Correlation

### Basic Usage

Measure the linear correlation between age and income:

``` r
survey_data %>%
  pearson_cor(age, income)
#> Pearson Correlation: age x income
#>   r = -0.007, p = 0.761 , N = 2186
```

Interpretation of *r*:

- $r = 1$: Perfect positive correlation
- $r = 0$: No linear relationship
- $r = - 1$: Perfect negative correlation

### With Survey Weights

Get population-representative correlations:

``` r
survey_data %>%
  pearson_cor(age, income, weights = sampling_weight)
#> Pearson Correlation: age x income [Weighted]
#>   r = -0.005, p = 0.828 , N = 2201
```

### Multiple Variables

Create a correlation matrix for several variables at once:

``` r
survey_data %>%
  pearson_cor(trust_government, trust_media, trust_science,
              weights = sampling_weight)
#> Pearson Correlation: 3 variables [Weighted]
#>   trust_government x trust_media: r = 0.012, p = 0.582  
#>   trust_government x trust_science: r = 0.031, p = 0.145  
#>   trust_media x trust_science:   r = 0.024, p = 0.259  
#>   0/3 pairs significant (p < .05), N = 2242
```

### Grouped Analysis

Calculate correlations within subgroups:

``` r
survey_data %>%
  group_by(region) %>%
  pearson_cor(age, income, weights = sampling_weight)
#> [region = 1]
#> Pearson Correlation: age x income [Weighted]
#>   r = 0.050, p = 0.293 , N = 449
#> [region = 2]
#> Pearson Correlation: age x income [Weighted]
#>   r = -0.019, p = 0.427 , N = 1751
```

### Confidence Intervals

Specify the confidence level for the interval around the estimate:

``` r
survey_data %>%
  pearson_cor(age, income,
              conf.level = 0.95,
              weights = sampling_weight)
#> Pearson Correlation: age x income [Weighted]
#>   r = -0.005, p = 0.828 , N = 2201
```

## Spearman Correlation

### When to Use

Use Spearman’s $\rho$ when:

- The relationship is **monotonic** but not necessarily linear
- Your data contains **outliers** that might distort Pearson’s *r*
- Variables are **ordinal** (e.g., Likert scales, rankings)

Spearman’s correlation works on the ranks of the data rather than the
raw values.

### Basic Usage

``` r
survey_data %>%
  spearman_rho(political_orientation, environmental_concern)
#> Spearman Correlation: political_orientation x environmental_concern
#>   rho = -0.576, p < 0.001 ***, N = 2207
```

### With Weights

``` r
survey_data %>%
  spearman_rho(political_orientation, environmental_concern,
               weights = sampling_weight)
#> Spearman Correlation: political_orientation x environmental_concern [Weighted]
#>   rho = -0.576, p < 0.001 ***, N = 2207
```

### Multiple Variables

``` r
survey_data %>%
  spearman_rho(political_orientation, environmental_concern,
               life_satisfaction, trust_government,
               weights = sampling_weight)
#> Spearman Correlation: 4 variables [Weighted]
#>   political_orientation x environmental_concern: rho = -0.576, p < 0.001 *** 
#>   political_orientation x life_satisfaction: rho = -0.004, p = 0.833  
#>   political_orientation x trust_government: rho = -0.055, p = 0.011 * 
#>   environmental_concern x life_satisfaction: rho = 0.003, p = 0.904  
#>   environmental_concern x trust_government: rho = 0.067, p = 0.002 ** 
#>   life_satisfaction x trust_government: rho = 0.002, p = 0.942  
#>   3/6 pairs significant (p < .05), N = 2207
```

## Kendall’s Tau

### When to Use

Use Kendall’s $\tau$ when:

- Data is **ordinal**
- The sample size is **small** ($n < 30$)
- There are **many tied values**

Kendall’s $\tau$ is typically smaller in magnitude than Spearman’s
$\rho$, but is considered more robust.

### Basic Usage

``` r
survey_data %>%
  kendall_tau(political_orientation, life_satisfaction)
#> Kendall's Tau: political_orientation x life_satisfaction
#>   tau = -0.004, p = 0.832 , N = 2228
```

### With Weights

``` r
survey_data %>%
  kendall_tau(political_orientation, life_satisfaction,
              weights = sampling_weight)
#> Kendall's Tau: political_orientation x life_satisfaction [Weighted]
#>   tau = -0.004, p = 0.766 , N = 2241
```

## Interpreting Correlations

### Strength Guidelines

| Absolute value of *r* | Interpretation |
|-----------------------|----------------|
| 0.0 – 0.3             | Weak           |
| 0.3 – 0.7             | Moderate       |
| 0.7 – 1.0             | Strong         |

These are rough guidelines. Context matters — in some fields, $r = 0.3$
is considered a strong finding.

### Statistical Significance

Check the p-value alongside the correlation coefficient:

``` r
result <- survey_data %>%
  pearson_cor(age, income, weights = sampling_weight)
result             # compact overview
#> Pearson Correlation: age x income [Weighted]
#>   r = -0.005, p = 0.828 , N = 2201
summary(result)    # detailed output with confidence intervals
#> 
#> Weighted Pearson Correlation 
#> -----------------------------
#> 
#> - Weights variable: sampling_weight
#> - Missing data handling: pairwise deletion
#> - Confidence level: 95.0%
#> 
#> 
#>   Correlation: r = -0.005
#>   p-value: p = 0.828 
#>   N = 2201
#>   95% CI: [-0.046, 0.037]
#>   r-squared: 0.000
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```

A significant p-value ($p < 0.05$) means the correlation is unlikely to
be zero in the population. But with large samples, even very small
correlations can be significant — always consider the magnitude of *r*.

### Correlation Does Not Imply Causation

This is the most important caveat in correlation analysis:

- A **third variable** might explain the relationship
- The **direction** of causality is unknown
- There may be **no causal link** at all

For example, ice cream sales and drowning rates are correlated — but
both are driven by warm weather, not by each other.

## Choosing the Right Method

1.  **Are both variables continuous?**
    - Yes, and relationship is linear → **Pearson**
    - Yes, but relationship is non-linear (monotonic) → **Spearman**
2.  **Are variables ordinal or ranked?**
    - Yes → **Spearman** or **Kendall**
    - Many tied values or small sample → prefer **Kendall**

### Comparing All Three Methods

``` r
pearson_result <- survey_data %>%
  pearson_cor(life_satisfaction, income, weights = sampling_weight)

spearman_result <- survey_data %>%
  spearman_rho(life_satisfaction, income, weights = sampling_weight)

kendall_result <- survey_data %>%
  kendall_tau(life_satisfaction, income, weights = sampling_weight)

comparison <- data.frame(
  Method = c("Pearson", "Spearman", "Kendall"),
  Correlation = c(pearson_result$correlations$correlation[1],
                  spearman_result$correlations$correlation[1],
                  kendall_result$correlations$correlation[1]),
  P_Value = c(pearson_result$correlations$p_value[1],
              spearman_result$correlations$p_value[1],
              kendall_result$correlations$p_value[1])
)
print(comparison)
#>     Method Correlation       P_Value
#> 1  Pearson   0.4501535 8.997507e-107
#> 2 Spearman   0.4501535 2.322944e-113
#> 3  Kendall   0.4501535 5.179415e-130
```

If Pearson and Spearman give very different results, the relationship
may be non-linear.

## Common Patterns in Survey Data

### Age and Attitudes

``` r
survey_data %>%
  pearson_cor(age, political_orientation, environmental_concern,
              life_satisfaction, trust_government,
              weights = sampling_weight)
#> Pearson Correlation: 5 variables [Weighted]
#>   age x political_orientation:   r = -0.029, p = 0.168  
#>   age x environmental_concern:   r = 0.024, p = 0.244  
#>   age x life_satisfaction:       r = -0.029, p = 0.150  
#>   age x trust_government:        r = 0.005, p = 0.804  
#>   political_orientation x environmental_concern: r = -0.584, p < 0.001 *** 
#>   political_orientation x life_satisfaction: r = -0.004, p = 0.836  
#>   political_orientation x trust_government: r = -0.057, p = 0.008 ** 
#>   environmental_concern x life_satisfaction: r = -0.003, p = 0.866  
#>   environmental_concern x trust_government: r = 0.064, p = 0.002 ** 
#>   life_satisfaction x trust_government: r = 0.011, p = 0.604  
#>   3/10 pairs significant (p < .05), N = 2312
```

### Income Effects

``` r
survey_data %>%
  pearson_cor(income, life_satisfaction, trust_government,
              trust_media, trust_science,
              weights = sampling_weight)
#> Pearson Correlation: 5 variables [Weighted]
#>   income x life_satisfaction:    r = 0.450, p < 0.001 *** 
#>   income x trust_government:     r = -0.001, p = 0.975  
#>   income x trust_media:          r = -0.011, p = 0.629  
#>   income x trust_science:        r = -0.024, p = 0.270  
#>   life_satisfaction x trust_government: r = 0.011, p = 0.604  
#>   life_satisfaction x trust_media: r = 0.020, p = 0.330  
#>   life_satisfaction x trust_science: r = -0.019, p = 0.371  
#>   trust_government x trust_media: r = 0.012, p = 0.582  
#>   trust_government x trust_science: r = 0.031, p = 0.145  
#>   trust_media x trust_science:   r = 0.024, p = 0.259  
#>   1/10 pairs significant (p < .05), N = 2130
```

## Complete Example

A comprehensive correlation analysis workflow:

``` r
# 1. Correlation matrix
cor_matrix <- survey_data %>%
  pearson_cor(age, income, life_satisfaction,
              political_orientation, environmental_concern,
              weights = sampling_weight)
print(cor_matrix)
#> Pearson Correlation: 5 variables [Weighted]
#>   age x income:                  r = -0.005, p = 0.828  
#>   age x life_satisfaction:       r = -0.029, p = 0.150  
#>   age x political_orientation:   r = -0.029, p = 0.168  
#>   age x environmental_concern:   r = 0.024, p = 0.244  
#>   income x life_satisfaction:    r = 0.450, p < 0.001 *** 
#>   income x political_orientation: r = -0.034, p = 0.125  
#>   income x environmental_concern: r = 0.015, p = 0.503  
#>   life_satisfaction x political_orientation: r = -0.004, p = 0.836  
#>   life_satisfaction x environmental_concern: r = -0.003, p = 0.866  
#>   political_orientation x environmental_concern: r = -0.584, p < 0.001 *** 
#>   2/10 pairs significant (p < .05), N = 2201

# 2. Focus on significant correlations
significant_cors <- cor_matrix$correlations %>%
  filter(p_value < 0.05) %>%
  arrange(desc(abs(correlation)))
print(significant_cors)
#>                    var1                  var2 correlation       p_value
#> 1 political_orientation environmental_concern  -0.5843752 1.558234e-203
#> 2                income     life_satisfaction   0.4501535 8.997507e-107
#>   conf_int_lower conf_int_upper    n sig r_squared
#> 1     -0.6111168     -0.5563011 2221 *** 0.3414944
#> 2      0.4156264      0.4833852 2130 *** 0.2026382

# 3. Regional differences in key relationship
survey_data %>%
  group_by(region) %>%
  pearson_cor(age, income, weights = sampling_weight)
#> [region = 1]
#> Pearson Correlation: age x income [Weighted]
#>   r = 0.050, p = 0.293 , N = 449
#> [region = 2]
#> Pearson Correlation: age x income [Weighted]
#>   r = -0.019, p = 0.427 , N = 1751
```

## Reporting Results

When writing up correlation results, include:

- The correlation coefficient (*r*, $\rho$, or $\tau$)
- The confidence interval
- The p-value
- The sample size or effective *N*
- Whether weights were used

**Example (APA style):** “There was a moderate positive correlation
between age and income (*r* = .34, 95% CI \[.29, .39\], *p* \< .001,
*N*_(eff) = 2,341), indicating that older respondents tended to report
higher incomes.”

## Summary

1.  **Pearson** for linear relationships between continuous variables
2.  **Spearman** for monotonic relationships, outliers, or ordinal data
3.  **Kendall** for ordinal data, small samples, or many ties
4.  Always report both the **magnitude** and **significance** of
    correlations
5.  Correlation does **not** imply causation

## Next Steps

- Compare groups statistically — see
  [`vignette("hypothesis-testing")`](https://YannickDiehl.github.io/mariposa/dev/articles/hypothesis-testing.md)
- Learn about weighted analysis — see
  [`vignette("survey-weights")`](https://YannickDiehl.github.io/mariposa/dev/articles/survey-weights.md)
- Start with descriptive exploration — see
  [`vignette("descriptive-statistics")`](https://YannickDiehl.github.io/mariposa/dev/articles/descriptive-statistics.md)
