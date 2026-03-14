# Understanding Relationships: Correlation Analysis

``` r
library(mariposa)
library(dplyr)
data(survey_data)
```

## Overview

Correlation measures how two variables move together. mariposa provides
three methods for different situations:

| Method            | Function                                                                              | Best for                                                     |
|-------------------|---------------------------------------------------------------------------------------|--------------------------------------------------------------|
| Pearson’s *r*     | [`pearson_cor()`](https://YannickDiehl.github.io/mariposa/reference/pearson_cor.md)   | Linear relationships between continuous variables            |
| Spearman’s $\rho$ | [`spearman_rho()`](https://YannickDiehl.github.io/mariposa/reference/spearman_rho.md) | Monotonic relationships, ordinal data, or data with outliers |
| Kendall’s $\tau$  | [`kendall_tau()`](https://YannickDiehl.github.io/mariposa/reference/kendall_tau.md)   | Ordinal data, small samples, or many tied values             |

All three support survey weights, multiple variables (correlation
matrices), and grouped analysis.

## Pearson Correlation

### Basic Usage

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

``` r
survey_data %>%
  pearson_cor(age, income, weights = sampling_weight)
#> Pearson Correlation: age x income [Weighted]
#>   r = -0.005, p = 0.828 , N = 2201
```

### Correlation Matrix

Pass multiple variables to get all pairwise correlations:

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

### Detailed Output

``` r
result <- survey_data %>%
  pearson_cor(age, income, weights = sampling_weight)

summary(result)
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

### Grouped Analysis

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

## Spearman Correlation

Use Spearman’s $\rho$ when the relationship is monotonic but not
necessarily linear, or when working with ordinal data or data with
outliers:

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

Use Kendall’s $\tau$ for ordinal data, small samples ($n < 30$), or data
with many tied values. It is more robust than Spearman but typically
produces smaller absolute values:

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

These are general guidelines. In survey research, correlations of
0.2–0.4 between different constructs are common and often substantively
meaningful.

### Choosing the Right Method

1.  **Both variables continuous, relationship is linear** → Pearson
2.  **Monotonic but non-linear relationship, or outliers present** →
    Spearman
3.  **Ordinal data, small sample, or many ties** → Kendall

### Comparing Methods

If Pearson and Spearman give very different results, the relationship
may be non-linear:

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

### Correlation Does Not Imply Causation

A significant correlation does not mean one variable causes the other:

- A **third variable** may drive both (e.g., warm weather → both ice
  cream sales and drowning)
- The **direction of causality** is unknown
- There may be **no causal link** at all

## Complete Example

``` r
# 1. Correlation matrix for key variables
cor_result <- survey_data %>%
  pearson_cor(age, income, life_satisfaction,
              political_orientation, environmental_concern,
              weights = sampling_weight)
cor_result
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

# 2. Identify the strongest correlations
significant <- cor_result$correlations %>%
  filter(p_value < 0.05) %>%
  arrange(desc(abs(correlation)))
print(significant)
#>                    var1                  var2 correlation       p_value
#> 1 political_orientation environmental_concern  -0.5843752 1.558234e-203
#> 2                income     life_satisfaction   0.4501535 8.997507e-107
#>   conf_int_lower conf_int_upper    n sig r_squared
#> 1     -0.6111168     -0.5563011 2221 *** 0.3414944
#> 2      0.4156264      0.4833852 2130 *** 0.2026382

# 3. Check if relationships vary by region
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

## Reporting Results (APA Style)

Include the correlation coefficient, confidence interval, p-value, and
sample size:

> “There was a moderate positive correlation between age and income (*r*
> = .34, 95% CI \[.29, .39\], *p* \< .001, *N*_(eff) = 2,341),
> indicating that older respondents tended to report higher incomes.”

## Practical Tips

1.  **Always check the scatterplot.** Correlations can miss non-linear
    relationships, and a single outlier can inflate or deflate the
    coefficient.

2.  **Use Spearman when in doubt.** It makes fewer assumptions than
    Pearson and works with both continuous and ordinal data.

3.  **Report the magnitude, not just significance.** With large samples,
    even $r = .05$ can be significant but is practically meaningless.

4.  **Use correlation matrices to prioritize.** Before regression, check
    which variables are most strongly related to your outcome.

## Summary

1.  **Pearson** measures linear relationships between continuous
    variables
2.  **Spearman** measures monotonic relationships and works with ordinal
    data
3.  **Kendall** is the most robust choice for ordinal data and small
    samples
4.  All three support **weights**, **correlation matrices**, and
    **group_by()**
5.  Correlation does **not** imply causation

## Next Steps

- Build predictive models — see
  [`vignette("regression-analysis")`](https://YannickDiehl.github.io/mariposa/articles/regression-analysis.md)
- Compare groups — see
  [`vignette("hypothesis-testing")`](https://YannickDiehl.github.io/mariposa/articles/hypothesis-testing.md)
- Construct reliable scales — see
  [`vignette("scale-analysis")`](https://YannickDiehl.github.io/mariposa/articles/scale-analysis.md)
