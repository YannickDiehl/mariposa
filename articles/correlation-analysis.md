# Understanding Relationships: Correlation Analysis

``` r
library(mariposa)
#> 
#> Attaching package: 'mariposa'
#> The following object is masked from 'package:stats':
#> 
#>     frequency
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
data(survey_data)
```

## What is Correlation?

Correlation measures how two variables are related. When one goes up,
does the other tend to go up (positive correlation) or down (negative
correlation)?

mariposa provides three correlation methods: - **Pearson**: For linear
relationships between continuous variables - **Spearman**: For monotonic
relationships (doesn’t have to be linear) - **Kendall**: For ordinal
data or when you have many ties

## Pearson Correlation

### Basic Usage

Measure linear correlation between age and income:

``` r
survey_data %>%
  pearson_cor(age, income)
#> ── Pearson Correlation  ────────────────────────────────────────────────────────
#> • Missing data handling: pairwise deletion
#> • Confidence level: 95.0%
#> 
#> 
#> --- age × income ---
#> 
#>   Correlation: r = -0.007
#>   Effect size: r² = 0.000
#>   Sample size: n = 2186
#>   95% CI: [-0.048, 0.035]
#>   p-value: 0.7608
#>   Significance: ns
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Correlation Strength Interpretation:
#>   |r| < 0.30:        Weak correlation
#>   0.30 ≤ |r| < 0.70: Moderate correlation
#>   |r| ≥ 0.70:        Strong correlation
#> 
#> r² represents the proportion of variance explained
```

Interpretation: - **r = 1**: Perfect positive correlation - **r = 0**:
No linear relationship - **r = -1**: Perfect negative correlation

### With Survey Weights

Get population-representative correlations:

``` r
survey_data %>%
  pearson_cor(age, income, weights = sampling_weight)
#> ── Weighted Pearson Correlation  ───────────────────────────────────────────────
#> • Weights variable: sampling_weight
#> • Missing data handling: pairwise deletion
#> • Confidence level: 95.0%
#> 
#> 
#> --- age × income ---
#> 
#>   Correlation: r = -0.005
#>   Effect size: r² = 0.000
#>   Sample size: n = 2201
#>   95% CI: [-0.046, 0.037]
#>   p-value: 0.8276
#>   Significance: ns
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Correlation Strength Interpretation:
#>   |r| < 0.30:        Weak correlation
#>   0.30 ≤ |r| < 0.70: Moderate correlation
#>   |r| ≥ 0.70:        Strong correlation
#> 
#> r² represents the proportion of variance explained
```

### Multiple Variables

Create a correlation matrix:

``` r
# Correlate all trust variables
survey_data %>%
  pearson_cor(trust_government, trust_media, trust_science,
              weights = sampling_weight)
#> ── Weighted Pearson Correlation  ───────────────────────────────────────────────
#> • Weights variable: sampling_weight
#> • Missing data handling: pairwise deletion
#> • Confidence level: 95.0%
#> 
#> 
#> Correlation Matrix:
#> ------------------- 
#>                  trust_government trust_media trust_science
#> trust_government            1.000       0.012         0.031
#> trust_media                 0.012       1.000         0.024
#> trust_science               0.031       0.024         1.000
#> ------------------- 
#> 
#> Significance Matrix (p-values):
#> ------------------------------- 
#>                  trust_government trust_media trust_science
#> trust_government           0.0000      0.5823        0.1452
#> trust_media                0.5823      0.0000        0.2586
#> trust_science              0.1452      0.2586        0.0000
#> ------------------------------- 
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
#> ---------------- 
#>                     Variable_Pair     r r_squared p_value           CI_95    n
#>    trust_government × trust_media 0.012     0.000  0.5823 [-0.030, 0.053] 2242
#>  trust_government × trust_science 0.031     0.001  0.1452 [-0.011, 0.072] 2271
#>       trust_media × trust_science 0.024     0.001  0.2586 [-0.017, 0.065] 2286
#>  sig
#>     
#>     
#>     
#> ---------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Correlation Strength Interpretation:
#>   |r| < 0.30:        Weak correlation
#>   0.30 ≤ |r| < 0.70: Moderate correlation
#>   |r| ≥ 0.70:        Strong correlation
#> 
#> r² represents the proportion of variance explained
```

### Grouped Analysis

Calculate correlations within groups:

``` r
survey_data %>%
  group_by(region) %>%
  pearson_cor(age, income, weights = sampling_weight)
#> ── Weighted Pearson Correlation  ───────────────────────────────────────────────
#> • Weights variable: sampling_weight
#> • Missing data handling: pairwise deletion
#> • Confidence level: 95.0%
#> 
#> 
#> Group: region = East
#> 
#> --- age × income ---
#> 
#>   Correlation: r = 0.050
#>   Effect size: r-squared = 0.002
#>   Sample size: n = 449
#>   95% CI: [-0.043, 0.142]
#>   p-value: 0.2931
#>   Significance: ns
#> 
#> Group: region = West
#> 
#> --- age × income ---
#> 
#>   Correlation: r = -0.019
#>   Effect size: r-squared = 0.000
#>   Sample size: n = 1751
#>   95% CI: [-0.066, 0.028]
#>   p-value: 0.4267
#>   Significance: ns
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Correlation Strength Interpretation:
#>   |r| < 0.30:        Weak correlation
#>   0.30 ≤ |r| < 0.70: Moderate correlation
#>   |r| ≥ 0.70:        Strong correlation
#> 
#> r² represents the proportion of variance explained
```

### With Confidence Intervals

Get confidence intervals for correlations:

``` r
survey_data %>%
  pearson_cor(age, income,
              conf.level = 0.95,
              weights = sampling_weight)
#> ── Weighted Pearson Correlation  ───────────────────────────────────────────────
#> • Weights variable: sampling_weight
#> • Missing data handling: pairwise deletion
#> • Confidence level: 95.0%
#> 
#> 
#> --- age × income ---
#> 
#>   Correlation: r = -0.005
#>   Effect size: r² = 0.000
#>   Sample size: n = 2201
#>   95% CI: [-0.046, 0.037]
#>   p-value: 0.8276
#>   Significance: ns
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Correlation Strength Interpretation:
#>   |r| < 0.30:        Weak correlation
#>   0.30 ≤ |r| < 0.70: Moderate correlation
#>   |r| ≥ 0.70:        Strong correlation
#> 
#> r² represents the proportion of variance explained
```

## Spearman Correlation

### When to Use

Use Spearman when: - Relationship is monotonic but not linear - Data
contains outliers - Variables are ordinal

### Basic Usage

``` r
# Correlation between ordinal variables
survey_data %>%
  spearman_rho(political_orientation, environmental_concern)
#> 
#> Spearman's Rank Correlation Analysis  
#> ══════════════════════════════════════════════════════════════════════
#> Method: Spearman's rho (rank correlation)
#> Variables: political_orientation, environmental_concern
#> Alternative hypothesis: two.sided
#> Missing data handling: pairwise deletion
#> 
#> ┌─ political_orientation × environmental_concern ─┐
#> 
#>   Spearman's rho: ρ = -0.576
#>   Sample size: n = 2207
#>   t-statistic: -33.093
#>   p-value (2-tailed): 0.0000
#>   Significance: ***
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation: moderate negative monotonic relationship
```

### With Weights

``` r
survey_data %>%
  spearman_rho(political_orientation, environmental_concern,
               weights = sampling_weight)
#> 
#> Weighted Spearman's Rank Correlation Analysis  
#> ══════════════════════════════════════════════════════════════════════
#> Method: Spearman's rho (rank correlation)
#> Variables: political_orientation, environmental_concern
#> Weights: sampling_weight
#> Alternative hypothesis: two.sided
#> Missing data handling: pairwise deletion
#> 
#> ┌─ political_orientation × environmental_concern ─┐
#> 
#>   Spearman's rho: ρ = -0.576
#>   Sample size: n = 2207
#>   t-statistic: -33.093
#>   p-value (2-tailed): 0.0000
#>   Significance: ***
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation: moderate negative monotonic relationship
```

### Multiple Variables

``` r
# Create rank correlation matrix
survey_data %>%
  spearman_rho(political_orientation, environmental_concern,
               life_satisfaction, trust_government,
               weights = sampling_weight)
#> 
#> Weighted Spearman's Rank Correlation Analysis  
#> ══════════════════════════════════════════════════════════════════════
#> Method: Spearman's rho (rank correlation)
#> Variables: political_orientation, environmental_concern, life_satisfaction, trust_government
#> Weights: sampling_weight
#> Alternative hypothesis: two.sided
#> Missing data handling: pairwise deletion
#> 
#> 
#> Spearman's Rho Matrix:
#> ─────────────────────────────────────────────────────────────────────
#>               Variable political_orientation environmental_concern
#>  political_orientation                 1.000                -0.576
#>  environmental_concern                -0.576                 1.000
#>      life_satisfaction                -0.004                 0.003
#>       trust_government                -0.055                 0.067
#>  life_satisfaction trust_government
#>             -0.004           -0.055
#>              0.003            0.067
#>              1.000            0.002
#>              0.002            1.000
#> 
#> Significance Matrix (p-values, 2-tailed):
#> ─────────────────────────────────────────────────────────────────────
#>               Variable political_orientation environmental_concern
#>  political_orientation                   -                  0.0000
#>  environmental_concern                0.0000                   -  
#>      life_satisfaction                0.8334                0.9041
#>       trust_government                0.0110                0.0015
#>  life_satisfaction trust_government
#>             0.8334           0.0110
#>             0.9041           0.0015
#>                -             0.9425
#>             0.9425              -  
#> 
#> Sample Size Matrix:
#> ─────────────────────────────────────────────────────────────────────
#>               Variable political_orientation environmental_concern
#>  political_orientation                  2299                  2207
#>  environmental_concern                  2207                  2400
#>      life_satisfaction                  2228                  2324
#>       trust_government                  2168                  2260
#>  life_satisfaction trust_government
#>               2228             2168
#>               2324             2260
#>               2421             2280
#>               2280             2354
#> 
#> Pairwise Results:
#> ─────────────────────────────────────────────────────────────────────
#>                                           Pair    rho       t      p    n sig
#>  political_orientation × environmental_concern -0.576 -33.093 0.0000 2207 ***
#>      political_orientation × life_satisfaction -0.004  -0.210 0.8334 2228    
#>       political_orientation × trust_government -0.055  -2.545 0.0110 2168   *
#>      environmental_concern × life_satisfaction  0.003   0.121 0.9041 2324    
#>       environmental_concern × trust_government  0.067   3.175 0.0015 2260  **
#>           life_satisfaction × trust_government  0.002   0.072 0.9425 2280    
#> ─────────────────────────────────────────────────────────────────────
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```

## Kendall’s Tau

### When to Use

Use Kendall when: - Data is ordinal - Sample size is small - Many tied
values exist

### Basic Usage

``` r
survey_data %>%
  kendall_tau(political_orientation, life_satisfaction)
#> ── Kendall's Tau-b Correlation  ────────────────────────────────────────────────
#> • Missing data handling: pairwise deletion
#> • Alternative hypothesis: two.sided
#> 
#> 
#> ┌─ political_orientation × life_satisfaction ─┐
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
```

### With Weights

``` r
survey_data %>%
  kendall_tau(political_orientation, life_satisfaction,
              weights = sampling_weight)
#> ── Weighted Kendall's Tau-b Correlation  ───────────────────────────────────────
#> • Weights variable: sampling_weight
#> • Missing data handling: pairwise deletion
#> • Alternative hypothesis: two.sided
#> 
#> 
#> ┌─ political_orientation × life_satisfaction ─┐
#> 
#>   Kendall's tau-b: τ = -0.004
#>   Sample size: n = 2241
#>   z-score: -0.298
#>   p-value (2-tailed): 0.7659
#>   Significance: ns
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation: weak negative correlation
```

## Interpreting Correlations

### Strength Guidelines

| Correlation | Interpretation |
|-------------|----------------|
| 0.0 - 0.3   | Weak           |
| 0.3 - 0.7   | Moderate       |
| 0.7 - 1.0   | Strong         |

Remember: These are rough guidelines. Context matters!

### Statistical Significance

Check the p-value: - **p \< 0.05**: Correlation is statistically
significant - **p ≥ 0.05**: Could be due to chance

``` r
# Example with interpretation
result <- survey_data %>%
  pearson_cor(age, income, weights = sampling_weight)

# Check structure and extract values properly
if (!is.null(result) && !is.null(result$correlations) && nrow(result$correlations) > 0) {
  p_value <- result$correlations$p_value[1]
  estimate <- result$correlations$correlation[1]

  if (!is.na(p_value) && p_value < 0.05) {
    cat("Age and income are significantly correlated (r =",
        round(estimate, 3), ", p =", round(p_value, 4), ")\n")
  } else {
    cat("No significant correlation between age and income\n")
  }
}
#> No significant correlation between age and income
```

### Correlation ≠ Causation

Important reminders: - Correlation doesn’t imply causation - Third
variables might explain the relationship - Direction of causality is
unknown

## Visualizing Correlations

### Correlation Matrix

When examining multiple variables:

``` r
# Calculate correlations among multiple variables
cor_matrix <- survey_data %>%
  pearson_cor(age, income, life_satisfaction,
              trust_government, trust_media, trust_science,
              weights = sampling_weight)

print(cor_matrix)
#> ── Weighted Pearson Correlation  ───────────────────────────────────────────────
#> • Weights variable: sampling_weight
#> • Missing data handling: pairwise deletion
#> • Confidence level: 95.0%
#> 
#> 
#> Correlation Matrix:
#> ------------------- 
#>                       age  income life_satisfaction trust_government
#> age                 1.000  -0.005            -0.029            0.005
#> income             -0.005   1.000             0.450           -0.001
#> life_satisfaction  -0.029   0.450             1.000            0.011
#> trust_government    0.005  -0.001             0.011            1.000
#> trust_media         0.005  -0.011             0.020            0.012
#> trust_science       0.003  -0.024            -0.019            0.031
#>                   trust_media trust_science
#> age                     0.005         0.003
#> income                 -0.011        -0.024
#> life_satisfaction       0.020        -0.019
#> trust_government        0.012         0.031
#> trust_media             1.000         0.024
#> trust_science           0.024         1.000
#> ------------------- 
#> 
#> Significance Matrix (p-values):
#> ------------------------------- 
#>                       age  income life_satisfaction trust_government
#> age                0.0000  0.8276            0.1496           0.8037
#> income             0.8276  0.0000            0.0000           0.9755
#> life_satisfaction  0.1496  0.0000            0.0000           0.6045
#> trust_government   0.8037  0.9755            0.6045           0.0000
#> trust_media        0.8202  0.6292            0.3299           0.5823
#> trust_science      0.9021  0.2698            0.3708           0.1452
#>                   trust_media trust_science
#> age                    0.8202        0.9021
#> income                 0.6292        0.2698
#> life_satisfaction      0.3299        0.3708
#> trust_government       0.5823        0.1452
#> trust_media            0.0000        0.2586
#> trust_science          0.2586        0.0000
#> ------------------------------- 
#> 
#> Sample Size Matrix:
#> ------------------- 
#>                     age income life_satisfaction trust_government trust_media
#> age                2516   2201              2437             2371        2382
#> income             2201   2201              2130             2076        2090
#> life_satisfaction  2437   2130              2437             2296        2305
#> trust_government   2371   2076              2296             2371        2242
#> trust_media        2382   2090              2305             2242        2382
#> trust_science      2414   2112              2336             2271        2286
#>                   trust_science
#> age                        2414
#> income                     2112
#> life_satisfaction          2336
#> trust_government           2271
#> trust_media                2286
#> trust_science              2414
#> ------------------- 
#> 
#> Pairwise Results:
#> ---------------- 
#>                         Variable_Pair      r r_squared p_value           CI_95
#>                          age × income -0.005     0.000  0.8276 [-0.046, 0.037]
#>               age × life_satisfaction -0.029     0.001  0.1496 [-0.069, 0.011]
#>                age × trust_government  0.005     0.000  0.8037 [-0.035, 0.045]
#>                     age × trust_media  0.005     0.000  0.8202 [-0.036, 0.045]
#>                   age × trust_science  0.003     0.000  0.9021 [-0.037, 0.042]
#>            income × life_satisfaction  0.450     0.203  0.0000  [0.416, 0.483]
#>             income × trust_government -0.001     0.000  0.9755 [-0.044, 0.042]
#>                  income × trust_media -0.011     0.000  0.6292 [-0.053, 0.032]
#>                income × trust_science -0.024     0.001  0.2698 [-0.067, 0.019]
#>  life_satisfaction × trust_government  0.011     0.000  0.6045 [-0.030, 0.052]
#>       life_satisfaction × trust_media  0.020     0.000  0.3299 [-0.021, 0.061]
#>     life_satisfaction × trust_science -0.019     0.000  0.3708 [-0.059, 0.022]
#>        trust_government × trust_media  0.012     0.000  0.5823 [-0.030, 0.053]
#>      trust_government × trust_science  0.031     0.001  0.1452 [-0.011, 0.072]
#>           trust_media × trust_science  0.024     0.001  0.2586 [-0.017, 0.065]
#>     n sig
#>  2201    
#>  2437    
#>  2371    
#>  2382    
#>  2414    
#>  2130 ***
#>  2076    
#>  2090    
#>  2112    
#>  2296    
#>  2305    
#>  2336    
#>  2242    
#>  2271    
#>  2286    
#> ---------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Correlation Strength Interpretation:
#>   |r| < 0.30:        Weak correlation
#>   0.30 ≤ |r| < 0.70: Moderate correlation
#>   |r| ≥ 0.70:        Strong correlation
#> 
#> r² represents the proportion of variance explained
```

### Grouped Correlations

Compare correlations across groups:

``` r
# How does age-income correlation vary by region?
regional_cors <- survey_data %>%
  group_by(region) %>%
  pearson_cor(age, income, weights = sampling_weight)

print(regional_cors)
#> ── Weighted Pearson Correlation  ───────────────────────────────────────────────
#> • Weights variable: sampling_weight
#> • Missing data handling: pairwise deletion
#> • Confidence level: 95.0%
#> 
#> 
#> Group: region = East
#> 
#> --- age × income ---
#> 
#>   Correlation: r = 0.050
#>   Effect size: r-squared = 0.002
#>   Sample size: n = 449
#>   95% CI: [-0.043, 0.142]
#>   p-value: 0.2931
#>   Significance: ns
#> 
#> Group: region = West
#> 
#> --- age × income ---
#> 
#>   Correlation: r = -0.019
#>   Effect size: r-squared = 0.000
#>   Sample size: n = 1751
#>   95% CI: [-0.066, 0.028]
#>   p-value: 0.4267
#>   Significance: ns
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Correlation Strength Interpretation:
#>   |r| < 0.30:        Weak correlation
#>   0.30 ≤ |r| < 0.70: Moderate correlation
#>   |r| ≥ 0.70:        Strong correlation
#> 
#> r² represents the proportion of variance explained
```

## Choosing the Right Method

### Decision Tree

1.  **Are both variables continuous?**
    - Yes → Consider Pearson
    - No → Go to step 2
2.  **Are variables ordinal or ranked?**
    - Yes → Use Spearman or Kendall
    - No → Correlation may not be appropriate
3.  **Is the relationship linear?**
    - Yes → Use Pearson
    - No, but monotonic → Use Spearman
    - Many ties → Use Kendall

### Comparison Example

Let’s compare all three methods:

``` r
# Pearson correlation
pearson_result <- survey_data %>%
  pearson_cor(life_satisfaction, income, weights = sampling_weight)

# Spearman correlation
spearman_result <- survey_data %>%
  spearman_rho(life_satisfaction, income, weights = sampling_weight)

# Kendall correlation
kendall_result <- survey_data %>%
  kendall_tau(life_satisfaction, income, weights = sampling_weight)

# Compare results
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

## Common Patterns in Survey Data

### Age and Attitudes

``` r
# How age relates to various attitudes
age_correlations <- survey_data %>%
  pearson_cor(age, political_orientation, environmental_concern,
              life_satisfaction, trust_government,
              weights = sampling_weight)

print(age_correlations)
#> ── Weighted Pearson Correlation  ───────────────────────────────────────────────
#> • Weights variable: sampling_weight
#> • Missing data handling: pairwise deletion
#> • Confidence level: 95.0%
#> 
#> 
#> Correlation Matrix:
#> ------------------- 
#>                           age political_orientation environmental_concern
#> age                     1.000                -0.029                 0.024
#> political_orientation  -0.029                 1.000                -0.584
#> environmental_concern   0.024                -0.584                 1.000
#> life_satisfaction      -0.029                -0.004                -0.003
#> trust_government        0.005                -0.057                 0.064
#>                       life_satisfaction trust_government
#> age                              -0.029            0.005
#> political_orientation            -0.004           -0.057
#> environmental_concern            -0.003            0.064
#> life_satisfaction                 1.000            0.011
#> trust_government                  0.011            1.000
#> ------------------- 
#> 
#> Significance Matrix (p-values):
#> ------------------------------- 
#>                           age political_orientation environmental_concern
#> age                    0.0000                0.1678                0.2435
#> political_orientation  0.1678                0.0000                0.0000
#> environmental_concern  0.2435                0.0000                0.0000
#> life_satisfaction      0.1496                0.8361                0.8658
#> trust_government       0.8037                0.0076                0.0021
#>                       life_satisfaction trust_government
#> age                              0.1496           0.8037
#> political_orientation            0.8361           0.0076
#> environmental_concern            0.8658           0.0021
#> life_satisfaction                0.0000           0.6045
#> trust_government                 0.6045           0.0000
#> ------------------------------- 
#> 
#> Sample Size Matrix:
#> ------------------- 
#>                         age political_orientation environmental_concern
#> age                    2516                  2312                  2416
#> political_orientation  2312                  2312                  2221
#> environmental_concern  2416                  2221                  2416
#> life_satisfaction      2437                  2241                  2340
#> trust_government       2371                  2182                  2276
#>                       life_satisfaction trust_government
#> age                                2437             2371
#> political_orientation              2241             2182
#> environmental_concern              2340             2276
#> life_satisfaction                  2437             2296
#> trust_government                   2296             2371
#> ------------------- 
#> 
#> Pairwise Results:
#> ---------------- 
#>                                  Variable_Pair      r r_squared p_value
#>                    age × political_orientation -0.029     0.001  0.1678
#>                    age × environmental_concern  0.024     0.001  0.2435
#>                        age × life_satisfaction -0.029     0.001  0.1496
#>                         age × trust_government  0.005     0.000  0.8037
#>  political_orientation × environmental_concern -0.584     0.341  0.0000
#>      political_orientation × life_satisfaction -0.004     0.000  0.8361
#>       political_orientation × trust_government -0.057     0.003  0.0076
#>      environmental_concern × life_satisfaction -0.003     0.000  0.8658
#>       environmental_concern × trust_government  0.064     0.004  0.0021
#>           life_satisfaction × trust_government  0.011     0.000  0.6045
#>             CI_95    n sig
#>   [-0.069, 0.012] 2312    
#>   [-0.016, 0.064] 2416    
#>   [-0.069, 0.011] 2437    
#>   [-0.035, 0.045] 2371    
#>  [-0.611, -0.556] 2221 ***
#>   [-0.046, 0.037] 2241    
#>  [-0.099, -0.015] 2182  **
#>   [-0.044, 0.037] 2340    
#>    [0.023, 0.105] 2276  **
#>   [-0.030, 0.052] 2296    
#> ---------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Correlation Strength Interpretation:
#>   |r| < 0.30:        Weak correlation
#>   0.30 ≤ |r| < 0.70: Moderate correlation
#>   |r| ≥ 0.70:        Strong correlation
#> 
#> r² represents the proportion of variance explained
```

### Income Effects

``` r
# Income's relationship with satisfaction and trust
income_correlations <- survey_data %>%
  pearson_cor(income, life_satisfaction, trust_government,
              trust_media, trust_science,
              weights = sampling_weight)

print(income_correlations)
#> ── Weighted Pearson Correlation  ───────────────────────────────────────────────
#> • Weights variable: sampling_weight
#> • Missing data handling: pairwise deletion
#> • Confidence level: 95.0%
#> 
#> 
#> Correlation Matrix:
#> ------------------- 
#>                    income life_satisfaction trust_government trust_media
#> income              1.000             0.450           -0.001      -0.011
#> life_satisfaction   0.450             1.000            0.011       0.020
#> trust_government   -0.001             0.011            1.000       0.012
#> trust_media        -0.011             0.020            0.012       1.000
#> trust_science      -0.024            -0.019            0.031       0.024
#>                   trust_science
#> income                   -0.024
#> life_satisfaction        -0.019
#> trust_government          0.031
#> trust_media               0.024
#> trust_science             1.000
#> ------------------- 
#> 
#> Significance Matrix (p-values):
#> ------------------------------- 
#>                    income life_satisfaction trust_government trust_media
#> income             0.0000            0.0000           0.9755      0.6292
#> life_satisfaction  0.0000            0.0000           0.6045      0.3299
#> trust_government   0.9755            0.6045           0.0000      0.5823
#> trust_media        0.6292            0.3299           0.5823      0.0000
#> trust_science      0.2698            0.3708           0.1452      0.2586
#>                   trust_science
#> income                   0.2698
#> life_satisfaction        0.3708
#> trust_government         0.1452
#> trust_media              0.2586
#> trust_science            0.0000
#> ------------------------------- 
#> 
#> Sample Size Matrix:
#> ------------------- 
#>                   income life_satisfaction trust_government trust_media
#> income              2201              2130             2076        2090
#> life_satisfaction   2130              2437             2296        2305
#> trust_government    2076              2296             2371        2242
#> trust_media         2090              2305             2242        2382
#> trust_science       2112              2336             2271        2286
#>                   trust_science
#> income                     2112
#> life_satisfaction          2336
#> trust_government           2271
#> trust_media                2286
#> trust_science              2414
#> ------------------- 
#> 
#> Pairwise Results:
#> ---------------- 
#>                         Variable_Pair      r r_squared p_value           CI_95
#>            income × life_satisfaction  0.450     0.203  0.0000  [0.416, 0.483]
#>             income × trust_government -0.001     0.000  0.9755 [-0.044, 0.042]
#>                  income × trust_media -0.011     0.000  0.6292 [-0.053, 0.032]
#>                income × trust_science -0.024     0.001  0.2698 [-0.067, 0.019]
#>  life_satisfaction × trust_government  0.011     0.000  0.6045 [-0.030, 0.052]
#>       life_satisfaction × trust_media  0.020     0.000  0.3299 [-0.021, 0.061]
#>     life_satisfaction × trust_science -0.019     0.000  0.3708 [-0.059, 0.022]
#>        trust_government × trust_media  0.012     0.000  0.5823 [-0.030, 0.053]
#>      trust_government × trust_science  0.031     0.001  0.1452 [-0.011, 0.072]
#>           trust_media × trust_science  0.024     0.001  0.2586 [-0.017, 0.065]
#>     n sig
#>  2130 ***
#>  2076    
#>  2090    
#>  2112    
#>  2296    
#>  2305    
#>  2336    
#>  2242    
#>  2271    
#>  2286    
#> ---------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Correlation Strength Interpretation:
#>   |r| < 0.30:        Weak correlation
#>   0.30 ≤ |r| < 0.70: Moderate correlation
#>   |r| ≥ 0.70:        Strong correlation
#> 
#> r² represents the proportion of variance explained
```

## Advanced Topics

### Partial Correlation

When you want to control for a third variable:

``` r
# Note: mariposa focuses on bivariate correlations
# For partial correlations, combine with regression approaches

# Example: Age-income correlation might be confounded by education
# First, check bivariate correlations
survey_data %>%
  group_by(education) %>%
  pearson_cor(age, income, weights = sampling_weight)
#> ── Weighted Pearson Correlation  ───────────────────────────────────────────────
#> • Weights variable: sampling_weight
#> • Missing data handling: pairwise deletion
#> • Confidence level: 95.0%
#> 
#> 
#> Group: education = Basic Secondary
#> 
#> --- age × income ---
#> 
#>   Correlation: r = -0.013
#>   Effect size: r-squared = 0.000
#>   Sample size: n = 741
#>   95% CI: [-0.085, 0.059]
#>   p-value: 0.7146
#>   Significance: ns
#> 
#> Group: education = Intermediate Secondary
#> 
#> --- age × income ---
#> 
#>   Correlation: r = 0.049
#>   Effect size: r-squared = 0.002
#>   Sample size: n = 558
#>   95% CI: [-0.035, 0.131]
#>   p-value: 0.2521
#>   Significance: ns
#> 
#> Group: education = Academic Secondary
#> 
#> --- age × income ---
#> 
#>   Correlation: r = -0.057
#>   Effect size: r-squared = 0.003
#>   Sample size: n = 558
#>   95% CI: [-0.140, 0.026]
#>   p-value: 0.1762
#>   Significance: ns
#> 
#> Group: education = University
#> 
#> --- age × income ---
#> 
#>   Correlation: r = 0.020
#>   Effect size: r-squared = 0.000
#>   Sample size: n = 343
#>   95% CI: [-0.086, 0.125]
#>   p-value: 0.7157
#>   Significance: ns
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Correlation Strength Interpretation:
#>   |r| < 0.30:        Weak correlation
#>   0.30 ≤ |r| < 0.70: Moderate correlation
#>   |r| ≥ 0.70:        Strong correlation
#> 
#> r² represents the proportion of variance explained
```

### Non-Linear Relationships

If correlation is weak but you suspect a relationship:

``` r
# Check for non-linear patterns
# Low correlation doesn't mean no relationship

# Example: Life satisfaction might have U-shaped relationship with age
young <- survey_data %>% filter(age < 30)
middle <- survey_data %>% filter(age >= 30 & age < 60)
older <- survey_data %>% filter(age >= 60)

cat("Correlation in young adults:\n")
#> Correlation in young adults:
young %>% pearson_cor(age, life_satisfaction, weights = sampling_weight)
#> ── Weighted Pearson Correlation  ───────────────────────────────────────────────
#> • Weights variable: sampling_weight
#> • Missing data handling: pairwise deletion
#> • Confidence level: 95.0%
#> 
#> 
#> --- age × life_satisfaction ---
#> 
#>   Correlation: r = -0.033
#>   Effect size: r² = 0.001
#>   Sample size: n = 298
#>   95% CI: [-0.146, 0.081]
#>   p-value: 0.5704
#>   Significance: ns
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Correlation Strength Interpretation:
#>   |r| < 0.30:        Weak correlation
#>   0.30 ≤ |r| < 0.70: Moderate correlation
#>   |r| ≥ 0.70:        Strong correlation
#> 
#> r² represents the proportion of variance explained

cat("\nCorrelation in middle-aged:\n")
#> 
#> Correlation in middle-aged:
middle %>% pearson_cor(age, life_satisfaction, weights = sampling_weight)
#> ── Weighted Pearson Correlation  ───────────────────────────────────────────────
#> • Weights variable: sampling_weight
#> • Missing data handling: pairwise deletion
#> • Confidence level: 95.0%
#> 
#> 
#> --- age × life_satisfaction ---
#> 
#>   Correlation: r = 0.001
#>   Effect size: r² = 0.000
#>   Sample size: n = 1387
#>   95% CI: [-0.052, 0.053]
#>   p-value: 0.9835
#>   Significance: ns
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Correlation Strength Interpretation:
#>   |r| < 0.30:        Weak correlation
#>   0.30 ≤ |r| < 0.70: Moderate correlation
#>   |r| ≥ 0.70:        Strong correlation
#> 
#> r² represents the proportion of variance explained

cat("\nCorrelation in older adults:\n")
#> 
#> Correlation in older adults:
older %>% pearson_cor(age, life_satisfaction, weights = sampling_weight)
#> ── Weighted Pearson Correlation  ───────────────────────────────────────────────
#> • Weights variable: sampling_weight
#> • Missing data handling: pairwise deletion
#> • Confidence level: 95.0%
#> 
#> 
#> --- age × life_satisfaction ---
#> 
#>   Correlation: r = -0.058
#>   Effect size: r² = 0.003
#>   Sample size: n = 752
#>   95% CI: [-0.129, 0.013]
#>   p-value: 0.1092
#>   Significance: ns
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Correlation Strength Interpretation:
#>   |r| < 0.30:        Weak correlation
#>   0.30 ≤ |r| < 0.70: Moderate correlation
#>   |r| ≥ 0.70:        Strong correlation
#> 
#> r² represents the proportion of variance explained
```

## Complete Example

Here’s a comprehensive correlation analysis workflow:

``` r
# 1. Explore all correlations
cat("=== Correlation Matrix ===\n")
#> === Correlation Matrix ===
cor_matrix <- survey_data %>%
  pearson_cor(age, income, life_satisfaction,
              political_orientation, environmental_concern,
              weights = sampling_weight)
print(cor_matrix)
#> ── Weighted Pearson Correlation  ───────────────────────────────────────────────
#> • Weights variable: sampling_weight
#> • Missing data handling: pairwise deletion
#> • Confidence level: 95.0%
#> 
#> 
#> Correlation Matrix:
#> ------------------- 
#>                           age  income life_satisfaction political_orientation
#> age                     1.000  -0.005            -0.029                -0.029
#> income                 -0.005   1.000             0.450                -0.034
#> life_satisfaction      -0.029   0.450             1.000                -0.004
#> political_orientation  -0.029  -0.034            -0.004                 1.000
#> environmental_concern   0.024   0.015            -0.003                -0.584
#>                       environmental_concern
#> age                                   0.024
#> income                                0.015
#> life_satisfaction                    -0.003
#> political_orientation                -0.584
#> environmental_concern                 1.000
#> ------------------- 
#> 
#> Significance Matrix (p-values):
#> ------------------------------- 
#>                           age  income life_satisfaction political_orientation
#> age                    0.0000  0.8276            0.1496                0.1678
#> income                 0.8276  0.0000            0.0000                0.1255
#> life_satisfaction      0.1496  0.0000            0.0000                0.8361
#> political_orientation  0.1678  0.1255            0.8361                0.0000
#> environmental_concern  0.2435  0.5026            0.8658                0.0000
#>                       environmental_concern
#> age                                  0.2435
#> income                               0.5026
#> life_satisfaction                    0.8658
#> political_orientation                0.0000
#> environmental_concern                0.0000
#> ------------------------------- 
#> 
#> Sample Size Matrix:
#> ------------------- 
#>                         age income life_satisfaction political_orientation
#> age                    2516   2201              2437                  2312
#> income                 2201   2201              2130                  2020
#> life_satisfaction      2437   2130              2437                  2241
#> political_orientation  2312   2020              2241                  2312
#> environmental_concern  2416   2112              2340                  2221
#>                       environmental_concern
#> age                                    2416
#> income                                 2112
#> life_satisfaction                      2340
#> political_orientation                  2221
#> environmental_concern                  2416
#> ------------------- 
#> 
#> Pairwise Results:
#> ---------------- 
#>                                  Variable_Pair      r r_squared p_value
#>                                   age × income -0.005     0.000  0.8276
#>                        age × life_satisfaction -0.029     0.001  0.1496
#>                    age × political_orientation -0.029     0.001  0.1678
#>                    age × environmental_concern  0.024     0.001  0.2435
#>                     income × life_satisfaction  0.450     0.203  0.0000
#>                 income × political_orientation -0.034     0.001  0.1255
#>                 income × environmental_concern  0.015     0.000  0.5026
#>      life_satisfaction × political_orientation -0.004     0.000  0.8361
#>      life_satisfaction × environmental_concern -0.003     0.000  0.8658
#>  political_orientation × environmental_concern -0.584     0.341  0.0000
#>             CI_95    n sig
#>   [-0.046, 0.037] 2201    
#>   [-0.069, 0.011] 2437    
#>   [-0.069, 0.012] 2312    
#>   [-0.016, 0.064] 2416    
#>    [0.416, 0.483] 2130 ***
#>   [-0.078, 0.010] 2020    
#>   [-0.028, 0.057] 2112    
#>   [-0.046, 0.037] 2241    
#>   [-0.044, 0.037] 2340    
#>  [-0.611, -0.556] 2221 ***
#> ---------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Correlation Strength Interpretation:
#>   |r| < 0.30:        Weak correlation
#>   0.30 ≤ |r| < 0.70: Moderate correlation
#>   |r| ≥ 0.70:        Strong correlation
#> 
#> r² represents the proportion of variance explained

# 2. Focus on significant correlations
cat("\n=== Significant Correlations (p < 0.05) ===\n")
#> 
#> === Significant Correlations (p < 0.05) ===
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

# 3. Compare methods for key relationship
cat("\n=== Method Comparison: Income vs Life Satisfaction ===\n")
#> 
#> === Method Comparison: Income vs Life Satisfaction ===
pearson <- survey_data %>%
  pearson_cor(income, life_satisfaction, weights = sampling_weight)
spearman <- survey_data %>%
  spearman_rho(income, life_satisfaction, weights = sampling_weight)

if (!is.null(pearson$correlations) && nrow(pearson$correlations) > 0) {
  corr_val <- pearson$correlations$correlation[1]
  p_val <- pearson$correlations$p_value[1]
  if (!is.null(corr_val) && !is.na(corr_val) && !is.null(p_val) && !is.na(p_val)) {
    cat("Pearson r =", round(corr_val, 3),
        "(p =", round(p_val, 4), ")\n")
  }
}
#> Pearson r = 0.45 (p = 0 )
if (!is.null(spearman$correlations) && nrow(spearman$correlations) > 0) {
  corr_val <- spearman$correlations$correlation[1]
  p_val <- spearman$correlations$p_value[1]
  if (!is.null(corr_val) && !is.na(corr_val) && !is.null(p_val) && !is.na(p_val)) {
    cat("Spearman rho =", round(corr_val, 3),
        "(p =", round(p_val, 4), ")\n")
  }
}

# 4. Group differences
cat("\n=== Regional Differences in Age-Income Correlation ===\n")
#> 
#> === Regional Differences in Age-Income Correlation ===
survey_data %>%
  group_by(region) %>%
  pearson_cor(age, income, weights = sampling_weight)
#> ── Weighted Pearson Correlation  ───────────────────────────────────────────────
#> • Weights variable: sampling_weight
#> • Missing data handling: pairwise deletion
#> • Confidence level: 95.0%
#> 
#> 
#> Group: region = East
#> 
#> --- age × income ---
#> 
#>   Correlation: r = 0.050
#>   Effect size: r-squared = 0.002
#>   Sample size: n = 449
#>   95% CI: [-0.043, 0.142]
#>   p-value: 0.2931
#>   Significance: ns
#> 
#> Group: region = West
#> 
#> --- age × income ---
#> 
#>   Correlation: r = -0.019
#>   Effect size: r-squared = 0.000
#>   Sample size: n = 1751
#>   95% CI: [-0.066, 0.028]
#>   p-value: 0.4267
#>   Significance: ns
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Correlation Strength Interpretation:
#>   |r| < 0.30:        Weak correlation
#>   0.30 ≤ |r| < 0.70: Moderate correlation
#>   |r| ≥ 0.70:        Strong correlation
#> 
#> r² represents the proportion of variance explained
```

## Reporting Results

### Professional Format

“There was a moderate positive correlation between age and income (r =
0.34, 95% CI \[0.29, 0.39\], p \< .001), indicating that older
respondents tended to report higher incomes.”

### What to Include

- Correlation coefficient (r, ρ, or τ)
- Confidence interval
- p-value
- Sample size or effective N
- Whether weights were used

## Summary

Key points about correlations:

1.  **Choose the right method**: Pearson for linear, Spearman for
    monotonic, Kendall for ordinal
2.  **Use weights**: Ensure representativeness with survey weights
3.  **Check significance**: But remember effect size matters too
4.  **Avoid over-interpretation**: Correlation ≠ causation
5.  **Consider context**: Domain knowledge helps interpret strength

mariposa makes correlation analysis straightforward while ensuring
proper handling of survey weights!
