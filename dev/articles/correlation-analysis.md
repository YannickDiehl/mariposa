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
#> 
#> Pearson Correlation 
#> --------------------
#> 
#> - Missing data handling: pairwise deletion
#> - Confidence level: 95.0%
#> 
#> 
#> --- age × income ---
#> 
#>   Correlation: r = -0.007
#>   Effect size: r²: 0.000
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

Interpretation of *r*:

- $r = 1$: Perfect positive correlation
- $r = 0$: No linear relationship
- $r = - 1$: Perfect negative correlation

### With Survey Weights

Get population-representative correlations:

``` r
survey_data %>%
  pearson_cor(age, income, weights = sampling_weight)
#> 
#> Weighted Pearson Correlation 
#> -----------------------------
#> 
#> - Weights variable: sampling_weight
#> - Missing data handling: pairwise deletion
#> - Confidence level: 95.0%
#> 
#> 
#> --- age × income ---
#> 
#>   Correlation: r = -0.005
#>   Effect size: r²: 0.000
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

Create a correlation matrix for several variables at once:

``` r
survey_data %>%
  pearson_cor(trust_government, trust_media, trust_science,
              weights = sampling_weight)
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

Calculate correlations within subgroups:

``` r
survey_data %>%
  group_by(region) %>%
  pearson_cor(age, income, weights = sampling_weight)
#> 
#> Weighted Pearson Correlation 
#> -----------------------------
#> 
#> - Weights variable: sampling_weight
#> - Missing data handling: pairwise deletion
#> - Confidence level: 95.0%
#> 
#> 
#> Group: region = East
#> --------------------
#> 
#> --- age × income ---
#> 
#>   Correlation: r = 0.050
#>   Effect size: r-squared: 0.002
#>   Sample size: n = 449
#>   95% CI: [-0.043, 0.142]
#>   p-value: 0.2931
#>   Significance: ns
#> 
#> Group: region = West
#> --------------------
#> 
#> --- age × income ---
#> 
#>   Correlation: r = -0.019
#>   Effect size: r-squared: 0.000
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

### Confidence Intervals

Specify the confidence level for the interval around the estimate:

``` r
survey_data %>%
  pearson_cor(age, income,
              conf.level = 0.95,
              weights = sampling_weight)
#> 
#> Weighted Pearson Correlation 
#> -----------------------------
#> 
#> - Weights variable: sampling_weight
#> - Missing data handling: pairwise deletion
#> - Confidence level: 95.0%
#> 
#> 
#> --- age × income ---
#> 
#>   Correlation: r = -0.005
#>   Effect size: r²: 0.000
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
#> 
#> Spearman's Rank Correlation Analysis 
#> -------------------------------------
#> 
#> - Method: Spearman's rho (rank correlation)
#> - Variables: political_orientation, environmental_concern
#> - Missing data handling: pairwise deletion
#> - Alternative hypothesis: two.sided
#> 
#> --- political_orientation × environmental_concern ---
#> 
#>   Spearman's rho: ρ = -0.576
#>   t-statistic: -33.093
#>   Sample size: n = 2207
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
#> ----------------------------------------------
#> 
#> - Method: Spearman's rho (rank correlation)
#> - Variables: political_orientation, environmental_concern
#> - Weights variable: sampling_weight
#> - Missing data handling: pairwise deletion
#> - Alternative hypothesis: two.sided
#> 
#> --- political_orientation × environmental_concern ---
#> 
#>   Spearman's rho: ρ = -0.576
#>   t-statistic: -33.093
#>   Sample size: n = 2207
#>   p-value (2-tailed): 0.0000
#>   Significance: ***
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation: moderate negative monotonic relationship
```

### Multiple Variables

``` r
survey_data %>%
  spearman_rho(political_orientation, environmental_concern,
               life_satisfaction, trust_government,
               weights = sampling_weight)
#> 
#> Weighted Spearman's Rank Correlation Analysis 
#> ----------------------------------------------
#> 
#> - Method: Spearman's rho (rank correlation)
#> - Variables: political_orientation, environmental_concern, life_satisfaction, trust_government
#> - Weights variable: sampling_weight
#> - Missing data handling: pairwise deletion
#> - Alternative hypothesis: two.sided
#> 
#> 
#> Spearman's Rho Matrix:
#> --------------------------------------------------------------------- 
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
#> --------------------------------------------------------------------- 
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
#> --------------------------------------------------------------------- 
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
#> --------------------------------------------------------------------- 
#>                                           Pair    rho       t      p    n sig
#>  political_orientation × environmental_concern -0.576 -33.093 0.0000 2207 ***
#>      political_orientation × life_satisfaction -0.004  -0.210 0.8334 2228    
#>       political_orientation × trust_government -0.055  -2.545 0.0110 2168   *
#>      environmental_concern × life_satisfaction  0.003   0.121 0.9041 2324    
#>       environmental_concern × trust_government  0.067   3.175 0.0015 2260  **
#>           life_satisfaction × trust_government  0.002   0.072 0.9425 2280    
#> --------------------------------------------------------------------- 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
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
#> 
#> Kendall's Tau-b Correlation 
#> ----------------------------
#> 
#> - Missing data handling: pairwise deletion
#> - Alternative hypothesis: two.sided
#> 
#> 
#> --- political_orientation × life_satisfaction ---
#> 
#>   Kendall's tau-b: τ = -0.004
#>   z-score: -0.212
#>   Sample size: n = 2228
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
#> 
#> Weighted Kendall's Tau-b Correlation 
#> -------------------------------------
#> 
#> - Weights variable: sampling_weight
#> - Missing data handling: pairwise deletion
#> - Alternative hypothesis: two.sided
#> 
#> 
#> --- political_orientation × life_satisfaction ---
#> 
#>   Kendall's tau-b: τ = -0.004
#>   z-score: -0.298
#>   Sample size: n = 2241
#>   p-value (2-tailed): 0.7659
#>   Significance: ns
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Interpretation: weak negative correlation
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
print(result)
#> 
#> Weighted Pearson Correlation 
#> -----------------------------
#> 
#> - Weights variable: sampling_weight
#> - Missing data handling: pairwise deletion
#> - Confidence level: 95.0%
#> 
#> 
#> --- age × income ---
#> 
#>   Correlation: r = -0.005
#>   Effect size: r²: 0.000
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
survey_data %>%
  pearson_cor(income, life_satisfaction, trust_government,
              trust_media, trust_science,
              weights = sampling_weight)
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

## Complete Example

A comprehensive correlation analysis workflow:

``` r
# 1. Correlation matrix
cor_matrix <- survey_data %>%
  pearson_cor(age, income, life_satisfaction,
              political_orientation, environmental_concern,
              weights = sampling_weight)
print(cor_matrix)
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
#> 
#> Weighted Pearson Correlation 
#> -----------------------------
#> 
#> - Weights variable: sampling_weight
#> - Missing data handling: pairwise deletion
#> - Confidence level: 95.0%
#> 
#> 
#> Group: region = East
#> --------------------
#> 
#> --- age × income ---
#> 
#>   Correlation: r = 0.050
#>   Effect size: r-squared: 0.002
#>   Sample size: n = 449
#>   95% CI: [-0.043, 0.142]
#>   p-value: 0.2931
#>   Significance: ns
#> 
#> Group: region = West
#> --------------------
#> 
#> --- age × income ---
#> 
#>   Correlation: r = -0.019
#>   Effect size: r-squared: 0.000
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
