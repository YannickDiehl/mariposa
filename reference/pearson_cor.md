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
pearson_cor(data, ..., weights = NULL, conf.level = 0.95, na.rm = "pairwise")
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

- na.rm:

  How to handle missing values:

  - `"pairwise"` (default): Use all available data for each pair

  - `"listwise"`: Only use complete cases across all variables

## Value

Correlation results showing relationships between variables, including:

- Correlation coefficient (r): Strength and direction of relationship

- P-value: Whether the relationship is statistically significant

- Confidence interval: Range of plausible correlation values

- Sample size: Number of observations used

## Details

### Understanding Correlation Values

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

- p ≥ 0.05: No significant relationship found

### When to Use Pearson Correlation

Use this when:

- Both variables are numeric and continuous

- You expect a linear relationship

- Data is roughly normally distributed

- You want to measure strength of linear association

Don't use when:

- Data has extreme outliers (consider Spearman instead)

- Relationship is curved/non-linear

- Variables are categorical (use chi-squared test)

- You need to establish causation (correlation ≠ causation!)

### Reading the Results

A correlation of 0.65 with p \< 0.001 means:

- Strong positive relationship (r = 0.65)

- As one variable increases, the other tends to increase

- Very unlikely to be due to chance (p \< 0.001)

- About 42% of variation is shared (r² = 0.65² = 0.42)

### Tips for Success

- Always plot your data first to check for non-linear patterns

- Consider both statistical significance (p-value) and practical
  importance (r value)

- Remember: correlation does not imply causation

- Check for outliers that might inflate or deflate correlations

- Use Spearman correlation for ordinal data or non-normal distributions

## References

Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences
(2nd ed.). Lawrence Erlbaum Associates.

Fisher, R.A. (1915). Frequency distribution of the values of the
correlation coefficient in samples from an indefinitely large
population. Biometrika, 10(4), 507-521.

## See also

[`cor`](https://rdrr.io/r/stats/cor.html) for the base R correlation
function [`cor.test`](https://rdrr.io/r/stats/cor.test.html) for
correlation significance testing

Other correlation:
[`kendall_tau()`](https://YannickDiehl.github.io/mariposa/reference/kendall_tau.md),
[`spearman_rho()`](https://YannickDiehl.github.io/mariposa/reference/spearman_rho.md)

## Examples

``` r
# Load required packages and data
library(dplyr)
data(survey_data)

# Basic correlation between two variables
survey_data %>% 
  pearson_cor(age, income)
#> 
#> ── Pearson Correlation  ────────────────────────────────────────────────────────
#> 
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

# Correlation matrix for multiple variables
survey_data %>% 
  pearson_cor(age, income, life_satisfaction)
#> 
#> ── Pearson Correlation  ────────────────────────────────────────────────────────
#> 
#> • Missing data handling: pairwise deletion
#> • Confidence level: 95.0%
#> 
#> 
#> Correlation Matrix:
#> ------------------- 
#>                       age  income life_satisfaction
#> age                 1.000  -0.007            -0.029
#> income             -0.007   1.000             0.448
#> life_satisfaction  -0.029   0.448             1.000
#> ------------------- 
#> 
#> Significance Matrix (p-values):
#> ------------------------------- 
#>                       age  income life_satisfaction
#> age                0.0000  0.7608            0.1578
#> income             0.7608  0.0000            0.0000
#> life_satisfaction  0.1578  0.0000            0.0000
#> ------------------------------- 
#> 
#> Sample Size Matrix:
#> ------------------- 
#>                     age income life_satisfaction
#> age                2500   2186              2421
#> income             2186   2186              2115
#> life_satisfaction  2421   2115              2421
#> ------------------- 
#> 
#> Pairwise Results:
#> ---------------- 
#>               Variable_Pair      r r_squared p_value           CI_95    n sig
#>                age × income -0.007     0.000  0.7608 [-0.048, 0.035] 2186    
#>     age × life_satisfaction -0.029     0.001  0.1578 [-0.068, 0.011] 2421    
#>  income × life_satisfaction  0.448     0.201  0.0000  [0.413, 0.482] 2115 ***
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

# Weighted correlations
survey_data %>% 
  pearson_cor(age, income, weights = sampling_weight)
#> 
#> ── Weighted Pearson Correlation  ───────────────────────────────────────────────
#> 
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

# Grouped correlations
survey_data %>% 
  group_by(region) %>% 
  pearson_cor(age, income, life_satisfaction)
#> 
#> ── Pearson Correlation  ────────────────────────────────────────────────────────
#> 
#> • Missing data handling: pairwise deletion
#> • Confidence level: 95.0%
#> 
#> 
#> Group: region = East
#> 
#> Correlation Matrix:
#> ------------------- 
#>                       age  income life_satisfaction
#> age                 1.000   0.039            -0.043
#> income              0.039   1.000             0.448
#> life_satisfaction  -0.043   0.448             1.000
#> ------------------- 
#> 
#> Significance Matrix (p-values):
#> ------------------------------- 
#>                       age  income life_satisfaction
#> age                0.0000  0.4154            0.3498
#> income             0.4154  0.0000            0.0000
#> life_satisfaction  0.3498  0.0000            0.0000
#> ------------------------------- 
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
#> ---------------- 
#>               Variable_Pair      r r_squared p_value           CI_95   n sig
#>                age × income  0.039     0.002  0.4154 [-0.055, 0.134] 429    
#>     age × life_satisfaction -0.043     0.002  0.3498 [-0.134, 0.048] 465    
#>  income × life_satisfaction  0.448     0.201  0.0000  [0.367, 0.522] 410 ***
#> ---------------- 
#> 
#> Group: region = West
#> 
#> Correlation Matrix:
#> ------------------- 
#>                       age  income life_satisfaction
#> age                 1.000  -0.018            -0.025
#> income             -0.018   1.000             0.449
#> life_satisfaction  -0.025   0.449             1.000
#> ------------------- 
#> 
#> Significance Matrix (p-values):
#> ------------------------------- 
#>                       age  income life_satisfaction
#> age                0.0000  0.4615            0.2741
#> income             0.4615  0.0000            0.0000
#> life_satisfaction  0.2741  0.0000            0.0000
#> ------------------------------- 
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
#> ---------------- 
#>               Variable_Pair      r r_squared p_value           CI_95    n sig
#>                age × income -0.018     0.000  0.4615 [-0.064, 0.029] 1757    
#>     age × life_satisfaction -0.025     0.001  0.2741 [-0.069, 0.020] 1956    
#>  income × life_satisfaction  0.449     0.201  0.0000  [0.410, 0.486] 1705 ***
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

# Using tidyselect helpers
survey_data %>% 
  pearson_cor(where(is.numeric), weights = sampling_weight)
#> 
#> ── Weighted Pearson Correlation  ───────────────────────────────────────────────
#> 
#> • Weights variable: sampling_weight
#> • Missing data handling: pairwise deletion
#> • Confidence level: 95.0%
#> 
#> 
#> Correlation Matrix:
#> ------------------- 
#>                           id    age income political_orientation environmental_concern
#> id                      1.00   0.01   0.03                 -0.02                 -0.01
#> age                     0.01   1.00   0.00                 -0.03                  0.02
#> income                  0.03   0.00   1.00                 -0.03                  0.01
#> political_orientation  -0.02  -0.03  -0.03                  1.00                 -0.58
#> environmental_concern  -0.01   0.02   0.01                 -0.58                  1.00
#> life_satisfaction       0.01  -0.03   0.45                  0.00                  0.00
#> trust_government        0.02   0.01   0.00                 -0.06                  0.06
#> trust_media            -0.01   0.00  -0.01                  0.00                  0.00
#> trust_science           0.02   0.00  -0.02                  0.04                 -0.01
#> sampling_weight        -0.02  -0.01  -0.04                  0.01                  0.02
#>                       life_satisfaction trust_government trust_media trust_science sampling_weight
#> id                                 0.01             0.02       -0.01          0.02           -0.02
#> age                               -0.03             0.01        0.00          0.00           -0.01
#> income                             0.45             0.00       -0.01         -0.02           -0.04
#> political_orientation              0.00            -0.06        0.00          0.04            0.01
#> environmental_concern              0.00             0.06        0.00         -0.01            0.02
#> life_satisfaction                  1.00             0.01        0.02         -0.02           -0.02
#> trust_government                   0.01             1.00        0.01          0.03           -0.01
#> trust_media                        0.02             0.01        1.00          0.02            0.02
#> trust_science                     -0.02             0.03        0.02          1.00            0.00
#> sampling_weight                   -0.02            -0.01        0.02          0.00            1.00
#> ------------------- 
#> 
#> Significance Matrix (p-values):
#> ------------------------------- 
#>                            id     age  income political_orientation environmental_concern
#> id                     0.0000  0.6478  0.2058                0.2714                0.7634
#> age                    0.6478  0.0000  0.8276                0.1678                0.2435
#> income                 0.2058  0.8276  0.0000                0.1255                0.5026
#> political_orientation  0.2714  0.1678  0.1255                0.0000                0.0000
#> environmental_concern  0.7634  0.2435  0.5026                0.0000                0.0000
#> life_satisfaction      0.5871  0.1496  0.0000                0.8361                0.8658
#> trust_government       0.3105  0.8037  0.9755                0.0076                0.0021
#> trust_media            0.7139  0.8202  0.6292                0.8354                0.9074
#> trust_science          0.4392  0.9021  0.2698                0.0586                0.5073
#> sampling_weight        0.4360  0.6454  0.0582                0.5897                0.3279
#>                       life_satisfaction trust_government trust_media trust_science sampling_weight
#> id                               0.5871           0.3105      0.7139        0.4392          0.4360
#> age                              0.1496           0.8037      0.8202        0.9021          0.6454
#> income                           0.0000           0.9755      0.6292        0.2698          0.0582
#> political_orientation            0.8361           0.0076      0.8354        0.0586          0.5897
#> environmental_concern            0.8658           0.0021      0.9074        0.5073          0.3279
#> life_satisfaction                0.0000           0.6045      0.3299        0.3708          0.3592
#> trust_government                 0.6045           0.0000      0.5823        0.1452          0.6789
#> trust_media                      0.3299           0.5823      0.0000        0.2586          0.2814
#> trust_science                    0.3708           0.1452      0.2586        0.0000          0.9607
#> sampling_weight                  0.3592           0.6789      0.2814        0.9607          0.0000
#> ------------------------------- 
#> 
#> Sample Size Matrix:
#> ------------------- 
#>                          id   age income political_orientation
#> id                     2516  2516   2201                  2312
#> age                    2516  2516   2201                  2312
#> income                 2201  2201   2201                  2020
#> political_orientation  2312  2312   2020                  2312
#> environmental_concern  2416  2416   2112                  2221
#> life_satisfaction      2437  2437   2130                  2241
#> trust_government       2371  2371   2076                  2182
#> trust_media            2382  2382   2090                  2190
#> trust_science          2414  2414   2112                  2215
#> sampling_weight        2516  2516   2201                  2312
#>                       environmental_concern life_satisfaction trust_government
#> id                                     2416              2437             2371
#> age                                    2416              2437             2371
#> income                                 2112              2130             2076
#> political_orientation                  2221              2241             2182
#> environmental_concern                  2416              2340             2276
#> life_satisfaction                      2340              2437             2296
#> trust_government                       2276              2296             2371
#> trust_media                            2288              2305             2242
#> trust_science                          2316              2336             2271
#> sampling_weight                        2416              2437             2371
#>                       trust_media trust_science sampling_weight
#> id                           2382          2414            2516
#> age                          2382          2414            2516
#> income                       2090          2112            2201
#> political_orientation        2190          2215            2312
#> environmental_concern        2288          2316            2416
#> life_satisfaction            2305          2336            2437
#> trust_government             2242          2271            2371
#> trust_media                  2382          2286            2382
#> trust_science                2286          2414            2414
#> sampling_weight              2382          2414            2516
#> ------------------- 
#> 
#> Pairwise Results:
#> ---------------- 
#>                                  Variable_Pair      r r_squared p_value
#>                                       id × age  0.009     0.000  0.6478
#>                                    id × income  0.027     0.001  0.2058
#>                     id × political_orientation -0.023     0.001  0.2714
#>                     id × environmental_concern -0.006     0.000  0.7634
#>                         id × life_satisfaction  0.011     0.000  0.5871
#>                          id × trust_government  0.021     0.000  0.3105
#>                               id × trust_media -0.008     0.000  0.7139
#>                             id × trust_science  0.016     0.000  0.4392
#>                           id × sampling_weight -0.016     0.000  0.4360
#>                                   age × income -0.005     0.000  0.8276
#>                    age × political_orientation -0.029     0.001  0.1678
#>                    age × environmental_concern  0.024     0.001  0.2435
#>                        age × life_satisfaction -0.029     0.001  0.1496
#>                         age × trust_government  0.005     0.000  0.8037
#>                              age × trust_media  0.005     0.000  0.8202
#>                            age × trust_science  0.003     0.000  0.9021
#>                          age × sampling_weight -0.009     0.000  0.6454
#>                 income × political_orientation -0.034     0.001  0.1255
#>                 income × environmental_concern  0.015     0.000  0.5026
#>                     income × life_satisfaction  0.450     0.203  0.0000
#>                      income × trust_government -0.001     0.000  0.9755
#>                           income × trust_media -0.011     0.000  0.6292
#>                         income × trust_science -0.024     0.001  0.2698
#>                       income × sampling_weight -0.040     0.002  0.0582
#>  political_orientation × environmental_concern -0.584     0.341  0.0000
#>      political_orientation × life_satisfaction -0.004     0.000  0.8361
#>       political_orientation × trust_government -0.057     0.003  0.0076
#>            political_orientation × trust_media  0.004     0.000  0.8354
#>          political_orientation × trust_science  0.040     0.002  0.0586
#>        political_orientation × sampling_weight  0.011     0.000  0.5897
#>      environmental_concern × life_satisfaction -0.003     0.000  0.8658
#>       environmental_concern × trust_government  0.064     0.004  0.0021
#>            environmental_concern × trust_media  0.002     0.000  0.9074
#>          environmental_concern × trust_science -0.014     0.000  0.5073
#>        environmental_concern × sampling_weight  0.020     0.000  0.3279
#>           life_satisfaction × trust_government  0.011     0.000  0.6045
#>                life_satisfaction × trust_media  0.020     0.000  0.3299
#>              life_satisfaction × trust_science -0.019     0.000  0.3708
#>            life_satisfaction × sampling_weight -0.019     0.000  0.3592
#>                 trust_government × trust_media  0.012     0.000  0.5823
#>               trust_government × trust_science  0.031     0.001  0.1452
#>             trust_government × sampling_weight -0.009     0.000  0.6789
#>                    trust_media × trust_science  0.024     0.001  0.2586
#>                  trust_media × sampling_weight  0.022     0.000  0.2814
#>                trust_science × sampling_weight  0.001     0.000  0.9607
#>             CI_95    n sig
#>   [-0.030, 0.048] 2516    
#>   [-0.015, 0.069] 2201    
#>   [-0.064, 0.018] 2312    
#>   [-0.046, 0.034] 2416    
#>   [-0.029, 0.051] 2437    
#>   [-0.019, 0.061] 2371    
#>   [-0.048, 0.033] 2382    
#>   [-0.024, 0.056] 2414    
#>   [-0.055, 0.024] 2516    
#>   [-0.046, 0.037] 2201    
#>   [-0.069, 0.012] 2312    
#>   [-0.016, 0.064] 2416    
#>   [-0.069, 0.011] 2437    
#>   [-0.035, 0.045] 2371    
#>   [-0.036, 0.045] 2382    
#>   [-0.037, 0.042] 2414    
#>   [-0.048, 0.030] 2516    
#>   [-0.078, 0.010] 2020    
#>   [-0.028, 0.057] 2112    
#>    [0.416, 0.483] 2130 ***
#>   [-0.044, 0.042] 2076    
#>   [-0.053, 0.032] 2090    
#>   [-0.067, 0.019] 2112    
#>   [-0.082, 0.001] 2201    
#>  [-0.611, -0.556] 2221 ***
#>   [-0.046, 0.037] 2241    
#>  [-0.099, -0.015] 2182  **
#>   [-0.037, 0.046] 2190    
#>   [-0.001, 0.082] 2215    
#>   [-0.030, 0.052] 2312    
#>   [-0.044, 0.037] 2340    
#>    [0.023, 0.105] 2276  **
#>   [-0.039, 0.043] 2288    
#>   [-0.054, 0.027] 2316    
#>   [-0.020, 0.060] 2416    
#>   [-0.030, 0.052] 2296    
#>   [-0.021, 0.061] 2305    
#>   [-0.059, 0.022] 2336    
#>   [-0.058, 0.021] 2437    
#>   [-0.030, 0.053] 2242    
#>   [-0.011, 0.072] 2271    
#>   [-0.049, 0.032] 2371    
#>   [-0.017, 0.065] 2286    
#>   [-0.018, 0.062] 2382    
#>   [-0.039, 0.041] 2414    
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

# Listwise deletion for missing data
survey_data %>% 
  pearson_cor(age, income, na.rm = "listwise")
#> 
#> ── Pearson Correlation  ────────────────────────────────────────────────────────
#> 
#> • Missing data handling: listwise deletion
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

# Store results for further analysis
result <- survey_data %>% 
  pearson_cor(age, income, life_satisfaction, weights = sampling_weight)
print(result)
#> 
#> ── Weighted Pearson Correlation  ───────────────────────────────────────────────
#> 
#> • Weights variable: sampling_weight
#> • Missing data handling: pairwise deletion
#> • Confidence level: 95.0%
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
#> 
#> Correlation Strength Interpretation:
#>   |r| < 0.30:        Weak correlation
#>   0.30 ≤ |r| < 0.70: Moderate correlation
#>   |r| ≥ 0.70:        Strong correlation
#> 
#> r² represents the proportion of variance explained
```
