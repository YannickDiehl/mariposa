# Working with Survey Weights

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

## Why Survey Weights Matter

Survey weights adjust your analysis to make results representative of
the target population. Without weights, your results only describe your
sample, which might be biased.

Common reasons for weights: - **Oversampling**: Deliberately surveying
more people from small groups - **Non-response**: Some groups less
likely to participate - **Post-stratification**: Adjusting sample to
match known population totals

## Impact of Weights

Let’s see the difference weights can make:

``` r
# Without weights (biased)
unweighted_stats <- survey_data %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    mean_satisfaction = mean(life_satisfaction, na.rm = TRUE)
  )
print(unweighted_stats)
#> # A tibble: 1 × 2
#>   mean_age mean_satisfaction
#>      <dbl>             <dbl>
#> 1     50.5              3.63

# With weights (representative)
age_weighted <- w_mean(survey_data, age, weights = sampling_weight)
satisfaction_weighted <- w_mean(survey_data, life_satisfaction, weights = sampling_weight)

weighted_stats <- data.frame(
  mean_age = age_weighted$results$weighted_mean,
  mean_satisfaction = satisfaction_weighted$results$weighted_mean
)
print(weighted_stats)
#>   mean_age mean_satisfaction
#> 1  50.5144          3.624863
```

The weighted results represent what you’d find if you surveyed the
entire population.

## Weighted Statistics Functions

mariposa provides specialized functions for weighted calculations:

### Central Tendency

``` r
# Weighted mean
mean_result <- w_mean(survey_data, income, weights = sampling_weight)
print(mean_result)
#> ── Weighted Mean Statistics ────────────────────────────────────────────────────
#> 
#> --- income ---
#>  Variable weighted_mean Effective_N
#>    income      3743.099      2158.9

# Weighted median
median_result <- w_median(survey_data, income, weights = sampling_weight)
print(median_result)
#> ── Weighted Median Statistics ──────────────────────────────────────────────────
#> 
#> --- income ---
#>  Variable weighted_median Effective_N
#>    income            3500      2158.9

# Weighted mode
mode_result <- w_modus(survey_data, education, weights = sampling_weight)
print(mode_result)
#> 
#> Weighted Mode Statistics
#> ------------------------ 
#> # A tibble: 1 × 3
#>   Variable  weighted_mode   effective_n
#>   <chr>     <ord>                 <dbl>
#> 1 education Basic Secondary       2469.
```

### Dispersion

``` r
# Weighted standard deviation
sd_result <- w_sd(survey_data, income, weights = sampling_weight)
print(sd_result)
#> ── Weighted Standard Deviation Statistics ──────────────────────────────────────
#> 
#> --- income ---
#>  Variable weighted_sd Effective_N
#>    income    1423.966      2158.9

# Weighted variance
var_result <- w_var(survey_data, income, weights = sampling_weight)
print(var_result)
#> ── Weighted Variance Statistics ────────────────────────────────────────────────
#> 
#> --- income ---
#>  Variable weighted_var Effective_N
#>    income      2027678      2158.9

# Weighted IQR
iqr_result <- w_iqr(survey_data, income, weights = sampling_weight)
print(iqr_result)
#> ── Weighted Interquartile Range Statistics ─────────────────────────────────────
#> 
#> --- income ---
#>  Variable weighted_iqr Effective_N
#>    income         1900      2158.9
```

### Distribution Shape

``` r
# Weighted skewness
skew_result <- w_skew(survey_data, income, weights = sampling_weight)
print(skew_result)
#> ── Weighted Skewness Statistics ────────────────────────────────────────────────
#> 
#> --- income ---
#>  Variable weighted_skew Effective_N
#>    income         0.725      2158.9

# Weighted kurtosis
kurt_result <- w_kurtosis(survey_data, income, weights = sampling_weight)
print(kurt_result)
#> ── Weighted Excess Kurtosis Statistics ─────────────────────────────────────────
#> 
#> --- income ---
#>  Variable weighted_kurtosis Effective_N
#>    income             0.388      2158.9
```

### Other Weighted Functions

``` r
# Weighted quantiles (25th, 50th, 75th percentiles)
quantile_result <- w_quantile(survey_data, income,
                              probs = c(0.25, 0.5, 0.75),
                              weights = sampling_weight)
print(quantile_result)
#> ── Weighted Quantile Statistics ────────────────────────────────────────────────
#>  Variable Quantile    Value         Weights
#>    income      25% 2700.000 sampling_weight
#>    income      50% 3500.000 sampling_weight
#>    income      75% 4600.000 sampling_weight
#>    income        n 2186.000 sampling_weight
#>    income    eff_n 2158.917 sampling_weight
#> ────────────────────────────────────────────────────────────────────────────────

# Weighted standard error
se_result <- w_se(survey_data, income, weights = sampling_weight)
print(se_result)
#> ── Weighted Standard Error Statistics ──────────────────────────────────────────
#> 
#> --- income ---
#>  Variable weighted_se Effective_N
#>    income      30.353      2158.9
```

## Effective Sample Size

When using weights, the “effective” sample size is often smaller than
the actual sample size:

``` r
# Compare actual vs effective N
actual_n <- nrow(survey_data)
age_weighted <- w_mean(survey_data, age, weights = sampling_weight)
effective_n <- age_weighted$results$effective_n

cat("Actual sample size:", actual_n, "\n")
#> Actual sample size: 2500
cat("Effective sample size:", round(effective_n), "\n")
#> Effective sample size: 2469
cat("Design effect:", round(actual_n / effective_n, 2), "\n")
#> Design effect: 1.01
```

A design effect \> 1 means weights reduce statistical power.

## Weights in Statistical Tests

### Weighted t-test

``` r
# Compare incomes between genders (weighted)
survey_data %>%
  t_test(income, group = gender, weights = sampling_weight)
#> ── Weighted t-Test Results ─────────────────────────────────────────────────────
#> • Grouping variable: gender
#> • Groups compared: Male vs. Female
#> • Weights variable: sampling_weight
#> • Confidence level: 95.0%
#> • Alternative hypothesis: two.sided
#> • Null hypothesis (mu): 0.000
#> 
#> 
#> --- income ---
#> 
#>   Male: mean = 3766.999, n = 1048.0
#>   Female: mean = 3721.379, n = 1153.0
#> 
#> Weighted t-test Results:
#> -------------------------------------------------------------------------------- 
#>         Assumption t_stat       df p_value mean_diff           conf_int sig
#>    Equal variances  0.751 2199.000   0.453     45.62 [-73.570, 164.809]    
#>  Unequal variances  0.751 2178.884   0.453     45.62 [-73.574, 164.813]    
#> -------------------------------------------------------------------------------- 
#> 
#> Effect Sizes:
#> ------------ 
#>  Variable Cohens_d Hedges_g Glass_Delta Effect_Size
#>    income    0.032    0.032       0.032  negligible
#> 
#> Signif. codes: 0 *** 0.001 ** 0.01 * 0.05
#> 
#> Effect Size Interpretation:
#> - Cohen's d: pooled standard deviation (classic)
#> - Hedges' g: bias-corrected Cohen's d (preferred)
#> - Glass' Delta: control group standard deviation only
#> - Small effect: |effect| ~ 0.2
#> - Medium effect: |effect| ~ 0.5
#> - Large effect: |effect| ~ 0.8
```

### Weighted ANOVA

``` r
# Compare life satisfaction across education levels
survey_data %>%
  oneway_anova(life_satisfaction, group = education,
               weights = sampling_weight)
#> ── Weighted One-Way ANOVA Results ──────────────────────────────────────────────
#> 
#> Dependent Variable: life_satisfaction
#> Grouping Variable: education
#> Weights Variable: sampling_weight
#> Null hypothesis: All group means are equal
#> Alternative hypothesis: At least one group mean differs
#> Confidence level: 95.0%
#> 
#> 
#> --- life_satisfaction ---
#> 
#> Weighted Descriptive Statistics by Group:
#>   Basic Secondary: mean = 3.208, sd = 1.243, n = 815.7
#>   Intermediate Secondary: mean = 3.698, sd = 1.109, n = 629.6
#>   Academic Secondary: mean = 3.851, sd = 0.996, n = 618.2
#>   University: mean = 4.040, sd = 0.961, n = 373.2
#> 
#> Weighted ANOVA Results:
#> -------------------------------------------------------------------------------- 
#>          Source Sum_Squares   df Mean_Square      F p_value sig
#>  Between Groups     241.130    3      80.377 65.359   <.001 ***
#>   Within Groups    2992.019 2433        1.23                   
#>           Total    3233.149 2436                               
#> -------------------------------------------------------------------------------- 
#> 
#> Assumption Tests:
#> ---------------- 
#>  Assumption Statistic df1  df2 p_value sig
#>       Welch    62.636   3 1216   <.001 ***
#> 
#> Effect Sizes:
#> ------------ 
#>           Variable Eta_Squared Epsilon_Squared Omega_Squared Effect_Size
#>  life_satisfaction       0.075           0.073         0.073      medium
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
#> 
#> Effect Size Interpretation:
#> - Eta-squared: Proportion of variance explained (biased upward)
#> - Epsilon-squared: Less biased than eta-squared
#> - Omega-squared: Unbiased estimate (preferred for publication)
#> - Small effect: eta-squared ~ 0.01, Medium effect: eta-squared ~ 0.06, Large effect: eta-squared ~ 0.14
#> 
#> Post-hoc tests: Use tukey_test() for pairwise comparisons
```

### Weighted Chi-square

``` r
# Test relationship between education and employment
survey_data %>%
  chi_square(education, employment, weights = sampling_weight)
#> ── Weighted Chi-Squared Test of Independence  ──────────────────────────────────
#> 
#> Variables: education x employment
#> Weights variable: sampling_weight
#> 
#> Observed Frequencies:
#>                         var2
#> var1                     Student Employed Unemployed Retired Other
#>   Basic Secondary              0      573         66     175    34
#>   Intermediate Secondary       0      420         52     139    29
#>   Academic Secondary          46      370         45     149    33
#>   University                  34      240         21      72    20
#> 
#> Expected Frequencies:
#>                         var2
#> var1                     Student Employed Unemployed Retired  Other
#>   Basic Secondary         26.942  539.851     61.967 180.175 39.066
#>   Intermediate Secondary  20.334  407.434     46.767 135.981 29.484
#>   Academic Secondary      20.429  409.344     46.986 136.618 29.622
#>   University              12.295  246.371     28.280  82.226 17.828
#> 
#> Chi-Squared Test Results:
#> -------------------------------------------------- 
#>  Chi_squared df p_value sig
#>      130.696 12   <.001 ***
#> -------------------------------------------------- 
#> 
#> Effect Sizes:
#> ---------------------------------------------------------------------- 
#>     Measure  Value p_value sig Interpretation
#>  Cramer's V  0.132   <.001 ***          Small
#>       Gamma -0.062   0.122               Weak
#> ---------------------------------------------------------------------- 
#> Table size: 4×5 | N = 2518
#> Note: Phi coefficient only shown for 2x2 tables
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05
```

### Weighted Correlations

``` r
# Correlation between age and income
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

## Grouped Analysis with Weights

All functions work seamlessly with dplyr’s group_by:

``` r
# Regional comparisons with weights
survey_data %>%
  group_by(region) %>%
  describe(age, income, life_satisfaction,
           weights = sampling_weight)
#> ── Weighted Descriptive Statistics ─────────────────────────────────────────────
#> 
#> ── Group: region = East ──
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#>           Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>                age   52.278     53   17.595    77   24    0.098       477.0
#>             income 3760.687   3600 1388.321  7200 1700    0.718       421.9
#>  life_satisfaction    3.623      4    1.203     4    2   -0.556       457.4
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> ── Group: region = West ──
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#>           Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>                age   50.067     49   16.927    77   24    0.170      1993.1
#>             income 3738.586   3500 1433.325  7200 1900    0.726      1738.1
#>  life_satisfaction    3.625      4    1.139     4    2   -0.481      1934.8
#> ────────────────────────────────────────────────────────────────────────────────
```

## Common Weight Issues

### 1. Extreme Weights

Check for outliers that might unduly influence results:

``` r
# Examine weight distribution
weight_stats <- survey_data %>%
  summarise(
    min_weight = min(sampling_weight, na.rm = TRUE),
    max_weight = max(sampling_weight, na.rm = TRUE),
    mean_weight = mean(sampling_weight, na.rm = TRUE),
    sd_weight = sd(sampling_weight, na.rm = TRUE)
  )
print(weight_stats)
#> # A tibble: 1 × 4
#>   min_weight max_weight mean_weight sd_weight
#>        <dbl>      <dbl>       <dbl>     <dbl>
#> 1      0.702       1.40        1.01     0.113

# Weights should typically range from 0.5 to 3.0
# Extreme weights (>5 or <0.2) may indicate problems
```

### 2. Missing Weights

Check how many cases have missing weights:

``` r
missing_weights <- sum(is.na(survey_data$sampling_weight))
total_cases <- nrow(survey_data)
cat("Cases with missing weights:", missing_weights, "/", total_cases,
    "(", round(missing_weights/total_cases * 100, 1), "%)\n")
#> Cases with missing weights: 0 / 2500 ( 0 %)
```

### 3. Weight Trimming

If weights are extreme, consider trimming:

``` r
# Create trimmed weights (cap at 5)
survey_data_trimmed <- survey_data %>%
  mutate(
    weight_trimmed = case_when(
      sampling_weight > 5 ~ 5,
      sampling_weight < 0.2 ~ 0.2,
      TRUE ~ sampling_weight
    )
  )

# Compare results
original <- w_mean(survey_data, income, weights = sampling_weight)
trimmed <- w_mean(survey_data_trimmed, income, weights = weight_trimmed)

cat("Original weighted mean:", original$results$weighted_mean, "\n")
#> Original weighted mean: 3743.099
cat("Trimmed weighted mean:", trimmed$results$weighted_mean, "\n")
#> Trimmed weighted mean: 3743.099
```

## Best Practices

### 1. Always Document Weight Source

Clearly explain: - How weights were calculated - What they adjust for -
Any limitations

### 2. Report Both Weighted and Unweighted

Show both for transparency:

``` r
# Create comparison table
comparison <- data.frame(
  Statistic = c("Mean Age", "Mean Income"),
  Unweighted = c(
    mean(survey_data$age, na.rm = TRUE),
    mean(survey_data$income, na.rm = TRUE)
  ),
  Weighted = c(
    w_mean(survey_data, age, weights = sampling_weight)$results$weighted_mean,
    w_mean(survey_data, income, weights = sampling_weight)$results$weighted_mean
  )
)
print(comparison)
#>     Statistic Unweighted  Weighted
#> 1    Mean Age    50.5496   50.5144
#> 2 Mean Income  3753.9341 3743.0994
```

### 3. Check Weight Impact

Large differences between weighted and unweighted results indicate: -
Sample is not representative - Weights are doing important work -
Results would be biased without weights

### 4. Consider Variance Estimation

Weights affect standard errors. The effective sample size shows this
impact.

## Complete Example

Here’s a full weighted analysis workflow:

``` r
# 1. Check weight properties
cat("=== Weight Distribution ===\n")
#> === Weight Distribution ===
weight_summary <- survey_data %>%
  summarise(
    n = n(),
    mean = mean(sampling_weight, na.rm = TRUE),
    sd = sd(sampling_weight, na.rm = TRUE),
    min = min(sampling_weight, na.rm = TRUE),
    max = max(sampling_weight, na.rm = TRUE)
  )
print(weight_summary)
#> # A tibble: 1 × 5
#>       n  mean    sd   min   max
#>   <int> <dbl> <dbl> <dbl> <dbl>
#> 1  2500  1.01 0.113 0.702  1.40

# 2. Compare weighted vs unweighted means
cat("\n=== Impact of Weights ===\n")
#> 
#> === Impact of Weights ===
vars_to_check <- c("age", "income", "life_satisfaction")
for (var in vars_to_check) {
  unweighted <- mean(survey_data[[var]], na.rm = TRUE)
  weighted_result <- w_mean(survey_data, !!sym(var), weights = sampling_weight)
  weighted <- weighted_result$results$weighted_mean
  diff_pct <- (weighted - unweighted) / unweighted * 100

  cat(sprintf("%s: Unweighted=%.2f, Weighted=%.2f (%.1f%% diff)\n",
              var, unweighted, weighted, diff_pct))
}
#> age: Unweighted=50.55, Weighted=50.51 (-0.1% diff)
#> income: Unweighted=3753.93, Weighted=3743.10 (-0.3% diff)
#> life_satisfaction: Unweighted=3.63, Weighted=3.62 (-0.1% diff)

# 3. Weighted analysis by group
cat("\n=== Weighted Group Comparisons ===\n")
#> 
#> === Weighted Group Comparisons ===
survey_data %>%
  group_by(region) %>%
  describe(income, weights = sampling_weight)
#> ── Weighted Descriptive Statistics ─────────────────────────────────────────────
#> 
#> ── Group: region = East ──
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#>  Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>    income 3760.687   3600 1388.321  7200 1700    0.718       421.9
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> ── Group: region = West ──
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#>  Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>    income 3738.586   3500 1433.325  7200 1900    0.726      1738.1
#> ────────────────────────────────────────────────────────────────────────────────

# 4. Weighted hypothesis test
cat("\n=== Weighted Statistical Test ===\n")
#> 
#> === Weighted Statistical Test ===
survey_data %>%
  t_test(income, group = gender, weights = sampling_weight)
#> ── Weighted t-Test Results ─────────────────────────────────────────────────────
#> • Grouping variable: gender
#> • Groups compared: Male vs. Female
#> • Weights variable: sampling_weight
#> • Confidence level: 95.0%
#> • Alternative hypothesis: two.sided
#> • Null hypothesis (mu): 0.000
#> 
#> 
#> --- income ---
#> 
#>   Male: mean = 3766.999, n = 1048.0
#>   Female: mean = 3721.379, n = 1153.0
#> 
#> Weighted t-test Results:
#> -------------------------------------------------------------------------------- 
#>         Assumption t_stat       df p_value mean_diff           conf_int sig
#>    Equal variances  0.751 2199.000   0.453     45.62 [-73.570, 164.809]    
#>  Unequal variances  0.751 2178.884   0.453     45.62 [-73.574, 164.813]    
#> -------------------------------------------------------------------------------- 
#> 
#> Effect Sizes:
#> ------------ 
#>  Variable Cohens_d Hedges_g Glass_Delta Effect_Size
#>    income    0.032    0.032       0.032  negligible
#> 
#> Signif. codes: 0 *** 0.001 ** 0.01 * 0.05
#> 
#> Effect Size Interpretation:
#> - Cohen's d: pooled standard deviation (classic)
#> - Hedges' g: bias-corrected Cohen's d (preferred)
#> - Glass' Delta: control group standard deviation only
#> - Small effect: |effect| ~ 0.2
#> - Medium effect: |effect| ~ 0.5
#> - Large effect: |effect| ~ 0.8
```

## Summary

Key takeaways about survey weights:

1.  **Always use weights** when they’re provided - they ensure
    representativeness
2.  **Check weight properties** - extreme values may need attention
3.  **Report effective N** - weights reduce statistical power
4.  **Document everything** - explain what weights adjust for
5.  **Compare weighted/unweighted** - large differences indicate
    importance

mariposa makes weighted analysis as easy as unweighted - just add the
`weights` parameter!
