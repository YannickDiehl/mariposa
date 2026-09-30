# Working with Survey Weights

``` r

library(mariposa)
library(dplyr)
data(survey_data)
```

## Overview

Survey weights correct for sampling biases so your results represent the
target population, not just your sample. Common reasons for weighting
include oversampling of small groups, differential non-response, and
post-stratification to known demographics.

In mariposa, adding weights is as simple as `weights = sampling_weight`
in any function call. This guide covers the dedicated `w_*` functions,
the effective sample size concept, and best practices.

## The Impact of Weights

``` r

# Without weights (describes the sample)
unweighted <- survey_data %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    mean_income = mean(income, na.rm = TRUE)
  )

# With weights (represents the population)
age_w <- w_mean(survey_data, age, weights = sampling_weight)
income_w <- w_mean(survey_data, income, weights = sampling_weight)

comparison <- data.frame(
  Variable = c("Age", "Income"),
  Unweighted = c(unweighted$mean_age, unweighted$mean_income),
  Weighted = c(age_w$results$weighted_mean, income_w$results$weighted_mean)
)
print(comparison)
#>   Variable Unweighted  Weighted
#> 1      Age    50.5496   50.5144
#> 2   Income  3753.9341 3743.0994
```

The difference between weighted and unweighted results reflects how
biased your sample is. Larger differences mean weights are doing more
important work.

## Weighted Statistics Functions

mariposa provides 11 `w_*` functions for individual weighted statistics.

### Central Tendency

``` r

# Weighted mean
w_mean(survey_data, income, weights = sampling_weight)
#> 
#> Weighted Mean Statistics
#> ------------------------
#> Weights: sampling_weight
#> 
#>   ---------------------------------
#>   Variable      Mean     N  Missing
#>   ---------------------------------
#>   income    3743.099  2201      315
#>   ---------------------------------

# Weighted median
w_median(survey_data, income, weights = sampling_weight)
#> 
#> Weighted Median Statistics
#> --------------------------
#> Weights: sampling_weight
#> 
#>   ---------------------------------
#>   Variable    Median     N  Missing
#>   ---------------------------------
#>   income    3500.000  2201      315
#>   ---------------------------------

# Weighted mode
w_modus(survey_data, education, weights = sampling_weight)
#> 
#> Weighted Mode Statistics
#> ------------------------
#> Weights: sampling_weight
#> 
#>   -----------------------------------------
#>   Variable   Mode                N  Missing
#>   -----------------------------------------
#>   education  Basic Secondary  2516        0
#>   -----------------------------------------
```

### Dispersion

``` r

# Weighted standard deviation
w_sd(survey_data, income, weights = sampling_weight)
#> 
#> Weighted Standard Deviation Statistics
#> --------------------------------------
#> Weights: sampling_weight
#> 
#>   ---------------------------------
#>   Variable        SD     N  Missing
#>   ---------------------------------
#>   income    1423.966  2201      315
#>   ---------------------------------

# Weighted variance
w_var(survey_data, income, weights = sampling_weight)
#> 
#> Weighted Variance Statistics
#> ----------------------------
#> Weights: sampling_weight
#> 
#>   ------------------------------------
#>   Variable     Variance     N  Missing
#>   ------------------------------------
#>   income    2027677.966  2201      315
#>   ------------------------------------

# Weighted interquartile range
w_iqr(survey_data, income, weights = sampling_weight)
#> 
#> Weighted Interquartile Range Statistics
#> ---------------------------------------
#> Weights: sampling_weight
#> 
#>   ---------------------------------
#>   Variable       IQR     N  Missing
#>   ---------------------------------
#>   income    1900.000  2201      315
#>   ---------------------------------
```

### Distribution Shape

``` r

# Weighted skewness
w_skew(survey_data, income, weights = sampling_weight)
#> 
#> Weighted Skewness Statistics
#> ----------------------------
#> Weights: sampling_weight
#> 
#>   ---------------------------------
#>   Variable  Skewness     N  Missing
#>   ---------------------------------
#>   income       0.725  2201      315
#>   ---------------------------------

# Weighted kurtosis
w_kurtosis(survey_data, income, weights = sampling_weight)
#> 
#> Weighted Excess Kurtosis Statistics
#> -----------------------------------
#> Weights: sampling_weight
#> 
#>   ---------------------------------
#>   Variable  Kurtosis     N  Missing
#>   ---------------------------------
#>   income       0.388  2201      315
#>   ---------------------------------
```

### Precision and Range

``` r

# Weighted standard error
w_se(survey_data, income, weights = sampling_weight)
#> 
#> Weighted Standard Error Statistics
#> ----------------------------------
#> Weights: sampling_weight
#> 
#>   -------------------------------
#>   Variable      SE     N  Missing
#>   -------------------------------
#>   income    30.353  2201      315
#>   -------------------------------

# Weighted quantiles
w_quantile(survey_data, income,
           probs = c(0.25, 0.5, 0.75),
           weights = sampling_weight)
#> 
#> Weighted Quantile Statistics
#> ----------------------------
#> Weights: sampling_weight
#> 
#>   -----------------------------------------------------
#>   Variable       25%       50%       75%     N  Missing
#>   -----------------------------------------------------
#>   income    2700.000  3500.000  4600.000  2201      315
#>   -----------------------------------------------------

# Weighted range
w_range(survey_data, income, weights = sampling_weight)
#> 
#> Weighted Range Statistics
#> -------------------------
#> Weights: sampling_weight
#> 
#>   ---------------------------------
#>   Variable     Range     N  Missing
#>   ---------------------------------
#>   income    7200.000  2201      315
#>   ---------------------------------
```

### Multiple Variables

All `w_*` functions accept multiple variables:

``` r

w_mean(survey_data, age, income, life_satisfaction,
       weights = sampling_weight)
#> 
#> Weighted Mean Statistics
#> ------------------------
#> Weights: sampling_weight
#> 
#>   ------------------------------------------
#>   Variable               Mean     N  Missing
#>   ------------------------------------------
#>   age                  50.514  2516        0
#>   income             3743.099  2201      315
#>   life_satisfaction     3.625  2437       79
#>   ------------------------------------------
```

## Effective Sample Size

Weighting reduces statistical precision. The *effective sample size*
(n\_{eff}) tells you how much information your weighted sample actually
carries:

n\_{eff} = \frac{\left(\sum w_i\right)^2}{\sum w_i^2}

``` r

actual_n <- nrow(survey_data)
age_w <- w_mean(survey_data, age, weights = sampling_weight)
effective_n <- age_w$results$effective_n

cat("Actual sample size:", actual_n, "\n")
#> Actual sample size: 2500
cat("Effective sample size:", round(effective_n), "\n")
#> Effective sample size: 2469
cat("Design effect:", round(actual_n / effective_n, 2), "\n")
#> Design effect: 1.01
```

A design effect of 1.0 means weights have no impact on precision. Values
above 1 mean larger standard errors than an unweighted sample of the
same size.

## Weights in Analysis Functions

Every analysis function in mariposa supports the `weights` argument:

``` r

# Descriptive statistics
survey_data %>%
  describe(income, life_satisfaction, weights = sampling_weight)
#> 
#> Weighted Descriptive Statistics
#> -------------------------------
#> 
#>   --------------------------------------------------------------------------------------------
#>   Variable               Mean    Median        SD     Range       IQR  Skewness     N  Missing
#>   --------------------------------------------------------------------------------------------
#>   income             3743.099  3500.000  1423.966  7200.000  1900.000     0.725  2201      315
#>   life_satisfaction     3.625     4.000     1.152     4.000     2.000    -0.499  2437       79
#>   --------------------------------------------------------------------------------------------
```

``` r

# Frequency tables
survey_data %>%
  frequency(education, weights = sampling_weight)
#> 
#> Weighted Frequency Analysis Results
#> -----------------------------------
#> 
#> education (Highest educational attainment)
#> # total N=2516 valid N=2516
#> 
#> +------------------------+------+--------+---------+--------+
#> | Value                  |    N |  Raw % | Valid % | Cum. % |
#> +------------------------+------+--------+---------+--------+
#> | Basic Secondary        |  848 |  33.71 |   33.71 |  33.71 |
#> | Intermediate Secondary |  641 |  25.47 |   25.47 |  59.18 |
#> | Academic Secondary     |  642 |  25.51 |   25.51 |  84.69 |
#> | University             |  385 |  15.31 |   15.31 | 100.00 |
#> +------------------------+------+--------+---------+--------+
#> | Total                  | 2516 | 100.00 |  100.00 |        |
#> +------------------------+------+--------+---------+--------+
```

``` r

# t-test
survey_data %>%
  t_test(income, group = gender, weights = sampling_weight)
#> t-Test: income by gender [Weighted]
#>   t(2178.7) = 0.751, p = 0.453, g = 0.032 (negligible), N = 2201
#> Use summary() for detailed output.
```

``` r

# ANOVA
survey_data %>%
  oneway_anova(life_satisfaction, group = education,
               weights = sampling_weight)
#> One-Way ANOVA: life_satisfaction by education [Weighted]
#>   F(3, 2432) = 65.333, p < 0.001 ***, eta2 = 0.075 (medium), N = 2437
#> Use summary() for detailed output.
```

``` r

# Chi-square
survey_data %>%
  chi_square(education, employment, weights = sampling_weight)
#> Chi-Squared Test: education x employment [Weighted]
#>   chi2(12) = 130.696, p < 0.001 ***, V = 0.132 (small), N = 2518
#> Use summary() for detailed output.
```

``` r

# Correlation
survey_data %>%
  pearson_cor(age, income, weights = sampling_weight)
#> Pearson Correlation: age x income [Weighted]
#>   r = -0.005, p = 0.828, N = 2201
#> Use summary() for detailed output.
```

``` r

# Regression
survey_data %>%
  linear_regression(life_satisfaction ~ age + income,
                    weights = sampling_weight)
#> Linear Regression: life_satisfaction ~ age + income [Weighted]
#>   R2 = 0.203, adj.R2 = 0.202, F(2, 2127) = 270.42, p < 0.001 ***, N = 2130
#> Use summary() for detailed output.
```

## Grouped Weighted Analysis

All functions work seamlessly with
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html):

``` r

survey_data %>%
  group_by(region) %>%
  describe(age, income, life_satisfaction,
           weights = sampling_weight)
#> 
#> Weighted Descriptive Statistics
#> -------------------------------
#> 
#> Group: region = East
#> --------------------
#> 
#>   -------------------------------------------------------------------------------------------
#>   Variable               Mean    Median        SD     Range       IQR  Skewness    N  Missing
#>   -------------------------------------------------------------------------------------------
#>   age                  52.278    53.000    17.595    77.000    24.000     0.098  509        0
#>   income             3760.687  3600.000  1388.321  7200.000  1700.000     0.721  449       60
#>   life_satisfaction     3.623     4.000     1.203     4.000     2.000    -0.558  488       21
#>   -------------------------------------------------------------------------------------------
#> 
#> Group: region = West
#> --------------------
#> 
#>   --------------------------------------------------------------------------------------------
#>   Variable               Mean    Median        SD     Range       IQR  Skewness     N  Missing
#>   --------------------------------------------------------------------------------------------
#>   age                  50.067    49.000    16.927    77.000    24.000     0.170  2007        0
#>   income             3738.586  3500.000  1433.325  7200.000  1900.000     0.727  1751      256
#>   life_satisfaction     3.625     4.000     1.139     4.000     2.000    -0.481  1949       58
#>   --------------------------------------------------------------------------------------------
```

``` r

survey_data %>%
  group_by(region) %>%
  t_test(income, group = gender, weights = sampling_weight)
#> [region = East]
#> t-Test: income by gender [Weighted]
#>   t(431.2) = 1.674, p = 0.095, g = 0.158 (negligible), N = 450
#> [region = West]
#> t-Test: income by gender [Weighted]
#>   t(1740.2) = 0.009, p = 0.993, g = 0.000 (negligible), N = 1751
#> Use summary() for detailed output.
```

## Diagnosing Weight Issues

### Checking Weight Distribution

``` r

weight_stats <- survey_data %>%
  summarise(
    min = min(sampling_weight, na.rm = TRUE),
    max = max(sampling_weight, na.rm = TRUE),
    mean = mean(sampling_weight, na.rm = TRUE),
    sd = sd(sampling_weight, na.rm = TRUE)
  )
print(weight_stats)
#> # A tibble: 1 × 4
#>     min   max  mean    sd
#>   <dbl> <dbl> <dbl> <dbl>
#> 1 0.702  1.40  1.01 0.113
```

A rule of thumb: weights should range from about 0.2 to 5. Extreme
values (below 0.1 or above 10) may indicate problems with the weighting
scheme.

### Missing Weights

``` r

missing <- sum(is.na(survey_data$sampling_weight))
total <- nrow(survey_data)
cat("Missing weights:", missing, "/", total,
    "(", round(missing / total * 100, 1), "%)\n")
#> Missing weights: 0 / 2500 ( 0 %)
```

### Weight Trimming

If weights are extreme, trim them to reduce variance at the cost of a
small increase in bias:

``` r

trimmed <- survey_data %>%
  mutate(
    weight_trimmed = case_when(
      sampling_weight > 5 ~ 5,
      sampling_weight < 0.2 ~ 0.2,
      TRUE ~ sampling_weight
    )
  )

original <- w_mean(survey_data, income, weights = sampling_weight)
trimmed_result <- w_mean(trimmed, income, weights = weight_trimmed)

cat("Original weighted mean:", round(original$results$weighted_mean, 1), "\n")
#> Original weighted mean: 3743.1
cat("Trimmed weighted mean:", round(trimmed_result$results$weighted_mean, 1), "\n")
#> Trimmed weighted mean: 3743.1
```

## Complete Example

``` r

# 1. Inspect weight properties
survey_data %>%
  summarise(
    n = n(),
    mean_weight = mean(sampling_weight, na.rm = TRUE),
    sd_weight = sd(sampling_weight, na.rm = TRUE),
    min_weight = min(sampling_weight, na.rm = TRUE),
    max_weight = max(sampling_weight, na.rm = TRUE)
  )
#> # A tibble: 1 × 5
#>       n mean_weight sd_weight min_weight max_weight
#>   <int>       <dbl>     <dbl>      <dbl>      <dbl>
#> 1  2500        1.01     0.113      0.702       1.40

# 2. Compare weighted vs unweighted
vars <- c("age", "income", "life_satisfaction")
for (var in vars) {
  unw <- mean(survey_data[[var]], na.rm = TRUE)
  w_result <- w_mean(survey_data, !!sym(var), weights = sampling_weight)
  w <- w_result$results$weighted_mean
  cat(sprintf("%s: unweighted = %.2f, weighted = %.2f (diff = %.1f%%)\n",
              var, unw, w, (w - unw) / unw * 100))
}
#> age: unweighted = 50.55, weighted = 50.51 (diff = -0.1%)
#> income: unweighted = 3753.93, weighted = 3743.10 (diff = -0.3%)
#> life_satisfaction: unweighted = 3.63, weighted = 3.62 (diff = -0.1%)

# 3. Weighted group comparison
survey_data %>%
  group_by(region) %>%
  describe(income, weights = sampling_weight)
#> 
#> Weighted Descriptive Statistics
#> -------------------------------
#> 
#> Group: region = East
#> --------------------
#> 
#>   ----------------------------------------------------------------------------------
#>   Variable      Mean    Median        SD     Range       IQR  Skewness    N  Missing
#>   ----------------------------------------------------------------------------------
#>   income    3760.687  3600.000  1388.321  7200.000  1700.000     0.721  449       60
#>   ----------------------------------------------------------------------------------
#> 
#> Group: region = West
#> --------------------
#> 
#>   -----------------------------------------------------------------------------------
#>   Variable      Mean    Median        SD     Range       IQR  Skewness     N  Missing
#>   -----------------------------------------------------------------------------------
#>   income    3738.586  3500.000  1433.325  7200.000  1900.000     0.727  1751      256
#>   -----------------------------------------------------------------------------------

# 4. Weighted hypothesis test
survey_data %>%
  t_test(income, group = gender, weights = sampling_weight)
#> t-Test: income by gender [Weighted]
#>   t(2178.7) = 0.751, p = 0.453, g = 0.032 (negligible), N = 2201
#> Use summary() for detailed output.
```

## Best Practices

1.  **Always use weights when they are provided.** They correct for
    sampling biases and make your results generalizable.

2.  **Report both weighted and unweighted sample sizes.** The actual *n*
    tells readers about data collection; the effective *n* tells them
    about statistical precision.

3.  **Check for extreme weights.** Large variation inflates standard
    errors and reduces statistical power. Consider trimming if
    necessary.

4.  **Document the weight source.** Explain how weights were calculated
    and what they adjust for (e.g., age, gender, region
    post-stratification).

5.  **Report the effective sample size.** This tells readers how much
    precision was lost due to weighting.

## Summary

1.  Survey weights make results **representative of the population**,
    not just the sample
2.  The 11 `w_*` functions provide individual weighted statistics
3.  **All analysis functions** support weights via `weights =`
4.  The **effective sample size** measures how much precision weighting
    costs
5.  **Check and document** weight distributions, extreme values, and
    missing weights

## Next Steps

- Get started with the package — see
  [`vignette("introduction")`](https://YannickDiehl.github.io/mariposa/articles/introduction.md)
- Explore your data — see
  [`vignette("descriptive-statistics")`](https://YannickDiehl.github.io/mariposa/articles/descriptive-statistics.md)
- Test hypotheses — see
  [`vignette("hypothesis-testing")`](https://YannickDiehl.github.io/mariposa/articles/hypothesis-testing.md)
- Import weighted data from SPSS — see
  [`vignette("data-io")`](https://YannickDiehl.github.io/mariposa/articles/data-io.md)
