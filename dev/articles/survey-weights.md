# Working with Survey Weights

``` r
library(mariposa)
library(dplyr)
data(survey_data)
```

## Overview

Survey weights adjust your analysis to make results representative of
the target population. Without weights, your results only describe your
sample — which might be biased if some groups were over- or
under-represented.

Common reasons for weighting:

- **Oversampling**: Deliberately surveying more people from small groups
  to ensure adequate sample sizes
- **Non-response**: Some groups are less likely to participate, creating
  bias
- **Post-stratification**: Adjusting the sample to match known
  population demographics

In mariposa, adding weights is as simple as including
`weights = sampling_weight` in any function call.

## Impact of Weights

Let’s see the difference weights make:

``` r
# Without weights (describes the sample)
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

# With weights (represents the population)
age_weighted <- w_mean(survey_data, age, weights = sampling_weight)
satisfaction_weighted <- w_mean(survey_data, life_satisfaction,
                                weights = sampling_weight)

weighted_stats <- data.frame(
  mean_age = age_weighted$results$weighted_mean,
  mean_satisfaction = satisfaction_weighted$results$weighted_mean
)
print(weighted_stats)
#>   mean_age mean_satisfaction
#> 1  50.5144          3.624863
```

The difference between weighted and unweighted results tells you how
biased your sample is. Larger differences mean weights are doing more
important work.

## Weighted Statistics Functions

mariposa provides specialized `w_*` functions for individual weighted
statistics.

### Central Tendency

``` r
# Weighted mean
w_mean(survey_data, income, weights = sampling_weight)
#> 
#> Weighted Mean Statistics
#> ------------------------
#> 
#> --- income ---
#>  Variable weighted_mean Effective_N
#>    income      3743.099      2158.9

# Weighted median
w_median(survey_data, income, weights = sampling_weight)
#> 
#> Weighted Median Statistics
#> --------------------------
#> 
#> --- income ---
#>  Variable weighted_median Effective_N
#>    income            3500      2158.9

# Weighted mode (most common value)
w_modus(survey_data, education, weights = sampling_weight)
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
w_sd(survey_data, income, weights = sampling_weight)
#> 
#> Weighted Standard Deviation Statistics
#> --------------------------------------
#> 
#> --- income ---
#>  Variable weighted_sd Effective_N
#>    income    1423.966      2158.9

# Weighted variance
w_var(survey_data, income, weights = sampling_weight)
#> 
#> Weighted Variance Statistics
#> ----------------------------
#> 
#> --- income ---
#>  Variable weighted_var Effective_N
#>    income      2027678      2158.9

# Weighted interquartile range
w_iqr(survey_data, income, weights = sampling_weight)
#> 
#> Weighted Interquartile Range Statistics
#> ---------------------------------------
#> 
#> --- income ---
#>  Variable weighted_iqr Effective_N
#>    income         1900      2158.9
```

### Distribution Shape

``` r
# Weighted skewness
w_skew(survey_data, income, weights = sampling_weight)
#> 
#> Weighted Skewness Statistics
#> ----------------------------
#> 
#> --- income ---
#>  Variable weighted_skew Effective_N
#>    income         0.725      2158.9

# Weighted kurtosis
w_kurtosis(survey_data, income, weights = sampling_weight)
#> 
#> Weighted Excess Kurtosis Statistics
#> -----------------------------------
#> 
#> --- income ---
#>  Variable weighted_kurtosis Effective_N
#>    income             0.388      2158.9
```

### Other Useful Functions

``` r
# Weighted quantiles (25th, 50th, 75th percentiles)
w_quantile(survey_data, income,
           probs = c(0.25, 0.5, 0.75),
           weights = sampling_weight)
#> 
#> Weighted Quantile Statistics
#> ----------------------------
#>  Variable Quantile Value    N Effective_N         Weights
#>    income      25%  2700 2186      2158.9 sampling_weight
#>    income      50%  3500 2186      2158.9 sampling_weight
#>    income      75%  4600 2186      2158.9 sampling_weight
#> ----------------------------------------

# Weighted standard error
w_se(survey_data, income, weights = sampling_weight)
#> 
#> Weighted Standard Error Statistics
#> ----------------------------------
#> 
#> --- income ---
#>  Variable weighted_se Effective_N
#>    income      30.353      2158.9
```

## Effective Sample Size

Weighting reduces the statistical precision of your estimates. The
*effective sample size* ($n_{eff}$) tells you how much information your
weighted sample actually carries:

$$n_{eff} = \frac{\left( \sum w_{i} \right)^{2}}{\sum w_{i}^{2}}$$

The *design effect* is the ratio of actual to effective sample size:

``` r
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

A design effect of 1.0 means weights have no impact on precision. Values
above 1 mean the effective sample is smaller than the actual sample —
your standard errors are larger than they would be without weights.

## Weights in Statistical Tests

All main analysis functions support the `weights` argument:

### Weighted t-test

``` r
survey_data %>%
  t_test(income, group = gender, weights = sampling_weight)
#> t-Test: income by gender [Weighted]
#>   t(2178.9) = 0.751, p = 0.453 , g = 0.032 (negligible), N = 2201
```

### Weighted ANOVA

``` r
survey_data %>%
  oneway_anova(life_satisfaction, group = education,
               weights = sampling_weight)
#> One-Way ANOVA: life_satisfaction by education [Weighted]
#>   F(3, 2433) = 65.359, p < 0.001 ***, eta2 = 0.075 (medium), N = 2437
```

### Weighted Chi-square

``` r
survey_data %>%
  chi_square(education, employment, weights = sampling_weight)
#> Chi-Squared Test: education × employment [Weighted]
#>   chi2(12) = 130.696, p < 0.001 ***, V = 0.132 (small), N = 2518
```

### Weighted Correlations

``` r
survey_data %>%
  pearson_cor(age, income, weights = sampling_weight)
#> Pearson Correlation: age x income [Weighted]
#>   r = -0.005, p = 0.828 , N = 2201
```

## Grouped Analysis with Weights

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
#> ----------------------------------------
#>           Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>                age   52.278     53   17.595    77   24    0.098       477.0
#>             income 3760.687   3600 1388.321  7200 1700    0.718       421.9
#>  life_satisfaction    3.623      4    1.203     4    2   -0.556       457.4
#> ----------------------------------------
#> 
#> Group: region = West
#> --------------------
#> ----------------------------------------
#>           Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>                age   50.067     49   16.927    77   24    0.170      1993.1
#>             income 3738.586   3500 1433.325  7200 1900    0.726      1738.1
#>  life_satisfaction    3.625      4    1.139     4    2   -0.481      1934.8
#> ----------------------------------------
```

## Diagnosing Weight Issues

### Extreme Weights

Weights that are very large or very small can destabilize your
estimates. Check the distribution of your weights:

``` r
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
```

A rule of thumb: weights should typically range from about 0.2 to 5.
Extreme values (below 0.1 or above 10) may indicate problems with the
weighting scheme.

### Missing Weights

Check how many cases lack weight values:

``` r
missing_weights <- sum(is.na(survey_data$sampling_weight))
total_cases <- nrow(survey_data)
cat("Cases with missing weights:", missing_weights, "/", total_cases,
    "(", round(missing_weights / total_cases * 100, 1), "%)\n")
#> Cases with missing weights: 0 / 2500 ( 0 %)
```

### Weight Trimming

If weights are extreme, consider trimming them to reduce variance at the
cost of a small increase in bias:

``` r
survey_data_trimmed <- survey_data %>%
  mutate(
    weight_trimmed = case_when(
      sampling_weight > 5 ~ 5,
      sampling_weight < 0.2 ~ 0.2,
      TRUE ~ sampling_weight
    )
  )

original <- w_mean(survey_data, income, weights = sampling_weight)
trimmed <- w_mean(survey_data_trimmed, income, weights = weight_trimmed)

cat("Original weighted mean:", round(original$results$weighted_mean, 1), "\n")
#> Original weighted mean: 3743.1
cat("Trimmed weighted mean:", round(trimmed$results$weighted_mean, 1), "\n")
#> Trimmed weighted mean: 3743.1
```

## Best Practices

1.  **Always use weights when they are provided.** They ensure your
    results generalize to the population, not just the sample.
2.  **Report both weighted and unweighted results** for transparency:

``` r
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

3.  **Document the weight source.** Explain how weights were calculated
    and what they adjust for.
4.  **Report the effective sample size.** This tells readers about the
    precision of your estimates.
5.  **Check for extreme weights.** Large variation in weights inflates
    standard errors and reduces power.

## Complete Example

A full weighted analysis workflow:

``` r
# 1. Inspect weight properties
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

# 2. Compare weighted vs unweighted
vars_to_check <- c("age", "income", "life_satisfaction")
for (var in vars_to_check) {
  unw <- mean(survey_data[[var]], na.rm = TRUE)
  w_result <- w_mean(survey_data, !!sym(var), weights = sampling_weight)
  w <- w_result$results$weighted_mean
  cat(sprintf("%s: unweighted = %.2f, weighted = %.2f (diff = %.1f%%)\n",
              var, unw, w, (w - unw) / unw * 100))
}
#> age: unweighted = 50.55, weighted = 50.51 (diff = -0.1%)
#> income: unweighted = 3753.93, weighted = 3743.10 (diff = -0.3%)
#> life_satisfaction: unweighted = 3.63, weighted = 3.62 (diff = -0.1%)

# 3. Weighted group comparisons
survey_data %>%
  group_by(region) %>%
  describe(income, weights = sampling_weight)
#> 
#> Weighted Descriptive Statistics
#> -------------------------------
#> 
#> Group: region = East
#> --------------------
#> ----------------------------------------
#>  Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>    income 3760.687   3600 1388.321  7200 1700    0.718       421.9
#> ----------------------------------------
#> 
#> Group: region = West
#> --------------------
#> ----------------------------------------
#>  Variable     Mean Median       SD Range  IQR Skewness Effective_N
#>    income 3738.586   3500 1433.325  7200 1900    0.726      1738.1
#> ----------------------------------------

# 4. Weighted hypothesis test
survey_data %>%
  t_test(income, group = gender, weights = sampling_weight)
#> t-Test: income by gender [Weighted]
#>   t(2178.9) = 0.751, p = 0.453 , g = 0.032 (negligible), N = 2201
```

## Summary

1.  **Always use weights** when they are available — they correct for
    sampling biases
2.  **Check weight properties** — extreme values may need trimming
3.  **Report the effective $n$** — weights reduce statistical precision
4.  **Document everything** — explain what weights adjust for
5.  In mariposa, adding weights is as simple as
    `weights = sampling_weight`

## Next Steps

- Get started with the package — see
  [`vignette("introduction")`](https://YannickDiehl.github.io/mariposa/dev/articles/introduction.md)
- Explore your data — see
  [`vignette("descriptive-statistics")`](https://YannickDiehl.github.io/mariposa/dev/articles/descriptive-statistics.md)
- Test hypotheses — see
  [`vignette("hypothesis-testing")`](https://YannickDiehl.github.io/mariposa/dev/articles/hypothesis-testing.md)
- Measure relationships — see
  [`vignette("correlation-analysis")`](https://YannickDiehl.github.io/mariposa/dev/articles/correlation-analysis.md)
