## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(mariposa)
library(dplyr)
data(survey_data)

## -----------------------------------------------------------------------------
# Without weights (biased)
unweighted_stats <- survey_data %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    mean_satisfaction = mean(life_satisfaction, na.rm = TRUE)
  )
print(unweighted_stats)

# With weights (representative)
age_weighted <- w_mean(survey_data, age, weights = sampling_weight)
satisfaction_weighted <- w_mean(survey_data, life_satisfaction, weights = sampling_weight)

weighted_stats <- data.frame(
  mean_age = age_weighted$results$weighted_mean,
  mean_satisfaction = satisfaction_weighted$results$weighted_mean
)
print(weighted_stats)

## -----------------------------------------------------------------------------
# Weighted mean
mean_result <- w_mean(survey_data, income, weights = sampling_weight)
print(mean_result)

# Weighted median
median_result <- w_median(survey_data, income, weights = sampling_weight)
print(median_result)

# Weighted mode
mode_result <- w_modus(survey_data, education, weights = sampling_weight)
print(mode_result)

## -----------------------------------------------------------------------------
# Weighted standard deviation
sd_result <- w_sd(survey_data, income, weights = sampling_weight)
print(sd_result)

# Weighted variance
var_result <- w_var(survey_data, income, weights = sampling_weight)
print(var_result)

# Weighted IQR
iqr_result <- w_iqr(survey_data, income, weights = sampling_weight)
print(iqr_result)

## -----------------------------------------------------------------------------
# Weighted skewness
skew_result <- w_skew(survey_data, income, weights = sampling_weight)
print(skew_result)

# Weighted kurtosis
kurt_result <- w_kurtosis(survey_data, income, weights = sampling_weight)
print(kurt_result)

## -----------------------------------------------------------------------------
# Weighted quantiles (25th, 50th, 75th percentiles)
quantile_result <- w_quantile(survey_data, income,
                              probs = c(0.25, 0.5, 0.75),
                              weights = sampling_weight)
print(quantile_result)

# Weighted standard error
se_result <- w_se(survey_data, income, weights = sampling_weight)
print(se_result)

## -----------------------------------------------------------------------------
# Compare actual vs effective N
actual_n <- nrow(survey_data)
age_weighted <- w_mean(survey_data, age, weights = sampling_weight)
effective_n <- age_weighted$results$effective_n

cat("Actual sample size:", actual_n, "\n")
cat("Effective sample size:", round(effective_n), "\n")
cat("Design effect:", round(actual_n / effective_n, 2), "\n")

## -----------------------------------------------------------------------------
# Compare incomes between genders (weighted)
survey_data %>%
  t_test(income, group = gender, weights = sampling_weight)

## -----------------------------------------------------------------------------
# Compare life satisfaction across education levels
survey_data %>%
  oneway_anova(life_satisfaction, group = education,
               weights = sampling_weight)

## -----------------------------------------------------------------------------
# Test relationship between education and employment
survey_data %>%
  chi_square(education, employment, weights = sampling_weight)

## -----------------------------------------------------------------------------
# Correlation between age and income
survey_data %>%
  pearson_cor(age, income, weights = sampling_weight)

## -----------------------------------------------------------------------------
# Regional comparisons with weights
survey_data %>%
  group_by(region) %>%
  describe(age, income, life_satisfaction,
           weights = sampling_weight)

## -----------------------------------------------------------------------------
# Examine weight distribution
weight_stats <- survey_data %>%
  summarise(
    min_weight = min(sampling_weight, na.rm = TRUE),
    max_weight = max(sampling_weight, na.rm = TRUE),
    mean_weight = mean(sampling_weight, na.rm = TRUE),
    sd_weight = sd(sampling_weight, na.rm = TRUE)
  )
print(weight_stats)

# Weights should typically range from 0.5 to 3.0
# Extreme weights (>5 or <0.2) may indicate problems

## -----------------------------------------------------------------------------
missing_weights <- sum(is.na(survey_data$sampling_weight))
total_cases <- nrow(survey_data)
cat("Cases with missing weights:", missing_weights, "/", total_cases,
    "(", round(missing_weights/total_cases * 100, 1), "%)\n")

## -----------------------------------------------------------------------------
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
cat("Trimmed weighted mean:", trimmed$results$weighted_mean, "\n")

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
# 1. Check weight properties
cat("=== Weight Distribution ===\n")
weight_summary <- survey_data %>%
  summarise(
    n = n(),
    mean = mean(sampling_weight, na.rm = TRUE),
    sd = sd(sampling_weight, na.rm = TRUE),
    min = min(sampling_weight, na.rm = TRUE),
    max = max(sampling_weight, na.rm = TRUE)
  )
print(weight_summary)

# 2. Compare weighted vs unweighted means
cat("\n=== Impact of Weights ===\n")
vars_to_check <- c("age", "income", "life_satisfaction")
for (var in vars_to_check) {
  unweighted <- mean(survey_data[[var]], na.rm = TRUE)
  weighted_result <- w_mean(survey_data, !!sym(var), weights = sampling_weight)
  weighted <- weighted_result$results$weighted_mean
  diff_pct <- (weighted - unweighted) / unweighted * 100

  cat(sprintf("%s: Unweighted=%.2f, Weighted=%.2f (%.1f%% diff)\n",
              var, unweighted, weighted, diff_pct))
}

# 3. Weighted analysis by group
cat("\n=== Weighted Group Comparisons ===\n")
survey_data %>%
  group_by(region) %>%
  describe(income, weights = sampling_weight)

# 4. Weighted hypothesis test
cat("\n=== Weighted Statistical Test ===\n")
survey_data %>%
  t_test(income, group = gender, weights = sampling_weight)

