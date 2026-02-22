## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)

## ----setup--------------------------------------------------------------------
library(mariposa)
library(dplyr)
data(survey_data)

## -----------------------------------------------------------------------------
# Without weights (describes the sample)
unweighted_stats <- survey_data %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    mean_satisfaction = mean(life_satisfaction, na.rm = TRUE)
  )
print(unweighted_stats)

# With weights (represents the population)
age_weighted <- w_mean(survey_data, age, weights = sampling_weight)
satisfaction_weighted <- w_mean(survey_data, life_satisfaction,
                                weights = sampling_weight)

weighted_stats <- data.frame(
  mean_age = age_weighted$results$weighted_mean,
  mean_satisfaction = satisfaction_weighted$results$weighted_mean
)
print(weighted_stats)

## -----------------------------------------------------------------------------
# Weighted mean
w_mean(survey_data, income, weights = sampling_weight)

# Weighted median
w_median(survey_data, income, weights = sampling_weight)

# Weighted mode (most common value)
w_modus(survey_data, education, weights = sampling_weight)

## -----------------------------------------------------------------------------
# Weighted standard deviation
w_sd(survey_data, income, weights = sampling_weight)

# Weighted variance
w_var(survey_data, income, weights = sampling_weight)

# Weighted interquartile range
w_iqr(survey_data, income, weights = sampling_weight)

## -----------------------------------------------------------------------------
# Weighted skewness
w_skew(survey_data, income, weights = sampling_weight)

# Weighted kurtosis
w_kurtosis(survey_data, income, weights = sampling_weight)

## -----------------------------------------------------------------------------
# Weighted quantiles (25th, 50th, 75th percentiles)
w_quantile(survey_data, income,
           probs = c(0.25, 0.5, 0.75),
           weights = sampling_weight)

# Weighted standard error
w_se(survey_data, income, weights = sampling_weight)

## -----------------------------------------------------------------------------
actual_n <- nrow(survey_data)
age_weighted <- w_mean(survey_data, age, weights = sampling_weight)
effective_n <- age_weighted$results$effective_n

cat("Actual sample size:", actual_n, "\n")
cat("Effective sample size:", round(effective_n), "\n")
cat("Design effect:", round(actual_n / effective_n, 2), "\n")

## -----------------------------------------------------------------------------
survey_data %>%
  t_test(income, group = gender, weights = sampling_weight)

## -----------------------------------------------------------------------------
survey_data %>%
  oneway_anova(life_satisfaction, group = education,
               weights = sampling_weight)

## -----------------------------------------------------------------------------
survey_data %>%
  chi_square(education, employment, weights = sampling_weight)

## -----------------------------------------------------------------------------
survey_data %>%
  pearson_cor(age, income, weights = sampling_weight)

## -----------------------------------------------------------------------------
survey_data %>%
  group_by(region) %>%
  describe(age, income, life_satisfaction,
           weights = sampling_weight)

## -----------------------------------------------------------------------------
weight_stats <- survey_data %>%
  summarise(
    min_weight = min(sampling_weight, na.rm = TRUE),
    max_weight = max(sampling_weight, na.rm = TRUE),
    mean_weight = mean(sampling_weight, na.rm = TRUE),
    sd_weight = sd(sampling_weight, na.rm = TRUE)
  )
print(weight_stats)

## -----------------------------------------------------------------------------
missing_weights <- sum(is.na(survey_data$sampling_weight))
total_cases <- nrow(survey_data)
cat("Cases with missing weights:", missing_weights, "/", total_cases,
    "(", round(missing_weights / total_cases * 100, 1), "%)\n")

## -----------------------------------------------------------------------------
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
cat("Trimmed weighted mean:", round(trimmed$results$weighted_mean, 1), "\n")

## -----------------------------------------------------------------------------
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

# 2. Compare weighted vs unweighted
vars_to_check <- c("age", "income", "life_satisfaction")
for (var in vars_to_check) {
  unw <- mean(survey_data[[var]], na.rm = TRUE)
  w_result <- w_mean(survey_data, !!sym(var), weights = sampling_weight)
  w <- w_result$results$weighted_mean
  cat(sprintf("%s: unweighted = %.2f, weighted = %.2f (diff = %.1f%%)\n",
              var, unw, w, (w - unw) / unw * 100))
}

# 3. Weighted group comparisons
survey_data %>%
  group_by(region) %>%
  describe(income, weights = sampling_weight)

# 4. Weighted hypothesis test
survey_data %>%
  t_test(income, group = gender, weights = sampling_weight)

