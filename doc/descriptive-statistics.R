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
survey_data %>% describe(age)

## -----------------------------------------------------------------------------
survey_data %>%
  describe(age, income, life_satisfaction)

## -----------------------------------------------------------------------------
survey_data %>%
  describe(age, income, life_satisfaction,
           weights = sampling_weight)

## -----------------------------------------------------------------------------
survey_data %>%
  group_by(region) %>%
  describe(age, income, weights = sampling_weight)

## -----------------------------------------------------------------------------
# Just the essentials
survey_data %>%
  describe(age, income, show = c("mean", "sd", "range"))

# Everything available
survey_data %>%
  describe(age, show = "all")

## -----------------------------------------------------------------------------
survey_data %>%
  frequency(education)

## -----------------------------------------------------------------------------
survey_data %>%
  frequency(education, weights = sampling_weight)

## -----------------------------------------------------------------------------
survey_data %>%
  frequency(education, employment, region,
            weights = sampling_weight)

## -----------------------------------------------------------------------------
survey_data %>%
  group_by(gender) %>%
  frequency(education, weights = sampling_weight)

## -----------------------------------------------------------------------------
survey_data %>%
  crosstab(education, employment)

## -----------------------------------------------------------------------------
survey_data %>%
  crosstab(education, employment, weights = sampling_weight)

## -----------------------------------------------------------------------------
survey_data %>%
  group_by(region) %>%
  crosstab(education, employment, weights = sampling_weight)

## -----------------------------------------------------------------------------
# Check for outliers and missing data
survey_data %>%
  describe(age, income, show = "all")

## -----------------------------------------------------------------------------
# Compare weighted vs unweighted
unweighted_mean <- survey_data %>%
  summarise(mean_age = mean(age, na.rm = TRUE)) %>%
  pull()

weighted_result <- w_mean(survey_data, age, weights = sampling_weight)

cat("Unweighted mean age:", unweighted_mean, "\n")
cat("Weighted mean age:", weighted_result$results$weighted_mean, "\n")

## -----------------------------------------------------------------------------
# 1. Overall summary
cat("=== Overall Summary ===\n")
survey_data %>%
  describe(age, income, life_satisfaction,
           weights = sampling_weight,
           show = "short")

# 2. Check distributions
cat("\n=== Key Frequencies ===\n")
survey_data %>%
  frequency(education, employment,
            weights = sampling_weight)

# 3. Examine relationships
cat("\n=== Education by Employment ===\n")
survey_data %>%
  crosstab(education, employment,
           weights = sampling_weight)

# 4. Group comparisons
cat("\n=== Regional Differences ===\n")
survey_data %>%
  group_by(region) %>%
  describe(income, life_satisfaction,
           weights = sampling_weight)

