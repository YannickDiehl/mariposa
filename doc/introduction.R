## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(mariposa)
library(dplyr)

## -----------------------------------------------------------------------------
# Load the example survey data
data(survey_data)

# Take a quick look at what we have
glimpse(survey_data)

## -----------------------------------------------------------------------------
# Get a summary of age and income
# The weights ensure results represent the population correctly
survey_data %>%
  describe(age, income, weights = sampling_weight)

## -----------------------------------------------------------------------------
survey_data %>%
  frequency(education, weights = sampling_weight)

## -----------------------------------------------------------------------------
survey_data %>%
  t_test(life_satisfaction, group = gender, weights = sampling_weight)

## -----------------------------------------------------------------------------
survey_data %>%
  oneway_anova(life_satisfaction, group = education, weights = sampling_weight)

## -----------------------------------------------------------------------------
survey_data %>%
  pearson_cor(age, income, weights = sampling_weight)

## -----------------------------------------------------------------------------
survey_data %>%
  crosstab(education, employment, weights = sampling_weight)

## -----------------------------------------------------------------------------
survey_data %>%
  group_by(region) %>%
  describe(income, life_satisfaction, weights = sampling_weight)

## -----------------------------------------------------------------------------
survey_data %>%
  t_test(trust_government, trust_media, trust_science,
         group = gender, weights = sampling_weight)

## -----------------------------------------------------------------------------
# Without weights - just your sample
unweighted <- mean(survey_data$age, na.rm = TRUE)

# With weights - represents the population
weighted_result <- w_mean(survey_data, age, weights = sampling_weight)
weighted <- weighted_result$results$weighted_mean

cat("Sample average age:", round(unweighted, 1), "\n")
cat("Population average age:", round(weighted, 1), "\n")
cat("Difference:", round(weighted - unweighted, 1), "years\n")

## -----------------------------------------------------------------------------
# 1. Start with descriptive statistics
cat("=== Basic Summary ===\n")
survey_data %>%
  describe(life_satisfaction, weights = sampling_weight)

# 2. Check distribution across groups
cat("\n=== By Education Level ===\n")
survey_data %>%
  group_by(education) %>%
  describe(life_satisfaction, weights = sampling_weight)

# 3. Test for significant differences
cat("\n=== Statistical Test ===\n")
result <- survey_data %>%
  oneway_anova(life_satisfaction, group = education,
               weights = sampling_weight)
print(result)

# 4. If significant, see which groups differ
# Check if result has p_value column (data frame structure)
if (!is.null(result) && "p_value" %in% names(result) && result$p_value[1] < 0.05) {
  cat("\n=== Post-hoc Comparisons ===\n")
  tukey_test(result)
}

## -----------------------------------------------------------------------------
survey_data %>%
  describe(income, show = "all", weights = sampling_weight)

## -----------------------------------------------------------------------------
# Question: Does income relate to life satisfaction?

# 1. Look at both variables
cat("=== Income Summary ===\n")
survey_data %>%
  describe(income, weights = sampling_weight)

cat("\n=== Life Satisfaction Summary ===\n")
survey_data %>%
  describe(life_satisfaction, weights = sampling_weight)

# 2. Check correlation
cat("\n=== Correlation ===\n")
cor_result <- survey_data %>%
  pearson_cor(income, life_satisfaction, weights = sampling_weight)
print(cor_result)

# 3. Interpret
if (!is.null(cor_result) && "p_value" %in% names(cor_result) && cor_result$p_value[1] < 0.05) {
  if (cor_result$correlation[1] > 0) {
    cat("\nHigher income is associated with greater life satisfaction.\n")
  } else {
    cat("\nHigher income is associated with lower life satisfaction.\n")
  }
  cat("Correlation strength:", round(cor_result$correlation[1], 2),
      "(", ifelse(abs(cor_result$correlation[1]) < 0.3, "weak",
                  ifelse(abs(cor_result$correlation[1]) < 0.7, "moderate", "strong")),
      ")\n")
} else {
  cat("\nNo significant relationship found between income and life satisfaction.\n")
}

