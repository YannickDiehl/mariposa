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
survey_data %>%
  pearson_cor(age, income)

## -----------------------------------------------------------------------------
survey_data %>%
  pearson_cor(age, income, weights = sampling_weight)

## -----------------------------------------------------------------------------
# Correlate all trust variables
survey_data %>%
  pearson_cor(trust_government, trust_media, trust_science,
              weights = sampling_weight)

## -----------------------------------------------------------------------------
survey_data %>%
  group_by(region) %>%
  pearson_cor(age, income, weights = sampling_weight)

## -----------------------------------------------------------------------------
survey_data %>%
  pearson_cor(age, income,
              conf.level = 0.95,
              weights = sampling_weight)

## -----------------------------------------------------------------------------
# Correlation between ordinal variables
survey_data %>%
  spearman_rho(political_orientation, environmental_concern)

## -----------------------------------------------------------------------------
survey_data %>%
  spearman_rho(political_orientation, environmental_concern,
               weights = sampling_weight)

## -----------------------------------------------------------------------------
# Create rank correlation matrix
survey_data %>%
  spearman_rho(political_orientation, environmental_concern,
               life_satisfaction, trust_government,
               weights = sampling_weight)

## -----------------------------------------------------------------------------
survey_data %>%
  kendall_tau(political_orientation, life_satisfaction)

## -----------------------------------------------------------------------------
survey_data %>%
  kendall_tau(political_orientation, life_satisfaction,
              weights = sampling_weight)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
# Calculate correlations among multiple variables
cor_matrix <- survey_data %>%
  pearson_cor(age, income, life_satisfaction,
              trust_government, trust_media, trust_science,
              weights = sampling_weight)

print(cor_matrix)

## -----------------------------------------------------------------------------
# How does age-income correlation vary by region?
regional_cors <- survey_data %>%
  group_by(region) %>%
  pearson_cor(age, income, weights = sampling_weight)

print(regional_cors)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
# How age relates to various attitudes
age_correlations <- survey_data %>%
  pearson_cor(age, political_orientation, environmental_concern,
              life_satisfaction, trust_government,
              weights = sampling_weight)

print(age_correlations)

## -----------------------------------------------------------------------------
# Income's relationship with satisfaction and trust
income_correlations <- survey_data %>%
  pearson_cor(income, life_satisfaction, trust_government,
              trust_media, trust_science,
              weights = sampling_weight)

print(income_correlations)

## -----------------------------------------------------------------------------
# Note: mariposa focuses on bivariate correlations
# For partial correlations, combine with regression approaches

# Example: Age-income correlation might be confounded by education
# First, check bivariate correlations
survey_data %>%
  group_by(education) %>%
  pearson_cor(age, income, weights = sampling_weight)

## -----------------------------------------------------------------------------
# Check for non-linear patterns
# Low correlation doesn't mean no relationship

# Example: Life satisfaction might have U-shaped relationship with age
young <- survey_data %>% filter(age < 30)
middle <- survey_data %>% filter(age >= 30 & age < 60)
older <- survey_data %>% filter(age >= 60)

cat("Correlation in young adults:\n")
young %>% pearson_cor(age, life_satisfaction, weights = sampling_weight)

cat("\nCorrelation in middle-aged:\n")
middle %>% pearson_cor(age, life_satisfaction, weights = sampling_weight)

cat("\nCorrelation in older adults:\n")
older %>% pearson_cor(age, life_satisfaction, weights = sampling_weight)

## -----------------------------------------------------------------------------
# 1. Explore all correlations
cat("=== Correlation Matrix ===\n")
cor_matrix <- survey_data %>%
  pearson_cor(age, income, life_satisfaction,
              political_orientation, environmental_concern,
              weights = sampling_weight)
print(cor_matrix)

# 2. Focus on significant correlations
cat("\n=== Significant Correlations (p < 0.05) ===\n")
significant_cors <- cor_matrix$correlations %>%
  filter(p_value < 0.05) %>%
  arrange(desc(abs(correlation)))
print(significant_cors)

# 3. Compare methods for key relationship
cat("\n=== Method Comparison: Income vs Life Satisfaction ===\n")
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
survey_data %>%
  group_by(region) %>%
  pearson_cor(age, income, weights = sampling_weight)

