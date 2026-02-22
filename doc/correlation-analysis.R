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
survey_data %>%
  pearson_cor(age, income)

## -----------------------------------------------------------------------------
survey_data %>%
  pearson_cor(age, income, weights = sampling_weight)

## -----------------------------------------------------------------------------
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
survey_data %>%
  spearman_rho(political_orientation, environmental_concern)

## -----------------------------------------------------------------------------
survey_data %>%
  spearman_rho(political_orientation, environmental_concern,
               weights = sampling_weight)

## -----------------------------------------------------------------------------
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
result <- survey_data %>%
  pearson_cor(age, income, weights = sampling_weight)
print(result)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
survey_data %>%
  pearson_cor(age, political_orientation, environmental_concern,
              life_satisfaction, trust_government,
              weights = sampling_weight)

## -----------------------------------------------------------------------------
survey_data %>%
  pearson_cor(income, life_satisfaction, trust_government,
              trust_media, trust_science,
              weights = sampling_weight)

## -----------------------------------------------------------------------------
# 1. Correlation matrix
cor_matrix <- survey_data %>%
  pearson_cor(age, income, life_satisfaction,
              political_orientation, environmental_concern,
              weights = sampling_weight)
print(cor_matrix)

# 2. Focus on significant correlations
significant_cors <- cor_matrix$correlations %>%
  filter(p_value < 0.05) %>%
  arrange(desc(abs(correlation)))
print(significant_cors)

# 3. Regional differences in key relationship
survey_data %>%
  group_by(region) %>%
  pearson_cor(age, income, weights = sampling_weight)

