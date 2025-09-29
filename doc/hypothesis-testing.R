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
  t_test(life_satisfaction, group = gender)

## -----------------------------------------------------------------------------
survey_data %>%
  t_test(life_satisfaction, group = gender, weights = sampling_weight)

## -----------------------------------------------------------------------------
# Test all trust variables between genders
survey_data %>%
  t_test(trust_government, trust_science, trust_media,
         group = gender, weights = sampling_weight)

## -----------------------------------------------------------------------------
# Is average life satisfaction different from 3 (neutral)?
survey_data %>%
  t_test(life_satisfaction, mu = 3, weights = sampling_weight)

## -----------------------------------------------------------------------------
survey_data %>%
  group_by(region) %>%
  t_test(income, group = gender, weights = sampling_weight)

## -----------------------------------------------------------------------------
# Compare life satisfaction across education levels
survey_data %>%
  oneway_anova(life_satisfaction, group = education)

## -----------------------------------------------------------------------------
survey_data %>%
  oneway_anova(life_satisfaction, group = education,
               weights = sampling_weight)

## -----------------------------------------------------------------------------
# First run ANOVA and save result
anova_result <- survey_data %>%
  oneway_anova(life_satisfaction, group = education,
               weights = sampling_weight)

# Then run Tukey test on the result
tukey_test(anova_result)

# Or Scheffe test (more conservative)
scheffe_test(anova_result)

## -----------------------------------------------------------------------------
levene_test(anova_result)

## -----------------------------------------------------------------------------
# Is education related to employment status?
survey_data %>%
  chi_square(education, employment)

## -----------------------------------------------------------------------------
survey_data %>%
  chi_square(education, employment, weights = sampling_weight)

## -----------------------------------------------------------------------------
# Test association between employment and education
survey_data %>%
  chi_square(employment, education, weights = sampling_weight)

# Test association between employment and gender
survey_data %>%
  chi_square(employment, gender, weights = sampling_weight)

# Test association between employment and region
survey_data %>%
  chi_square(employment, region, weights = sampling_weight)

## -----------------------------------------------------------------------------
# Compare political orientation between regions
survey_data %>%
  mann_whitney(political_orientation, group = region)

## -----------------------------------------------------------------------------
survey_data %>%
  mann_whitney(political_orientation, group = region,
               weights = sampling_weight)

## -----------------------------------------------------------------------------
# ANOVA provides eta-squared effect size
result <- survey_data %>%
  oneway_anova(income, group = education, weights = sampling_weight)
print(result)

## -----------------------------------------------------------------------------
# 1. Descriptive statistics first
cat("=== Descriptive Summary ===\n")
survey_data %>%
  group_by(education) %>%
  describe(life_satisfaction, weights = sampling_weight)

# 2. Test for overall difference
cat("\n=== ANOVA Test ===\n")
anova_result <- survey_data %>%
  oneway_anova(life_satisfaction, group = education,
               weights = sampling_weight)
print(anova_result)

# 3. If significant, see which groups differ
if (!is.null(anova_result) && "p_value" %in% names(anova_result) && !is.na(anova_result$p_value[1]) && anova_result$p_value[1] < 0.05) {
  cat("\n=== Post-hoc Comparisons ===\n")
  tukey_test(anova_result)
}

# 4. Check assumptions
cat("\n=== Assumption Check ===\n")
levene_test(anova_result)

## -----------------------------------------------------------------------------
# Visual check for normality
# (In practice, create histograms or Q-Q plots)
survey_data %>%
  group_by(gender) %>%
  describe(life_satisfaction, show = c("skew", "kurtosis"))

