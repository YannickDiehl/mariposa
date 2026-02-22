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
  t_test(life_satisfaction, group = gender)

## -----------------------------------------------------------------------------
survey_data %>%
  t_test(life_satisfaction, group = gender, weights = sampling_weight)

## -----------------------------------------------------------------------------
survey_data %>%
  t_test(trust_government, trust_science, trust_media,
         group = gender, weights = sampling_weight)

## -----------------------------------------------------------------------------
survey_data %>%
  t_test(life_satisfaction, mu = 3, weights = sampling_weight)

## -----------------------------------------------------------------------------
survey_data %>%
  group_by(region) %>%
  t_test(income, group = gender, weights = sampling_weight)

## -----------------------------------------------------------------------------
survey_data %>%
  oneway_anova(life_satisfaction, group = education)

## -----------------------------------------------------------------------------
survey_data %>%
  oneway_anova(life_satisfaction, group = education,
               weights = sampling_weight)

## -----------------------------------------------------------------------------
# Save the ANOVA result
anova_result <- survey_data %>%
  oneway_anova(life_satisfaction, group = education,
               weights = sampling_weight)

# Tukey HSD: identifies which pairs of groups differ
tukey_test(anova_result)

# Scheffe test: more conservative (fewer false positives)
scheffe_test(anova_result)

## -----------------------------------------------------------------------------
levene_test(anova_result)

## -----------------------------------------------------------------------------
survey_data %>%
  chi_square(education, employment)

## -----------------------------------------------------------------------------
survey_data %>%
  chi_square(education, employment, weights = sampling_weight)

## -----------------------------------------------------------------------------
survey_data %>%
  chi_square(employment, education, weights = sampling_weight)

survey_data %>%
  chi_square(employment, gender, weights = sampling_weight)

## -----------------------------------------------------------------------------
survey_data %>%
  mann_whitney(political_orientation, group = region)

## -----------------------------------------------------------------------------
survey_data %>%
  mann_whitney(political_orientation, group = region,
               weights = sampling_weight)

## -----------------------------------------------------------------------------
result <- survey_data %>%
  oneway_anova(income, group = education, weights = sampling_weight)
print(result)

## -----------------------------------------------------------------------------
# 1. Descriptive overview
survey_data %>%
  group_by(education) %>%
  describe(life_satisfaction, weights = sampling_weight)

# 2. Test for overall differences
anova_result <- survey_data %>%
  oneway_anova(life_satisfaction, group = education,
               weights = sampling_weight)
print(anova_result)

# 3. Post-hoc: which groups differ?
tukey_test(anova_result)

# 4. Check assumptions
levene_test(anova_result)

