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

## -----------------------------------------------------------------------------
data(survey_data)
glimpse(survey_data)

## -----------------------------------------------------------------------------
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
unweighted <- mean(survey_data$age, na.rm = TRUE)

weighted_result <- w_mean(survey_data, age, weights = sampling_weight)
weighted <- weighted_result$results$weighted_mean

cat("Sample average age:", round(unweighted, 1), "\n")
cat("Population average age:", round(weighted, 1), "\n")
cat("Difference:", round(weighted - unweighted, 1), "years\n")

## -----------------------------------------------------------------------------
# 1. Descriptive overview
survey_data %>%
  describe(life_satisfaction, weights = sampling_weight)

# 2. Compare across groups
survey_data %>%
  group_by(education) %>%
  describe(life_satisfaction, weights = sampling_weight)

# 3. Test for significant differences
result <- survey_data %>%
  oneway_anova(life_satisfaction, group = education,
               weights = sampling_weight)
print(result)

# 4. If significant, find out which groups differ
tukey_test(result)

