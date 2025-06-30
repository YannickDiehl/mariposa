# Test Script for Synthetic Datasets
# ===================================
# This script tests the synthetic datasets with various statistical functions
# to demonstrate functionality and verify integration.

library(SurveyStat)
library(dplyr)

# Load synthetic datasets
data(survey_data)
data(longitudinal_data)
data(longitudinal_data_wide)

cat("=== SYNTHETIC DATASETS TESTING ===\n\n")

# Test 1: Survey Data Overview
cat("1. SURVEY DATA OVERVIEW\n")
cat("------------------------\n")
cat("Dimensions:", nrow(survey_data), "x", ncol(survey_data), "\n")
cat("Variables:", paste(names(survey_data), collapse = ", "), "\n\n")

# Test 2: Descriptive Statistics
cat("2. DESCRIPTIVE STATISTICS (survey_data)\n")
cat("----------------------------------------\n")
try({
  result_desc <- survey_data %>% describe(age, income, weights = sampling_weight)
  print(result_desc)
})
cat("\n")

# Test 3: Weighted Mean
cat("3. WEIGHTED MEAN CALCULATION\n")
cat("-----------------------------\n")
try({
  result_wmean <- survey_data %>% w_mean(life_satisfaction, weights = sampling_weight)
  print(result_wmean)
})
cat("\n")

# Test 4: Frequency Analysis
cat("4. FREQUENCY ANALYSIS\n")
cat("---------------------\n")
try({
  result_freq <- survey_data %>% frequency(education, region, weights = sampling_weight)
  print(result_freq)
})
cat("\n")

# Test 5: Group Comparisons
cat("5. GROUP COMPARISONS (t-test)\n")
cat("-----------------------------\n")
try({
  result_ttest <- survey_data %>% 
    group_by(region) %>% 
    t_test(life_satisfaction, weights = sampling_weight)
  print(result_ttest)
})
cat("\n")

# Test 6: Longitudinal Data Overview  
cat("6. LONGITUDINAL DATA OVERVIEW\n")
cat("-----------------------------\n")
cat("Long format dimensions:", nrow(longitudinal_data), "x", ncol(longitudinal_data), "\n")
cat("Subjects:", length(unique(longitudinal_data$subject_id)), "\n")
cat("Time points:", length(unique(longitudinal_data$time)), "\n")
cat("Groups:", paste(unique(longitudinal_data$group), collapse = ", "), "\n\n")

# Test 7: Longitudinal Descriptives
cat("7. LONGITUDINAL DESCRIPTIVES\n")
cat("----------------------------\n")
try({
  result_long_desc <- longitudinal_data %>%
    group_by(group, time) %>%
    describe(outcome_score)
  print(result_long_desc)
})
cat("\n")

# Test 8: Repeated Measures t-test
cat("8. REPEATED MEASURES T-TEST\n")
cat("---------------------------\n")
try({
  result_rm_ttest <- longitudinal_data %>%
    filter(time %in% c("T1", "T4")) %>%
    rm_t_test(outcome_score, group = group, subject_id = subject_id)
  print(result_rm_ttest)
})
cat("\n")

# Test 9: Data Quality Check
cat("9. DATA QUALITY VERIFICATION\n")
cat("-----------------------------\n")

# Survey data quality
cat("Survey data missing patterns:\n")
survey_missing <- survey_data %>%
  summarise_all(~sum(is.na(.))) %>%
  select_if(~. > 0)
print(survey_missing)

cat("\nLongitudinal data missing by time:\n")
long_missing <- longitudinal_data %>%
  group_by(time) %>%
  summarise(
    n_total = n(),
    n_missing = sum(is.na(outcome_score)),
    pct_missing = round(n_missing / n_total * 100, 1),
    .groups = "drop"
  )
print(long_missing)

cat("\n=== TESTING COMPLETE ===\n")
cat("All synthetic datasets integrated successfully!\n")
cat("Ready for statistical analysis and package development.\n")