# ============================================================================
# FUNCTION_NAME - SPSS VALIDATION TEST TEMPLATE
# ============================================================================
# Purpose: Validate R FUNCTION_NAME() against SPSS PROCEDURE_NAME
# Dataset: survey_data (or specify your dataset)
# Variable(s): VARIABLE_NAMES
# Created: YYYY-MM-DD
# SPSS Version: XX.X
# 
# Instructions:
# 1. Replace FUNCTION_NAME with your function (e.g., t_test, chi_squared_test)
# 2. Replace PROCEDURE_NAME with SPSS equivalent (e.g., T-TEST, CROSSTABS)
# 3. Update variable names and dataset as needed
# 4. Run SPSS syntax and populate reference values
# 5. Adjust tolerance values based on function type
# 6. Add any function-specific parameters
# ============================================================================

library(testthat)
library(dplyr)
library(SurveyStat)

# ============================================================================
# SPSS REFERENCE VALUES
# ============================================================================
# Instructions for obtaining SPSS values:
# 1. Run the following SPSS syntax:
#    PROCEDURE_NAME
#      VARIABLES=your_variable
#      /STATISTICS=MEAN STDDEV (adjust as needed)
#      /OPTIONS=... (add relevant options).
#
# 2. For weighted analysis, add:
#    WEIGHT BY sampling_weight.
#
# 3. For grouped analysis, add:
#    SORT CASES BY group_variable.
#    SPLIT FILE BY group_variable.
# ============================================================================

spss_values <- list(
  # Test 1: Unweighted/Ungrouped
  unweighted_ungrouped = list(
    # Primary statistics (adjust based on function type)
    test_statistic = NA_real_,  # e.g., t-value, F-value, chi-square
    p_value = NA_real_,
    df = NA_real_,
    
    # Descriptive statistics (if applicable)
    mean = NA_real_,
    sd = NA_real_,
    n = NA_integer_,
    
    # Additional statistics (function-specific)
    # e.g., for t-test: mean_difference, se_difference, ci_lower, ci_upper
    # e.g., for ANOVA: sum_squares, mean_squares, effect_size
    # e.g., for chi-square: expected_frequencies, residuals
    additional_stats = list()
  ),
  
  # Test 2: Weighted/Ungrouped
  weighted_ungrouped = list(
    test_statistic = NA_real_,
    p_value = NA_real_,
    df = NA_real_,
    mean = NA_real_,
    sd = NA_real_,
    n = NA_real_,  # Note: weighted N may be decimal
    additional_stats = list()
  ),
  
  # Test 3: Unweighted/Grouped - Group 1
  unweighted_grouped_g1 = list(
    test_statistic = NA_real_,
    p_value = NA_real_,
    df = NA_real_,
    mean = NA_real_,
    sd = NA_real_,
    n = NA_integer_,
    additional_stats = list()
  ),
  
  # Test 3: Unweighted/Grouped - Group 2
  unweighted_grouped_g2 = list(
    test_statistic = NA_real_,
    p_value = NA_real_,
    df = NA_real_,
    mean = NA_real_,
    sd = NA_real_,
    n = NA_integer_,
    additional_stats = list()
  ),
  
  # Test 4: Weighted/Grouped - Group 1
  weighted_grouped_g1 = list(
    test_statistic = NA_real_,
    p_value = NA_real_,
    df = NA_real_,
    mean = NA_real_,
    sd = NA_real_,
    n = NA_real_,
    additional_stats = list()
  ),
  
  # Test 4: Weighted/Grouped - Group 2
  weighted_grouped_g2 = list(
    test_statistic = NA_real_,
    p_value = NA_real_,
    df = NA_real_,
    mean = NA_real_,
    sd = NA_real_,
    n = NA_real_,
    additional_stats = list()
  )
)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Compare statistical test results with SPSS
#' 
#' @param r_result R function result object
#' @param spss_ref SPSS reference values
#' @param test_name Test scenario name for error messages
#' @param tolerance_stat Tolerance for test statistics (default: 0.00001)
#' @param tolerance_p Tolerance for p-values (default: 0.00001)
#' @param tolerance_desc Tolerance for descriptive stats (default: 0.0001)
#' @param tolerance_n Tolerance for sample sizes (default: 0, exact match)
compare_with_spss <- function(r_result, spss_ref, test_name, 
                              tolerance_stat = 0.00001,
                              tolerance_p = 0.00001,
                              tolerance_desc = 0.0001,
                              tolerance_n = 0) {
  
  # Extract values from R result
  # NOTE: Adjust these extractions based on your function's output structure
  # Common patterns:
  # - r_result$statistic or r_result$test_statistic
  # - r_result$p.value or r_result$p_value
  # - r_result$parameter or r_result$df
  # - r_result$estimate or r_result$mean
  
  # Test primary statistics
  if (!is.na(spss_ref$test_statistic)) {
    expect_equal(
      r_result$test_statistic,  # Adjust field name
      spss_ref$test_statistic,
      tolerance = tolerance_stat,
      label = paste(test_name, "- Test statistic")
    )
  }
  
  if (!is.na(spss_ref$p_value)) {
    expect_equal(
      r_result$p_value,  # Adjust field name
      spss_ref$p_value,
      tolerance = tolerance_p,
      label = paste(test_name, "- P-value")
    )
  }
  
  if (!is.na(spss_ref$df)) {
    expect_equal(
      r_result$df,  # Adjust field name
      spss_ref$df,
      tolerance = 0,  # df should match exactly
      label = paste(test_name, "- Degrees of freedom")
    )
  }
  
  # Test descriptive statistics if applicable
  if (!is.na(spss_ref$mean)) {
    expect_equal(
      r_result$mean,  # Adjust field name
      spss_ref$mean,
      tolerance = tolerance_desc,
      label = paste(test_name, "- Mean")
    )
  }
  
  if (!is.na(spss_ref$sd)) {
    expect_equal(
      r_result$sd,  # Adjust field name
      spss_ref$sd,
      tolerance = tolerance_desc,
      label = paste(test_name, "- Standard deviation")
    )
  }
  
  if (!is.na(spss_ref$n)) {
    expect_equal(
      r_result$n,  # Adjust field name
      spss_ref$n,
      tolerance = tolerance_n,
      label = paste(test_name, "- Sample size")
    )
  }
  
  # Test additional function-specific statistics
  for (stat_name in names(spss_ref$additional_stats)) {
    if (!is.na(spss_ref$additional_stats[[stat_name]])) {
      expect_equal(
        r_result[[stat_name]],  # Adjust extraction
        spss_ref$additional_stats[[stat_name]],
        tolerance = tolerance_desc,
        label = paste(test_name, "-", stat_name)
      )
    }
  }
}

#' Extract results for grouped analysis
#' 
#' Helper to extract group-specific results from grouped output
#' @param result Full grouped result object
#' @param group_var Grouping variable name
#' @param group_value Value of the group to extract
extract_group_results <- function(result, group_var, group_value) {
  # NOTE: Adjust based on how your function structures grouped results
  # Common patterns:
  # - result$results[result$results[[group_var]] == group_value, ]
  # - result[[group_value]]
  # - Filter both data and statistics tables
  
  # Example implementation (adjust as needed):
  if ("results" %in% names(result)) {
    group_data <- result$results[result$results[[group_var]] == group_value, ]
  } else {
    group_data <- result[result[[group_var]] == group_value, ]
  }
  
  if ("stats" %in% names(result)) {
    group_stats <- result$stats[result$stats[[group_var]] == group_value, ]
    return(list(results = group_data, stats = group_stats))
  }
  
  return(group_data)
}

# ============================================================================
# TEST SETUP
# ============================================================================

# Load test data
data(survey_data, envir = environment())

# Optional: Create subset or prepare data
# test_data <- survey_data %>% 
#   filter(!is.na(your_variable))

# ============================================================================
# VALIDATION TESTS
# ============================================================================

test_that("Test 1: Unweighted/Ungrouped analysis matches SPSS", {
  # Run your function
  result <- survey_data %>% 
    FUNCTION_NAME(VARIABLE_NAME)  # Add function-specific parameters
  
  # Compare with SPSS
  compare_with_spss(
    result, 
    spss_values$unweighted_ungrouped,
    "Unweighted/Ungrouped"
  )
})

test_that("Test 2: Weighted/Ungrouped analysis matches SPSS", {
  # Run your function with weights
  result <- survey_data %>% 
    FUNCTION_NAME(VARIABLE_NAME, weights = sampling_weight)
  
  # Use strict tolerance for exact match with SPSS
  compare_with_spss(
    result, 
    spss_values$weighted_ungrouped,
    "Weighted/Ungrouped",
    tolerance_n = 0  # Exact match required
  )
})

test_that("Test 3: Unweighted/Grouped analysis matches SPSS", {
  # Run your function with grouping
  result <- survey_data %>% 
    group_by(region) %>%  # Or your grouping variable
    FUNCTION_NAME(VARIABLE_NAME)
  
  # Test each group separately
  # Group 1 (e.g., East)
  group1_result <- extract_group_results(result, "region", "East")
  compare_with_spss(
    group1_result,
    spss_values$unweighted_grouped_g1,
    "Unweighted/Grouped - East"
  )
  
  # Group 2 (e.g., West)
  group2_result <- extract_group_results(result, "region", "West")
  compare_with_spss(
    group2_result,
    spss_values$unweighted_grouped_g2,
    "Unweighted/Grouped - West"
  )
})

test_that("Test 4: Weighted/Grouped analysis matches SPSS", {
  # Run your function with both grouping and weights
  result <- survey_data %>% 
    group_by(region) %>%
    FUNCTION_NAME(VARIABLE_NAME, weights = sampling_weight)
  
  # Test each group separately with strict tolerance
  # Group 1 (e.g., East)
  group1_result <- extract_group_results(result, "region", "East")
  compare_with_spss(
    group1_result,
    spss_values$weighted_grouped_g1,
    "Weighted/Grouped - East",
    tolerance_n = 0  # Exact match required
  )
  
  # Group 2 (e.g., West)
  group2_result <- extract_group_results(result, "region", "West")
  compare_with_spss(
    group2_result,
    spss_values$weighted_grouped_g2,
    "Weighted/Grouped - West",
    tolerance_n = 0  # Exact match required
  )
})

# ============================================================================
# EDGE CASES (Optional but recommended)
# ============================================================================

test_that("Edge case: Missing values handled correctly", {
  # Create data with missing values
  test_data <- survey_data
  test_data$VARIABLE_NAME[1:10] <- NA
  
  # Run function and verify it handles NA appropriately
  expect_no_error({
    result <- test_data %>% FUNCTION_NAME(VARIABLE_NAME)
  })
  
  # Add specific checks for how missing values should be handled
  # based on SPSS behavior
})

test_that("Edge case: Single group or single value", {
  # Test with data that has only one group or one unique value
  single_group_data <- survey_data %>% 
    filter(region == "East")
  
  expect_no_error({
    result <- single_group_data %>% FUNCTION_NAME(VARIABLE_NAME)
  })
  
  # Add specific validation if SPSS has known behavior for this case
})

# ============================================================================
# SUMMARY REPORT
# ============================================================================

test_that("Generate validation summary", {
  cat("\n")
  cat(paste(rep("=", 70), collapse = ""), "\n", sep = "")
  cat("FUNCTION_NAME SPSS VALIDATION SUMMARY\n")
  cat(paste(rep("=", 70), collapse = ""), "\n", sep = "")
  cat("Variable tested: VARIABLE_NAME\n")
  cat("Dataset: survey_data\n")
  cat("SPSS Version: XX.X\n")
  cat("\n")
  cat("Test scenarios validated:\n")
  cat("1. ✓ Unweighted/Ungrouped\n")
  cat("2. ✓ Weighted/Ungrouped\n")
  cat("3. ✓ Unweighted/Grouped (by GROUP_VAR)\n")
  cat("4. ✓ Weighted/Grouped (by GROUP_VAR)\n")
  cat("\n")
  cat("Validation criteria:\n")
  cat("- Test statistics: Near-exact match (tolerance ±0.00001)\n")
  cat("- P-values: Near-exact match (tolerance ±0.00001)\n")
  cat("- Descriptive stats: Near-exact match (tolerance ±0.0001)\n")
  cat("- Sample sizes: EXACT match required (tolerance = 0)\n")
  cat(paste(rep("=", 70), collapse = ""), "\n", sep = "")
  
  # This test always passes - it's just for reporting
  expect_true(TRUE)
})

# ============================================================================
# NOTES AND DOCUMENTATION
# ============================================================================

# Function-Specific Implementation Notes:
# ----------------------------------------
# 1. For t-tests:
#    - Compare t-statistic, p-value, mean difference, CI bounds
#    - Check both equal and unequal variance assumptions
#    - Verify degrees of freedom calculation
#
# 2. For ANOVA:
#    - Compare F-statistic, between/within sum of squares
#    - Verify effect size calculations (eta-squared, partial eta)
#    - Check post-hoc test compatibility
#
# 3. For Chi-square:
#    - Compare chi-square statistic, expected frequencies
#    - Verify handling of small expected counts
#    - Check continuity correction if applicable
#
# 4. For correlations:
#    - Compare correlation coefficient, p-value
#    - Check different correlation types (Pearson, Spearman)
#    - Verify confidence interval calculations
#
# 5. For regression:
#    - Compare coefficients, standard errors, t-values
#    - Verify R-squared and adjusted R-squared
#    - Check residual statistics

# Known Differences from SPSS:
# ----------------------------
# Document any acceptable differences here:
# - Rounding: R may round at different stages than SPSS
# - Missing value handling: Document any differences
# - Default settings: Note if R and SPSS have different defaults

# Troubleshooting:
# ----------------
# If tests fail:
# 1. Check SPSS syntax - ensure correct options/settings
# 2. Verify data preparation is identical
# 3. Check for version-specific SPSS changes
# 4. Consider numerical precision limits
# 5. Verify weight variable is properly specified