# SPSS Validation Test Template
# Copy this template and customize for your function

# ==============================================================================
# SETUP
# ==============================================================================

library(testthat)
library(dplyr)

# Source the parser (adjust path as needed)
source(test_path("helper-spss-parser.R"))

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

# Load the survey data
load_survey_data <- function() {
  data(survey_data, envir = environment())
  return(survey_data)
}

# Get path to SPSS output file
get_spss_output_path <- function(function_name = "[FUNCTION_NAME]") {
  # Try multiple possible paths
  paths <- c(
    test_path(paste0("../spss_reference/outputs/", function_name, "_output.txt")),
    paste0("tests/spss_reference/outputs/", function_name, "_output.txt"),
    paste0("outputs/", function_name, "_output.txt")
  )
  
  for (path in paths) {
    if (file.exists(path)) return(path)
  }
  
  skip(paste("SPSS output file not found for", function_name))
}

# ==============================================================================
# BASIC VALIDATION TEST
# ==============================================================================

test_that("[FUNCTION_NAME]() matches SPSS - TEST 1: Basic unweighted", {
  
  # Load data
  survey_data <- load_survey_data()
  
  # Get SPSS output file
  spss_file <- get_spss_output_path("[FUNCTION_NAME]")
  
  # Extract SPSS values
  spss_values <- extract_[function_name]_values(spss_file, test_number = 1)
  
  # Skip if parsing failed
  if (is.null(spss_values)) {
    skip("Could not parse SPSS values for Test 1")
  }
  
  # Run R function (adjust parameters as needed)
  result <- [FUNCTION_NAME](survey_data, [VARIABLE_NAME])
  
  # Extract R values (adjust based on your function's output structure)
  r_values <- result$results  # or result$statistics, etc.
  
  # Compare values with appropriate tolerances
  expect_equal(r_values$mean, spss_values$mean, tolerance = 0.01)
  expect_equal(r_values$sd, spss_values$sd, tolerance = 0.01)
  expect_equal(r_values$n, spss_values$n, tolerance = 0.1)
  
  # Add more comparisons as needed
})

# ==============================================================================
# WEIGHTED VALIDATION TEST
# ==============================================================================

test_that("[FUNCTION_NAME]() matches SPSS - TEST 3: Weighted analysis", {
  
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path("[FUNCTION_NAME]")
  
  # Extract SPSS weighted values
  spss_values <- extract_[function_name]_values(spss_file, test_number = 3)
  
  if (is.null(spss_values)) {
    skip("Could not parse SPSS values for Test 3")
  }
  
  # Run R function with weights
  result <- [FUNCTION_NAME](survey_data, [VARIABLE_NAME], 
                            weights = sampling_weight)
  
  r_values <- result$results
  
  # Compare with weighted tolerances (sometimes need higher tolerance)
  expect_equal(r_values$weighted_mean, spss_values$mean, tolerance = 0.01)
  expect_equal(r_values$weighted_sd, spss_values$sd, tolerance = 0.01)
})

# ==============================================================================
# GROUPED VALIDATION TEST
# ==============================================================================

test_that("[FUNCTION_NAME]() matches SPSS - TEST 4: Grouped analysis", {
  
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path("[FUNCTION_NAME]")
  
  # For grouped analysis, you may need to parse multiple sections
  spss_group1 <- extract_[function_name]_values(spss_file, test_number = 4)
  # May need additional parsing for second group
  
  if (is.null(spss_group1)) {
    skip("Could not parse SPSS values for Test 4")
  }
  
  # Run R function with grouping
  result <- survey_data %>%
    group_by([GROUP_VARIABLE]) %>%
    [FUNCTION_NAME]([VARIABLE_NAME])
  
  # Compare first group
  group1_results <- result$results[[1]]  # Adjust based on structure
  expect_equal(group1_results$mean, spss_group1$mean, tolerance = 0.01)
})

# ==============================================================================
# STATISTICAL TEST VALIDATION (e.g., t-test, chi-squared)
# ==============================================================================

test_that("[FUNCTION_NAME]() matches SPSS - TEST 2: Statistical test", {
  
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path("[FUNCTION_NAME]")
  
  spss_values <- extract_[function_name]_values(spss_file, test_number = 2)
  
  if (is.null(spss_values)) {
    skip("Could not parse SPSS values for Test 2")
  }
  
  # Run statistical test
  result <- [FUNCTION_NAME](survey_data, [VARIABLE], group = [GROUP_VAR])
  
  # IMPORTANT: Strip names from R statistics
  r_stats <- list(
    t_stat = as.numeric(result$statistic),    # Strip "t" name
    df = as.numeric(result$parameter),         # Strip "df" name
    p_value = as.numeric(result$p.value)      # Clean numeric
  )
  
  # Compare test statistics with tighter tolerances
  expect_equal(r_stats$t_stat, spss_values$t_stat, tolerance = 0.001)
  expect_equal(r_stats$df, spss_values$df, tolerance = 0.1)
  expect_equal(r_stats$p_value, spss_values$p_value, tolerance = 0.01)
})

# ==============================================================================
# COMPREHENSIVE VALIDATION TEST
# ==============================================================================

test_that("[FUNCTION_NAME]() comprehensive SPSS validation", {
  
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path("[FUNCTION_NAME]")
  
  # Test all scenarios
  test_scenarios <- list(
    list(test_num = 1, desc = "Basic unweighted"),
    list(test_num = 2, desc = "Multiple variables"),
    list(test_num = 3, desc = "Weighted"),
    list(test_num = 4, desc = "Grouped"),
    list(test_num = 5, desc = "With missing")
  )
  
  passed <- 0
  failed <- 0
  
  for (scenario in test_scenarios) {
    spss_values <- extract_[function_name]_values(spss_file, 
                                                  test_number = scenario$test_num)
    
    if (is.null(spss_values)) {
      message(paste("Skipped:", scenario$desc, "- Could not parse"))
      next
    }
    
    # Run appropriate R function based on scenario
    # (Add conditional logic here based on test_num)
    
    # Track results
    # if (all_comparisons_pass) passed <- passed + 1
    # else failed <- failed + 1
  }
  
  # Report summary
  cat("\nValidation Summary:\n")
  cat("Passed:", passed, "\n")
  cat("Failed:", failed, "\n")
  cat("Pass rate:", sprintf("%.1f%%", 100 * passed / (passed + failed)), "\n")
})

# ==============================================================================
# TOLERANCE REFERENCE
# ==============================================================================

# Standard tolerances by statistic type:
# - Means, SDs, medians: 0.01
# - Test statistics (t, F, chi-squared): 0.001
# - p-values: 0.01
# - Standard errors: 0.001
# - Skewness/Kurtosis: 0.05
# - Counts (N, df): 0.1 (should be exact)
# - Percentages: 0.1
# - Correlations: 0.001

# ==============================================================================
# PLACEHOLDERS TO REPLACE
# ==============================================================================

# [FUNCTION_NAME] - Replace with your R function name (e.g., describe, t_test)
# [function_name] - Replace with lowercase version for parser function
# [VARIABLE_NAME] - Replace with variable to analyze (e.g., age, income)
# [GROUP_VARIABLE] - Replace with grouping variable (e.g., gender, region)

# ==============================================================================
# DEBUGGING TIPS
# ==============================================================================

# 1. If values don't parse, run this to see the output structure:
#    lines <- readLines(spss_file)
#    cat(lines[1:50], sep = "\n")
#
# 2. To see what's being extracted:
#    print(spss_values)
#    str(result)
#
# 3. To check exact differences:
#    cat("SPSS:", spss_values$mean, "\n")
#    cat("R:   ", r_values$mean, "\n")
#    cat("Diff:", abs(spss_values$mean - r_values$mean), "\n")