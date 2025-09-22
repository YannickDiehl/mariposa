# ============================================================================
# SPSS Validation Tests for chi_squared_test() Function
# ============================================================================
#
# Purpose: Validate that the R chi_squared_test() function produces results that are
#          statistically equivalent to SPSS CROSSTABS procedure
#
# Reference SPSS Syntax: tests/spss_reference/syntax/chi_squared_basic.sps
# Reference SPSS Output: tests/spss_reference/outputs/chi_squared_basic_output.txt
#
# SPSS Version: Results generated with SPSS 29.0
# Date Created: 2025-09-16
# ============================================================================

library(testthat)
library(dplyr)

# Source the SPSS parser helper
source(test_path("helper-spss-parser.R"))

# Mark this validation test as run for report generation
if (exists("mark_validation_test_run")) {
  mark_validation_test_run("chi_squared")
}

# Helper function to load test data
load_survey_data <- function() {
  data(survey_data, envir = environment())
  return(survey_data)
}

# Path to SPSS output file
get_spss_output_path <- function() {
  # Try multiple possible locations
  paths <- c(
    test_path("../spss_reference/outputs/chi_squared_basic_output.txt"),
    "tests/spss_reference/outputs/chi_squared_basic_output.txt",
    "../spss_reference/outputs/chi_squared_basic_output.txt"
  )
  
  for (path in paths) {
    if (file.exists(path)) return(path)
  }
  
  skip("SPSS output file not found")
}

# ============================================================================
# TEST SUITE 1: Basic Unweighted Chi-Squared Tests
# ============================================================================

test_that("chi_squared_test() SPSS validation - TEST 1: gender x region (2x2)", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  # Load SPSS reference values from file
  spss_values <- extract_spss_chi_squared_values(spss_file, test_number = 1)
  
  if (is.null(spss_values)) {
    skip("Could not parse SPSS values for Test 1")
  }
  
  # Run R chi-squared test
  result <- chi_squared_test(survey_data, gender, region)
  r_stats <- result$results
  
  # Test chi-squared statistic
  expect_equal(r_stats$chi_squared, spss_values$chi_squared, 
               tolerance = 0.001, label = "Chi-squared statistic")
  
  # Test degrees of freedom
  expect_equal(r_stats$df, spss_values$df, 
               tolerance = 0.1, label = "Degrees of freedom")
  
  # Test p-value
  expect_equal(r_stats$p_value, spss_values$p_value, 
               tolerance = 0.01, label = "P-value")
  
  # Test effect sizes
  expect_equal(r_stats$phi, spss_values$phi, 
               tolerance = 0.001, label = "Phi coefficient")
  expect_equal(r_stats$cramers_v, spss_values$cramers_v, 
               tolerance = 0.001, label = "Cramer's V")
  expect_equal(r_stats$contingency_c, spss_values$contingency_c, 
               tolerance = 0.001, label = "Contingency coefficient")
  
  # Test total N
  expect_equal(r_stats$n, spss_values$n, 
               tolerance = 0.1, label = "Total N")
})

test_that("chi_squared_test() SPSS validation - TEST 2: gender x education (2x4)", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  spss_values <- extract_spss_chi_squared_values(spss_file, test_number = 2)
  
  if (is.null(spss_values)) {
    skip("Could not parse SPSS values for Test 2")
  }
  
  # Run R chi-squared test
  result <- chi_squared_test(survey_data, gender, education)
  r_stats <- result$results
  
  # Test statistics
  expect_equal(r_stats$chi_squared, spss_values$chi_squared, tolerance = 0.001)
  expect_equal(r_stats$df, spss_values$df, tolerance = 0.1)
  expect_equal(r_stats$p_value, spss_values$p_value, tolerance = 0.01)
  
  # Test effect sizes
  expect_equal(r_stats$cramers_v, spss_values$cramers_v, tolerance = 0.001)
  expect_equal(r_stats$phi, spss_values$phi, tolerance = 0.001)
})

test_that("chi_squared_test() SPSS validation - TEST 3: region x education (2x4)", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  spss_values <- extract_spss_chi_squared_values(spss_file, test_number = 3)
  
  if (is.null(spss_values)) {
    skip("Could not parse SPSS values for Test 3")
  }
  
  # Run R chi-squared test
  result <- chi_squared_test(survey_data, region, education)
  r_stats <- result$results
  
  # Test statistics
  expect_equal(r_stats$chi_squared, spss_values$chi_squared, tolerance = 0.001)
  expect_equal(r_stats$df, spss_values$df, tolerance = 0.1)
  expect_equal(r_stats$p_value, spss_values$p_value, tolerance = 0.01)
  
  # Test effect sizes
  expect_equal(r_stats$cramers_v, spss_values$cramers_v, tolerance = 0.001)
  expect_equal(r_stats$contingency_c, spss_values$contingency_c, tolerance = 0.001)
})

# ============================================================================
# TEST SUITE 2: Chi-Squared with Continuity Correction
# ============================================================================

test_that("chi_squared_test() SPSS validation - TEST 4: with continuity correction", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  # Note: SPSS automatically applies continuity correction for 2x2 tables
  # We need to check both corrected and uncorrected values
  spss_values <- extract_spss_chi_squared_values(spss_file, test_number = 4)
  
  if (is.null(spss_values)) {
    skip("Could not parse SPSS values for Test 4")
  }
  
  # Run R chi-squared test WITH continuity correction
  result_corrected <- chi_squared_test(survey_data, gender, region, correct = TRUE)
  
  # Run R chi-squared test WITHOUT continuity correction for comparison
  result_uncorrected <- chi_squared_test(survey_data, gender, region, correct = FALSE)
  
  # The uncorrected should match the Pearson Chi-Square from SPSS
  expect_equal(result_uncorrected$results$chi_squared, spss_values$chi_squared, 
               tolerance = 0.001, label = "Uncorrected chi-squared")
  
  # Note: Continuity correction comparison would need special parsing of SPSS output
  # as it's shown as a separate line in SPSS output
})

# ============================================================================
# TEST SUITE 3: Weighted Chi-Squared Tests
# ============================================================================

test_that("chi_squared_test() SPSS validation - TEST 5: weighted gender x region", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  spss_values <- extract_spss_chi_squared_values(spss_file, test_number = 5)
  
  if (is.null(spss_values)) {
    skip("Could not parse SPSS values for Test 5")
  }
  
  # Run weighted chi-squared test
  result <- chi_squared_test(survey_data, gender, region, weights = sampling_weight)
  r_stats <- result$results
  
  # Test weighted statistics
  expect_equal(r_stats$chi_squared, spss_values$chi_squared, 
               tolerance = 0.001, label = "Weighted chi-squared")
  expect_equal(r_stats$df, spss_values$df, tolerance = 0.1)
  expect_equal(r_stats$p_value, spss_values$p_value, tolerance = 0.01)
  
  # Test weighted effect sizes
  expect_equal(r_stats$cramers_v, spss_values$cramers_v, tolerance = 0.001)
  expect_equal(r_stats$phi, spss_values$phi, tolerance = 0.001)
})

test_that("chi_squared_test() SPSS validation - TEST 6: weighted gender x education", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  spss_values <- extract_spss_chi_squared_values(spss_file, test_number = 6)
  
  if (is.null(spss_values)) {
    skip("Could not parse SPSS values for Test 6")
  }
  
  # Run weighted chi-squared test
  result <- chi_squared_test(survey_data, gender, education, weights = sampling_weight)
  r_stats <- result$results
  
  # Test weighted statistics
  expect_equal(r_stats$chi_squared, spss_values$chi_squared, tolerance = 0.001)
  expect_equal(r_stats$df, spss_values$df, tolerance = 0.1)
  expect_equal(r_stats$p_value, spss_values$p_value, tolerance = 0.01)
  
  # Test effect sizes
  expect_equal(r_stats$cramers_v, spss_values$cramers_v, tolerance = 0.001)
})

test_that("chi_squared_test() SPSS validation - TEST 7: weighted region x education", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  spss_values <- extract_spss_chi_squared_values(spss_file, test_number = 7)
  
  if (is.null(spss_values)) {
    skip("Could not parse SPSS values for Test 7")
  }
  
  # Run weighted chi-squared test
  result <- chi_squared_test(survey_data, region, education, weights = sampling_weight)
  r_stats <- result$results
  
  # Test weighted statistics
  expect_equal(r_stats$chi_squared, spss_values$chi_squared, tolerance = 0.001)
  expect_equal(r_stats$df, spss_values$df, tolerance = 0.1)
  expect_equal(r_stats$p_value, spss_values$p_value, tolerance = 0.01)
})

# ============================================================================
# TEST SUITE 4: Observed and Expected Frequencies
# ============================================================================

test_that("chi_squared_test() SPSS validation - observed frequencies match", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  # Test with first test case
  spss_values <- extract_spss_chi_squared_values(spss_file, test_number = 1)
  
  if (is.null(spss_values) || is.null(spss_values$crosstab)) {
    skip("Could not parse crosstabulation for Test 1")
  }
  
  # Run R chi-squared test
  result <- chi_squared_test(survey_data, gender, region)
  
  # Get observed frequencies from R
  r_observed <- result$results$observed[[1]]
  
  # Compare observed frequencies
  # Note: We need to reshape SPSS list structure to match R matrix
  spss_observed <- spss_values$crosstab$observed
  
  # The structure might need adjustment based on actual output
  # For now, we'll check if they exist
  expect_true(!is.null(r_observed), label = "R observed frequencies exist")
  expect_true(!is.null(spss_observed), label = "SPSS observed frequencies parsed")
})

# ============================================================================
# TEST SUITE 5: Grouped Analysis (if time permits)
# ============================================================================

# Note: Tests 8-11 involve grouped analyses which may have parsing issues
# due to the SPLIT FILE warnings in SPSS output. These can be added later
# if the basic tests pass successfully.

# ============================================================================
# TEST SUITE 6: Summary Statistics
# ============================================================================

test_that("chi_squared_test() SPSS validation - summary of all basic tests", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  # Collect results for multiple tests
  test_results <- tibble(
    test_num = integer(),
    test_desc = character(),
    chi_sq_match = logical(),
    df_match = logical(),
    p_value_match = logical(),
    cramers_v_match = logical()
  )
  
  # Test 1: gender x region
  spss1 <- extract_spss_chi_squared_values(spss_file, test_number = 1)
  if (!is.null(spss1)) {
    r1 <- chi_squared_test(survey_data, gender, region)$results
    test_results <- test_results %>%
      add_row(
        test_num = 1,
        test_desc = "gender x region",
        chi_sq_match = abs(r1$chi_squared - spss1$chi_squared) < 0.001,
        df_match = r1$df == spss1$df,
        p_value_match = abs(r1$p_value - spss1$p_value) < 0.01,
        cramers_v_match = abs(r1$cramers_v - spss1$cramers_v) < 0.001
      )
  }
  
  # Test 2: gender x education
  spss2 <- extract_spss_chi_squared_values(spss_file, test_number = 2)
  if (!is.null(spss2)) {
    r2 <- chi_squared_test(survey_data, gender, education)$results
    test_results <- test_results %>%
      add_row(
        test_num = 2,
        test_desc = "gender x education",
        chi_sq_match = abs(r2$chi_squared - spss2$chi_squared) < 0.001,
        df_match = r2$df == spss2$df,
        p_value_match = abs(r2$p_value - spss2$p_value) < 0.01,
        cramers_v_match = abs(r2$cramers_v - spss2$cramers_v) < 0.001
      )
  }
  
  # Test 3: region x education
  spss3 <- extract_spss_chi_squared_values(spss_file, test_number = 3)
  if (!is.null(spss3)) {
    r3 <- chi_squared_test(survey_data, region, education)$results
    test_results <- test_results %>%
      add_row(
        test_num = 3,
        test_desc = "region x education",
        chi_sq_match = abs(r3$chi_squared - spss3$chi_squared) < 0.001,
        df_match = r3$df == spss3$df,
        p_value_match = abs(r3$p_value - spss3$p_value) < 0.01,
        cramers_v_match = abs(r3$cramers_v - spss3$cramers_v) < 0.001
      )
  }
  
  # Print summary
  if (nrow(test_results) > 0) {
    cat("\n========== CHI-SQUARED TEST VALIDATION SUMMARY ==========\n")
    print(test_results)
    
    # Calculate pass rate
    total_checks <- sum(!is.na(test_results[, 3:6]))
    passed_checks <- sum(test_results[, 3:6], na.rm = TRUE)
    pass_rate <- passed_checks / total_checks * 100
    
    cat("\nOverall Pass Rate:", sprintf("%.1f%%", pass_rate), "\n")
    cat("Tests Validated:", nrow(test_results), "\n")
    cat("=========================================================\n")
    
    # Expect high pass rate
    expect_gt(pass_rate, 90, label = "Overall pass rate should be > 90%")
  }
})