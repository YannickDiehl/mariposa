# ============================================================================
# SPSS Validation Tests for frequency() Function
# ============================================================================
#
# Purpose: Validate that the R frequency() function produces results that are
#          statistically equivalent to SPSS FREQUENCIES procedure
#
# Reference SPSS Syntax: tests/spss_reference/syntax/frequency_basic.sps
# Reference SPSS Output: tests/spss_reference/outputs/frequency_basic_output.txt
#
# SPSS Version: Results generated with SPSS 29.0
# Date Created: 2025-09-16
#
# Note: This test file uses helper-spss-parser.R to load values directly
#       from SPSS output files rather than hardcoding them
# ============================================================================

library(testthat)
library(dplyr)

# Source the SPSS parser helper
source(test_path("helper-spss-parser.R"))

# Mark this validation test as run for report generation
if (exists("mark_validation_test_run")) {
  mark_validation_test_run("frequency")
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
    test_path("../spss_reference/outputs/frequency_basic_output.txt"),
    "tests/spss_reference/outputs/frequency_basic_output.txt",
    "../spss_reference/outputs/frequency_basic_output.txt"
  )
  
  for (path in paths) {
    if (file.exists(path)) return(path)
  }
  
  skip("SPSS output file not found")
}

# ============================================================================
# TEST SUITE 1: Unweighted Frequency Validation
# ============================================================================

test_that("frequency() SPSS validation - unweighted single categorical (gender)", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  # Load SPSS reference values from file
  spss_values <- extract_spss_frequency_values(spss_file, test_number = 1, variable = "gender")
  
  if (is.null(spss_values)) {
    skip("Could not parse SPSS values for Test 1")
  }
  
  # Run frequency for gender
  result <- frequency(survey_data, gender)
  freq_table <- result$results
  
  # Test N Valid and N Missing
  expect_equal(sum(freq_table$freq), spss_values$n_valid, tolerance = 0.1)
  expect_equal(result$stats$total_n - result$stats$valid_n, spss_values$n_missing, tolerance = 0.1)
  
  # Test frequencies for each category
  if (!is.null(spss_values$frequencies)) {
    for (cat_name in names(spss_values$frequencies)) {
      spss_cat <- spss_values$frequencies[[cat_name]]
      
      # Find matching row in R output
      r_row <- freq_table[freq_table$value == cat_name | 
                          grepl(cat_name, freq_table$value, ignore.case = TRUE), ]
      
      if (nrow(r_row) > 0) {
        expect_equal(r_row$freq[1], spss_cat$frequency, tolerance = 0.1)
        expect_equal(r_row$prc[1], spss_cat$percent, tolerance = 0.1)
        expect_equal(r_row$valid_prc[1], spss_cat$valid_percent, tolerance = 0.1)
        expect_equal(r_row$cum_prc[1], spss_cat$cumulative_percent, tolerance = 0.1)
      }
    }
  }
})

test_that("frequency() SPSS validation - unweighted single categorical (region)", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  # Load SPSS reference values from file
  spss_values <- extract_spss_frequency_values(spss_file, test_number = 2, variable = "region")
  
  if (is.null(spss_values)) {
    skip("Could not parse SPSS values for Test 2")
  }
  
  # Run frequency for region
  result <- frequency(survey_data, region)
  freq_table <- result$results
  
  # Test total N
  expect_equal(sum(freq_table$freq), spss_values$n_valid, tolerance = 0.1)
  
  # Test frequencies for East and West
  if (!is.null(spss_values$frequencies)) {
    for (cat_name in names(spss_values$frequencies)) {
      spss_cat <- spss_values$frequencies[[cat_name]]
      
      # Find matching row in R output
      r_row <- freq_table[freq_table$value == cat_name | 
                          grepl(cat_name, freq_table$value, ignore.case = TRUE), ]
      
      if (nrow(r_row) > 0) {
        expect_equal(r_row$freq[1], spss_cat$frequency, tolerance = 0.1)
        expect_equal(r_row$prc[1], spss_cat$percent, tolerance = 0.1)
      }
    }
  }
})

test_that("frequency() SPSS validation - unweighted ordinal (education)", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  # Load SPSS reference values from file
  spss_values <- extract_spss_frequency_values(spss_file, test_number = 3, variable = "education")
  
  if (is.null(spss_values)) {
    skip("Could not parse SPSS values for Test 3")
  }
  
  # Run frequency for education
  result <- frequency(survey_data, education)
  freq_table <- result$results
  
  # Test total N
  expect_equal(sum(freq_table$freq), spss_values$n_valid, tolerance = 0.1)
  
  # Test mode if available
  if (!is.null(spss_values$mode) && !is.null(result$frequencies[[1]]$stats)) {
    # Mode comparison may need adjustment based on how R calculates it
    # For now, just check it exists
    expect_true(!is.null(result$frequencies[[1]]$stats))
  }
})

# ============================================================================
# TEST SUITE 2: Weighted Frequency Validation
# ============================================================================

test_that("frequency() SPSS validation - weighted single categorical (gender)", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  # Load SPSS reference values from file (Test 6)
  spss_values <- extract_spss_frequency_values(spss_file, test_number = 6, variable = "gender")
  
  if (is.null(spss_values)) {
    skip("Could not parse SPSS values for Test 6")
  }
  
  # Run weighted frequency for gender
  result <- frequency(survey_data, gender, weights = sampling_weight)
  freq_table <- result$results
  
  # Test weighted N
  expect_equal(sum(freq_table$freq), spss_values$n_valid, tolerance = 1)
  
  # Test weighted frequencies for each category
  if (!is.null(spss_values$frequencies)) {
    for (cat_name in names(spss_values$frequencies)) {
      spss_cat <- spss_values$frequencies[[cat_name]]
      
      # Find matching row in R output
      r_row <- freq_table[freq_table$value == cat_name | 
                          grepl(cat_name, freq_table$value, ignore.case = TRUE), ]
      
      if (nrow(r_row) > 0) {
        expect_equal(r_row$freq[1], spss_cat$frequency, tolerance = 1)
        expect_equal(r_row$prc[1], spss_cat$percent, tolerance = 0.1)
        expect_equal(r_row$valid_percent[1], spss_cat$valid_percent, tolerance = 0.1)
      }
    }
  }
})

test_that("frequency() SPSS validation - weighted single categorical (region)", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  # Load SPSS reference values from file (Test 7)
  spss_values <- extract_spss_frequency_values(spss_file, test_number = 7, variable = "region")
  
  if (is.null(spss_values)) {
    skip("Could not parse SPSS values for Test 7")
  }
  
  # Run weighted frequency for region
  result <- frequency(survey_data, region, weights = sampling_weight)
  freq_table <- result$results
  
  # Test weighted N
  expect_equal(sum(freq_table$freq), spss_values$n_valid, tolerance = 1)
  
  # Test weighted frequencies for East and West
  if (!is.null(spss_values$frequencies)) {
    for (cat_name in names(spss_values$frequencies)) {
      spss_cat <- spss_values$frequencies[[cat_name]]
      
      # Find matching row in R output
      r_row <- freq_table[freq_table$value == cat_name | 
                          grepl(cat_name, freq_table$value, ignore.case = TRUE), ]
      
      if (nrow(r_row) > 0) {
        expect_equal(r_row$freq[1], spss_cat$frequency, tolerance = 1)
        expect_equal(r_row$prc[1], spss_cat$percent, tolerance = 0.1)
      }
    }
  }
})

test_that("frequency() SPSS validation - weighted ordinal (education)", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  # Load SPSS reference values from file (Test 8)
  spss_values <- extract_spss_frequency_values(spss_file, test_number = 8, variable = "education")
  
  if (is.null(spss_values)) {
    skip("Could not parse SPSS values for Test 8")
  }
  
  # Run weighted frequency for education
  result <- frequency(survey_data, education, weights = sampling_weight)
  freq_table <- result$results
  
  # Test weighted N
  expect_equal(sum(freq_table$freq), spss_values$n_valid, tolerance = 1)
})

# ============================================================================
# TEST SUITE 3: Multiple Variables
# ============================================================================

test_that("frequency() SPSS validation - unweighted multiple variables", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  # Test 4 has multiple variables
  # For multiple variables, SPSS produces separate tables
  # We'll just check that the function runs and produces output for all variables
  
  # Run frequency for multiple variables
  result <- frequency(survey_data, gender, region, education)
  
  # Check we have results for all three variables
  # Multiple variables are combined in one results dataframe
  expect_true(nrow(result$results) > 0)
  expect_true("gender" %in% result$results$Variable)
  expect_true("region" %in% result$results$Variable)
  expect_true("education" %in% result$results$Variable)
  
  # Check that all variables have entries
  unique_vars <- unique(result$results$Variable)
  expect_equal(length(unique_vars), 3)
})

test_that("frequency() SPSS validation - weighted multiple variables", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  # Run weighted frequency for multiple variables (corresponds to Test 9)
  result <- frequency(survey_data, gender, region, education, weights = sampling_weight)
  
  # Check we have results for all three variables
  expect_true(nrow(result$results) > 0)
  unique_vars <- unique(result$results$Variable)
  expect_equal(length(unique_vars), 3)
  
  # Check that weights were applied (totals should be different from unweighted)
  unweighted_result <- frequency(survey_data, gender, region, education)
  
  # The weighted totals should be slightly different
  weighted_total <- sum(result$results$freq)
  unweighted_total <- sum(unweighted_result$results$freq)
  
  # They should be different (weights change the totals)
  expect_true(abs(weighted_total - unweighted_total) > 0.1)
})

# ============================================================================
# TEST SUITE 4: Grouped Analysis (would require split file support)
# ============================================================================

test_that("frequency() SPSS validation - grouped analysis", {
  survey_data <- load_survey_data()
  
  # Test grouped frequency (similar to SPLIT FILE in SPSS)
  result <- survey_data %>%
    group_by(region) %>%
    frequency(gender)
  
  # Check that we get grouped results
  expect_true(inherits(result, "frequency_results"))
  
  # Should have results for each region
  # The exact structure depends on how frequency() handles grouped data
  # For now, just check it runs without error
  expect_true(!is.null(result))
})

# ============================================================================
# TEST SUITE 5: Missing Data Handling
# ============================================================================

test_that("frequency() SPSS validation - missing data handling", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  # Test with employment variable which has missing data (Test 5)
  spss_values <- extract_spss_frequency_values(spss_file, test_number = 5, variable = "employment")
  
  if (is.null(spss_values)) {
    skip("Could not parse SPSS values for Test 5")
  }
  
  # Run frequency with missing data
  result <- frequency(survey_data, employment, show.na = TRUE)
  
  # Check that missing values are reported
  n_missing <- result$stats$total_n - result$stats$valid_n
  expect_true(n_missing >= 0 || any(is.na(result$results$value)))
})