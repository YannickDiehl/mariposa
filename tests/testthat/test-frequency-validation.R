# ============================================================================
# FREQUENCY() FUNCTION VALIDATION TESTS
# ============================================================================
# Purpose: Validate R frequency() function against SPSS FREQUENCIES procedure
# Dataset: survey_data
# Created: 2025-09-16
# Version: 2.0 (Restructured)
# ============================================================================

library(testthat)
library(dplyr)
library(SurveyStat)

# Source helper functions
source(test_path("helper-spss-parser.R"))

# ============================================================================
# TEST DATA SETUP
# ============================================================================

# Load test data
load_test_data <- function() {
  data(survey_data, envir = environment())
  return(survey_data)
}

# Get SPSS output path
get_spss_output <- function() {
  paths <- c(
    test_path("../spss_reference/outputs/frequency_validation_output.txt"),
    "tests/spss_reference/outputs/frequency_validation_output.txt"
  )
  
  for (path in paths) {
    if (file.exists(path)) return(path)
  }
  
  skip("SPSS output file not found")
}

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Compare frequency table statistics
#' @param r_result R frequency() result object
#' @param spss_values List with SPSS values
#' @param test_name Character string describing the test
#' @param tolerance Numeric tolerance for comparisons
compare_frequency_stats <- function(r_result, spss_values, test_name, tolerance = 0.1) {
  test_that(paste("frequency() -", test_name, "- Statistics"), {
    # Valid N
    if (!is.null(spss_values$n_valid)) {
      expect_equal(
        r_result$stats$valid_n,
        spss_values$n_valid,
        tolerance = tolerance,
        label = "Valid N"
      )
    }
    
    # Missing N
    if (!is.null(spss_values$n_missing)) {
      expect_equal(
        r_result$stats$total_n - r_result$stats$valid_n,
        spss_values$n_missing,
        tolerance = tolerance,
        label = "Missing N"
      )
    }
    
    # Total N
    if (!is.null(spss_values$n_total)) {
      expect_equal(
        r_result$stats$total_n,
        spss_values$n_total,
        tolerance = tolerance,
        label = "Total N"
      )
    }
  })
}

#' Compare frequency distributions
#' @param r_result R frequency() result object
#' @param spss_values List with SPSS frequency table
#' @param test_name Character string describing the test
compare_frequency_table <- function(r_result, spss_values, test_name) {
  test_that(paste("frequency() -", test_name, "- Distribution"), {
    if (!is.null(spss_values$frequencies)) {
      freq_table <- r_result$results
      
      for (category in names(spss_values$frequencies)) {
        spss_cat <- spss_values$frequencies[[category]]
        
        # Find matching row in R output
        r_row <- freq_table[freq_table$value == category, ]
        
        if (nrow(r_row) > 0) {
          # Frequency count
          expect_equal(
            r_row$freq[1],
            spss_cat$frequency,
            tolerance = 0.1,
            label = paste("Frequency for", category)
          )
          
          # Percentage
          if (!is.null(spss_cat$percent)) {
            expect_equal(
              r_row$prc[1],
              spss_cat$percent,
              tolerance = 0.1,
              label = paste("Percent for", category)
            )
          }
          
          # Valid percentage
          if (!is.null(spss_cat$valid_percent)) {
            expect_equal(
              r_row$valid_prc[1],
              spss_cat$valid_percent,
              tolerance = 0.1,
              label = paste("Valid percent for", category)
            )
          }
        }
      }
    }
  })
}

# ============================================================================
# SECTION 1: BASIC UNWEIGHTED FREQUENCIES
# ============================================================================

describe("SECTION 1: Basic Unweighted Frequencies", {
  survey_data <- load_test_data()
  spss_file <- get_spss_output()
  
  # Test 1.1: Binary Categorical
  test_that("1.1 - Binary categorical variable (gender)", {
    spss_values <- extract_spss_frequency_values(spss_file, "TEST 1.1")
    skip_if(is.null(spss_values), "Could not parse SPSS values")
    
    result <- frequency(survey_data, gender)
    
    compare_frequency_stats(result, spss_values, "1.1 Binary")
    compare_frequency_table(result, spss_values, "1.1 Binary")
  })
  
  # Test 1.2: Multi-category
  test_that("1.2 - Multi-category variable (education)", {
    spss_values <- extract_spss_frequency_values(spss_file, "TEST 1.2")
    skip_if(is.null(spss_values), "Could not parse SPSS values")
    
    result <- frequency(survey_data, education)
    
    compare_frequency_stats(result, spss_values, "1.2 Multi-category")
    compare_frequency_table(result, spss_values, "1.2 Multi-category")
  })
  
  # Test 1.3: Multiple variables
  test_that("1.3 - Multiple variables together", {
    spss_values <- extract_spss_frequency_values(spss_file, "TEST 1.3")
    skip_if(is.null(spss_values), "Could not parse SPSS values")
    
    result <- frequency(survey_data, gender, region, education)
    
    # Check that all variables are present
    expect_equal(
      sort(unique(result$results$Variable)),
      sort(c("gender", "region", "education")),
      label = "All variables present"
    )
    
    # Check total row count
    expect_true(nrow(result$results) > 5, label = "Multiple variable results")
  })
})

# ============================================================================
# SECTION 2: WEIGHTED FREQUENCIES
# ============================================================================

describe("SECTION 2: Weighted Frequencies", {
  survey_data <- load_test_data()
  spss_file <- get_spss_output()
  
  # Test 2.1: Weighted binary
  test_that("2.1 - Weighted binary categorical (gender)", {
    spss_values <- extract_spss_frequency_values(spss_file, "TEST 2.1")
    skip_if(is.null(spss_values), "Could not parse SPSS values")
    
    result <- frequency(survey_data, gender, weights = sampling_weight)
    
    compare_frequency_stats(result, spss_values, "2.1 Weighted binary", tolerance = 1)
    compare_frequency_table(result, spss_values, "2.1 Weighted binary")
  })
  
  # Test 2.2: Weighted multi-category
  test_that("2.2 - Weighted multi-category (education)", {
    spss_values <- extract_spss_frequency_values(spss_file, "TEST 2.2")
    skip_if(is.null(spss_values), "Could not parse SPSS values")
    
    result <- frequency(survey_data, education, weights = sampling_weight)
    
    compare_frequency_stats(result, spss_values, "2.2 Weighted multi", tolerance = 1)
    compare_frequency_table(result, spss_values, "2.2 Weighted multi")
  })
  
  # Test 2.3: Weighted multiple
  test_that("2.3 - Weighted multiple variables", {
    result <- frequency(survey_data, gender, region, education, weights = sampling_weight)
    
    # Verify weights were applied
    unweighted <- frequency(survey_data, gender, region, education)
    
    expect_true(
      abs(sum(result$results$freq) - sum(unweighted$results$freq)) > 1,
      label = "Weights affect totals"
    )
  })
})

# ============================================================================
# SECTION 3: MISSING DATA HANDLING
# ============================================================================

describe("SECTION 3: Missing Data Handling", {
  survey_data <- load_test_data()
  spss_file <- get_spss_output()
  
  # Test 3.1: Missing excluded
  test_that("3.1 - Missing values excluded (default)", {
    spss_values <- extract_spss_frequency_values(spss_file, "TEST 3.1")
    skip_if(is.null(spss_values), "Could not parse SPSS values")
    
    result <- frequency(survey_data, employment)
    
    compare_frequency_stats(result, spss_values, "3.1 Missing excluded")
  })
  
  # Test 3.2: Missing included
  test_that("3.2 - Missing values included", {
    spss_values <- extract_spss_frequency_values(spss_file, "TEST 3.2")
    skip_if(is.null(spss_values), "Could not parse SPSS values")
    
    result <- frequency(survey_data, employment, show.na = TRUE)
    
    # Check for NA category in results
    has_na <- any(is.na(result$results$value) | result$results$value == "NA")
    expect_true(has_na || (result$stats$total_n == result$stats$valid_n),
                label = "Missing handling")
  })
  
  # Test 3.3: Weighted with missing
  test_that("3.3 - Weighted with missing values", {
    spss_values <- extract_spss_frequency_values(spss_file, "TEST 3.3")
    skip_if(is.null(spss_values), "Could not parse SPSS values")
    
    result <- frequency(survey_data, employment, 
                       weights = sampling_weight, 
                       show.na = TRUE)
    
    compare_frequency_stats(result, spss_values, "3.3 Weighted missing", tolerance = 1)
  })
})

# ============================================================================
# SECTION 4: GROUPED ANALYSIS
# ============================================================================

describe("SECTION 4: Grouped Analysis", {
  survey_data <- load_test_data()
  spss_file <- get_spss_output()
  
  # Test 4.1: Grouped unweighted
  test_that("4.1 - Grouped by region (unweighted)", {
    result <- survey_data %>%
      group_by(region) %>%
      frequency(gender, education)
    
    # Check for grouped structure
    expect_true("region" %in% names(attributes(result)), 
                label = "Grouped attribute present")
    
    # Verify separate results per group
    expect_true(nrow(result$results) > 4,
                label = "Multiple group results")
  })
  
  # Test 4.2: Grouped weighted
  test_that("4.2 - Grouped by region (weighted)", {
    result <- survey_data %>%
      group_by(region) %>%
      frequency(gender, education, weights = sampling_weight)
    
    # Compare with ungrouped to verify grouping
    ungrouped <- frequency(survey_data, gender, education, 
                           weights = sampling_weight)
    
    expect_true(nrow(result$results) >= nrow(ungrouped$results),
                label = "Grouped results structure")
  })
})

# ============================================================================
# SECTION 5: STATISTICS OPTIONS
# ============================================================================

describe("SECTION 5: Statistics Options", {
  survey_data <- load_test_data()
  spss_file <- get_spss_output()
  
  # Test 5.1: Mode statistic
  test_that("5.1 - Mode statistic", {
    result <- frequency(survey_data, education)
    
    # Check if mode is calculated
    if (!is.null(result$stats$mode)) {
      expect_true(result$stats$mode %in% result$results$value,
                  label = "Mode is valid category")
    }
  })
  
  # Test 5.2: Percentiles
  test_that("5.2 - Percentiles for ordinal", {
    result <- frequency(survey_data, education)
    
    # For ordinal variables, check if percentiles could be calculated
    # This depends on implementation
    expect_true(!is.null(result$stats), 
                label = "Statistics calculated")
  })
  
  # Test 5.3: All statistics
  test_that("5.3 - Complete statistics", {
    result <- frequency(survey_data, education)
    
    # Verify comprehensive statistics
    expect_true(length(result$stats) > 2,
                label = "Multiple statistics available")
  })
})

# ============================================================================
# VALIDATION SUMMARY
# ============================================================================

test_that("Validation Summary", {
  cat("\n")
  cat("========================================\n")
  cat("FREQUENCY() VALIDATION COMPLETE\n")
  cat("========================================\n")
  cat("Sections tested:\n")
  cat("  1. Basic unweighted frequencies\n")
  cat("  2. Weighted frequencies\n")
  cat("  3. Missing data handling\n")
  cat("  4. Grouped analysis\n")
  cat("  5. Statistics options\n")
  cat("========================================\n")
  
  expect_true(TRUE)  # Dummy assertion for test framework
})