# ============================================================================
# SPSS Validation Tests for [FUNCTION_NAME]() Function
# ============================================================================
#
# Purpose: Validate that the R [function]() produces results that are
#          statistically equivalent to SPSS [procedure]
#
# Reference SPSS Syntax: tests/spss_reference/syntax/[function]_validation.sps
# Reference SPSS Output: tests/spss_reference/outputs/[function]_output.txt
#
# Created: [DATE]
# ============================================================================

library(testthat)
library(dplyr)

# Source the SPSS parser helper
source(test_path("helper-spss-parser.R"))

# Helper function to load test data
load_survey_data <- function() {
  data(survey_data, envir = environment())
  return(survey_data)
}

# Path to SPSS output file
get_spss_output_path <- function() {
  # Try multiple possible locations
  paths <- c(
    test_path("../spss_reference/outputs/[function]_output.txt"),
    "tests/spss_reference/outputs/[function]_output.txt"
  )
  
  for (path in paths) {
    if (file.exists(path)) return(path)
  }
  
  skip("SPSS output file not found")
}

# ============================================================================
# SECTION 1: Basic Unweighted Tests
# ============================================================================

describe("SECTION 1: Basic Unweighted Tests", {
  
  test_that("[function]() matches SPSS - single variable", {
    survey_data <- load_survey_data()
    spss_file <- get_spss_output_path()
    
    # Get SPSS values
    spss_values <- extract_[function]_values(spss_file, test_number = 1)
    
    if (is.null(spss_values)) {
      skip("Could not parse SPSS values for Test 1")
    }
    
    # Run R function
    result <- [function](survey_data, [arguments])
    
    # Compare with appropriate tolerances
    expect_equal(result$[statistic], spss_values$statistic1, tolerance = 0.01)
    # Add more comparisons as needed
  })
  
  test_that("[function]() matches SPSS - multiple variables", {
    survey_data <- load_survey_data()
    spss_file <- get_spss_output_path()
    
    # Get SPSS values
    spss_values <- extract_[function]_values(spss_file, test_number = 2)
    
    if (is.null(spss_values)) {
      skip("Could not parse SPSS values for Test 2")
    }
    
    # Run R function
    result <- [function](survey_data, [multiple_variables])
    
    # Compare
    expect_equal(result$[statistic], spss_values$statistic1, tolerance = 0.01)
  })
})

# ============================================================================
# SECTION 2: Weighted Tests
# ============================================================================

describe("SECTION 2: Weighted Tests", {
  
  test_that("[function]() matches SPSS - weighted single variable", {
    survey_data <- load_survey_data()
    spss_file <- get_spss_output_path()
    
    # Get SPSS values
    spss_values <- extract_[function]_values(spss_file, test_number = 3)
    
    if (is.null(spss_values)) {
      skip("Could not parse SPSS values for Test 3")
    }
    
    # Run R function with weights
    result <- [function](survey_data, [variable], weights = sampling_weight)
    
    # Compare
    expect_equal(result$[statistic], spss_values$statistic1, tolerance = 0.01)
  })
})

# ============================================================================
# SECTION 3: Grouped Analysis
# ============================================================================

describe("SECTION 3: Grouped Analysis", {
  
  test_that("[function]() matches SPSS - grouped by region", {
    survey_data <- load_survey_data()
    spss_file <- get_spss_output_path()
    
    # Get SPSS values
    spss_values <- extract_[function]_values(spss_file, test_number = 5)
    
    if (is.null(spss_values)) {
      skip("Could not parse SPSS values for Test 5")
    }
    
    # Run R function with grouping
    result <- survey_data %>%
      group_by(region) %>%
      [function]([variable])
    
    # Compare first group
    expect_equal(result$results[[1]]$[statistic], 
                 spss_values$statistic1, tolerance = 0.01)
  })
})

# ============================================================================
# Tolerance Guidelines
# ============================================================================
# Means, SDs:        0.01
# Test statistics:   0.001  
# P-values:          0.01
# Counts:            0.1 (should be exact)
# Skewness/Kurtosis: 0.05