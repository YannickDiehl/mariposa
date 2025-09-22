# ============================================================================
# SPSS Validation Tests for describe() Function
# ============================================================================
#
# Purpose: Validate that the R describe() function produces results that are
#          statistically equivalent to SPSS DESCRIPTIVES and FREQUENCIES procedures
#
# Reference SPSS Syntax: tests/spss_reference/syntax/describe_basic.sps
# Reference SPSS Output: tests/spss_reference/outputs/describe_basic_output.txt
#
# SPSS Version: Results generated with SPSS 29.0
# Date Created: 2025-09-05
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
  mark_validation_test_run("describe")
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
    test_path("../spss_reference/outputs/describe_basic_output.txt"),
    "tests/spss_reference/outputs/describe_basic_output.txt",
    "../spss_reference/outputs/describe_basic_output.txt"
  )
  
  for (path in paths) {
    if (file.exists(path)) return(path)
  }
  
  skip("SPSS output file not found")
}

# ============================================================================
# TEST SUITE 1: Unweighted Statistics Validation
# ============================================================================

test_that("describe() SPSS validation - unweighted single variable (age)", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  # Load SPSS reference values from file
  spss_values <- extract_spss_values(spss_file, test_number = 1, variable = "age")
  
  if (is.null(spss_values)) {
    skip("Could not parse SPSS values for Test 1")
  }
  
  # Run describe for age
  result <- describe(survey_data, age, show = "all")
  r_stats <- result$results
  
  # Test each statistic with appropriate tolerance
  expect_equal(r_stats$age_N, spss_values$n, tolerance = 0.1)
  expect_equal(r_stats$age_Mean, spss_values$mean, tolerance = 0.01)
  expect_equal(r_stats$age_SD, spss_values$sd, tolerance = 0.01)
  expect_equal(r_stats$age_Variance, spss_values$variance, tolerance = 0.01)
  expect_equal(r_stats$age_Range, spss_values$range, tolerance = 0.01)
  expect_equal(r_stats$age_SE, spss_values$se, tolerance = 0.001)
  
  # Skewness and kurtosis may have slight differences due to formula variations
  expect_equal(r_stats$age_Skewness, spss_values$skewness, tolerance = 0.05)
  expect_equal(r_stats$age_Kurtosis, spss_values$kurtosis, tolerance = 0.05)
})

test_that("describe() SPSS validation - unweighted multiple variables", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  # Parse SPSS output for Test 2
  lines <- readLines(spss_file, warn = FALSE)
  test_start <- grep("TEST 2:.*UNWEIGHTED MULTIPLE", lines)[1]
  
  if (is.na(test_start)) {
    skip("Could not find Test 2 in SPSS output")
  }
  
  # Extract values for each variable from the SPSS output
  # Look for the descriptive statistics table after Test 2
  desc_start <- grep("Descriptive Statistics", lines[test_start:length(lines)])[1]
  desc_start <- test_start + desc_start - 1
  
  # Parse the table - SPSS splits long variable names across lines
  # Based on actual output structure:
  # Line desc_start + 6: "Alter in Jahren" (age)
  # Line desc_start + 8: "Monatliches..." (income) 
  # Line desc_start + 13: "Lebenszuf..." (life satisfaction)
  spss_age <- extract_values_from_line(lines[desc_start + 6], "age")
  spss_income <- extract_values_from_line(lines[desc_start + 8], "income") 
  spss_life <- extract_values_from_line(lines[desc_start + 13], "life")
  
  # Run describe for multiple variables
  result <- describe(survey_data, age, income, life_satisfaction, show = "all")
  r_stats <- result$results
  
  # Test age statistics
  if (!is.null(spss_age)) {
    expect_equal(r_stats$age_Mean, spss_age$mean, tolerance = 0.01)
    expect_equal(r_stats$age_SD, spss_age$sd, tolerance = 0.01)
    expect_equal(r_stats$age_Variance, spss_age$variance, tolerance = 0.01)
  }
  
  # Test income statistics
  if (!is.null(spss_income)) {
    expect_equal(r_stats$income_Mean, spss_income$mean, tolerance = 0.1)
    expect_equal(r_stats$income_SD, spss_income$sd, tolerance = 0.1)
    expect_equal(r_stats$income_Variance, spss_income$variance, tolerance = 1.0)
  }
  
  # Test life_satisfaction statistics
  if (!is.null(spss_life)) {
    expect_equal(r_stats$life_satisfaction_Mean, spss_life$mean, tolerance = 0.01)
    expect_equal(r_stats$life_satisfaction_SD, spss_life$sd, tolerance = 0.01)
    expect_equal(r_stats$life_satisfaction_Variance, spss_life$variance, tolerance = 0.01)
  }
})

# ============================================================================
# TEST SUITE 2: Weighted Statistics Validation
# ============================================================================

test_that("describe() SPSS validation - weighted single variable (age)", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  # Load SPSS weighted values (Test 4 in the output)
  spss_weighted <- extract_spss_values(spss_file, test_number = 4, variable = "age")
  
  if (is.null(spss_weighted)) {
    skip("Could not parse SPSS weighted values for Test 4")
  }
  
  # Run describe with weights
  result <- describe(survey_data, age, weights = sampling_weight, show = "all")
  r_stats <- result$results
  
  # Test weighted statistics
  expect_equal(r_stats$age_Mean, spss_weighted$mean, tolerance = 0.01)
  expect_equal(r_stats$age_SD, spss_weighted$sd, tolerance = 0.01)
  expect_equal(r_stats$age_Variance, spss_weighted$variance, tolerance = 0.01)
  expect_equal(r_stats$age_Range, spss_weighted$range, tolerance = 0.01)
  expect_equal(r_stats$age_SE, spss_weighted$se, tolerance = 0.01)  # Slightly higher tolerance for weighted SE
  expect_equal(r_stats$age_Skewness, spss_weighted$skewness, tolerance = 0.05)
  expect_equal(r_stats$age_Kurtosis, spss_weighted$kurtosis, tolerance = 0.05)
})

test_that("describe() SPSS validation - weighted multiple variables", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  # Parse SPSS output for Test 5 (weighted multiple variables)
  lines <- readLines(spss_file, warn = FALSE)
  test_start <- grep("TEST 5:.*WEIGHTED MULTIPLE", lines)[1]
  
  if (is.na(test_start)) {
    skip("Could not find Test 5 in SPSS output")
  }
  
  # Extract weighted statistics
  desc_start <- grep("Descriptive Statistics", lines[test_start:length(lines)])[1]
  if (!is.na(desc_start)) {
    desc_start <- test_start + desc_start - 1
    
    # Parse weighted values - same structure as unweighted
    # Line desc_start + 6: "Alter in Jahren" (age)
    # Line desc_start + 8: "Monatliches..." (income) 
    # Line desc_start + 13: "Lebenszuf..." (life satisfaction)
    spss_age_w <- extract_values_from_line(lines[desc_start + 6], "age")
    spss_income_w <- extract_values_from_line(lines[desc_start + 8], "income")
    spss_life_w <- extract_values_from_line(lines[desc_start + 13], "life")
  } else {
    skip("Could not parse weighted statistics table")
  }
  
  # Run describe with weights for multiple variables
  result <- describe(survey_data, age, income, life_satisfaction, 
                    weights = sampling_weight, show = "all")
  r_stats <- result$results
  
  # Test weighted statistics for each variable
  if (!is.null(spss_age_w)) {
    expect_equal(r_stats$age_Mean, spss_age_w$mean, tolerance = 0.01)
    expect_equal(r_stats$age_SD, spss_age_w$sd, tolerance = 0.01)
  }
  
  if (!is.null(spss_income_w)) {
    expect_equal(r_stats$income_Mean, spss_income_w$mean, tolerance = 0.1)
    expect_equal(r_stats$income_SD, spss_income_w$sd, tolerance = 0.1)
  }
  
  if (!is.null(spss_life_w)) {
    expect_equal(r_stats$life_satisfaction_Mean, spss_life_w$mean, tolerance = 0.01)
    expect_equal(r_stats$life_satisfaction_SD, spss_life_w$sd, tolerance = 0.01)
  }
})

# ============================================================================
# TEST SUITE 3: Grouped Analysis Validation
# ============================================================================

test_that("describe() SPSS validation - grouped by region (unweighted)", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  # Use the improved grouped parser function from helper
  grouped_stats <- parse_spss_grouped(spss_file, group_var = "Region", variable = "age")
  
  if (is.null(grouped_stats) || length(grouped_stats) == 0) {
    skip("Could not parse grouped analysis in SPSS output")
  }
  
  # Extract statistics for each region
  east_stats <- grouped_stats[["East"]]
  west_stats <- grouped_stats[["West"]]
  
  # Run grouped describe
  result <- survey_data %>%
    group_by(region) %>%
    describe(age, income, life_satisfaction, show = "all")
  
  # Get results for each group
  east_results <- result$results[result$results$region == "East", ]
  west_results <- result$results[result$results$region == "West", ]
  
  # Test East group
  if (!is.null(east_stats)) {
    expect_equal(east_results$age_Mean, east_stats$mean, tolerance = 0.01)
    expect_equal(east_results$age_SD, east_stats$sd, tolerance = 0.01)
  }
  
  # Test West group
  if (!is.null(west_stats)) {
    expect_equal(west_results$age_Mean, west_stats$mean, tolerance = 0.01)
    expect_equal(west_results$age_SD, west_stats$sd, tolerance = 0.01)
  }
})

# ============================================================================
# TEST SUITE 4: Percentiles and IQR Validation
# ============================================================================

test_that("describe() SPSS validation - percentiles and IQR (unweighted)", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  # Parse FREQUENCIES output for percentiles (Test 3)
  lines <- readLines(spss_file, warn = FALSE)
  freq_start <- grep("TEST 3:.*FREQUENCIES", lines)[1]
  
  if (is.na(freq_start)) {
    skip("Could not find FREQUENCIES output")
  }
  
  # Find the Statistics table with percentiles
  stats_table <- grep("^Statistics", lines[freq_start:length(lines)])[1]
  if (!is.na(stats_table)) {
    stats_table <- freq_start + stats_table - 1
    
    # Extract percentiles
    spss_percentiles <- list(age = list(), income = list(), life = list())
    
    # Look for percentile lines
    # SPSS format: "Percentiles 25      38.0000         2700.0000                            3.00"
    for (i in stats_table:(stats_table + 40)) {
      if (i > length(lines)) break
      line <- lines[i]
      
      # Check for line with "Percentiles 25"
      if (grepl("Percentiles\\s+25", line)) {
        # Extract values after the label
        values <- as.numeric(unlist(regmatches(line, 
                                              gregexpr("\\d+\\.?\\d*", line))))
        # First value is 25, then the actual percentile values
        if (length(values) >= 4) {
          spss_percentiles$age$q25 <- values[2]      # Skip the "25" label
          spss_percentiles$income$q25 <- values[3]
          spss_percentiles$life$q25 <- values[4]
        }
      }
      
      # Check for median (50th percentile)
      if (grepl("^\\s+50\\s", line)) {
        values <- as.numeric(unlist(regmatches(line, 
                                              gregexpr("\\d+\\.?\\d*", line))))
        # First value is 50, then the actual percentile values
        if (length(values) >= 4) {
          spss_percentiles$age$median <- values[2]
          spss_percentiles$income$median <- values[3]
          spss_percentiles$life$median <- values[4]
        }
      }
      
      # Check for 75th percentile
      if (grepl("^\\s+75\\s", line)) {
        values <- as.numeric(unlist(regmatches(line, 
                                              gregexpr("\\d+\\.?\\d*", line))))
        # First value is 75, then the actual percentile values
        if (length(values) >= 4) {
          spss_percentiles$age$q75 <- values[2]
          spss_percentiles$income$q75 <- values[3]
          spss_percentiles$life$q75 <- values[4]
        }
      }
    }
  }
  
  # Run describe with quantiles
  result <- describe(survey_data, age, income, life_satisfaction, 
                    show = "all", probs = c(0.25, 0.50, 0.75))
  r_stats <- result$results
  
  # Test percentiles if we found them
  if (length(spss_percentiles$age) > 0) {
    expect_equal(unname(r_stats$age_Q25), spss_percentiles$age$q25, tolerance = 0.1)
    expect_equal(unname(r_stats$age_Median), spss_percentiles$age$median, tolerance = 0.1)
    expect_equal(unname(r_stats$age_Q75), spss_percentiles$age$q75, tolerance = 0.1)
    
    # Test IQR calculation
    if (!is.null(spss_percentiles$age$q75) && !is.null(spss_percentiles$age$q25)) {
      spss_iqr <- spss_percentiles$age$q75 - spss_percentiles$age$q25
      calculated_iqr <- unname(r_stats$age_Q75) - unname(r_stats$age_Q25)
      expect_equal(calculated_iqr, spss_iqr, tolerance = 0.1)
    }
  }
})

# ============================================================================
# TEST SUITE 5: Direct Value Extraction Tests
# ============================================================================

test_that("SPSS parser correctly extracts values from output file", {
  spss_file <- get_spss_output_path()
  
  # Test that we can extract values from Test 1
  test1_values <- extract_spss_values(spss_file, test_number = 1)
  
  expect_true(!is.null(test1_values))
  expect_true("mean" %in% names(test1_values))
  expect_true("sd" %in% names(test1_values))
  expect_true("variance" %in% names(test1_values))
  
  # Check that values are reasonable
  expect_true(test1_values$mean > 40 && test1_values$mean < 60)  # Age should be around 50
  expect_true(test1_values$sd > 10 && test1_values$sd < 20)  # SD should be reasonable
  expect_true(test1_values$n == 2500)  # Sample size should be 2500
})

# ============================================================================
# End of SPSS Validation Tests for describe()
# ============================================================================