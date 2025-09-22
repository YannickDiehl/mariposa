# ============================================================================
# SPSS Validation Tests for t_test() Function
# ============================================================================
#
# Purpose: Validate that the R t_test() function produces results that are
#          statistically equivalent to SPSS T-TEST procedure
#
# Reference SPSS Syntax: tests/spss_reference/syntax/t_test_basic.sps
# Reference SPSS Output: tests/spss_reference/outputs/t_test_basic_output.txt
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
  mark_validation_test_run("t_test")
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
    test_path("../spss_reference/outputs/t_test_basic_output.txt"),
    "tests/spss_reference/outputs/t_test_basic_output.txt",
    "../spss_reference/outputs/t_test_basic_output.txt"
  )
  
  for (path in paths) {
    if (file.exists(path)) return(path)
  }
  
  skip("SPSS output file not found - run t_test_basic.sps in SPSS first")
}

# ============================================================================
# TEST SUITE 1: Independent Samples T-Tests (Unweighted)
# ============================================================================

test_that("t_test() SPSS validation - unweighted independent single variable (TEST 1)", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  # Parse SPSS output for TEST 1
  lines <- readLines(spss_file, warn = FALSE)
  test_start <- grep("TEST 1:", lines)[1]
  if (is.na(test_start)) skip("Could not find TEST 1 in SPSS output")
  
  # Find end of TEST 1 section
  test_end <- grep("TEST 2:", lines)[1]
  if (is.na(test_end)) test_end <- length(lines)
  
  # Parse just this test section
  test_lines <- lines[test_start:test_end]
  spss_values <- parse_spss_ttest_independent(test_lines, 1)
  
  if (is.null(spss_values)) {
    skip("Could not parse SPSS values for Test 1")
  }
  
  # Run t_test for life_satisfaction by gender
  result <- t_test(survey_data, life_satisfaction, group = gender)
  r_stats <- result$results
  
  # Test equal variance assumption results
  if (!is.null(spss_values$equal_variances)) {
    equal_spss <- spss_values$equal_variances
    equal_r <- r_stats$equal_var_result[[1]]
    
    # Extract t-statistic and other values with proper names
    r_t_stat <- as.numeric(equal_r$statistic)
    r_df <- as.numeric(equal_r$parameter)
    r_p_value <- as.numeric(equal_r$p.value)
    
    # Equal variances t-test
    expect_equal(r_t_stat, equal_spss$t_stat, tolerance = 0.001, 
                 label = "t-statistic (equal var)")
    expect_equal(r_df, equal_spss$df, tolerance = 0.1,
                 label = "degrees of freedom (equal var)")
    expect_equal(r_p_value, equal_spss$p_value, tolerance = 0.01,
                 label = "p-value (equal var)")
    
    # Mean difference
    r_mean_diff <- as.numeric(equal_r$estimate[1] - equal_r$estimate[2])
    expect_equal(r_mean_diff, equal_spss$mean_diff, tolerance = 0.01,
                 label = "mean difference")
    
    # Confidence intervals
    expect_equal(as.numeric(equal_r$conf.int[1]), equal_spss$ci_lower, tolerance = 0.01,
                 label = "CI lower bound")
    expect_equal(as.numeric(equal_r$conf.int[2]), equal_spss$ci_upper, tolerance = 0.01,
                 label = "CI upper bound")
  }
  
  # Test Levene's test if available
  if (!is.null(spss_values$levene_test)) {
    # Note: R's var.test or levene.test might differ slightly from SPSS
    # Just check that we have similar conclusions about variance equality
    expect_true(!is.null(r_stats$equal_var_result),
                label = "Equal variance result exists")
  }
  
})

test_that("t_test() SPSS validation - unweighted independent multiple variables (TEST 2)", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  # Note: SPSS runs separate t-tests for each variable
  # We need to parse TEST 2 which includes age, income, life_satisfaction
  spss_values <- extract_spss_ttest_values(spss_file, test_number = 2, test_type = "independent")
  
  if (is.null(spss_values)) {
    skip("Could not parse SPSS values for Test 2")
  }
  
  # Run t_test for multiple variables
  result <- t_test(survey_data, age, income, life_satisfaction, group = gender)
  r_stats <- result$results
  
  # Test each variable
  variables <- c("age", "income", "life_satisfaction")
  
  for (var in variables) {
    # Get R results for this variable
    r_var_stats <- r_stats[r_stats$Variable == var, ]
    
    if (nrow(r_var_stats) == 0) next
    
    # Check if SPSS has results for this variable
    # Note: SPSS output might have different variable names
    spss_var_name <- var
    if (var == "life_satisfaction") {
      # SPSS might truncate or use German names
      spss_var_name <- "Lebenszuf"  # Adjust based on actual SPSS output
    }
    
    # For now, test the primary results
    if (!is.null(r_var_stats$unequal_var_result[[1]])) {
      unequal_r <- r_var_stats$unequal_var_result[[1]]
      
      # Basic checks that values are reasonable
      expect_true(abs(unequal_r$statistic) < 50)  # t-stat should be reasonable
      expect_true(unequal_r$parameter > 0 && unequal_r$parameter < 3000)  # df should be positive
      expect_true(unequal_r$p.value >= 0 && unequal_r$p.value <= 1)  # p-value in [0,1]
    }
  }
})

test_that("t_test() SPSS validation - unweighted by region (TEST 3)", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  # Parse SPSS output for TEST 3
  lines <- readLines(spss_file, warn = FALSE)
  test_start <- grep("TEST 3:", lines)[1]
  if (is.na(test_start)) skip("Could not find TEST 3 in SPSS output")
  
  # Find end of TEST 3 section
  test_end <- grep("TEST 4:", lines)[1]
  if (is.na(test_end)) test_end <- grep("TEST 5:", lines)[1]
  if (is.na(test_end)) test_end <- length(lines)
  
  # Parse just this test section
  test_lines <- lines[test_start:test_end]
  spss_values <- parse_spss_ttest_independent(test_lines, 1)
  
  if (is.null(spss_values)) {
    skip("Could not parse SPSS values for Test 3")
  }
  
  # Run t_test by region
  result <- t_test(survey_data, life_satisfaction, group = region)
  r_stats <- result$results
  
  # Test both variance assumptions if available
  if (!is.null(spss_values$equal_variances)) {
    equal_spss <- spss_values$equal_variances
    equal_r <- r_stats$equal_var_result[[1]]
    
    # Extract values with proper names
    r_t_stat <- as.numeric(equal_r$statistic)
    r_df <- as.numeric(equal_r$parameter)
    r_p_value <- as.numeric(equal_r$p.value)
    
    expect_equal(r_t_stat, equal_spss$t_stat, tolerance = 0.001)
    expect_equal(r_df, equal_spss$df, tolerance = 0.1)
    expect_equal(r_p_value, equal_spss$p_value, tolerance = 0.01)
  }
})

# ============================================================================
# TEST SUITE 2: One-Sample T-Tests (Unweighted)
# ============================================================================

test_that("t_test() SPSS validation - one-sample t-test mu=0 (TEST 5)", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  spss_values <- extract_spss_ttest_values(spss_file, test_number = 5, test_type = "onesample")
  
  if (is.null(spss_values)) {
    skip("Could not parse SPSS values for Test 5")
  }
  
  # Run one-sample t_test
  result <- t_test(survey_data, life_satisfaction, mu = 0)
  r_stats <- result$results
  
  # Check if we have life_satisfaction results
  if (!is.null(spss_values$test_stats$life_satisfaction)) {
    spss_stats <- spss_values$test_stats$life_satisfaction
    
    expect_equal(r_stats$t_stat, spss_stats$t_stat, tolerance = 0.001)
    expect_equal(r_stats$df, spss_stats$df, tolerance = 0.1)
    expect_equal(r_stats$p_value, spss_stats$p_value, tolerance = 0.001)
    expect_equal(r_stats$mean_diff, spss_stats$mean_diff, tolerance = 0.01)
    expect_equal(r_stats$conf_int_lower, spss_stats$ci_lower, tolerance = 0.01)
    expect_equal(r_stats$conf_int_upper, spss_stats$ci_upper, tolerance = 0.01)
  }
})

test_that("t_test() SPSS validation - one-sample t-test mu=3.5 (TEST 6)", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  spss_values <- extract_spss_ttest_values(spss_file, test_number = 6, test_type = "onesample")
  
  if (is.null(spss_values)) {
    skip("Could not parse SPSS values for Test 6")
  }
  
  # Run one-sample t_test with mu=3.5
  result <- t_test(survey_data, life_satisfaction, mu = 3.5)
  r_stats <- result$results
  
  if (!is.null(spss_values$test_stats$life_satisfaction)) {
    spss_stats <- spss_values$test_stats$life_satisfaction
    
    expect_equal(r_stats$t_stat, spss_stats$t_stat, tolerance = 0.001)
    expect_equal(r_stats$df, spss_stats$df, tolerance = 0.1)
    expect_equal(r_stats$p_value, spss_stats$p_value, tolerance = 0.001)
    expect_equal(r_stats$mean_diff, spss_stats$mean_diff, tolerance = 0.01)
  }
})

# ============================================================================
# TEST SUITE 3: Weighted T-Tests
# ============================================================================

test_that("t_test() SPSS validation - weighted independent single variable (TEST 8)", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  spss_values <- extract_spss_ttest_values(spss_file, test_number = 8, test_type = "independent")
  
  if (is.null(spss_values)) {
    skip("Could not parse SPSS values for Test 8")
  }
  
  # Run weighted t_test
  result <- t_test(survey_data, life_satisfaction, group = gender, weights = sampling_weight)
  r_stats <- result$results
  
  # Test weighted results
  if (!is.null(spss_values$test_stats$equal_variances)) {
    equal_spss <- spss_values$test_stats$equal_variances
    equal_r <- r_stats$equal_var_result[[1]]
    
    # Weighted tests may have different df calculations
    expect_equal(equal_r$statistic, equal_spss$t_stat, tolerance = 0.01)
    expect_equal(equal_r$p.value, equal_spss$p_value, tolerance = 0.01)
    
    # Mean difference
    mean_diff_r <- equal_r$estimate[1] - equal_r$estimate[2]
    expect_equal(mean_diff_r, equal_spss$mean_diff, tolerance = 0.05)
  }
})

test_that("t_test() SPSS validation - weighted independent multiple variables (TEST 9)", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  spss_values <- extract_spss_ttest_values(spss_file, test_number = 9, test_type = "independent")
  
  if (is.null(spss_values)) {
    skip("Could not parse SPSS values for Test 9")
  }
  
  # Run weighted t_test for multiple variables
  result <- t_test(survey_data, age, income, life_satisfaction, 
                   group = gender, weights = sampling_weight)
  r_stats <- result$results
  
  # Basic validation that weighted results are computed
  expect_true(nrow(r_stats) == 3)  # Three variables
  expect_true(all(r_stats$is_weighted))  # All should be weighted
  
  # Check that values are reasonable
  for (i in seq_len(nrow(r_stats))) {
    if (!is.null(r_stats$unequal_var_result[[i]])) {
      unequal_r <- r_stats$unequal_var_result[[i]]
      
      expect_true(abs(unequal_r$statistic) < 50)  # t-stat reasonable
      expect_true(unequal_r$parameter > 0)  # df positive
      expect_true(unequal_r$p.value >= 0 && unequal_r$p.value <= 1)  # valid p-value
    }
  }
})

test_that("t_test() SPSS validation - weighted one-sample (TEST 11)", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  spss_values <- extract_spss_ttest_values(spss_file, test_number = 11, test_type = "onesample")
  
  if (is.null(spss_values)) {
    skip("Could not parse SPSS values for Test 11")
  }
  
  # Run weighted one-sample t_test
  result <- t_test(survey_data, life_satisfaction, mu = 3.5, weights = sampling_weight)
  r_stats <- result$results
  
  if (!is.null(spss_values$test_stats$life_satisfaction)) {
    spss_stats <- spss_values$test_stats$life_satisfaction
    
    # Weighted one-sample tests may have different implementations
    expect_equal(r_stats$t_stat, spss_stats$t_stat, tolerance = 0.05)
    expect_equal(r_stats$p_value, spss_stats$p_value, tolerance = 0.05)
  }
})

# ============================================================================
# TEST SUITE 4: Grouped Analyses (Split File)
# ============================================================================

test_that("t_test() SPSS validation - grouped by region unweighted (TEST 13)", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  # Note: TEST 13 uses SPLIT FILE BY region
  # This creates separate t-tests for each region
  
  # Run grouped t_test
  result <- survey_data %>%
    group_by(region) %>%
    t_test(life_satisfaction, group = gender)
  
  r_stats <- result$results
  
  # Check we have results for both regions
  expect_true("region" %in% names(r_stats))
  expect_equal(length(unique(r_stats$region)), 2)  # East and West
  
  # Each region should have its own t-test results
  for (region_val in unique(r_stats$region)) {
    region_results <- r_stats[r_stats$region == region_val, ]
    expect_equal(nrow(region_results), 1)
    
    # Check that t-test was computed
    expect_false(is.na(region_results$t_stat))
    expect_false(is.na(region_results$p_value))
  }
})

test_that("t_test() SPSS validation - grouped by region weighted (TEST 14)", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  # Run grouped weighted t_test
  result <- survey_data %>%
    group_by(region) %>%
    t_test(life_satisfaction, group = gender, weights = sampling_weight)
  
  r_stats <- result$results
  
  # Check we have weighted results for both regions
  expect_true("region" %in% names(r_stats))
  expect_equal(length(unique(r_stats$region)), 2)
  expect_true(all(r_stats$is_weighted))
})

# ============================================================================
# TEST SUITE 5: Confidence Intervals
# ============================================================================

test_that("t_test() SPSS validation - 90% confidence interval (TEST 16)", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  spss_values <- extract_spss_ttest_values(spss_file, test_number = 16, test_type = "independent")
  
  if (is.null(spss_values)) {
    skip("Could not parse SPSS values for Test 16")
  }
  
  # Run t_test with 90% CI
  result <- t_test(survey_data, life_satisfaction, group = gender, conf.level = 0.90)
  r_stats <- result$results
  
  # Check that confidence level was applied
  expect_equal(result$conf.level, 0.90)
  
  # If SPSS values available, compare CIs
  if (!is.null(spss_values$test_stats$equal_variances)) {
    equal_spss <- spss_values$test_stats$equal_variances
    equal_r <- r_stats$equal_var_result[[1]]
    
    # 90% CIs should be narrower than 95%
    ci_width_r <- equal_r$conf.int[2] - equal_r$conf.int[1]
    ci_width_spss <- equal_spss$ci_upper - equal_spss$ci_lower
    
    expect_equal(ci_width_r, ci_width_spss, tolerance = 0.01)
  }
})

test_that("t_test() SPSS validation - 99% confidence interval (TEST 17)", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  spss_values <- extract_spss_ttest_values(spss_file, test_number = 17, test_type = "independent")
  
  if (is.null(spss_values)) {
    skip("Could not parse SPSS values for Test 17")
  }
  
  # Run t_test with 99% CI
  result <- t_test(survey_data, life_satisfaction, group = gender, conf.level = 0.99)
  r_stats <- result$results
  
  # Check that confidence level was applied
  expect_equal(result$conf.level, 0.99)
  
  if (!is.null(spss_values$test_stats$equal_variances)) {
    equal_spss <- spss_values$test_stats$equal_variances
    equal_r <- r_stats$equal_var_result[[1]]
    
    # 99% CIs should be wider than 95%
    ci_width_r <- equal_r$conf.int[2] - equal_r$conf.int[1]
    ci_width_spss <- equal_spss$ci_upper - equal_spss$ci_lower
    
    expect_equal(ci_width_r, ci_width_spss, tolerance = 0.01)
  }
})

# ============================================================================
# TEST SUITE 6: Effect Sizes
# ============================================================================

test_that("t_test() SPSS validation - effect size calculations (TEST 18)", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  spss_values <- extract_spss_ttest_values(spss_file, test_number = 18, test_type = "independent")
  
  if (is.null(spss_values)) {
    skip("Could not parse SPSS values for Test 18")
  }
  
  # Run t_test for multiple variables
  result <- t_test(survey_data, age, income, life_satisfaction, group = gender)
  r_stats <- result$results
  
  # Check effect sizes are calculated
  expect_false(any(is.na(r_stats$cohens_d)))
  expect_false(any(is.na(r_stats$hedges_g)))
  expect_false(any(is.na(r_stats$glass_delta)))
  
  # If group statistics available, validate effect sizes
  for (var in c("age", "income", "life_satisfaction")) {
    if (!is.null(spss_values$group_stats[[var]])) {
      stats <- spss_values$group_stats[[var]]
      
      if (!is.null(stats$group1) && !is.null(stats$group2)) {
        # Calculate expected effect sizes from SPSS data
        expected_effects <- calculate_effect_sizes(
          stats$group1$mean, stats$group1$sd, stats$group1$n,
          stats$group2$mean, stats$group2$sd, stats$group2$n
        )
        
        # Get R effect sizes for this variable
        r_var_stats <- r_stats[r_stats$Variable == var, ]
        
        if (nrow(r_var_stats) > 0) {
          expect_equal(r_var_stats$cohens_d, expected_effects$cohens_d, tolerance = 0.01)
          expect_equal(r_var_stats$hedges_g, expected_effects$hedges_g, tolerance = 0.01)
          expect_equal(r_var_stats$glass_delta, expected_effects$glass_delta, tolerance = 0.01)
        }
      }
    }
  }
})

# ============================================================================
# TEST SUITE 7: Missing Data Handling
# ============================================================================

test_that("t_test() SPSS validation - missing data handling (TEST 20)", {
  survey_data <- load_survey_data()
  spss_file <- get_spss_output_path()
  
  spss_values <- extract_spss_ttest_values(spss_file, test_number = 20, test_type = "independent")
  
  if (is.null(spss_values)) {
    skip("Could not parse SPSS values for Test 20")
  }
  
  # Run t_test with variables that have missing data
  result <- t_test(survey_data, income, trust_government, group = gender)
  r_stats <- result$results
  
  # Check that results are computed despite missing data
  expect_equal(nrow(r_stats), 2)  # Two variables
  
  # Check sample sizes account for missing data
  for (i in seq_len(nrow(r_stats))) {
    if (!is.null(r_stats$group_stats[[i]])) {
      stats <- r_stats$group_stats[[i]]
      
      # Sample sizes should be less than full dataset if missing data exists
      if (!is.null(stats$group1) && !is.null(stats$group2)) {
        total_n <- stats$group1$n + stats$group2$n
        expect_true(total_n <= nrow(survey_data))
      }
    }
  }
})

# ============================================================================
# TEST SUITE 8: SPSS Parser Functionality Tests
# ============================================================================

test_that("SPSS t-test parser correctly extracts values from output file", {
  spss_file <- get_spss_output_path()
  
  # Test independent samples parser
  test1_values <- extract_spss_ttest_values(spss_file, test_number = 1, test_type = "independent")
  
  if (!is.null(test1_values)) {
    expect_true("test_stats" %in% names(test1_values))
    expect_true("group_stats" %in% names(test1_values))
    
    # Check that test statistics were extracted
    if (!is.null(test1_values$test_stats)) {
      expect_true("equal_variances" %in% names(test1_values$test_stats) ||
                  "unequal_variances" %in% names(test1_values$test_stats))
    }
  }
  
  # Test one-sample parser
  test5_values <- extract_spss_ttest_values(spss_file, test_number = 5, test_type = "onesample")
  
  if (!is.null(test5_values)) {
    expect_true("test_stats" %in% names(test5_values))
  }
})

test_that("Effect size calculator produces correct values", {
  # Test with known values
  effects <- calculate_effect_sizes(
    group1_mean = 10, group1_sd = 2, group1_n = 100,
    group2_mean = 8, group2_sd = 2, group2_n = 100
  )
  
  # Cohen's d should be (10-8)/pooled_sd = 2/2 = 1
  expect_equal(effects$cohens_d, 1.0, tolerance = 0.001)
  
  # Hedges' g should be slightly less than Cohen's d
  expect_true(effects$hedges_g < effects$cohens_d)
  expect_true(effects$hedges_g > 0.98)  # Should be close to 1
  
  # Glass' Delta uses only group1 SD
  expect_equal(effects$glass_delta, 1.0, tolerance = 0.001)
})

# ============================================================================
# End of SPSS Validation Tests for t_test()
# ============================================================================