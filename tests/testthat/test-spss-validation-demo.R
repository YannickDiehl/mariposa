# SPSS VALIDATION DEMO
# ===============================================
# Example integration of SPSS validation with testthat framework
# This demonstrates how to add SPSS compatibility testing to existing tests

# Load SPSS validation framework
source('tests/spss_reference/helpers/validation_utils.R')

# Test data availability
test_that("survey_data is available for SPSS validation", {
  data(survey_data)
  expect_true(exists("survey_data"))
  expect_true(is.data.frame(survey_data))
  expect_true("life_satisfaction" %in% names(survey_data))
  expect_true("gender" %in% names(survey_data))
})

# Example T-Test SPSS Validation
test_that("t_test SPSS validation - basic unweighted test", {
  # Skip if SPSS reference not available
  skip_if_no_spss_reference('t_test', 'basic')
  
  # Load test data
  data(survey_data)
  
  # Load validation functions
  load_spss_validation()
  
  # Execute SurveyStat t_test
  r_result <- t_test(survey_data, life_satisfaction, group = gender)
  
  # Get SPSS reference file
  spss_file <- get_spss_reference_path('t_test', 'basic')
  
  # Validate against SPSS (test unequal variances - default)
  validation <- validate_t_test_against_spss(r_result, spss_file, var.equal = FALSE)
  
  # Check validation results
  if (!validation$passed) {
    # Print detailed discrepancies for debugging
    cat("\nSPSS Validation Failed - Discrepancies:\n")
    for (disc_name in names(validation$discrepancies)) {
      disc <- validation$discrepancies[[disc_name]]
      cat(sprintf("  %s: R=%s, SPSS=%s, diff=%s (tolerance=%s)\n", 
                  disc_name, disc$r_value, disc$spss_value, 
                  disc$difference, disc$tolerance))
    }
  }
  
  # Test should pass SPSS validation
  expect_true(validation$passed, 
              info = paste("SPSS validation failed with", 
                          length(validation$discrepancies), "discrepancies"))
  
  # Expect high compatibility (95%+ pass rate)
  if (!is.null(validation$summary$pass_rate)) {
    expect_gte(validation$summary$pass_rate, 0.95)
  }
})

# Example T-Test SPSS Validation - Equal Variances
test_that("t_test SPSS validation - equal variances assumption", {
  # Skip if SPSS reference not available
  skip_if_no_spss_reference('t_test', 'basic')
  
  # Load test data and validation functions
  data(survey_data)
  load_spss_validation()
  
  # Execute SurveyStat t_test with equal variances
  r_result <- t_test(survey_data, life_satisfaction, group = gender, var.equal = TRUE)
  
  # Get SPSS reference file
  spss_file <- get_spss_reference_path('t_test', 'basic')
  
  # Validate against SPSS equal variances results
  validation <- validate_t_test_against_spss(r_result, spss_file, var.equal = TRUE)
  
  # Test should pass SPSS validation
  expect_true(validation$passed)
})

# Example Weighted T-Test SPSS Validation
test_that("t_test SPSS validation - weighted analysis", {
  # Skip if SPSS reference not available
  skip_if_no_spss_reference('t_test', 'weighted')
  
  # Load test data and validation functions
  data(survey_data)
  load_spss_validation()
  
  # Execute SurveyStat weighted t_test
  r_result <- t_test(survey_data, life_satisfaction, group = gender, weights = sampling_weight)
  
  # Get SPSS reference file for weighted analysis
  spss_file <- get_spss_reference_path('t_test', 'weighted')
  
  # Validate against SPSS weighted results
  validation <- validate_t_test_against_spss(r_result, spss_file, var.equal = FALSE)
  
  # Test should pass SPSS validation
  expect_true(validation$passed)
})

# Example Descriptive Statistics SPSS Validation
test_that("describe SPSS validation - basic descriptives", {
  # Skip if SPSS reference not available
  skip_if_no_spss_reference('describe', 'basic')
  
  # Load test data and validation functions
  data(survey_data)
  load_spss_validation()
  
  # Execute SurveyStat describe
  r_result <- describe(survey_data, life_satisfaction, trust_government)
  
  # Get SPSS reference file
  spss_file <- get_spss_reference_path('describe', 'basic')
  
  # Validate against SPSS descriptives
  validation <- validate_descriptives_against_spss(r_result, spss_file)
  
  # Test should pass SPSS validation
  expect_true(validation$passed)
  
  # Check that we tested multiple variables
  if (!is.null(validation$summary$variables_tested)) {
    expect_gte(validation$summary$variables_tested, 2)
  }
})

# Example Levene Test SPSS Validation
test_that("levene_test SPSS validation", {
  # Skip if SPSS reference not available  
  skip_if_no_spss_reference('levene_test', 'basic')
  
  # Load test data and validation functions
  data(survey_data)
  load_spss_validation()
  
  # Execute SurveyStat Levene test
  r_result <- levene_test(survey_data, life_satisfaction, group = gender)
  
  # Get SPSS reference file
  spss_file <- get_spss_reference_path('levene_test', 'basic')
  
  # Validate against SPSS Levene test
  validation <- validate_levene_against_spss(r_result, spss_file)
  
  # Test should pass SPSS validation
  expect_true(validation$passed)
})

# SPSS Reference Status Check
test_that("SPSS validation infrastructure is properly set up", {
  
  # Check that helper functions are available
  expect_true(exists("load_spss_validation"))
  expect_true(exists("get_spss_reference_path"))
  expect_true(exists("check_spss_references"))
  
  # Check directory structure
  expect_true(dir.exists("tests/spss_reference"))
  expect_true(dir.exists("tests/spss_reference/helpers"))
  
  # Check that parser files exist
  expect_true(file.exists("tests/spss_reference/helpers/spss_parser.R"))
  expect_true(file.exists("tests/spss_reference/helpers/validation_framework.R"))
  expect_true(file.exists("tests/spss_reference/helpers/validation_utils.R"))
})

# Batch Test Creation Example
test_that("batch SPSS test creation works", {
  
  # Define test configurations for t_test
  test_configs <- list(
    list(
      test_name = "basic_unweighted",
      analysis_type = "basic",
      r_call = quote(t_test(survey_data, life_satisfaction, group = gender)),
      validation_function = validate_t_test_against_spss,
      var.equal = FALSE
    ),
    list(
      test_name = "basic_equal_var",
      analysis_type = "basic", 
      r_call = quote(t_test(survey_data, life_satisfaction, group = gender, var.equal = TRUE)),
      validation_function = validate_t_test_against_spss,
      var.equal = TRUE
    )
  )
  
  # Batch create tests (would normally be used during test development)
  # tests <- batch_create_spss_tests("t_test", test_configs)
  
  # Just check that the function exists and configs are valid
  expect_true(exists("batch_create_spss_tests"))
  expect_equal(length(test_configs), 2)
  expect_true(all(sapply(test_configs, function(x) "test_name" %in% names(x))))
})