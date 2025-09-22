# ============================================================================
# FREQUENCY() VALIDATION REPORT GENERATOR
# ============================================================================
# Purpose: Generate comprehensive validation report for frequency() function
# Version: 2.0 (Restructured)
# Created: 2025-09-16
# ============================================================================

library(dplyr)
library(tibble)
library(SurveyStat)

# Source helper functions
source("tests/testthat/helper-spss-parser.R")

# ============================================================================
# REPORT CONFIGURATION
# ============================================================================

REPORT_CONFIG <- list(
  output_file = "tests/validation_reports/frequency_validation_report_v2.txt",
  spss_output = "tests/spss_reference/outputs/frequency_validation_output.txt",
  tolerance = list(
    counts = 0.1,
    percents = 0.1,
    weighted = 1.0
  )
)

# ============================================================================
# DATA LOADING
# ============================================================================

load_survey_data <- function() {
  data(survey_data, envir = environment())
  return(survey_data)
}

# ============================================================================
# VALIDATION FUNCTIONS
# ============================================================================

#' Validate a single test case
#' @param test_id Test identifier (e.g., "1.1", "2.3")
#' @param test_name Descriptive name
#' @param r_function Function to generate R results
#' @param spss_file Path to SPSS output file
#' @param tolerance Numeric tolerance for comparisons
#' @return List with validation results
validate_test <- function(test_id, test_name, r_function, spss_file, tolerance = 0.1) {
  
  # Initialize results
  results <- list(
    test_id = test_id,
    test_name = test_name,
    status = "PENDING",
    comparisons = NULL,
    summary = NULL,
    error = NULL
  )
  
  tryCatch({
    # Get SPSS values
    spss_pattern <- paste0("TEST ", test_id)
    spss_values <- extract_spss_frequency_values(spss_file, spss_pattern)
    
    if (is.null(spss_values)) {
      results$status <- "SKIP"
      results$error <- "Could not parse SPSS values"
      return(results)
    }
    
    # Get R results
    r_result <- r_function()
    
    # Create comparison table
    comparisons <- tibble(
      Metric = character(),
      SPSS = numeric(),
      R = numeric(),
      Difference = numeric(),
      Pass = logical()
    )
    
    # Compare Valid N
    if (!is.null(spss_values$n_valid)) {
      comparisons <- bind_rows(comparisons, tibble(
        Metric = "Valid N",
        SPSS = spss_values$n_valid,
        R = r_result$stats$valid_n,
        Difference = abs(spss_values$n_valid - r_result$stats$valid_n),
        Pass = abs(spss_values$n_valid - r_result$stats$valid_n) <= tolerance
      ))
    }
    
    # Compare Missing N
    if (!is.null(spss_values$n_missing)) {
      r_missing <- r_result$stats$total_n - r_result$stats$valid_n
      comparisons <- bind_rows(comparisons, tibble(
        Metric = "Missing N",
        SPSS = spss_values$n_missing,
        R = r_missing,
        Difference = abs(spss_values$n_missing - r_missing),
        Pass = abs(spss_values$n_missing - r_missing) <= tolerance
      ))
    }
    
    # Compare Total N
    if (!is.null(spss_values$n_total)) {
      comparisons <- bind_rows(comparisons, tibble(
        Metric = "Total N",
        SPSS = spss_values$n_total,
        R = r_result$stats$total_n,
        Difference = abs(spss_values$n_total - r_result$stats$total_n),
        Pass = abs(spss_values$n_total - r_result$stats$total_n) <= tolerance
      ))
    }
    
    # Store results
    results$comparisons <- comparisons
    results$summary <- list(
      passed = sum(comparisons$Pass, na.rm = TRUE),
      failed = sum(!comparisons$Pass, na.rm = TRUE),
      total = nrow(comparisons)
    )
    
    # Determine status
    if (results$summary$failed == 0) {
      results$status <- "PASS"
    } else if (results$summary$passed > 0) {
      results$status <- "PARTIAL"
    } else {
      results$status <- "FAIL"
    }
    
  }, error = function(e) {
    results$status <- "ERROR"
    results$error <- as.character(e)
  })
  
  return(results)
}

# ============================================================================
# SECTION VALIDATORS
# ============================================================================

validate_section1 <- function(survey_data, spss_file) {
  cat("\n### SECTION 1: BASIC UNWEIGHTED FREQUENCIES ###\n")
  
  section_results <- list()
  
  # Test 1.1: Binary categorical
  section_results[["1.1"]] <- validate_test(
    test_id = "1.1",
    test_name = "Binary categorical (gender)",
    r_function = function() frequency(survey_data, gender),
    spss_file = spss_file,
    tolerance = REPORT_CONFIG$tolerance$counts
  )
  
  # Test 1.2: Multi-category
  section_results[["1.2"]] <- validate_test(
    test_id = "1.2",
    test_name = "Multi-category (education)",
    r_function = function() frequency(survey_data, education),
    spss_file = spss_file,
    tolerance = REPORT_CONFIG$tolerance$counts
  )
  
  # Test 1.3: Multiple variables
  section_results[["1.3"]] <- validate_test(
    test_id = "1.3",
    test_name = "Multiple variables",
    r_function = function() frequency(survey_data, gender, region, education),
    spss_file = spss_file,
    tolerance = REPORT_CONFIG$tolerance$counts
  )
  
  return(section_results)
}

validate_section2 <- function(survey_data, spss_file) {
  cat("\n### SECTION 2: WEIGHTED FREQUENCIES ###\n")
  
  section_results <- list()
  
  # Test 2.1: Weighted binary
  section_results[["2.1"]] <- validate_test(
    test_id = "2.1",
    test_name = "Weighted binary (gender)",
    r_function = function() frequency(survey_data, gender, weights = sampling_weight),
    spss_file = spss_file,
    tolerance = REPORT_CONFIG$tolerance$weighted
  )
  
  # Test 2.2: Weighted multi-category
  section_results[["2.2"]] <- validate_test(
    test_id = "2.2",
    test_name = "Weighted multi-category (education)",
    r_function = function() frequency(survey_data, education, weights = sampling_weight),
    spss_file = spss_file,
    tolerance = REPORT_CONFIG$tolerance$weighted
  )
  
  # Test 2.3: Weighted multiple
  section_results[["2.3"]] <- validate_test(
    test_id = "2.3",
    test_name = "Weighted multiple variables",
    r_function = function() frequency(survey_data, gender, region, education, 
                                     weights = sampling_weight),
    spss_file = spss_file,
    tolerance = REPORT_CONFIG$tolerance$weighted
  )
  
  return(section_results)
}

validate_section3 <- function(survey_data, spss_file) {
  cat("\n### SECTION 3: MISSING DATA HANDLING ###\n")
  
  section_results <- list()
  
  # Test 3.1: Missing excluded
  section_results[["3.1"]] <- validate_test(
    test_id = "3.1",
    test_name = "Missing excluded (default)",
    r_function = function() frequency(survey_data, employment),
    spss_file = spss_file,
    tolerance = REPORT_CONFIG$tolerance$counts
  )
  
  # Test 3.2: Missing included
  section_results[["3.2"]] <- validate_test(
    test_id = "3.2",
    test_name = "Missing included",
    r_function = function() frequency(survey_data, employment, show.na = TRUE),
    spss_file = spss_file,
    tolerance = REPORT_CONFIG$tolerance$counts
  )
  
  # Test 3.3: Weighted with missing
  section_results[["3.3"]] <- validate_test(
    test_id = "3.3",
    test_name = "Weighted with missing",
    r_function = function() frequency(survey_data, employment, 
                                     weights = sampling_weight, 
                                     show.na = TRUE),
    spss_file = spss_file,
    tolerance = REPORT_CONFIG$tolerance$weighted
  )
  
  return(section_results)
}

# ============================================================================
# REPORT GENERATION
# ============================================================================

generate_frequency_validation_report <- function() {
  
  # Print header
  cat("\n")
  cat("================================================================================\n")
  cat("                 FREQUENCY() VALIDATION REPORT V2.0                            \n")
  cat("================================================================================\n")
  cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Dataset: survey_data\n")
  cat("SPSS Output:", REPORT_CONFIG$spss_output, "\n")
  
  # Load data
  survey_data <- load_survey_data()
  spss_file <- REPORT_CONFIG$spss_output
  
  if (!file.exists(spss_file)) {
    cat("\n❌ ERROR: SPSS output file not found\n")
    cat("Path:", spss_file, "\n")
    return(NULL)
  }
  
  # Initialize overall results
  all_results <- list()
  
  # Validate each section
  all_results$section1 <- validate_section1(survey_data, spss_file)
  all_results$section2 <- validate_section2(survey_data, spss_file)
  all_results$section3 <- validate_section3(survey_data, spss_file)
  
  # Print detailed results
  cat("\n")
  cat("================================================================================\n")
  cat("                            DETAILED RESULTS                                   \n")
  cat("================================================================================\n")
  
  for (section_name in names(all_results)) {
    cat("\n", toupper(section_name), "\n")
    cat(rep("-", 80), "\n", sep = "")
    
    section <- all_results[[section_name]]
    for (test_id in names(section)) {
      test <- section[[test_id]]
      
      cat("\nTest", test$test_id, "-", test$test_name, "\n")
      cat("Status:", test$status, "\n")
      
      if (!is.null(test$comparisons) && nrow(test$comparisons) > 0) {
        print(test$comparisons)
      }
      
      if (!is.null(test$error)) {
        cat("Error:", test$error, "\n")
      }
      
      if (!is.null(test$summary)) {
        cat("Summary: ", test$summary$passed, "/", test$summary$total, " passed\n", sep = "")
      }
    }
  }
  
  # Calculate overall summary
  total_passed <- 0
  total_failed <- 0
  total_error <- 0
  total_skip <- 0
  
  for (section in all_results) {
    for (test in section) {
      if (test$status == "PASS") {
        total_passed <- total_passed + 1
      } else if (test$status == "FAIL") {
        total_failed <- total_failed + 1
      } else if (test$status == "ERROR") {
        total_error <- total_error + 1
      } else if (test$status == "SKIP") {
        total_skip <- total_skip + 1
      }
    }
  }
  
  # Print summary
  cat("\n")
  cat("================================================================================\n")
  cat("                              OVERALL SUMMARY                                  \n")
  cat("================================================================================\n")
  cat("✅ PASSED:  ", total_passed, "\n")
  cat("❌ FAILED:  ", total_failed, "\n")
  cat("⚠️  ERROR:   ", total_error, "\n")
  cat("⏭️  SKIPPED: ", total_skip, "\n")
  
  total_tests <- total_passed + total_failed
  if (total_tests > 0) {
    cat("\nPASS RATE: ", sprintf("%.1f%%", 100 * total_passed / total_tests), "\n")
  }
  
  # Save to file
  sink(REPORT_CONFIG$output_file)
  cat("FREQUENCY() VALIDATION REPORT V2.0\n")
  cat("===================================\n")
  cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  # Write summary results
  for (section_name in names(all_results)) {
    cat("\n", toupper(section_name), "\n")
    cat(rep("-", 40), "\n", sep = "")
    
    section <- all_results[[section_name]]
    for (test_id in names(section)) {
      test <- section[[test_id]]
      cat("Test", test$test_id, "-", test$test_name, ": ", test$status, "\n")
    }
  }
  
  cat("\n\nOVERALL: ", total_passed, " passed, ", total_failed, " failed\n", sep = "")
  
  sink()
  
  cat("\n📄 Report saved to:", REPORT_CONFIG$output_file, "\n")
  
  return(all_results)
}

# ============================================================================
# EXECUTE REPORT GENERATION
# ============================================================================

if (interactive() || !exists("SKIP_EXECUTION")) {
  results <- generate_frequency_validation_report()
}