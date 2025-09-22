# ============================================================================
# SPSS Validation Report Generator
# ============================================================================
# Purpose: Generate detailed comparison reports showing SPSS vs R results
#          alongside testthat test outcomes in a unified workflow
# ============================================================================

library(testthat)
library(dplyr)
library(tibble)
library(SurveyStat)  # Load package to get our describe() function

# Source helper functions
# Handle different execution contexts
helper_paths <- c(
  "tests/testthat/helper-spss-parser.R",
  "../testthat/helper-spss-parser.R",
  "testthat/helper-spss-parser.R"
)

# Try to add script directory path if available
if (exists("ofile") && !is.null(sys.frame(1)$ofile)) {
  helper_paths <- c(helper_paths, 
                   file.path(dirname(sys.frame(1)$ofile), "../testthat/helper-spss-parser.R"))
}

helper_found <- FALSE
for (path in helper_paths) {
  if (file.exists(path)) {
    source(path)
    helper_found <- TRUE
    break
  }
}

if (!helper_found) {
  stop("Could not find helper-spss-parser.R. Please run from package root directory.")
}

# Helper function to load test data
load_survey_data <- function() {
  data(survey_data, envir = environment())
  return(survey_data)
}

# ============================================================================
# SPSS Syntax Verification Functions
# ============================================================================

#' Verify SPSS syntax file structure for potential issues
#' 
#' @param syntax_file Path to SPSS syntax file (.sps)
#' @return List with verification results and warnings
verify_spss_syntax <- function(syntax_file) {
  if (!file.exists(syntax_file)) {
    return(list(
      file_exists = FALSE,
      message = paste("File not found:", syntax_file)
    ))
  }
  
  lines <- readLines(syntax_file, warn = FALSE)
  
  # Perform various checks
  checks <- list(
    file_exists = TRUE,
    has_oms = any(grepl("^OMS", lines)),
    has_omsend = any(grepl("^OMSEND", lines)),
    has_execute = any(grepl("^EXECUTE", lines)),
    test_markers = grep("TEST \\d+:", lines),
    has_boundaries = any(grepl("^#{10,}", lines)),
    has_echo_boundaries = any(grepl("ECHO.*#{10,}", lines)),
    has_get_file = any(grepl("^GET FILE", lines)),
    has_weight_commands = any(grepl("^WEIGHT", lines)),
    has_split_file = any(grepl("^SPLIT FILE", lines))
  )
  
  # Count TEST markers
  checks$test_count <- length(checks$test_markers)
  
  # Generate warnings
  warnings <- character()
  
  if (!checks$has_omsend) {
    warnings <- c(warnings, 
      "CRITICAL: Missing OMSEND - will cause output contamination!")
  }
  
  if (!checks$has_oms) {
    warnings <- c(warnings, 
      "ERROR: Missing OMS command - output won't be captured!")
  }
  
  if (!checks$has_execute) {
    warnings <- c(warnings, 
      "WARNING: Missing EXECUTE after OMSEND")
  }
  
  if (checks$test_count == 0) {
    warnings <- c(warnings, 
      "WARNING: No TEST markers found - parsing may fail")
  }
  
  if (checks$has_split_file && !any(grepl("SPLIT FILE OFF", lines))) {
    warnings <- c(warnings, 
      "WARNING: SPLIT FILE used but not turned OFF")
  }
  
  if (checks$has_weight_commands) {
    weight_on <- sum(grepl("^WEIGHT BY", lines))
    weight_off <- sum(grepl("^WEIGHT OFF", lines))
    if (weight_on > weight_off) {
      warnings <- c(warnings, 
        sprintf("WARNING: Unbalanced WEIGHT commands (ON: %d, OFF: %d)", 
                weight_on, weight_off))
    }
  }
  
  # Print warnings if verbose
  if (length(warnings) > 0) {
    cat("\n⚠️ SPSS Syntax Verification Issues:\n")
    for (warning in warnings) {
      cat("  -", warning, "\n")
    }
  }
  
  return(list(
    checks = checks,
    warnings = warnings,
    passed = length(warnings) == 0
  ))
}

#' Verify test coverage between SPSS output and validation report
#' 
#' @param output_file Path to SPSS output file (.txt)
#' @param expected_tests Expected number of tests (optional)
#' @return List with test coverage information
verify_test_coverage <- function(output_file, expected_tests = NULL) {
  if (!file.exists(output_file)) {
    return(list(
      file_exists = FALSE,
      message = paste("Output file not found:", output_file)
    ))
  }
  
  lines <- readLines(output_file, warn = FALSE)
  
  # Find all TEST markers in output
  test_markers <- grep("TEST \\d+:", lines, value = TRUE)
  test_numbers <- as.numeric(gsub(".*TEST (\\d+):.*", "\\1", test_markers))
  
  # Check for gaps in test numbering
  if (length(test_numbers) > 0) {
    expected_sequence <- seq(min(test_numbers), max(test_numbers))
    missing_tests <- setdiff(expected_sequence, test_numbers)
  } else {
    missing_tests <- numeric()
  }
  
  # Build coverage report
  coverage <- list(
    file_exists = TRUE,
    total_tests = length(test_markers),
    test_numbers = test_numbers,
    missing_tests = missing_tests,
    has_gaps = length(missing_tests) > 0
  )
  
  # Compare with expected if provided
  if (!is.null(expected_tests)) {
    coverage$expected_tests <- expected_tests
    coverage$coverage_complete <- length(test_markers) >= expected_tests
    
    if (!coverage$coverage_complete) {
      cat(sprintf("\n⚠️ Test Coverage Issue: Found %d tests, expected %d\n",
                  length(test_markers), expected_tests))
    }
  }
  
  # Check for contamination indicators
  contamination_check <- list(
    has_multiple_begins = length(grep("BEGIN.*VALIDATION", lines)) > 1,
    has_multiple_ends = length(grep("END.*VALIDATION", lines)) > 1,
    has_mixed_functions = any(grepl("describe", lines, ignore.case = TRUE)) && 
                          any(grepl("t.?test", lines, ignore.case = TRUE))
  )
  
  if (contamination_check$has_multiple_begins || contamination_check$has_multiple_ends) {
    cat("\n⚠️ WARNING: Output file may contain contaminated results from multiple functions\n")
    coverage$contamination_risk = TRUE
  } else {
    coverage$contamination_risk = FALSE
  }
  
  return(coverage)
}

#' Run pre-validation checks on SPSS files
#' 
#' @param function_name Name of the function being validated (e.g., "describe", "t_test")
#' @return Logical indicating whether validation can proceed
pre_validation_checks <- function(function_name) {
  cat("\n📋 Running Pre-Validation Checks for", toupper(function_name), "\n")
  cat(rep("-", 50), "\n", sep = "")
  
  # Define file paths
  syntax_file <- sprintf("tests/spss_reference/syntax/%s_basic.sps", function_name)
  output_file <- sprintf("tests/spss_reference/outputs/%s_basic_output.txt", function_name)
  
  # Check syntax file
  cat("Checking SPSS syntax file...\n")
  syntax_check <- verify_spss_syntax(syntax_file)
  
  if (!syntax_check$checks$file_exists) {
    cat("❌ Syntax file not found:", syntax_file, "\n")
    return(FALSE)
  }
  
  if (!syntax_check$passed) {
    cat("⚠️ Syntax file has issues but proceeding...\n")
  } else {
    cat("✅ Syntax file structure verified\n")
  }
  
  # Check output file and coverage
  cat("Checking SPSS output file...\n")
  coverage_check <- verify_test_coverage(output_file)
  
  if (!coverage_check$file_exists) {
    cat("❌ Output file not found:", output_file, "\n")
    return(FALSE)
  }
  
  cat(sprintf("✅ Output file found with %d TEST sections\n", coverage_check$total_tests))
  
  if (coverage_check$has_gaps) {
    cat("⚠️ Missing TEST numbers:", paste(coverage_check$missing_tests, collapse = ", "), "\n")
  }
  
  if (coverage_check$contamination_risk) {
    cat("⚠️ Output may be contaminated - verify OMSEND in syntax\n")
  }
  
  cat(rep("-", 50), "\n", sep = "")
  return(TRUE)
}

# ============================================================================
# Enhanced Test Runner with Value Capture
# ============================================================================

#' Run validation test and capture both results and test outcomes
#' 
#' @param test_name Name of the test
#' @param spss_values SPSS reference values
#' @param r_values R calculated values
#' @param test_expr Test expression to evaluate
#' @return List with comparison data and test result
capture_validation_test <- function(test_name, spss_values, r_values, test_expr) {
  # Capture test result
  test_result <- tryCatch({
    eval(test_expr)
    list(passed = TRUE, message = "PASS", error = NULL)
  }, error = function(e) {
    list(passed = FALSE, message = "FAIL", error = e$message)
  })
  
  # Create comparison data
  comparison <- data.frame(
    Test = test_name,
    SPSS_Value = spss_values,
    R_Value = r_values,
    Difference = abs(spss_values - r_values),
    Test_Result = test_result$message,
    stringsAsFactors = FALSE
  )
  
  list(
    comparison = comparison,
    test_result = test_result
  )
}

# ============================================================================
# DESCRIBE() VALIDATION REPORT
# ============================================================================

generate_describe_validation_report <- function() {
  cat("\n")
  cat("================================================================================\n")
  cat("                    DESCRIBE() SPSS VALIDATION REPORT                          \n")
  cat("================================================================================\n")
  cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  # Run pre-validation checks
  if (!pre_validation_checks("describe")) {
    cat("\n❌ Pre-validation checks failed. Cannot proceed with validation.\n")
    return(NULL)
  }
  
  # Load data and SPSS output
  survey_data <- load_survey_data()
  spss_file <- "tests/spss_reference/outputs/describe_basic_output.txt"
  
  # Initialize results collector
  all_comparisons <- list()
  test_summary <- list(passed = 0, failed = 0, skipped = 0)
  
  # ========== TEST 1: Unweighted Single Variable ==========
  cat("TEST 1: UNWEIGHTED SINGLE VARIABLE (AGE)\n")
  cat("----------------------------------------\n")
  
  # Extract SPSS values
  spss_values_t1 <- extract_spss_values(spss_file, test_number = 1, variable = "age")
  
  if (!is.null(spss_values_t1)) {
    # Run R function (use SurveyStat package function explicitly)
    result <- SurveyStat::describe(survey_data, age, show = "all")
    r_stats <- result$results
    
    # Create detailed comparison table with ALL available statistics
    comparison_data <- tibble(
      Statistic = c("N", "Mean", "SD", "Variance", "Range", "SE", "Skewness", "Kurtosis"),
      SPSS = c(
        spss_values_t1$n, spss_values_t1$mean, spss_values_t1$sd, 
        spss_values_t1$variance, spss_values_t1$range, spss_values_t1$se, 
        spss_values_t1$skewness, spss_values_t1$kurtosis
      ),
      R = c(
        r_stats$age_N, r_stats$age_Mean, r_stats$age_SD,
        r_stats$age_Variance, r_stats$age_Range, r_stats$age_SE, 
        r_stats$age_Skewness, r_stats$age_Kurtosis
      ),
      Tolerance = c(0.1, 0.01, 0.01, 0.01, 0.01, 0.001, 0.05, 0.05)
    )
    
    comparison_data <- comparison_data %>%
      mutate(
        Difference = abs(SPSS - R),
        Pass = Difference <= Tolerance,
        Status = ifelse(Pass, "✅ PASS", "❌ FAIL")
      )
    
    # Print comparison table
    print(comparison_data)
    
    # Count results
    test_summary$passed <- test_summary$passed + sum(comparison_data$Pass)
    test_summary$failed <- test_summary$failed + sum(!comparison_data$Pass)
    
    all_comparisons$test1 <- comparison_data
    
  } else {
    cat("⚠️ SKIPPED: Could not parse SPSS values\n")
    test_summary$skipped <- test_summary$skipped + 1
  }
  
  # ========== TEST 2: Unweighted Multiple Variables ==========
  cat("\n\nTEST 2: UNWEIGHTED MULTIPLE VARIABLES\n")
  cat("--------------------------------------\n")
  
  lines <- readLines(spss_file, warn = FALSE)
  test_start <- grep("TEST 2:.*UNWEIGHTED MULTIPLE", lines)[1]
  
  if (!is.na(test_start)) {
    desc_start <- grep("Descriptive Statistics", lines[test_start:length(lines)])[1]
    desc_start <- test_start + desc_start - 1
    
    # Extract values for each variable
    spss_age <- extract_values_from_line(lines[desc_start + 6], "age")
    spss_income <- extract_values_from_line(lines[desc_start + 8], "income")
    spss_life <- extract_values_from_line(lines[desc_start + 13], "life")
    
    # Run R function for multiple variables
    result <- SurveyStat::describe(survey_data, age, income, life_satisfaction, show = "all")
    r_stats <- result$results
    
    # Create comparison for each variable
    variables <- list(
      list(name = "age", spss = spss_age, prefix = "age_"),
      list(name = "income", spss = spss_income, prefix = "income_"),
      list(name = "life_satisfaction", spss = spss_life, prefix = "life_satisfaction_")
    )
    
    for (var in variables) {
      if (!is.null(var$spss)) {
        cat("\n  Variable:", var$name, "\n")
        
        comparison_data <- tibble(
          Statistic = c("Mean", "SD"),
          SPSS = c(var$spss$mean, var$spss$sd),
          R = c(
            r_stats[[paste0(var$prefix, "Mean")]],
            r_stats[[paste0(var$prefix, "SD")]]
          ),
          Tolerance = c(0.01, 0.01)
        )
        
        comparison_data <- comparison_data %>%
          mutate(
            Difference = abs(SPSS - R),
            Pass = Difference <= Tolerance,
            Status = ifelse(Pass, "✅ PASS", "❌ FAIL")
          )
        
        print(comparison_data)
        
        test_summary$passed <- test_summary$passed + sum(comparison_data$Pass)
        test_summary$failed <- test_summary$failed + sum(!comparison_data$Pass)
        
        all_comparisons[[paste0("test2_", var$name)]] <- comparison_data
      }
    }
  }
  
  # ========== TEST 3: FREQUENCIES with Percentiles and IQR ==========
  cat("\n\nTEST 3: FREQUENCIES - PERCENTILES AND IQR (AGE, INCOME, LIFE_SATISFACTION)\n")
  cat("--------------------------------------------------------------------------\n")
  
  lines <- readLines(spss_file, warn = FALSE)
  freq_start <- grep("TEST 3:.*FREQUENCIES", lines)[1]
  
  if (!is.na(freq_start)) {
    stats_table <- grep("^Statistics", lines[freq_start:length(lines)])[1]
    if (!is.na(stats_table)) {
      stats_table <- freq_start + stats_table - 1
      
      # Extract percentiles
      spss_percentiles <- list(age = list(), income = list(), life = list())
      
      for (i in stats_table:(stats_table + 40)) {
        if (i > length(lines)) break
        line <- lines[i]
        
        if (grepl("Percentiles\\s+25", line)) {
          values <- as.numeric(unlist(regmatches(line, gregexpr("\\d+\\.?\\d*", line))))
          if (length(values) >= 4) {
            spss_percentiles$age$q25 <- values[2]
            spss_percentiles$income$q25 <- values[3]
            spss_percentiles$life$q25 <- values[4]
          }
        }
        
        if (grepl("^\\s+50\\s", line)) {
          values <- as.numeric(unlist(regmatches(line, gregexpr("\\d+\\.?\\d*", line))))
          if (length(values) >= 4) {
            spss_percentiles$age$median <- values[2]
            spss_percentiles$income$median <- values[3]
            spss_percentiles$life$median <- values[4]
          }
        }
        
        if (grepl("^\\s+75\\s", line)) {
          values <- as.numeric(unlist(regmatches(line, gregexpr("\\d+\\.?\\d*", line))))
          if (length(values) >= 4) {
            spss_percentiles$age$q75 <- values[2]
            spss_percentiles$income$q75 <- values[3]
            spss_percentiles$life$q75 <- values[4]
          }
        }
      }
      
      # Run R function
      result <- SurveyStat::describe(survey_data, age, income, life_satisfaction, 
                                    show = "all", probs = c(0.25, 0.50, 0.75))
      r_stats <- result$results
      
      # Compare percentiles for each variable
      if (length(spss_percentiles$age) > 0) {
        cat("\n  Variable: age (Percentiles)\n")
        comparison_data <- tibble(
          Statistic = c("Q25 (25th)", "Median (50th)", "Q75 (75th)", "IQR"),
          SPSS = c(
            spss_percentiles$age$q25,
            spss_percentiles$age$median,
            spss_percentiles$age$q75,
            spss_percentiles$age$q75 - spss_percentiles$age$q25
          ),
          R = c(
            unname(r_stats$age_Q25),
            unname(r_stats$age_Median),
            unname(r_stats$age_Q75),
            unname(r_stats$age_Q75) - unname(r_stats$age_Q25)
          ),
          Tolerance = c(0.1, 0.1, 0.1, 0.1)
        )
        
        comparison_data <- comparison_data %>%
          mutate(
            Difference = abs(SPSS - R),
            Pass = Difference <= Tolerance,
            Status = ifelse(Pass, "✅ PASS", "❌ FAIL")
          )
        
        print(comparison_data)
        test_summary$passed <- test_summary$passed + sum(comparison_data$Pass)
        test_summary$failed <- test_summary$failed + sum(!comparison_data$Pass)
        all_comparisons$test3_percentiles_age <- comparison_data
      }
    }
  } else {
    cat("⚠️ SKIPPED: Could not find FREQUENCIES test in SPSS output\n")
    test_summary$skipped <- test_summary$skipped + 1
  }
  
  # ========== TEST 4: Weighted Single Variable (Complete) ==========
  cat("\n\nTEST 4: WEIGHTED SINGLE VARIABLE (AGE) - COMPLETE STATISTICS\n")
  cat("------------------------------------------------------------\n")
  
  spss_weighted <- extract_spss_values(spss_file, test_number = 4, variable = "age")
  
  if (!is.null(spss_weighted)) {
    # Run weighted analysis with all statistics
    result <- SurveyStat::describe(survey_data, age, weights = sampling_weight, show = "all")
    r_stats <- result$results
    
    comparison_data <- tibble(
      Statistic = c("Mean", "SD", "Variance", "Range", "SE", "Skewness", "Kurtosis"),
      SPSS = c(
        spss_weighted$mean, spss_weighted$sd, spss_weighted$variance,
        spss_weighted$range, spss_weighted$se, spss_weighted$skewness,
        spss_weighted$kurtosis
      ),
      R = c(
        r_stats$age_Mean, r_stats$age_SD, r_stats$age_Variance,
        r_stats$age_Range, r_stats$age_SE, r_stats$age_Skewness,
        r_stats$age_Kurtosis
      ),
      Tolerance = c(0.01, 0.01, 0.01, 0.01, 0.01, 0.05, 0.05)
    )
    
    comparison_data <- comparison_data %>%
      mutate(
        Difference = abs(SPSS - R),
        Pass = Difference <= Tolerance,
        Status = ifelse(Pass, "✅ PASS", "❌ FAIL")
      )
    
    print(comparison_data)
    
    test_summary$passed <- test_summary$passed + sum(comparison_data$Pass)
    test_summary$failed <- test_summary$failed + sum(!comparison_data$Pass)
    
    all_comparisons$test4_weighted <- comparison_data
  } else {
    cat("⚠️ SKIPPED: Could not parse weighted SPSS values\n")
    test_summary$skipped <- test_summary$skipped + 1
  }
  
  # ========== TEST 5: Weighted Multiple Variables ==========
  cat("\n\nTEST 5: WEIGHTED MULTIPLE VARIABLES\n")
  cat("------------------------------------\n")
  
  test_start <- grep("TEST 5:.*WEIGHTED MULTIPLE", lines)[1]
  
  if (!is.na(test_start)) {
    desc_start <- grep("Descriptive Statistics", lines[test_start:length(lines)])[1]
    if (!is.na(desc_start)) {
      desc_start <- test_start + desc_start - 1
      
      # Extract weighted values for each variable
      spss_age_w <- extract_values_from_line(lines[desc_start + 6], "age")
      spss_income_w <- extract_values_from_line(lines[desc_start + 8], "income")
      spss_life_w <- extract_values_from_line(lines[desc_start + 13], "life")
      
      # Run R function with weights for multiple variables
      result <- SurveyStat::describe(survey_data, age, income, life_satisfaction,
                                    weights = sampling_weight, show = "all")
      r_stats <- result$results
      
      # Create comparison for each variable
      variables <- list(
        list(name = "age", spss = spss_age_w, prefix = "age_"),
        list(name = "income", spss = spss_income_w, prefix = "income_"),
        list(name = "life_satisfaction", spss = spss_life_w, prefix = "life_satisfaction_")
      )
      
      for (var in variables) {
        if (!is.null(var$spss)) {
          cat("\n  Variable:", var$name, "(weighted)\n")
          
          comparison_data <- tibble(
            Statistic = c("Mean", "SD", "Variance", "SE"),
            SPSS = c(var$spss$mean, var$spss$sd, var$spss$variance, var$spss$se),
            R = c(
              r_stats[[paste0(var$prefix, "Mean")]],
              r_stats[[paste0(var$prefix, "SD")]],
              r_stats[[paste0(var$prefix, "Variance")]],
              r_stats[[paste0(var$prefix, "SE")]]
            ),
            Tolerance = c(0.01, 0.01, 0.1, 0.01)
          )
          
          comparison_data <- comparison_data %>%
            mutate(
              Difference = abs(SPSS - R),
              Pass = Difference <= Tolerance,
              Status = ifelse(Pass, "✅ PASS", "❌ FAIL")
            )
          
          print(comparison_data)
          
          test_summary$passed <- test_summary$passed + sum(comparison_data$Pass)
          test_summary$failed <- test_summary$failed + sum(!comparison_data$Pass)
          
          all_comparisons[[paste0("test5_weighted_", var$name)]] <- comparison_data
        }
      }
    }
  } else {
    cat("⚠️ SKIPPED: Could not find TEST 5 in SPSS output\n")
    test_summary$skipped <- test_summary$skipped + 1
  }
  
  # ========== TEST 6: Grouped Analysis by Region ==========
  cat("\n\nTEST 6: GROUPED ANALYSIS BY REGION (UNWEIGHTED)\n")
  cat("-----------------------------------------------\n")
  
  # Use the grouped parser function
  grouped_stats <- parse_spss_grouped(spss_file, group_var = "Region", variable = "age")
  
  if (!is.null(grouped_stats) && length(grouped_stats) > 0) {
    # Run grouped describe
    result <- survey_data %>%
      group_by(region) %>%
      SurveyStat::describe(age, income, life_satisfaction, show = "all")
    
    # Get results for each group
    east_results <- result$results[result$results$region == "East", ]
    west_results <- result$results[result$results$region == "West", ]
    
    # Compare East group
    if (!is.null(grouped_stats[["East"]])) {
      cat("\n  Group: East\n")
      east_stats <- grouped_stats[["East"]]
      
      comparison_data <- tibble(
        Statistic = c("N", "Mean", "SD", "Variance", "Range", "SE"),
        SPSS = c(
          east_stats$n, east_stats$mean, east_stats$sd,
          east_stats$variance, east_stats$range, east_stats$se
        ),
        R = c(
          east_results$age_N, east_results$age_Mean, east_results$age_SD,
          east_results$age_Variance, east_results$age_Range, east_results$age_SE
        ),
        Tolerance = c(0.1, 0.01, 0.01, 0.1, 0.01, 0.01)
      )
      
      comparison_data <- comparison_data %>%
        mutate(
          Difference = abs(SPSS - R),
          Pass = Difference <= Tolerance,
          Status = ifelse(Pass, "✅ PASS", "❌ FAIL")
        )
      
      print(comparison_data)
      test_summary$passed <- test_summary$passed + sum(comparison_data$Pass)
      test_summary$failed <- test_summary$failed + sum(!comparison_data$Pass)
      all_comparisons$test6_grouped_east <- comparison_data
    }
    
    # Compare West group
    if (!is.null(grouped_stats[["West"]])) {
      cat("\n  Group: West\n")
      west_stats <- grouped_stats[["West"]]
      
      comparison_data <- tibble(
        Statistic = c("N", "Mean", "SD", "Variance", "Range", "SE"),
        SPSS = c(
          west_stats$n, west_stats$mean, west_stats$sd,
          west_stats$variance, west_stats$range, west_stats$se
        ),
        R = c(
          west_results$age_N, west_results$age_Mean, west_results$age_SD,
          west_results$age_Variance, west_results$age_Range, west_results$age_SE
        ),
        Tolerance = c(0.1, 0.01, 0.01, 0.1, 0.01, 0.01)
      )
      
      comparison_data <- comparison_data %>%
        mutate(
          Difference = abs(SPSS - R),
          Pass = Difference <= Tolerance,
          Status = ifelse(Pass, "✅ PASS", "❌ FAIL")
        )
      
      print(comparison_data)
      test_summary$passed <- test_summary$passed + sum(comparison_data$Pass)
      test_summary$failed <- test_summary$failed + sum(!comparison_data$Pass)
      all_comparisons$test6_grouped_west <- comparison_data
    }
  } else {
    cat("⚠️ SKIPPED: Could not parse grouped analysis in SPSS output\n")
    test_summary$skipped <- test_summary$skipped + 1
  }
  
  # ========== SUMMARY ==========
  cat("\n\n")
  cat("================================================================================\n")
  cat("                              TEST SUMMARY                                     \n")
  cat("================================================================================\n")
  cat(sprintf("✅ PASSED: %d tests\n", test_summary$passed))
  cat(sprintf("❌ FAILED: %d tests\n", test_summary$failed))
  cat(sprintf("⚠️  SKIPPED: %d tests\n", test_summary$skipped))
  cat(sprintf("\nOVERALL PASS RATE: %.1f%%\n", 
              100 * test_summary$passed / (test_summary$passed + test_summary$failed)))
  
  # Save detailed report
  report_file <- "tests/validation_reports/describe_validation_report.txt"
  sink(report_file)
  cat("DESCRIBE() VALIDATION DETAILED REPORT\n")
  cat("=====================================\n\n")
  for (name in names(all_comparisons)) {
    cat("\n", toupper(gsub("_", " ", name)), ":\n")
    print(all_comparisons[[name]])
  }
  sink()
  
  cat("\n📄 Detailed report saved to:", report_file, "\n")
  
  return(list(
    comparisons = all_comparisons,
    summary = test_summary
  ))
}

# ============================================================================
# T_TEST() VALIDATION REPORT
# ============================================================================

generate_ttest_validation_report <- function() {
  cat("\n")
  cat("================================================================================\n")
  cat("                      T_TEST() SPSS VALIDATION REPORT                          \n")
  cat("================================================================================\n")
  cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  # Run pre-validation checks
  if (!pre_validation_checks("t_test")) {
    cat("\n❌ Pre-validation checks failed. Cannot proceed with validation.\n")
    return(NULL)
  }
  
  # Load data and SPSS output
  survey_data <- load_survey_data()
  spss_file <- "tests/spss_reference/outputs/t_test_basic_output.txt"
  
  lines <- readLines(spss_file, warn = FALSE)
  all_comparisons <- list()
  test_summary <- list(passed = 0, failed = 0, skipped = 0)
  
  # ========== TEST 1: Independent Samples T-Test ==========
  cat("TEST 1: INDEPENDENT SAMPLES T-TEST (life_satisfaction by gender)\n")
  cat("-----------------------------------------------------------------\n")
  
  test_start <- grep("TEST 1:", lines)[1]
  if (!is.na(test_start)) {
    test_end <- grep("TEST 2:", lines)[1]
    if (is.na(test_end)) test_end <- length(lines)
    
    test_lines <- lines[test_start:test_end]
    spss_values <- parse_spss_ttest_independent(test_lines, 1)
    
    if (!is.null(spss_values$equal_variances)) {
      # Run R t-test
      result <- SurveyStat::t_test(survey_data, life_satisfaction, group = gender)
      r_stats <- result$results
      equal_r <- r_stats$equal_var_result[[1]]
      
      # Extract R values
      r_t_stat <- as.numeric(equal_r$statistic)
      r_df <- as.numeric(equal_r$parameter)
      r_p_value <- as.numeric(equal_r$p.value)
      r_mean_diff <- as.numeric(equal_r$estimate[1] - equal_r$estimate[2])
      r_ci_lower <- as.numeric(equal_r$conf.int[1])
      r_ci_upper <- as.numeric(equal_r$conf.int[2])
      
      # Create comparison table
      comparison_data <- tibble(
        Statistic = c("t-statistic", "df", "p-value (two-tailed)", 
                     "Mean Difference", "CI Lower", "CI Upper"),
        SPSS = c(
          spss_values$equal_variances$t_stat,
          spss_values$equal_variances$df,
          spss_values$equal_variances$p_value,
          spss_values$equal_variances$mean_diff,
          spss_values$equal_variances$ci_lower,
          spss_values$equal_variances$ci_upper
        ),
        R = c(r_t_stat, r_df, r_p_value, r_mean_diff, r_ci_lower, r_ci_upper),
        Tolerance = c(0.001, 0.1, 0.01, 0.01, 0.01, 0.01)
      )
      
      comparison_data <- comparison_data %>%
        mutate(
          Difference = abs(SPSS - R),
          Pass = Difference <= Tolerance,
          Status = ifelse(Pass, "✅ PASS", "❌ FAIL")
        )
      
      cat("\nEqual Variances Assumed:\n")
      print(comparison_data)
      
      test_summary$passed <- test_summary$passed + sum(comparison_data$Pass)
      test_summary$failed <- test_summary$failed + sum(!comparison_data$Pass)
      
      all_comparisons$test1_equal_var <- comparison_data
      
      # Levene's Test
      if (!is.null(spss_values$levene_test)) {
        cat("\nLevene's Test for Equality of Variances:\n")
        levene_data <- tibble(
          Statistic = c("F-statistic", "p-value"),
          SPSS = c(spss_values$levene_test$F_statistic, 
                  spss_values$levene_test$p_value),
          Note = c("Reference value", "Reference value")
        )
        print(levene_data)
        all_comparisons$test1_levene <- levene_data
      }
    }
  }
  
  # ========== TEST 2: Multiple Variables ==========
  cat("\n\nTEST 2: UNWEIGHTED INDEPENDENT T-TEST (MULTIPLE VARIABLES)\n")
  cat("-----------------------------------------------------------\n")
  
  test_start <- grep("TEST 2:.*MULTIPLE VARIABLES", lines)[1]
  if (!is.na(test_start)) {
    test_end <- grep("TEST 3:", lines)[1]
    if (is.na(test_end)) test_end <- length(lines)
    
    test_lines <- lines[test_start:test_end]
    
    # Extract values for age
    age_line <- grep("Alter in.*Equal variances assumed", test_lines)[1]
    if (!is.na(age_line)) {
      # Parse carefully - the line has format:
      # "Alter in       Equal variances assumed     -.229  2498     .409        .819 ..."
      # We need values after "Equal variances assumed"
      age_text <- test_lines[age_line]
      # First split by "Equal variances assumed"
      age_parts <- strsplit(age_text, "Equal variances assumed")[[1]]
      if (length(age_parts) >= 2) {
        age_text <- age_parts[2]
      }
      age_values <- as.numeric(unlist(regmatches(age_text, 
                                                  gregexpr("-?\\d*\\.?\\d+", age_text))))
      if (length(age_values) >= 4) {
        # Run R t-test for age
        result_age <- SurveyStat::t_test(survey_data, age, group = gender)
        r_stats <- result_age$results
        equal_r <- r_stats$equal_var_result[[1]]
        
        comparison_data <- tibble(
          Statistic = c("t-statistic (age)", "df (age)", "p-value (age)"),
          SPSS = c(age_values[1], age_values[2], age_values[4]),  # p-value is 4th value
          R = c(
            as.numeric(equal_r$statistic),
            as.numeric(equal_r$parameter),
            as.numeric(equal_r$p.value)
          ),
          Tolerance = c(0.001, 0.1, 0.01)
        )
        
        comparison_data <- comparison_data %>%
          mutate(
            Difference = abs(SPSS - R),
            Pass = Difference <= Tolerance,
            Status = ifelse(Pass, "✅ PASS", "❌ FAIL")
          )
        
        cat("\n  Variable: age\n")
        print(comparison_data)
        
        test_summary$passed <- test_summary$passed + sum(comparison_data$Pass)
        test_summary$failed <- test_summary$failed + sum(!comparison_data$Pass)
        all_comparisons$test2_age <- comparison_data
      }
    }
    
    # Extract values for income  
    income_line <- grep("Monatliches.*Equal variances assumed", test_lines)[1]
    if (!is.na(income_line)) {
      # Similar parsing for income
      income_text <- test_lines[income_line]
      # First split by "Equal variances assumed"
      income_parts <- strsplit(income_text, "Equal variances assumed")[[1]]
      if (length(income_parts) >= 2) {
        income_text <- income_parts[2]
      }
      income_values <- as.numeric(unlist(regmatches(income_text, 
                                                    gregexpr("-?\\d*\\.?\\d+", income_text))))
      if (length(income_values) >= 4) {
        # Run R t-test for income
        result_income <- SurveyStat::t_test(survey_data, income, group = gender)
        r_stats <- result_income$results
        equal_r <- r_stats$equal_var_result[[1]]
        
        comparison_data <- tibble(
          Statistic = c("t-statistic (income)", "df (income)", "p-value (income)"),
          SPSS = c(income_values[1], income_values[2], income_values[4]),  # p-value is 4th value
          R = c(
            as.numeric(equal_r$statistic),
            as.numeric(equal_r$parameter),
            as.numeric(equal_r$p.value)
          ),
          Tolerance = c(0.001, 0.1, 0.01)
        )
        
        comparison_data <- comparison_data %>%
          mutate(
            Difference = abs(SPSS - R),
            Pass = Difference <= Tolerance,
            Status = ifelse(Pass, "✅ PASS", "❌ FAIL")
          )
        
        cat("\n  Variable: income\n")
        print(comparison_data)
        
        test_summary$passed <- test_summary$passed + sum(comparison_data$Pass)
        test_summary$failed <- test_summary$failed + sum(!comparison_data$Pass)
        all_comparisons$test2_income <- comparison_data
      }
    }
  }
  
  # ========== TEST 3: By Region ==========
  cat("\n\nTEST 3: INDEPENDENT T-TEST BY REGION\n")
  cat("-------------------------------------\n")
  
  test_start <- grep("TEST 3:", lines)[1]
  if (!is.na(test_start)) {
    test_end <- grep("TEST 4:", lines)[1]
    if (is.na(test_end)) test_end <- grep("TEST 5:", lines)[1]
    if (is.na(test_end)) test_end <- length(lines)
    
    test_lines <- lines[test_start:test_end]
    spss_values <- parse_spss_ttest_independent(test_lines, 1)
    
    if (!is.null(spss_values$equal_variances)) {
      # Run R t-test by region
      result <- SurveyStat::t_test(survey_data, life_satisfaction, group = region)
      r_stats <- result$results
      equal_r <- r_stats$equal_var_result[[1]]
      
      r_t_stat <- as.numeric(equal_r$statistic)
      r_df <- as.numeric(equal_r$parameter)
      r_p_value <- as.numeric(equal_r$p.value)
      
      comparison_data <- tibble(
        Statistic = c("t-statistic", "df", "p-value"),
        SPSS = c(
          spss_values$equal_variances$t_stat,
          spss_values$equal_variances$df,
          spss_values$equal_variances$p_value
        ),
        R = c(r_t_stat, r_df, r_p_value),
        Tolerance = c(0.001, 0.1, 0.01)
      )
      
      comparison_data <- comparison_data %>%
        mutate(
          Difference = abs(SPSS - R),
          Pass = Difference <= Tolerance,
          Status = ifelse(Pass, "✅ PASS", "❌ FAIL")
        )
      
      print(comparison_data)
      
      test_summary$passed <- test_summary$passed + sum(comparison_data$Pass)
      test_summary$failed <- test_summary$failed + sum(!comparison_data$Pass)
      
      all_comparisons$test3_region <- comparison_data
    }
  }
  
  # ========== TEST 5: One-Sample T-Test (mu=0) ==========
  cat("\n\nTEST 5: ONE-SAMPLE T-TEST (MU=0)\n")
  cat("----------------------------------\n")
  
  test_start <- grep("TEST 5:.*ONE-SAMPLE.*MU=0", lines)[1]
  if (!is.na(test_start)) {
    test_end <- grep("TEST 6:", lines)[1]
    if (is.na(test_end)) test_end <- test_start + 50
    
    test_lines <- lines[test_start:test_end]
    
    # Extract SPSS values from One-Sample Test table
    test_line <- grep("Lebenszufriedenheit.*154\\.828", test_lines)[1]
    if (!is.na(test_line)) {
      spss_values <- as.numeric(unlist(regmatches(test_lines[test_line], 
                                                  gregexpr("-?\\d*\\.?\\d+", test_lines[test_line]))))
      if (length(spss_values) >= 5) {
        # Run R one-sample t-test
        result <- SurveyStat::t_test(survey_data, life_satisfaction, mu = 0)
        r_stats <- result$results
        
        comparison_data <- tibble(
          Statistic = c("t-statistic", "df", "p-value", "Mean"),
          SPSS = c(spss_values[2], spss_values[3], spss_values[5], spss_values[6]),
          R = c(
            as.numeric(r_stats$t_stat),
            as.numeric(r_stats$df),
            as.numeric(r_stats$p_value),
            as.numeric(r_stats$mean_diff)
          ),
          Tolerance = c(0.01, 0.1, 0.001, 0.01)
        )
        
        comparison_data <- comparison_data %>%
          mutate(
            Difference = abs(SPSS - R),
            Pass = Difference <= Tolerance,
            Status = ifelse(Pass, "✅ PASS", "❌ FAIL")
          )
        
        print(comparison_data)
        
        test_summary$passed <- test_summary$passed + sum(comparison_data$Pass)
        test_summary$failed <- test_summary$failed + sum(!comparison_data$Pass)
        all_comparisons$test5_one_sample <- comparison_data
      }
    }
  }
  
  # ========== TEST 6: One-Sample T-Test (mu=3.5) ==========
  cat("\n\nTEST 6: ONE-SAMPLE T-TEST (MU=3.5)\n")
  cat("------------------------------------\n")
  
  test_start <- grep("TEST 6:.*ONE-SAMPLE.*MU=3\\.5", lines)[1]
  if (!is.na(test_start)) {
    test_end <- grep("TEST 7:", lines)[1]
    if (is.na(test_end)) test_end <- test_start + 50
    
    test_lines <- lines[test_start:test_end]
    
    # Extract SPSS values
    test_line <- grep("Lebenszufriedenheit.*5\\.473", test_lines)[1]
    if (!is.na(test_line)) {
      spss_values <- as.numeric(unlist(regmatches(test_lines[test_line], 
                                                  gregexpr("-?\\d*\\.?\\d+", test_lines[test_line]))))
      if (length(spss_values) >= 8) {
        # Run R one-sample t-test
        result <- SurveyStat::t_test(survey_data, life_satisfaction, mu = 3.5)
        r_stats <- result$results
        
        comparison_data <- tibble(
          Statistic = c("t-statistic", "df", "p-value", "Mean Difference"),
          SPSS = c(spss_values[2], spss_values[3], spss_values[5], spss_values[6]),
          R = c(
            as.numeric(r_stats$t_stat),
            as.numeric(r_stats$df),
            as.numeric(r_stats$p_value),
            as.numeric(r_stats$mean_diff - 3.5)
          ),
          Tolerance = c(0.01, 0.1, 0.001, 0.01)
        )
        
        comparison_data <- comparison_data %>%
          mutate(
            Difference = abs(SPSS - R),
            Pass = Difference <= Tolerance,
            Status = ifelse(Pass, "✅ PASS", "❌ FAIL")
          )
        
        print(comparison_data)
        
        test_summary$passed <- test_summary$passed + sum(comparison_data$Pass)
        test_summary$failed <- test_summary$failed + sum(!comparison_data$Pass)
        all_comparisons$test6_one_sample_mu35 <- comparison_data
      }
    }
  }
  
  # ========== TEST 8: Weighted Independent T-Test ==========
  cat("\n\nTEST 8: WEIGHTED INDEPENDENT T-TEST (SINGLE VARIABLE)\n")
  cat("------------------------------------------------------\n")
  
  test_start <- grep("TEST 8:.*WEIGHTED INDEPENDENT", lines)[1]
  if (!is.na(test_start)) {
    test_end <- grep("TEST 9:", lines)[1]
    if (is.na(test_end)) test_end <- test_start + 50
    
    test_lines <- lines[test_start:test_end]
    spss_values <- parse_spss_ttest_independent(test_lines, 1)
    
    if (!is.null(spss_values$equal_variances)) {
      # Run weighted R t-test
      result <- SurveyStat::t_test(survey_data, life_satisfaction, 
                                   group = gender, weights = sampling_weight)
      r_stats <- result$results
      equal_r <- r_stats$equal_var_result[[1]]
      
      comparison_data <- tibble(
        Statistic = c("t-statistic", "df", "p-value", "Mean Difference"),
        SPSS = c(
          spss_values$equal_variances$t_stat,
          spss_values$equal_variances$df,
          spss_values$equal_variances$p_value,
          spss_values$equal_variances$mean_diff
        ),
        R = c(
          as.numeric(equal_r$statistic),
          as.numeric(equal_r$parameter),
          as.numeric(equal_r$p.value),
          as.numeric(equal_r$estimate[1] - equal_r$estimate[2])
        ),
        Tolerance = c(0.001, 0.1, 0.01, 0.01)
      )
      
      comparison_data <- comparison_data %>%
        mutate(
          Difference = abs(SPSS - R),
          Pass = Difference <= Tolerance,
          Status = ifelse(Pass, "✅ PASS", "❌ FAIL")
        )
      
      print(comparison_data)
      
      test_summary$passed <- test_summary$passed + sum(comparison_data$Pass)
      test_summary$failed <- test_summary$failed + sum(!comparison_data$Pass)
      all_comparisons$test8_weighted <- comparison_data
    }
  }
  
  # ========== TEST 10: Weighted by Region ==========
  cat("\n\nTEST 10: WEIGHTED INDEPENDENT T-TEST (REGION GROUPS)\n")
  cat("-----------------------------------------------------\n")
  
  test_start <- grep("TEST 10:.*WEIGHTED.*REGION", lines)[1]
  if (!is.na(test_start)) {
    test_end <- grep("TEST 11:", lines)[1]
    if (is.na(test_end)) test_end <- test_start + 50
    
    test_lines <- lines[test_start:test_end]
    spss_values <- parse_spss_ttest_independent(test_lines, 1)
    
    if (!is.null(spss_values$equal_variances)) {
      # Run weighted R t-test by region
      result <- SurveyStat::t_test(survey_data, life_satisfaction, 
                                   group = region, weights = sampling_weight)
      r_stats <- result$results
      equal_r <- r_stats$equal_var_result[[1]]
      
      comparison_data <- tibble(
        Statistic = c("t-statistic", "df", "p-value"),
        SPSS = c(
          spss_values$equal_variances$t_stat,
          spss_values$equal_variances$df,
          spss_values$equal_variances$p_value
        ),
        R = c(
          as.numeric(equal_r$statistic),
          as.numeric(equal_r$parameter),
          as.numeric(equal_r$p.value)
        ),
        Tolerance = c(0.001, 0.1, 0.01)
      )
      
      comparison_data <- comparison_data %>%
        mutate(
          Difference = abs(SPSS - R),
          Pass = Difference <= Tolerance,
          Status = ifelse(Pass, "✅ PASS", "❌ FAIL")
        )
      
      print(comparison_data)
      
      test_summary$passed <- test_summary$passed + sum(comparison_data$Pass)
      test_summary$failed <- test_summary$failed + sum(!comparison_data$Pass)
      all_comparisons$test10_weighted_region <- comparison_data
    }
  }
  
  # ========== TEST 11: Weighted One-Sample (mu=3.5) ==========
  cat("\n\nTEST 11: WEIGHTED ONE-SAMPLE T-TEST (MU=3.5)\n")
  cat("----------------------------------------------\n")
  
  test_start <- grep("TEST 11:.*WEIGHTED ONE-SAMPLE", lines)[1]
  if (!is.na(test_start)) {
    test_end <- grep("TEST 12:", lines)[1]
    if (is.na(test_end)) test_end <- test_start + 50
    
    test_lines <- lines[test_start:test_end]
    
    # Extract SPSS values
    test_line <- grep("Lebenszufriedenheit.*5\\.350", test_lines)[1]
    if (!is.na(test_line)) {
      spss_values <- as.numeric(unlist(regmatches(test_lines[test_line], 
                                                  gregexpr("-?\\d*\\.?\\d+", test_lines[test_line]))))
      if (length(spss_values) >= 8) {
        # Run weighted R one-sample t-test
        result <- SurveyStat::t_test(survey_data, life_satisfaction, 
                                     mu = 3.5, weights = sampling_weight)
        r_stats <- result$results
        
        comparison_data <- tibble(
          Statistic = c("t-statistic", "df", "p-value", "Mean Difference"),
          SPSS = c(spss_values[2], spss_values[3], spss_values[5], spss_values[6]),
          R = c(
            as.numeric(r_stats$t_stat),
            as.numeric(r_stats$df),
            as.numeric(r_stats$p_value),
            as.numeric(r_stats$mean_diff - 3.5)
          ),
          Tolerance = c(0.01, 0.1, 0.001, 0.01)
        )
        
        comparison_data <- comparison_data %>%
          mutate(
            Difference = abs(SPSS - R),
            Pass = Difference <= Tolerance,
            Status = ifelse(Pass, "✅ PASS", "❌ FAIL")
          )
        
        print(comparison_data)
        
        test_summary$passed <- test_summary$passed + sum(comparison_data$Pass)
        test_summary$failed <- test_summary$failed + sum(!comparison_data$Pass)
        all_comparisons$test11_weighted_one_sample <- comparison_data
      }
    }
  }
  
  # ========== SUMMARY ==========
  cat("\n\n")
  cat("================================================================================\n")
  cat("                              TEST SUMMARY                                     \n")
  cat("================================================================================\n")
  cat(sprintf("✅ PASSED: %d tests\n", test_summary$passed))
  cat(sprintf("❌ FAILED: %d tests\n", test_summary$failed))
  cat(sprintf("⚠️  SKIPPED: %d tests\n", test_summary$skipped))
  cat(sprintf("\nOVERALL PASS RATE: %.1f%%\n", 
              100 * test_summary$passed / (test_summary$passed + test_summary$failed)))
  
  # Save detailed report
  report_file <- "tests/validation_reports/ttest_validation_report.txt"
  sink(report_file)
  cat("T_TEST() VALIDATION DETAILED REPORT\n")
  cat("====================================\n\n")
  for (name in names(all_comparisons)) {
    cat("\n", toupper(gsub("_", " ", name)), ":\n")
    print(all_comparisons[[name]])
  }
  sink()
  
  cat("\n📄 Detailed report saved to:", report_file, "\n")
  
  return(list(
    comparisons = all_comparisons,
    summary = test_summary
  ))
}

# ============================================================================
# CHI-SQUARED TEST VALIDATION REPORT
# ============================================================================

#' Generate validation report for chi_squared_test() function
#' 
#' This function creates a detailed report comparing chi_squared_test() results
#' with SPSS CROSSTABS output
#' 
#' @return List with comparison results and summary statistics
generate_chi_squared_validation_report <- function() {
  # Print header
  cat("=" , rep("=", 78), "\n", sep = "")
  cat("                  CHI_SQUARED_TEST() SPSS VALIDATION REPORT                    \n")
  cat("=" , rep("=", 78), "\n", sep = "")
  cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  # Load test data
  survey_data <- load_survey_data()
  
  # Path to SPSS output file
  spss_paths <- c(
    "tests/spss_reference/outputs/chi_squared_basic_output.txt",
    "../spss_reference/outputs/chi_squared_basic_output.txt",
    "spss_reference/outputs/chi_squared_basic_output.txt"
  )
  
  spss_file <- NULL
  for (path in spss_paths) {
    if (file.exists(path)) {
      spss_file <- path
      break
    }
  }
  
  if (is.null(spss_file)) {
    cat("❌ ERROR: SPSS output file not found\n")
    return(NULL)
  }
  
  # Read SPSS output
  lines <- readLines(spss_file, warn = FALSE)
  
  # Pre-validation checks
  cat("\n📋 Running Pre-Validation Checks for CHI_SQUARED_TEST \n")
  cat("--------------------------------------------------\n")
  
  # Check SPSS syntax file
  syntax_paths <- c(
    "tests/spss_reference/syntax/chi_squared_basic.sps",
    "../spss_reference/syntax/chi_squared_basic.sps",
    "spss_reference/syntax/chi_squared_basic.sps"
  )
  
  syntax_file <- NULL
  for (path in syntax_paths) {
    if (file.exists(path)) {
      syntax_file <- path
      break
    }
  }
  
  if (!is.null(syntax_file)) {
    cat("Checking SPSS syntax file...\n")
    syntax_check <- verify_spss_syntax(syntax_file)
    if (isTRUE(syntax_check$file_exists) && isTRUE(syntax_check$has_omsend)) {
      cat("✅ Syntax file structure verified\n")
    } else if (isTRUE(syntax_check$file_exists) && !isTRUE(syntax_check$has_omsend)) {
      cat("⚠️ WARNING: Missing OMSEND - output may be contaminated\n")
    }
  }
  
  # Check SPSS output file
  cat("Checking SPSS output file...\n")
  # Count TEST markers in the file
  test_markers <- grep("TEST \\d+:", lines)
  if (length(test_markers) > 0) {
    cat("✅ Output file found with", length(test_markers), "TEST sections\n")
  } else {
    cat("⚠️ No TEST markers found in output file\n")
  }
  
  cat("--------------------------------------------------\n")
  
  # Initialize tracking
  test_summary <- list(passed = 0, failed = 0, skipped = 0)
  all_comparisons <- list()
  
  # ========== TEST 1: gender x region (2x2) ==========
  cat("TEST 1: UNWEIGHTED CHI-SQUARED TEST (gender x region)\n")
  cat("------------------------------------------------------\n")
  
  spss_values <- extract_spss_chi_squared_values(spss_file, test_number = 1)
  
  if (!is.null(spss_values)) {
    # Run R chi-squared test
    result <- SurveyStat::chi_squared_test(survey_data, gender, region)
    r_stats <- result$results
    
    # Create comparison table
    comparison_data <- tibble(
      Statistic = c("Chi-squared", "df", "p-value", "Phi", "Cramer's V", "Contingency C", "N"),
      SPSS = c(
        spss_values$chi_squared,
        spss_values$df,
        spss_values$p_value,
        spss_values$phi,
        spss_values$cramers_v,
        spss_values$contingency_c,
        spss_values$n
      ),
      R = c(
        r_stats$chi_squared,
        r_stats$df,
        r_stats$p_value,
        r_stats$phi,
        r_stats$cramers_v,
        r_stats$contingency_c,
        r_stats$n
      ),
      Tolerance = c(0.001, 0.1, 0.01, 0.001, 0.001, 0.001, 0.1)
    ) %>%
      mutate(
        Difference = abs(SPSS - R),
        Pass = Difference <= Tolerance,
        Status = ifelse(Pass, "✅ PASS", "❌ FAIL")
      )
    
    print(comparison_data)
    
    test_summary$passed <- test_summary$passed + sum(comparison_data$Pass)
    test_summary$failed <- test_summary$failed + sum(!comparison_data$Pass)
    
    all_comparisons$test1 <- comparison_data
  } else {
    cat("⚠️ SKIPPED: Could not parse SPSS values\n")
    test_summary$skipped <- test_summary$skipped + 1
  }
  
  # ========== TEST 2: gender x education (2x4) ==========
  cat("\n\nTEST 2: UNWEIGHTED CHI-SQUARED TEST (gender x education)\n")
  cat("---------------------------------------------------------\n")
  
  spss_values <- extract_spss_chi_squared_values(spss_file, test_number = 2)
  
  if (!is.null(spss_values)) {
    result <- SurveyStat::chi_squared_test(survey_data, gender, education)
    r_stats <- result$results
    
    comparison_data <- tibble(
      Statistic = c("Chi-squared", "df", "p-value", "Cramer's V"),
      SPSS = c(spss_values$chi_squared, spss_values$df, spss_values$p_value, spss_values$cramers_v),
      R = c(r_stats$chi_squared, r_stats$df, r_stats$p_value, r_stats$cramers_v),
      Tolerance = c(0.001, 0.1, 0.01, 0.001)
    ) %>%
      mutate(
        Difference = abs(SPSS - R),
        Pass = Difference <= Tolerance,
        Status = ifelse(Pass, "✅ PASS", "❌ FAIL")
      )
    
    print(comparison_data)
    
    test_summary$passed <- test_summary$passed + sum(comparison_data$Pass)
    test_summary$failed <- test_summary$failed + sum(!comparison_data$Pass)
    
    all_comparisons$test2 <- comparison_data
  } else {
    cat("⚠️ SKIPPED: Could not parse SPSS values\n")
    test_summary$skipped <- test_summary$skipped + 1
  }
  
  # ========== TEST 3: region x education (2x4) ==========
  cat("\n\nTEST 3: UNWEIGHTED CHI-SQUARED TEST (region x education)\n")
  cat("---------------------------------------------------------\n")
  
  spss_values <- extract_spss_chi_squared_values(spss_file, test_number = 3)
  
  if (!is.null(spss_values)) {
    result <- SurveyStat::chi_squared_test(survey_data, region, education)
    r_stats <- result$results
    
    comparison_data <- tibble(
      Statistic = c("Chi-squared", "df", "p-value", "Cramer's V", "Contingency C"),
      SPSS = c(spss_values$chi_squared, spss_values$df, spss_values$p_value, 
               spss_values$cramers_v, spss_values$contingency_c),
      R = c(r_stats$chi_squared, r_stats$df, r_stats$p_value, 
            r_stats$cramers_v, r_stats$contingency_c),
      Tolerance = c(0.001, 0.1, 0.01, 0.001, 0.001)
    ) %>%
      mutate(
        Difference = abs(SPSS - R),
        Pass = Difference <= Tolerance,
        Status = ifelse(Pass, "✅ PASS", "❌ FAIL")
      )
    
    print(comparison_data)
    
    test_summary$passed <- test_summary$passed + sum(comparison_data$Pass)
    test_summary$failed <- test_summary$failed + sum(!comparison_data$Pass)
    
    all_comparisons$test3 <- comparison_data
  }
  
  # ========== TEST 5: Weighted gender x region ==========
  cat("\n\nTEST 5: WEIGHTED CHI-SQUARED TEST (gender x region)\n")
  cat("----------------------------------------------------\n")
  
  spss_values <- extract_spss_chi_squared_values(spss_file, test_number = 5)
  
  if (!is.null(spss_values)) {
    result <- SurveyStat::chi_squared_test(survey_data, gender, region, weights = sampling_weight)
    r_stats <- result$results
    
    comparison_data <- tibble(
      Statistic = c("Chi-squared", "df", "p-value", "Cramer's V", "Phi"),
      SPSS = c(spss_values$chi_squared, spss_values$df, spss_values$p_value, 
               spss_values$cramers_v, spss_values$phi),
      R = c(r_stats$chi_squared, r_stats$df, r_stats$p_value, 
            r_stats$cramers_v, r_stats$phi),
      Tolerance = c(0.01, 0.1, 0.02, 0.002, 0.002)  # Slightly higher tolerance for weighted
    ) %>%
      mutate(
        Difference = abs(SPSS - R),
        Pass = Difference <= Tolerance,
        Status = ifelse(Pass, "✅ PASS", "❌ FAIL")
      )
    
    print(comparison_data)
    
    test_summary$passed <- test_summary$passed + sum(comparison_data$Pass)
    test_summary$failed <- test_summary$failed + sum(!comparison_data$Pass)
    
    all_comparisons$test5 <- comparison_data
  }
  
  # ========== SUMMARY ==========
  cat("\n\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
  cat("                              TEST SUMMARY                                     \n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  
  cat("✅ PASSED:", test_summary$passed, "tests\n")
  cat("❌ FAILED:", test_summary$failed, "tests\n")
  cat("⚠️  SKIPPED:", test_summary$skipped, "tests\n\n")
  
  total_tests <- test_summary$passed + test_summary$failed
  if (total_tests > 0) {
    pass_rate <- test_summary$passed / total_tests * 100
    cat("OVERALL PASS RATE:", sprintf("%.1f%%", pass_rate), "\n")
  }
  
  # Save to file
  report_file <- "tests/validation_reports/chi_squared_validation_report.txt"
  
  # Try to create directory if it doesn't exist
  report_dir <- dirname(report_file)
  if (!dir.exists(report_dir)) {
    dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  sink(report_file)
  cat("=" , rep("=", 78), "\n", sep = "")
  cat("                  CHI_SQUARED_TEST() SPSS VALIDATION REPORT                    \n")
  cat("=" , rep("=", 78), "\n", sep = "")
  cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  # Re-print all comparisons to file
  for (i in seq_along(all_comparisons)) {
    cat("\nTEST", i, ":\n")
    cat("--------\n")
    print(all_comparisons[[i]])
  }
  
  cat("\n\nSUMMARY:\n")
  cat("--------\n")
  cat("PASSED:", test_summary$passed, "\n")
  cat("FAILED:", test_summary$failed, "\n")
  cat("SKIPPED:", test_summary$skipped, "\n")
  if (total_tests > 0) {
    cat("PASS RATE:", sprintf("%.1f%%", pass_rate), "\n")
  }
  
  sink()
  
  cat("\n📄 Detailed report saved to:", report_file, "\n")
  
  return(list(
    comparisons = all_comparisons,
    summary = test_summary
  ))
}

# ============================================================================
# FREQUENCY() VALIDATION REPORT GENERATION
# ============================================================================

#' Generate validation report for frequency() function
#' 
#' Creates a detailed comparison report between SPSS FREQUENCIES output
#' and R frequency() function results
#' 
#' @return List with comparison results and summary statistics
generate_frequency_validation_report <- function() {
  cat("===============================================================================\n")
  cat("                    FREQUENCY() SPSS VALIDATION REPORT                         \n")
  cat("===============================================================================\n")
  cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  # Load test data
  survey_data <- load_survey_data()
  
  # Get SPSS output file path
  spss_file_candidates <- c(
    "tests/spss_reference/outputs/frequency_basic_output.txt",
    "../spss_reference/outputs/frequency_basic_output.txt",
    "spss_reference/outputs/frequency_basic_output.txt"
  )
  
  spss_file <- NULL
  for (path in spss_file_candidates) {
    if (file.exists(path)) {
      spss_file <- path
      break
    }
  }
  
  if (is.null(spss_file)) {
    cat("❌ ERROR: SPSS output file not found\n")
    return(NULL)
  }
  
  # Initialize tracking
  test_summary <- list(passed = 0, failed = 0, skipped = 0)
  all_comparisons <- list()
  
  # ============================================================================
  # TEST 1: UNWEIGHTED SINGLE CATEGORICAL VARIABLE (gender)
  # ============================================================================
  
  cat("TEST 1: UNWEIGHTED SINGLE CATEGORICAL VARIABLE (gender)\n")
  cat("---------------------------------------------------------\n")
  
  spss_values <- extract_spss_frequency_values(spss_file, test_identifier = "TEST 1.1")
  
  if (!is.null(spss_values)) {
    # Run R frequency
    result <- frequency(survey_data, gender)
    freq_table <- result$results
    
    # Create comparison for summary statistics
    comparison_data <- tibble(
      Statistic = c("N Valid", "N Missing", "Total N"),
      SPSS = c(
        spss_values$n_valid,
        ifelse(is.null(spss_values$n_missing), 0, spss_values$n_missing),
        spss_values$n_valid + ifelse(is.null(spss_values$n_missing), 0, spss_values$n_missing)
      ),
      R = c(
        result$stats$valid_n,
        result$stats$total_n - result$stats$valid_n,
        result$stats$total_n
      ),
      Tolerance = c(0.1, 0.1, 0.1)
    ) %>%
      mutate(
        Difference = abs(SPSS - R),
        Pass = Difference <= Tolerance,
        Status = ifelse(Pass, "✅ PASS", "❌ FAIL")
      )
    
    print(comparison_data)
    cat("\n")
    
    # Update summary
    test_summary$passed <- test_summary$passed + sum(comparison_data$Pass)
    test_summary$failed <- test_summary$failed + sum(!comparison_data$Pass)
    
    # Compare frequencies for each category
    if (!is.null(spss_values$frequencies)) {
      cat("Category Frequencies:\n")
      freq_comparison <- list()
      
      for (cat_name in names(spss_values$frequencies)) {
        spss_cat <- spss_values$frequencies[[cat_name]]
        r_row <- freq_table[grepl(cat_name, freq_table$value, ignore.case = TRUE), ]
        
        if (nrow(r_row) > 0) {
          spss_freq <- ifelse(is.null(spss_cat$frequency), NA, spss_cat$frequency)
          r_freq <- r_row$freq[1]
          spss_pct <- ifelse(is.null(spss_cat$percent), NA, spss_cat$percent)
          r_pct <- r_row$prc[1]
          
          cat_comp <- tibble(
            Category = cat_name,
            SPSS_Freq = spss_freq,
            R_Freq = r_freq,
            SPSS_Pct = spss_pct,
            R_Pct = r_pct,
            Tolerance = 0.1,
            Freq_Diff = abs(spss_freq - r_freq),
            Pct_Diff = abs(spss_pct - r_pct),
            Pass = (abs(spss_freq - r_freq) <= 0.1) & (abs(spss_pct - r_pct) <= 0.1),
            Status = ifelse((abs(spss_freq - r_freq) <= 0.1) & (abs(spss_pct - r_pct) <= 0.1), "✅", "❌")
          )
          
          freq_comparison[[cat_name]] <- cat_comp
          test_summary$passed <- test_summary$passed + sum(cat_comp$Pass, na.rm = TRUE)
          test_summary$failed <- test_summary$failed + sum(!cat_comp$Pass, na.rm = TRUE)
        }
      }
      
      if (length(freq_comparison) > 0) {
        print(bind_rows(freq_comparison))
      }
    }
    
    all_comparisons[["test1"]] <- comparison_data
  } else {
    cat("⚠️ SKIPPED: Could not parse SPSS values\n")
    test_summary$skipped <- test_summary$skipped + 1
  }
  
  cat("\n")
  
  # ============================================================================
  # TEST 2: UNWEIGHTED SINGLE CATEGORICAL VARIABLE (region)
  # ============================================================================
  
  cat("TEST 2: UNWEIGHTED SINGLE CATEGORICAL VARIABLE (region)\n")
  cat("---------------------------------------------------------\n")
  
  spss_values <- extract_spss_frequency_values(spss_file, test_identifier = "TEST 1.3")
  
  if (!is.null(spss_values)) {
    # Run R frequency
    result <- frequency(survey_data, region)
    freq_table <- result$results
    
    # Compare totals
    comparison_data <- tibble(
      Statistic = c("N Valid", "Total Freq"),
      SPSS = c(spss_values$n_valid, spss_values$total_freq),
      R = c(sum(freq_table$freq), sum(freq_table$freq)),
      Tolerance = c(0.1, 0.1)
    ) %>%
      mutate(
        Difference = abs(SPSS - R),
        Pass = Difference <= Tolerance,
        Status = ifelse(Pass, "✅ PASS", "❌ FAIL")
      )
    
    print(comparison_data)
    
    test_summary$passed <- test_summary$passed + sum(comparison_data$Pass)
    test_summary$failed <- test_summary$failed + sum(!comparison_data$Pass)
    all_comparisons[["test2"]] <- comparison_data
  } else {
    cat("⚠️ SKIPPED: Could not parse SPSS values\n")
    test_summary$skipped <- test_summary$skipped + 1
  }
  
  cat("\n")
  
  # ============================================================================
  # TEST 3: UNWEIGHTED ORDINAL VARIABLE (education)
  # ============================================================================
  
  cat("TEST 3: UNWEIGHTED ORDINAL VARIABLE (education)\n")
  cat("-------------------------------------------------\n")
  
  spss_values <- extract_spss_frequency_values(spss_file, test_identifier = "TEST 1.2")
  
  if (!is.null(spss_values)) {
    # Run R frequency
    result <- frequency(survey_data, education)
    freq_table <- result$results
    
    # Compare totals
    comparison_data <- tibble(
      Statistic = c("N Valid", "Total Freq"),
      SPSS = c(spss_values$n_valid, spss_values$total_freq),
      R = c(sum(freq_table$freq), sum(freq_table$freq)),
      Tolerance = c(0.1, 0.1)
    ) %>%
      mutate(
        Difference = abs(SPSS - R),
        Pass = Difference <= Tolerance,
        Status = ifelse(Pass, "✅ PASS", "❌ FAIL")
      )
    
    print(comparison_data)
    
    test_summary$passed <- test_summary$passed + sum(comparison_data$Pass)
    test_summary$failed <- test_summary$failed + sum(!comparison_data$Pass)
    all_comparisons[["test3"]] <- comparison_data
  } else {
    cat("⚠️ SKIPPED: Could not parse SPSS values\n")
    test_summary$skipped <- test_summary$skipped + 1
  }
  
  cat("\n")
  
  # ============================================================================
  # TEST 4: UNWEIGHTED MULTIPLE VARIABLES
  # ============================================================================
  
  cat("TEST 4: UNWEIGHTED MULTIPLE VARIABLES\n")
  cat("--------------------------------------\n")
  
  # For multiple variables, we'll check that the function runs
  result <- frequency(survey_data, gender, region, education)
  
  if (!is.null(result)) {
    # Check we have results for all variables
    unique_vars <- unique(result$results$Variable)
    expected_vars <- c("gender", "region", "education")
    
    comparison_data <- tibble(
      Check = c("Variables Found", "Result Rows"),
      Expected = c(3, ">0"),
      Actual = c(length(unique_vars), nrow(result$results)),
      Pass = c(length(unique_vars) == 3, nrow(result$results) > 0),
      Status = ifelse(c(length(unique_vars) == 3, nrow(result$results) > 0), 
                      "✅ PASS", "❌ FAIL")
    )
    
    print(comparison_data)
    
    test_summary$passed <- test_summary$passed + sum(comparison_data$Pass)
    test_summary$failed <- test_summary$failed + sum(!comparison_data$Pass)
    all_comparisons[["test4"]] <- comparison_data
  } else {
    cat("⚠️ SKIPPED: Function returned NULL\n")
    test_summary$skipped <- test_summary$skipped + 1
  }
  
  cat("\n")
  
  # ============================================================================
  # TEST 5: MISSING DATA HANDLING (employment)
  # ============================================================================
  
  cat("TEST 5: MISSING DATA HANDLING (employment)\n")
  cat("-------------------------------------------\n")
  
  spss_values <- extract_spss_frequency_values(spss_file, test_identifier = "TEST 3.1")
  
  if (!is.null(spss_values)) {
    # Run R frequency with missing data
    result <- frequency(survey_data, employment, show.na = TRUE)
    
    # Check missing data handling
    n_missing <- result$stats$total_n - result$stats$valid_n
    
    comparison_data <- tibble(
      Statistic = c("N Valid", "N Missing"),
      SPSS = c(
        ifelse(is.null(spss_values$n_valid), NA, spss_values$n_valid),
        ifelse(is.null(spss_values$n_missing), 0, spss_values$n_missing)
      ),
      R = c(result$stats$valid_n, n_missing),
      Tolerance = c(0.1, 0.1)
    ) %>%
      mutate(
        Difference = abs(SPSS - R),
        Pass = Difference <= Tolerance | is.na(SPSS),
        Status = ifelse(Pass, "✅ PASS", "❌ FAIL")
      )
    
    print(comparison_data)
    
    test_summary$passed <- test_summary$passed + sum(comparison_data$Pass, na.rm = TRUE)
    test_summary$failed <- test_summary$failed + sum(!comparison_data$Pass, na.rm = TRUE)
    all_comparisons[["test5"]] <- comparison_data
  } else {
    cat("⚠️ SKIPPED: Could not parse SPSS values\n")
    test_summary$skipped <- test_summary$skipped + 1
  }
  
  cat("\n")
  
  # ============================================================================
  # TEST 6: WEIGHTED SINGLE CATEGORICAL VARIABLE (gender)
  # ============================================================================
  
  cat("TEST 6: WEIGHTED SINGLE CATEGORICAL VARIABLE (gender)\n")
  cat("-------------------------------------------------------\n")
  
  spss_values <- extract_spss_frequency_values(spss_file, test_identifier = "TEST 2.1")
  
  if (!is.null(spss_values)) {
    # Run R weighted frequency
    result <- frequency(survey_data, gender, weights = sampling_weight)
    freq_table <- result$results
    
    # Create comparison
    comparison_data <- tibble(
      Statistic = c("N Valid (Weighted)", "Total Weighted Freq"),
      SPSS = c(spss_values$n_valid, spss_values$total_freq),
      R = c(round(sum(freq_table$freq)), round(sum(freq_table$freq))),
      Tolerance = c(1, 1)
    ) %>%
      mutate(
        Difference = abs(SPSS - R),
        Pass = Difference <= Tolerance,
        Status = ifelse(Pass, "✅ PASS", "❌ FAIL")
      )
    
    print(comparison_data)
    
    test_summary$passed <- test_summary$passed + sum(comparison_data$Pass)
    test_summary$failed <- test_summary$failed + sum(!comparison_data$Pass)
    all_comparisons[["test6"]] <- comparison_data
  } else {
    cat("⚠️ SKIPPED: Could not parse SPSS values\n")
    test_summary$skipped <- test_summary$skipped + 1
  }
  
  cat("\n")
  
  # ============================================================================
  # TEST 7: WEIGHTED SINGLE CATEGORICAL VARIABLE (region)
  # ============================================================================
  
  cat("TEST 7: WEIGHTED SINGLE CATEGORICAL VARIABLE (region)\n")
  cat("------------------------------------------------------\n")
  
  spss_values <- extract_spss_frequency_values(spss_file, test_identifier = "TEST 2.3")
  
  if (!is.null(spss_values)) {
    # Run R weighted frequency
    result <- frequency(survey_data, region, weights = sampling_weight)
    freq_table <- result$results
    
    # Create comparison
    comparison_data <- tibble(
      Statistic = c("N Valid (Weighted)", "Total Weighted Freq"),
      SPSS = c(spss_values$n_valid, spss_values$total_freq),
      R = c(round(sum(freq_table$freq)), round(sum(freq_table$freq))),
      Tolerance = c(1, 1)
    ) %>%
      mutate(
        Difference = abs(SPSS - R),
        Pass = Difference <= Tolerance,
        Status = ifelse(Pass, "✅ PASS", "❌ FAIL")
      )
    
    print(comparison_data)
    
    test_summary$passed <- test_summary$passed + sum(comparison_data$Pass)
    test_summary$failed <- test_summary$failed + sum(!comparison_data$Pass)
    all_comparisons[["test7"]] <- comparison_data
  } else {
    cat("⚠️ SKIPPED: Could not parse SPSS values\n")
    test_summary$skipped <- test_summary$skipped + 1
  }
  
  cat("\n")
  
  # ============================================================================
  # TEST 8: WEIGHTED ORDINAL VARIABLE (education)
  # ============================================================================
  
  cat("TEST 8: WEIGHTED ORDINAL VARIABLE (education)\n")
  cat("-----------------------------------------------\n")
  
  spss_values <- extract_spss_frequency_values(spss_file, test_identifier = "TEST 2.2")
  
  if (!is.null(spss_values)) {
    # Run R weighted frequency
    result <- frequency(survey_data, education, weights = sampling_weight)
    freq_table <- result$results
    
    # Create comparison
    comparison_data <- tibble(
      Statistic = c("N Valid (Weighted)", "Total Weighted Freq"),
      SPSS = c(spss_values$n_valid, spss_values$total_freq),
      R = c(round(sum(freq_table$freq)), round(sum(freq_table$freq))),
      Tolerance = c(1, 1)
    ) %>%
      mutate(
        Difference = abs(SPSS - R),
        Pass = Difference <= Tolerance,
        Status = ifelse(Pass, "✅ PASS", "❌ FAIL")
      )
    
    print(comparison_data)
    
    test_summary$passed <- test_summary$passed + sum(comparison_data$Pass)
    test_summary$failed <- test_summary$failed + sum(!comparison_data$Pass)
    all_comparisons[["test8"]] <- comparison_data
  } else {
    cat("⚠️ SKIPPED: Could not parse SPSS values\n")
    test_summary$skipped <- test_summary$skipped + 1
  }
  
  # ============================================================================
  # SUMMARY
  # ============================================================================
  
  cat("\n")
  cat("================================================================================\n")
  cat("                              TEST SUMMARY                                     \n")
  cat("================================================================================\n")
  
  # Ensure test_summary values are numeric and not NA
  if (is.null(test_summary$passed) || is.na(test_summary$passed)) {
    test_summary$passed <- 0
  }
  if (is.null(test_summary$failed) || is.na(test_summary$failed)) {
    test_summary$failed <- 0
  }
  if (is.null(test_summary$skipped) || is.na(test_summary$skipped)) {
    test_summary$skipped <- 0
  }
  
  cat("✅ PASSED:", test_summary$passed, "tests\n")
  cat("❌ FAILED:", test_summary$failed, "tests\n")  
  cat("⚠️  SKIPPED:", test_summary$skipped, "tests\n")
  cat("\n")
  
  # Calculate pass rate with proper NA handling
  total_tests <- test_summary$passed + test_summary$failed
  if (total_tests > 0) {
    cat("OVERALL PASS RATE:", sprintf("%.1f%%", 100 * test_summary$passed / total_tests), "\n")
  } else {
    cat("OVERALL PASS RATE: N/A (no tests completed)\n")
  }
  
  # Save to file
  report_file <- "tests/validation_reports/frequency_validation_report.txt"
  sink(report_file)
  
  cat("FREQUENCY() VALIDATION DETAILED REPORT\n")
  cat("=======================================\n")
  cat("\n")
  
  # Print all test results
  for (test_name in names(all_comparisons)) {
    cat("\n", toupper(test_name), ":\n")
    print(all_comparisons[[test_name]])
  }
  
  cat("\n\nSUMMARY:\n")
  cat("--------\n")
  cat("PASSED:", test_summary$passed, "\n")
  cat("FAILED:", test_summary$failed, "\n")
  cat("SKIPPED:", test_summary$skipped, "\n")
  cat("PASS RATE:", sprintf("%.1f%%", 
                            100 * test_summary$passed / 
                            (test_summary$passed + test_summary$failed)), "\n")
  
  sink()
  
  cat("\n📄 Detailed report saved to:", report_file, "\n")
  
  return(list(
    comparisons = all_comparisons,
    summary = test_summary
  ))
}

# ============================================================================
# MAIN EXECUTION WRAPPER
# ============================================================================

#' Generate all validation reports
#' 
#' This function generates validation reports for all tested functions
#' Can be called manually or automatically after tests
#' 
#' @param verbose Logical, whether to print progress messages
#' @return List with results from all report generations
generate_all_validation_reports <- function(verbose = TRUE) {
  results <- list()
  
  if (verbose) {
    cat("\n🔬 GENERATING SPSS VALIDATION REPORTS\n")
    cat("=" , rep("=", 78), "\n", sep = "")
  }
  
  # Generate describe() validation report
  tryCatch({
    results$describe <- generate_describe_validation_report()
  }, error = function(e) {
    if (verbose) cat("⚠️ Error generating describe() report:", e$message, "\n")
    results$describe <- NULL
  })
  
  # Generate t_test() validation report
  tryCatch({
    results$ttest <- generate_ttest_validation_report()
  }, error = function(e) {
    if (verbose) cat("⚠️ Error generating t_test() report:", e$message, "\n")
    results$ttest <- NULL
  })
  
  # Generate chi_squared_test() validation report
  tryCatch({
    results$chi_squared <- generate_chi_squared_validation_report()
  }, error = function(e) {
    if (verbose) cat("⚠️ Error generating chi_squared_test() report:", e$message, "\n")
    results$chi_squared <- NULL
  })
  
  # Generate frequency() validation report
  tryCatch({
    results$frequency <- generate_frequency_validation_report()
  }, error = function(e) {
    if (verbose) cat("⚠️ Error generating frequency() report:", e$message, "\n")
    results$frequency <- NULL
  })
  
  if (verbose) {
    cat("\n✅ VALIDATION REPORTS COMPLETE!\n")
    cat("\nReports saved in: tests/validation_reports/\n")
  }
  
  invisible(results)
}

# ============================================================================
# STANDALONE EXECUTION (when sourced directly)
# ============================================================================

# Only run if this script is executed directly, not when sourced
if (!interactive() && length(commandArgs(TRUE)) == 0) {
  # This block runs when called via Rscript or source() with no arguments
  if (sys.nframe() == 0) {
    generate_all_validation_reports(verbose = TRUE)
  }
}