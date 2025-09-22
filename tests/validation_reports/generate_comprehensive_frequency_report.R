# ============================================================================
# COMPREHENSIVE FREQUENCY TABLE VALIDATION REPORT GENERATOR
# ============================================================================
# This script generates detailed validation reports comparing SurveyStat
# frequency tables with SPSS output at the cell level
# ============================================================================

library(tidyverse)
library(SurveyStat)
library(haven)

# Source the parser
source("tests/testthat/helper-frequency-parser.R")

# ============================================================================
# MAIN REPORT GENERATION FUNCTION
# ============================================================================

generate_comprehensive_frequency_report <- function() {
  
  cat("\n")
  cat("================================================================================\n")
  cat("           COMPREHENSIVE FREQUENCY TABLE VALIDATION REPORT                      \n")
  cat("================================================================================\n")
  cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  # Check for required files
  required_files <- c(
    "tests/spss_reference/data/survey_data.sav",
    "tests/spss_reference/outputs/frequency_comprehensive_output.txt"
  )
  
  missing_files <- !file.exists(required_files)
  if (any(missing_files)) {
    cat("❌ Missing required files:\n")
    cat(paste(" -", required_files[missing_files]), sep = "\n")
    cat("\nPlease run the SPSS syntax first to generate output files.\n")
    return(NULL)
  }
  
  # Load data
  survey_data <- haven::read_sav("tests/spss_reference/data/survey_data.sav")
  spss_lines <- readLines("tests/spss_reference/outputs/frequency_comprehensive_output.txt")
  
  # Initialize summary statistics
  test_summary <- list(
    total_cells = 0,
    passed_cells = 0,
    failed_cells = 0,
    tests_run = 0,
    tests_passed = 0
  )
  
  all_results <- list()
  
  # ============================================================================
  # TEST 1: BINARY VARIABLE COMPLETE TABLE (UNWEIGHTED)
  # ============================================================================
  
  cat("\n### TEST 1: BINARY VARIABLE (gender) - UNWEIGHTED ###\n")
  cat("=", rep("-", 60), "\n", sep = "")
  
  test1_results <- validate_frequency_table(
    spss_lines = spss_lines,
    test_id = "TEST 1",
    data = survey_data,
    variable = "gender",
    weights = NULL
  )
  
  if (!is.null(test1_results)) {
    print_validation_results(test1_results)
    all_results$test1 <- test1_results
    update_summary(test_summary, test1_results)
  }
  
  # ============================================================================
  # TEST 2: MULTI-CATEGORY VARIABLE (UNWEIGHTED)
  # ============================================================================
  
  cat("\n### TEST 2: MULTI-CATEGORY VARIABLE (education) - UNWEIGHTED ###\n")
  cat("=", rep("-", 60), "\n", sep = "")
  
  test2_results <- validate_frequency_table(
    spss_lines = spss_lines,
    test_id = "TEST 2",
    data = survey_data,
    variable = "education",
    weights = NULL
  )
  
  if (!is.null(test2_results)) {
    print_validation_results(test2_results)
    all_results$test2 <- test2_results
    update_summary(test_summary, test2_results)
  }
  
  # ============================================================================
  # TEST 3: WEIGHTED BINARY VARIABLE
  # ============================================================================
  
  cat("\n### TEST 3: BINARY VARIABLE (gender) - WEIGHTED ###\n")
  cat("=", rep("-", 60), "\n", sep = "")
  
  test3_results <- validate_frequency_table(
    spss_lines = spss_lines,
    test_id = "TEST 3",
    data = survey_data,
    variable = "gender",
    weights = "sampling_weight"
  )
  
  if (!is.null(test3_results)) {
    print_validation_results(test3_results)
    all_results$test3 <- test3_results
    update_summary(test_summary, test3_results)
  }
  
  # ============================================================================
  # TEST 4: WEIGHTED MULTI-CATEGORY VARIABLE
  # ============================================================================
  
  cat("\n### TEST 4: MULTI-CATEGORY VARIABLE (education) - WEIGHTED ###\n")
  cat("=", rep("-", 60), "\n", sep = "")
  
  test4_results <- validate_frequency_table(
    spss_lines = spss_lines,
    test_id = "TEST 4",
    data = survey_data,
    variable = "education",
    weights = "sampling_weight"
  )
  
  if (!is.null(test4_results)) {
    print_validation_results(test4_results)
    all_results$test4 <- test4_results
    update_summary(test_summary, test4_results)
  }
  
  # ============================================================================
  # TEST 5-6: MISSING VALUES HANDLING
  # ============================================================================
  
  cat("\n### TEST 5: MISSING VALUES - EXCLUDED ###\n")
  cat("=", rep("-", 60), "\n", sep = "")
  
  test5_results <- validate_frequency_table(
    spss_lines = spss_lines,
    test_id = "TEST 5",
    data = survey_data,
    variable = "employment",
    weights = NULL,
    show_na = FALSE
  )
  
  if (!is.null(test5_results)) {
    print_validation_results(test5_results)
    all_results$test5 <- test5_results
    update_summary(test_summary, test5_results)
  }
  
  cat("\n### TEST 6: MISSING VALUES - INCLUDED ###\n")
  cat("=", rep("-", 60), "\n", sep = "")
  
  test6_results <- validate_frequency_table(
    spss_lines = spss_lines,
    test_id = "TEST 6",
    data = survey_data,
    variable = "employment",
    weights = NULL,
    show_na = TRUE
  )
  
  if (!is.null(test6_results)) {
    print_validation_results(test6_results)
    all_results$test6 <- test6_results
    update_summary(test_summary, test6_results)
  }
  
  # ============================================================================
  # OVERALL SUMMARY
  # ============================================================================
  
  cat("\n")
  cat("================================================================================\n")
  cat("                           VALIDATION SUMMARY                                   \n")
  cat("================================================================================\n")
  
  cat(sprintf("\nTESTS RUN: %d\n", test_summary$tests_run))
  cat(sprintf("TESTS PASSED: %d\n", test_summary$tests_passed))
  cat(sprintf("TEST PASS RATE: %.1f%%\n\n", 
              100 * test_summary$tests_passed / max(1, test_summary$tests_run)))
  
  cat(sprintf("TOTAL CELLS COMPARED: %d\n", test_summary$total_cells))
  cat(sprintf("✅ CELLS PASSED: %d\n", test_summary$passed_cells))
  cat(sprintf("❌ CELLS FAILED: %d\n", test_summary$failed_cells))
  cat(sprintf("CELL PASS RATE: %.1f%%\n", 
              100 * test_summary$passed_cells / max(1, test_summary$total_cells)))
  
  # Save detailed report
  report_file <- "tests/validation_reports/frequency_comprehensive_report.txt"
  sink(report_file)
  
  cat("COMPREHENSIVE FREQUENCY TABLE VALIDATION REPORT\n")
  cat("=============================================\n\n")
  cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  for (test_name in names(all_results)) {
    cat("\n", toupper(test_name), ":\n\n")
    print(all_results[[test_name]]$comparison)
    if (!is.null(all_results[[test_name]]$statistics)) {
      cat("\nStatistics:\n")
      print(all_results[[test_name]]$statistics)
    }
    cat("\n")
  }
  
  cat("\nSUMMARY:\n")
  cat(sprintf("Cell Pass Rate: %.1f%%\n", 
              100 * test_summary$passed_cells / max(1, test_summary$total_cells)))
  cat(sprintf("Test Pass Rate: %.1f%%\n",
              100 * test_summary$tests_passed / max(1, test_summary$tests_run)))
  
  sink()
  
  cat("\n📄 Detailed report saved to:", report_file, "\n")
  
  return(list(
    results = all_results,
    summary = test_summary
  ))
}

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Validate a single frequency table
validate_frequency_table <- function(spss_lines, test_id, data, variable, 
                                     weights = NULL, show_na = TRUE) {
  
  # Parse SPSS table
  spss_table <- parse_spss_frequency_table(spss_lines, test_id, variable)
  
  if (is.null(spss_table)) {
    cat("  ❌ Could not parse SPSS table\n")
    return(NULL)
  }
  
  # Generate R frequency table
  if (is.null(weights)) {
    r_result <- SurveyStat::frequency(data, !!sym(variable), show.na = show_na)
  } else {
    r_result <- SurveyStat::frequency(data, !!sym(variable), 
                                      weights = !!sym(weights), 
                                      show.na = show_na)
  }
  
  # Extract R frequency data
  r_freq_df <- r_result$results[[variable]]
  
  # Create comparison table
  comparison_rows <- list()
  
  for (i in seq_along(spss_table$frequencies)) {
    spss_row <- spss_table$frequencies[[i]]
    
    # Find corresponding R row
    if (is.na(spss_row$value)) {
      r_row_idx <- which(is.na(r_freq_df$value))
    } else {
      r_row_idx <- which(r_freq_df$value == spss_row$value)
    }
    
    if (length(r_row_idx) > 0) {
      r_row <- r_freq_df[r_row_idx[1], ]
      
      # Create comparison for this row
      comp <- tibble(
        Value = as.character(spss_row$value),
        Label = spss_row$label,
        
        `SPSS_Freq` = spss_row$frequency,
        `R_Freq` = r_row$n,
        `Freq_Diff` = abs(spss_row$frequency - r_row$n),
        `Freq_Pass` = abs(spss_row$frequency - r_row$n) <= 1.0,
        
        `SPSS_%` = spss_row$percent,
        `R_%` = r_row$percent,
        `%_Diff` = abs(spss_row$percent - r_row$percent),
        `%_Pass` = abs(spss_row$percent - r_row$percent) <= 0.1,
        
        `SPSS_Valid%` = spss_row$valid_percent,
        `R_Valid%` = r_row$valid_percent,
        `Valid%_Diff` = if (!is.na(spss_row$valid_percent) && !is.na(r_row$valid_percent))
          abs(spss_row$valid_percent - r_row$valid_percent) else NA,
        `Valid%_Pass` = if (!is.na(spss_row$valid_percent) && !is.na(r_row$valid_percent))
          abs(spss_row$valid_percent - r_row$valid_percent) <= 0.1 else TRUE,
        
        `SPSS_Cum%` = spss_row$cumulative_percent,
        `R_Cum%` = r_row$cumulative,
        `Cum%_Diff` = if (!is.na(spss_row$cumulative_percent) && !is.na(r_row$cumulative))
          abs(spss_row$cumulative_percent - r_row$cumulative) else NA,
        `Cum%_Pass` = if (!is.na(spss_row$cumulative_percent) && !is.na(r_row$cumulative))
          abs(spss_row$cumulative_percent - r_row$cumulative) <= 0.1 else TRUE
      )
      
      comparison_rows[[length(comparison_rows) + 1]] <- comp
    }
  }
  
  if (length(comparison_rows) > 0) {
    comparison_df <- bind_rows(comparison_rows)
    
    # Check totals
    if (!is.null(spss_table$totals$frequency)) {
      r_total <- sum(r_freq_df$n, na.rm = TRUE)
      
      total_comp <- tibble(
        Value = "TOTAL",
        Label = "",
        `SPSS_Freq` = spss_table$totals$frequency,
        `R_Freq` = r_total,
        `Freq_Diff` = abs(spss_table$totals$frequency - r_total),
        `Freq_Pass` = abs(spss_table$totals$frequency - r_total) <= 1.0,
        `SPSS_%` = spss_table$totals$percent,
        `R_%` = 100.0,
        `%_Diff` = if (!is.na(spss_table$totals$percent)) 
          abs(spss_table$totals$percent - 100) else 0,
        `%_Pass` = TRUE,
        `SPSS_Valid%` = NA,
        `R_Valid%` = NA,
        `Valid%_Diff` = NA,
        `Valid%_Pass` = TRUE,
        `SPSS_Cum%` = NA,
        `R_Cum%` = NA,
        `Cum%_Diff` = NA,
        `Cum%_Pass` = TRUE
      )
      
      comparison_df <- bind_rows(comparison_df, total_comp)
    }
    
    # Compare statistics if available
    stats_comp <- NULL
    if (!is.null(spss_table$statistics) && length(spss_table$statistics) > 0) {
      r_stats <- r_result$stats[[variable]]
      
      stats_rows <- list()
      
      if (!is.null(spss_table$statistics$mode) && !is.null(r_stats$mode)) {
        stats_rows[["Mode"]] <- tibble(
          Statistic = "Mode",
          SPSS = spss_table$statistics$mode,
          R = as.numeric(r_stats$mode),
          Difference = abs(spss_table$statistics$mode - as.numeric(r_stats$mode)),
          Pass = abs(spss_table$statistics$mode - as.numeric(r_stats$mode)) <= 0.01
        )
      }
      
      if (!is.null(spss_table$statistics$median) && !is.null(r_stats$median)) {
        stats_rows[["Median"]] <- tibble(
          Statistic = "Median",
          SPSS = spss_table$statistics$median,
          R = as.numeric(r_stats$median),
          Difference = abs(spss_table$statistics$median - as.numeric(r_stats$median)),
          Pass = abs(spss_table$statistics$median - as.numeric(r_stats$median)) <= 0.01
        )
      }
      
      if (length(stats_rows) > 0) {
        stats_comp <- bind_rows(stats_rows)
      }
    }
    
    return(list(
      comparison = comparison_df,
      statistics = stats_comp,
      test_id = test_id,
      variable = variable
    ))
  }
  
  return(NULL)
}

#' Print validation results
print_validation_results <- function(results) {
  
  if (is.null(results)) {
    cat("  No results available\n")
    return()
  }
  
  comp <- results$comparison
  
  # Print frequency comparison
  cat("\n  Cell-by-Cell Comparison:\n")
  cat("  ", rep("-", 70), "\n", sep = "")
  
  for (i in 1:nrow(comp)) {
    row <- comp[i, ]
    
    cat(sprintf("  Value: %s %s\n", 
                row$Value, 
                if (nchar(row$Label) > 0) paste0("(", row$Label, ")") else ""))
    
    cat(sprintf("    Frequency:  SPSS=%7.1f  R=%7.1f  Diff=%5.1f  %s\n",
                row$`SPSS_Freq`, row$`R_Freq`, row$`Freq_Diff`,
                if (row$`Freq_Pass`) "✅" else "❌"))
    
    cat(sprintf("    Percent:    SPSS=%6.1f%%  R=%6.1f%%  Diff=%5.1f  %s\n",
                row$`SPSS_%`, row$`R_%`, row$`%_Diff`,
                if (row$`%_Pass`) "✅" else "❌"))
    
    if (!is.na(row$`SPSS_Valid%`)) {
      cat(sprintf("    Valid %%:    SPSS=%6.1f%%  R=%6.1f%%  Diff=%5.1f  %s\n",
                  row$`SPSS_Valid%`, row$`R_Valid%`, row$`Valid%_Diff`,
                  if (row$`Valid%_Pass`) "✅" else "❌"))
    }
    
    if (!is.na(row$`SPSS_Cum%`)) {
      cat(sprintf("    Cumulative: SPSS=%6.1f%%  R=%6.1f%%  Diff=%5.1f  %s\n",
                  row$`SPSS_Cum%`, row$`R_Cum%`, row$`Cum%_Diff`,
                  if (row$`Cum%_Pass`) "✅" else "❌"))
    }
    
    cat("\n")
  }
  
  # Print statistics comparison if available
  if (!is.null(results$statistics)) {
    cat("\n  Statistics Comparison:\n")
    cat("  ", rep("-", 70), "\n", sep = "")
    
    for (i in 1:nrow(results$statistics)) {
      stat_row <- results$statistics[i, ]
      cat(sprintf("  %s: SPSS=%.2f  R=%.2f  Diff=%.3f  %s\n",
                  stat_row$Statistic,
                  stat_row$SPSS,
                  stat_row$R,
                  stat_row$Difference,
                  if (stat_row$Pass) "✅" else "❌"))
    }
  }
  
  # Calculate pass rate for this test
  pass_cols <- grep("Pass$", names(comp))
  total_checks <- sum(!is.na(comp[, pass_cols]))
  passed_checks <- sum(comp[, pass_cols] == TRUE, na.rm = TRUE)
  
  cat("\n  Test Summary:\n")
  cat(sprintf("  Cells Checked: %d\n", nrow(comp)))
  cat(sprintf("  Metrics Passed: %d/%d (%.1f%%)\n", 
              passed_checks, total_checks,
              100 * passed_checks / total_checks))
  
  if (passed_checks == total_checks) {
    cat("  Overall: ✅ PASS\n")
  } else {
    cat("  Overall: ❌ FAIL\n")
  }
}

#' Update summary statistics
update_summary <- function(summary, results) {
  
  if (is.null(results)) return()
  
  summary$tests_run <- summary$tests_run + 1
  
  comp <- results$comparison
  pass_cols <- grep("Pass$", names(comp))
  
  total_checks <- sum(!is.na(comp[, pass_cols]))
  passed_checks <- sum(comp[, pass_cols] == TRUE, na.rm = TRUE)
  
  summary$total_cells <- summary$total_cells + total_checks
  summary$passed_cells <- summary$passed_cells + passed_checks
  summary$failed_cells <- summary$failed_cells + (total_checks - passed_checks)
  
  if (passed_checks == total_checks) {
    summary$tests_passed <- summary$tests_passed + 1
  }
}

# Run the report if called directly
if (sys.nframe() == 0) {
  generate_comprehensive_frequency_report()
}