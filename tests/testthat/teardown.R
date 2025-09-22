# ============================================================================
# Teardown for testthat tests
# ============================================================================
#
# This file runs after all tests complete
# Used to generate validation reports based on which tests were run
#
# ============================================================================

# Check if report generation is enabled
if (isTRUE(as.logical(Sys.getenv("SURVEYSTAT_GENERATE_REPORTS", "TRUE")))) {
  
  # Check which validation tests were run
  tests_run <- character()
  if (exists("get_validation_tests_run") && is.function(get_validation_tests_run)) {
    tests_run <- get_validation_tests_run()
  }
  
  # Generate reports if any validation tests were run
  if (length(tests_run) > 0 || 
      isTRUE(as.logical(Sys.getenv("SURVEYSTAT_FORCE_REPORTS", "FALSE")))) {
    
    cat("\n")
    cat("=" , rep("=", 78), "\n", sep = "")
    cat("                    POST-TEST VALIDATION REPORT GENERATION                    \n")
    cat("=" , rep("=", 78), "\n", sep = "")
    
    if (length(tests_run) > 0) {
      cat("\n📊 Validation tests executed:", paste(tests_run, collapse = ", "), "\n")
    } else {
      cat("\n📊 Forcing report generation (SURVEYSTAT_FORCE_REPORTS = TRUE)\n")
    }
    
    # Try to generate reports directly
    # Find the report generation script
    report_script <- NULL
    current_dir <- getwd()
    
    # Look for the script in various locations
    possible_paths <- c(
      # If we're in tests/testthat/
      "../../tests/validation_reports/generate_validation_reports.R",
      "../validation_reports/generate_validation_reports.R",
      # If we're at package root
      "tests/validation_reports/generate_validation_reports.R",
      # Use normalizePath to resolve
      normalizePath("tests/validation_reports/generate_validation_reports.R", mustWork = FALSE)
    )
    
    for (path in possible_paths) {
      if (file.exists(path)) {
        report_script <- normalizePath(path)
        break
      }
    }
    
    if (!is.null(report_script)) {
      tryCatch({
        cat("\n🔄 Generating validation reports...\n")
        cat("   Using script:", report_script, "\n")
        
        # Save current directory and change to package root
        orig_dir <- getwd()
        pkg_root <- dirname(dirname(dirname(report_script)))  # Go up one more level to package root
        setwd(pkg_root)
        cat("   Working directory:", getwd(), "\n")
        
        # Source and run the report generation
        source(report_script)
        if (exists("generate_all_validation_reports")) {
          generate_all_validation_reports(verbose = TRUE)
        }
        
        # Restore original directory
        setwd(orig_dir)
        
        cat("\n✅ Validation report generation complete\n")
      }, error = function(e) {
        cat("\n⚠️ Error generating validation reports:", e$message, "\n")
        cat("   You can manually generate reports by running:\n")
        cat("   source('tests/validation_reports/generate_validation_reports.R')\n")
        # Try to restore directory if error occurred
        if (exists("orig_dir")) try(setwd(orig_dir), silent = TRUE)
      })
    } else if (exists("generate_validation_reports_if_needed") && 
               is.function(generate_validation_reports_if_needed)) {
      # Fallback to helper function if available
      tryCatch({
        cat("\n🔄 Generating validation reports (via helper)...\n")
        generate_validation_reports_if_needed(verbose = TRUE, 
                                            force = isTRUE(as.logical(Sys.getenv("SURVEYSTAT_FORCE_REPORTS", "FALSE"))))
        cat("\n✅ Validation report generation complete\n")
      }, error = function(e) {
        cat("\n⚠️ Error generating validation reports:", e$message, "\n")
        cat("   Current directory:", getwd(), "\n")
        cat("   You can manually generate reports by running:\n")
        cat("   source('tests/validation_reports/generate_validation_reports.R')\n")
      })
      
    } else {
      cat("\n⚠️ Report generation functions not available\n")
      cat("   Ensure helper-validation-reports.R is properly sourced\n")
    }
    
    cat("=" , rep("=", 78), "\n", sep = "")
    
  } else {
    cat("\nℹ️ No SPSS validation tests were run - skipping report generation\n")
    cat("   To force report generation: Sys.setenv(SURVEYSTAT_FORCE_REPORTS = 'TRUE')\n")
  }
  
} else {
  cat("\nℹ️ Validation report generation is DISABLED\n")
  cat("   To enable: Sys.setenv(SURVEYSTAT_GENERATE_REPORTS = 'TRUE')\n")
}

# Clean up environment
if (exists(".validation_tests_run")) {
  rm(.validation_tests_run, envir = .GlobalEnv)
}