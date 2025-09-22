# ============================================================================
# Helper Functions for Validation Report Generation
# ============================================================================
#
# Purpose: Provide utilities for automatic generation of SPSS validation reports
#          after testthat tests complete
#
# This helper file is automatically sourced by testthat before running tests
# ============================================================================

# Track which validation tests have been run
.validation_tests_run <- new.env(parent = emptyenv())

#' Mark a validation test as executed
#' 
#' Call this at the beginning of each SPSS validation test file
#' to track which reports need to be generated
#' 
#' @param test_name Name of the test (e.g., "describe", "t_test")
mark_validation_test_run <- function(test_name) {
  .validation_tests_run[[test_name]] <- TRUE
}

#' Check if a validation test was run
#' 
#' @param test_name Name of the test to check
#' @return Logical indicating if the test was run
was_validation_test_run <- function(test_name) {
  isTRUE(.validation_tests_run[[test_name]])
}

#' Get list of all validation tests that were run
#' 
#' @return Character vector of test names
get_validation_tests_run <- function() {
  names(.validation_tests_run)[unlist(lapply(names(.validation_tests_run), 
                                            function(x) isTRUE(.validation_tests_run[[x]])))]
}

#' Reset validation test tracking
#' 
#' Called at the beginning of test suite to reset tracking
reset_validation_tracking <- function() {
  rm(list = ls(.validation_tests_run), envir = .validation_tests_run)
}

#' Generate validation reports for tests that were run
#' 
#' This function checks which validation tests were executed and generates
#' corresponding reports. It sources the report generation script if needed.
#' 
#' @param verbose Logical, whether to print progress messages
#' @param force Logical, whether to force generation even if tests weren't run
#' @return Invisible NULL or list of report results
generate_validation_reports_if_needed <- function(verbose = TRUE, force = FALSE) {
  tests_run <- get_validation_tests_run()
  
  # If no validation tests were run and not forcing, skip
  if (length(tests_run) == 0 && !force) {
    if (verbose) {
      cat("\nℹ️ No SPSS validation tests were run - skipping report generation\n")
    }
    return(invisible(NULL))
  }
  
  # Check if we're in the right directory structure
  # Get current working directory for debugging
  current_wd <- getwd()
  
  report_script_paths <- c(
    "tests/validation_reports/generate_validation_reports.R",
    "../validation_reports/generate_validation_reports.R",
    "validation_reports/generate_validation_reports.R",
    file.path(dirname(dirname(current_wd)), "validation_reports/generate_validation_reports.R"),
    normalizePath("../../validation_reports/generate_validation_reports.R", mustWork = FALSE)
  )
  
  # Try to use test_path if available
  if (exists("test_path", where = asNamespace("testthat"))) {
    report_script_paths <- c(report_script_paths,
                            testthat::test_path("../validation_reports/generate_validation_reports.R"))
  }
  
  report_script <- NULL
  for (path in report_script_paths) {
    if (file.exists(path)) {
      report_script <- path
      break
    }
  }
  
  if (is.null(report_script)) {
    if (verbose) {
      cat("\n⚠️ Could not find validation report generation script\n")
      cat("   Current directory:", current_wd, "\n")
      cat("   Searched paths:\n")
      for (path in report_script_paths) {
        cat("    -", path, "(exists:", file.exists(path), ")\n")
      }
    }
    return(invisible(NULL))
  }
  
  if (verbose) {
    cat("\n📊 Generating validation reports for:", paste(tests_run, collapse = ", "), "\n")
  }
  
  # Save current directory and change to correct location
  current_dir <- getwd()
  
  # Find the package root (where tests/ directory is)
  package_root <- NULL
  if (grepl("/testthat$", current_dir)) {
    # We're in tests/testthat/
    package_root <- dirname(dirname(current_dir))
  } else if (grepl("/tests$", current_dir)) {
    # We're in tests/
    package_root <- dirname(current_dir)
  } else {
    # We might be at package root
    package_root <- current_dir
  }
  
  # Change to package root to ensure paths work
  on.exit(setwd(current_dir), add = TRUE)
  setwd(package_root)
  
  # Source the report generation script
  source(report_script, local = TRUE)
  
  # Generate reports based on which tests were run
  results <- list()
  
  if (was_validation_test_run("describe") || force) {
    tryCatch({
      if (exists("generate_describe_validation_report")) {
        if (verbose) cat("  - Generating describe() validation report...\n")
        results$describe <- generate_describe_validation_report()
      }
    }, error = function(e) {
      if (verbose) cat("    ⚠️ Error:", e$message, "\n")
    })
  }
  
  if (was_validation_test_run("t_test") || force) {
    tryCatch({
      if (exists("generate_ttest_validation_report")) {
        if (verbose) cat("  - Generating t_test() validation report...\n")
        results$ttest <- generate_ttest_validation_report()
      }
    }, error = function(e) {
      if (verbose) cat("    ⚠️ Error:", e$message, "\n")
    })
  }
  
  if (was_validation_test_run("frequency") || force) {
    tryCatch({
      if (exists("generate_frequency_validation_report")) {
        if (verbose) cat("  - Generating frequency() validation report...\n")
        results$frequency <- generate_frequency_validation_report()
      }
    }, error = function(e) {
      if (verbose) cat("    ⚠️ Error:", e$message, "\n")
    })
  }
  
  # Save results summary
  if (length(results) > 0 && verbose) {
    cat("\n✅ Validation reports generated successfully\n")
  }
  
  invisible(results)
}

#' Set up automatic report generation after tests
#' 
#' This function sets up a deferred action to generate reports after tests complete.
#' Call this in setup.R or at the beginning of test files.
setup_automatic_report_generation <- function() {
  # Use withr::defer to ensure reports are generated after all tests
  if (requireNamespace("withr", quietly = TRUE)) {
    withr::defer({
      # Only generate reports if we're not in a check environment
      if (!isTRUE(as.logical(Sys.getenv("_R_CHECK_PACKAGE_NAME_", "FALSE")))) {
        generate_validation_reports_if_needed(verbose = TRUE)
      }
    }, envir = parent.frame())
  }
}