# SPSS Validation Utilities
# =============================================================
# Helper functions for easier SPSS validation integration
# with testthat framework.

#' Load SPSS Validation Functions
#' 
#' Sources all necessary SPSS validation functions
#' @keywords internal
load_spss_validation <- function() {
  base_path <- "tests/spss_reference/helpers"
  
  # Source parser functions
  source(file.path(base_path, "spss_parser.R"), local = parent.frame())
  
  # Source validation framework
  source(file.path(base_path, "validation_framework.R"), local = parent.frame())
}

#' Get SPSS Reference File Path
#' 
#' Constructs standardized path to SPSS reference files
#' 
#' @param function_name Name of statistical function (e.g., "t_test", "describe")
#' @param analysis_type Type of analysis (e.g., "basic", "weighted", "grouped")
#' @param file_extension File extension ("txt" for outputs, "sps" for syntax)
#' @return Character path to SPSS reference file
#' @keywords internal
get_spss_reference_path <- function(function_name, analysis_type = "basic", file_extension = "txt") {
  
  base_path <- "tests/spss_reference"
  
  if (file_extension == "txt") {
    subfolder <- "outputs"
  } else if (file_extension == "sps") {
    subfolder <- "syntax"
  } else if (file_extension == "sav") {
    subfolder <- "data"
  } else {
    stop("Unsupported file extension: ", file_extension)
  }
  
  file_name <- paste0(function_name, "_", analysis_type, ".", file_extension)
  file_path <- file.path(base_path, subfolder, function_name, file_name)
  
  return(file_path)
}

#' Check SPSS Reference File Availability
#' 
#' Verifies that required SPSS reference files exist
#' 
#' @param function_name Function to check
#' @param analysis_types Vector of analysis types to check
#' @return Logical vector indicating which files exist
#' @export
check_spss_references <- function(function_name, analysis_types = c("basic", "weighted", "grouped")) {
  
  results <- setNames(logical(length(analysis_types)), analysis_types)
  
  for (analysis_type in analysis_types) {
    file_path <- get_spss_reference_path(function_name, analysis_type)
    results[analysis_type] <- file.exists(file_path)
  }
  
  return(results)
}

#' Skip Test if SPSS Reference Missing
#' 
#' Testthat helper to skip tests when SPSS reference files are not available
#' 
#' @param function_name Function name to check
#' @param analysis_type Analysis type to check
#' @keywords internal
skip_if_no_spss_reference <- function(function_name, analysis_type = "basic") {
  file_path <- get_spss_reference_path(function_name, analysis_type)
  
  if (!file.exists(file_path)) {
    testthat::skip(paste("SPSS reference file not found:", file_path))
  }
}

#' Create SPSS Validation Test
#' 
#' Factory function for creating standardized SPSS validation tests
#' 
#' @param test_name Descriptive name for the test
#' @param function_name SurveyStat function to test
#' @param analysis_type Type of analysis (basic, weighted, grouped)
#' @param r_call Expression that calls the SurveyStat function
#' @param spss_validation_function Validation function to use
#' @param var.equal For t-test, which variance assumption to test
#' @return testthat test function
#' @export
create_spss_validation_test <- function(test_name, function_name, analysis_type, 
                                        r_call, spss_validation_function, 
                                        var.equal = FALSE) {
  
  testthat::test_that(paste("SPSS validation:", test_name), {
    
    # Skip if reference file missing
    skip_if_no_spss_reference(function_name, analysis_type)
    
    # Load validation functions
    load_spss_validation()
    
    # Get SPSS reference file
    spss_file <- get_spss_reference_path(function_name, analysis_type)
    
    # Execute R function call
    r_result <- eval(r_call)
    
    # Validate against SPSS
    if (function_name == "t_test") {
      validation <- spss_validation_function(r_result, spss_file, var.equal = var.equal)
    } else {
      validation <- spss_validation_function(r_result, spss_file)
    }
    
    # Check validation results
    if (!validation$passed) {
      # Create detailed error message
      error_msg <- paste("SPSS validation failed for", test_name, "\nDiscrepancies:")
      
      for (disc_name in names(validation$discrepancies)) {
        disc <- validation$discrepancies[[disc_name]]
        error_msg <- paste0(error_msg, "\n  ", disc_name, ": R=", disc$r_value, 
                           " SPSS=", disc$spss_value, " diff=", disc$difference,
                           " (tolerance=", disc$tolerance, ")")
      }
      
      fail(error_msg)
    }
    
    # If we get here, validation passed
    expect_true(validation$passed)
    
    # Optionally check pass rate
    if (!is.null(validation$summary$pass_rate)) {
      expect_gte(validation$summary$pass_rate, 0.95)  # At least 95% compatibility
    }
  })
}

#' Batch Create SPSS Validation Tests
#' 
#' Creates multiple SPSS validation tests for a function
#' 
#' @param function_name SurveyStat function name
#' @param test_configs List of test configurations
#' @return List of testthat test functions
#' @export
batch_create_spss_tests <- function(function_name, test_configs) {
  
  tests <- list()
  
  for (config in test_configs) {
    test_func <- create_spss_validation_test(
      test_name = config$test_name,
      function_name = function_name,
      analysis_type = config$analysis_type,
      r_call = config$r_call,
      spss_validation_function = config$validation_function,
      var.equal = config$var.equal %||% FALSE
    )
    
    tests[[config$test_name]] <- test_func
  }
  
  return(tests)
}

#' Generate SPSS Test File Template
#' 
#' Creates template files for SPSS validation setup
#' 
#' @param function_name Function to create template for
#' @param analysis_types Types of analyses to include
#' @export
generate_spss_test_template <- function(function_name, analysis_types = c("basic", "weighted", "grouped")) {
  
  template_lines <- c(
    paste("#", toupper(function_name), "SPSS VALIDATION TESTS"),
    "# Generated template for SPSS compatibility testing",
    "",
    "# Load SPSS validation functions",
    "source('tests/spss_reference/helpers/validation_utils.R')",
    "",
    "# Load test data",
    "data(survey_data)",
    ""
  )
  
  for (analysis_type in analysis_types) {
    
    template_lines <- c(template_lines,
      paste("test_that(\"", function_name, " SPSS validation -", analysis_type, "\", {"),
      paste("  skip_if_no_spss_reference('", function_name, "', '", analysis_type, "')", sep = ""),
      "  ",
      "  # Load validation functions",
      "  load_spss_validation()",
      "  ",
      paste("  # Execute", function_name, "function"),
      "  # TODO: Add specific function call here",
      paste("  # r_result <-", function_name, "(survey_data, ...)"),
      "  ",
      "  # Get SPSS reference",
      paste("  spss_file <- get_spss_reference_path('", function_name, "', '", analysis_type, "')", sep = ""),
      "  ",
      "  # Validate against SPSS",
      paste("  # validation <- validate_", function_name, "_against_spss(r_result, spss_file)", sep = ""),
      "  ",
      "  # Check results",
      "  # expect_true(validation$passed)",
      "  # expect_gte(validation$summary$pass_rate, 0.95)",
      "})",
      ""
    )
  }
  
  # Create output file
  output_file <- paste0("tests/testthat/test-", function_name, "-spss-validation.R")
  writeLines(template_lines, output_file)
  
  cat("SPSS validation template created:", output_file, "\n")
  cat("Please customize the function calls and validation logic.\n")
}

#' Print SPSS Validation Status
#' 
#' Shows overview of available SPSS references
#' 
#' @param functions Vector of function names to check
#' @export
print_spss_validation_status <- function(functions = c("t_test", "describe", "levene_test")) {
  
  cat("SPSS VALIDATION STATUS\n")
  cat("======================\n\n")
  
  analysis_types <- c("basic", "weighted", "grouped")
  
  for (func in functions) {
    cat("Function:", func, "\n")
    
    availability <- check_spss_references(func, analysis_types)
    
    for (analysis_type in analysis_types) {
      status <- if (availability[analysis_type]) "✓ Available" else "✗ Missing"
      cat("  ", analysis_type, ":", status, "\n")
    }
    
    cat("\n")
  }
  
  # Summary
  total_possible <- length(functions) * length(analysis_types)
  total_available <- sum(sapply(functions, function(f) sum(check_spss_references(f, analysis_types))))
  
  cat("SUMMARY:\n")
  cat("Available references:", total_available, "/", total_possible, "\n")
  cat("Coverage:", sprintf("%.1f%%", total_available / total_possible * 100), "\n")
}