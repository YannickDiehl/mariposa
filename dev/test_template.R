# Statistical Test Template - Complete Framework Design Guide
# ==============================================================
# This template documents all design patterns, structures, and conventions
# used in the statistical testing framework. Use this as reference for
# creating new tests and ensuring consistency across all functions.
#
# VERSION: 2.0 (includes all latest design patterns)
# AUTHOR: Framework Development Team
# UPDATED: 2024

library(tidyverse)
library(rlang)
library(tidyselect)

# ==========================================
# DESIGN PRINCIPLES & CONVENTIONS
# ==========================================

# 1. NAMING CONVENTIONS
# ---------------------
# - Function names: snake_case (e.g., t_test, mann_whitney_test, levene_test)
# - S3 classes: function_name + "_results" (e.g., "t_test_results")
# - Method dispatch: function_name.class_name (e.g., levene_test.t_test_results)
# - Internal helpers: .function_name (private functions with dot prefix)

# 2. FUNCTION STRUCTURE HIERARCHY
# --------------------------------
# All statistical test functions follow this structure:
# 
# main_function()
# ├── main_function.data.frame()      # Primary method
# ├── main_function.grouped_df()      # Grouped data method  
# ├── main_function.other_results()   # S3 method for other test results
# └── print.main_function_results()   # Print method

# 3. PARAMETER CONVENTIONS
# -------------------------
# Standard parameters (in this order):
# @param data          Data frame or vector
# @param ...           Variables (tidyselect expressions)
# @param group         Grouping variable (for between-group tests)
# @param weights       Weights variable
# @param paired        Logical for paired tests (default: FALSE)
# @param var.equal     Logical for equal variances (default: FALSE - Welch)
# @param alternative   "two.sided", "less", "greater" (default: "two.sided")
# @param conf.level    Confidence level (default: 0.95)
# @param mu            Null hypothesis value (default: 0)
# @param [test-specific parameters]

# ==========================================
# OUTPUT FORMATTING STANDARDS
# ==========================================

# 1. HEADER STRUCTURE
# -------------------
# Pattern: "[Weighted] Test Name for [Context]"
# Examples:
# - "t-test Results:"
# - "Weighted t-test Results:"
# - "Levene's Test for Homogeneity of Variance"
# - "Weighted ANOVA Table:"

# 2. GROUP DISPLAY PATTERN
# -------------------------
# For grouped data:
# Group: variable_name = value
# [NO underline - consistent with w_* functions]
# 
# Example:
# Group: eastwest = West
# 
# ┌─ variable_name ─┐

# 3. VARIABLE BOX PATTERN
# -----------------------
# Unicode box around variable names:
# ┌─ variable_name ─┐
# [blank line]
# [content]

# 4. TABLE FORMATTING
# --------------------
# Professional tables with dynamic borders:
# [Test Type] Results:
# ----------------------------------------------------------------
#  Column1 Column2 Column3 Column4
#      val     val     val     val
# ----------------------------------------------------------------

# 5. SIGNIFICANCE INDICATORS
# ---------------------------
# Standard pattern:
# sig <- cut(p_value, 
#           breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
#           labels = c("***", "**", "*", ""),
#           right = FALSE)

# ==========================================
# S3 OBJECT STRUCTURE STANDARDS
# ==========================================

# STANDARD RESULT OBJECT STRUCTURE:
# result <- list(
#   results = results_df,           # Main results data frame
#   method = "Test Name",           # Test description
#   variables = c("var1", "var2"),  # Variable names
#   group = "group_var",            # Grouping variable (if any)
#   group_var = "group_var",        # Alternative naming
#   weights = "weight_var",         # Weights variable (if any)
#   weight_var = "weight_var",      # Alternative naming
#   paired = FALSE,                 # For paired tests
#   var.equal = FALSE,              # For variance assumptions
#   alternative = "two.sided",      # Test direction
#   conf.level = 0.95,             # Confidence level
#   mu = 0,                        # Null hypothesis value
#   grouped = TRUE/FALSE,          # Is this grouped data?
#   is_grouped = TRUE/FALSE,       # Alternative naming
#   groups = c("group1", "group2"), # Group variable names
#   group_vars = c("g1", "g2"),    # Alternative naming
#   data = original_data,          # Original data (for S3 methods)
#   original_test = test_object    # Reference to original test (for S3 methods)
# )
# class(result) <- "function_name_results"

# ==========================================
# S3 GENERIC FUNCTIONS - DEVELOPMENT GUIDELINES
# ==========================================

# CRITICAL RULE: ALWAYS CREATE SEPARATE FILES FOR S3 GENERICS!
# ============================================================
# 
# When developing new S3 generic functions, follow this process:
# 
# 1. CREATE SEPARATE FILE FOR EACH S3 GENERIC
#    - File naming: "function_name.R" (e.g., "mauchly_test.R", "emmeans.R")
#    - Never add S3 generics to existing main function files
#    - Keep main functions (e.g., oneway_anova_test.R) focused on core functionality
# 
# 2. S3 GENERIC FILE STRUCTURE:
#    
#    function_name.R should contain:
#    ├── Comprehensive roxygen2 documentation
#    ├── Generic function declaration
#    ├── S3 method implementations  
#    ├── Print method for results
#    └── Additional helper functions (if needed)
# 
# 3. MANDATORY SECTIONS IN S3 GENERIC FILES:
#    
#    a) ROXYGEN DOCUMENTATION:
#       - @description with clear purpose
#       - @param for all parameters including ...
#       - @details with mathematical/statistical background
#       - @return describing result object structure
#       - @examples with realistic usage scenarios
#       - @export for generic and methods
#    
#    b) GENERIC FUNCTION:
#       generic_name <- function(x, ...) {
#         UseMethod("generic_name")
#       }
#    
#    c) S3 METHOD(S):
#       generic_name.class_name <- function(x, specific_params, ...) {
#         # Implementation
#       }
#    
#    d) PRINT METHOD:
#       print.generic_name_results <- function(x, digits = 3, ...) {
#         # Following template formatting standards
#       }

# ==========================================
# S3 GENERIC DESIGN PATTERNS
# ==========================================

# EXTENSIBLE S3 GENERICS FOR MULTIPLE STATISTICAL TESTS
# ======================================================
# 
# Both mauchly_test() and emmeans() demonstrate extensible design patterns
# that can be adapted for other statistical procedures:

# 1. MAUCHLY_TEST PATTERN (Assumption Testing)
# ---------------------------------------------
# Purpose: Test statistical assumptions for various models
# 
# Extensible to:
# - sphericity_test()       # For repeated measures (alias for mauchly_test)
# - normality_test()        # Shapiro-Wilk, Anderson-Darling for various models
# - homogeneity_test()      # Levene, Brown-Forsythe for various models  
# - linearity_test()        # For regression models
# - independence_test()     # Durbin-Watson for time series models
# 
# Design Pattern:
# assumption_test <- function(x, ...) UseMethod("assumption_test")
# assumption_test.anova_results <- function(x, ...) { }
# assumption_test.regression_results <- function(x, ...) { }
# assumption_test.timeseries_results <- function(x, ...) { }

# 2. EMMEANS PATTERN (Post-hoc Analysis)
# ---------------------------------------
# Purpose: Calculate adjusted means, contrasts, and effect sizes
# 
# Extensible to:
# - contrasts()             # Planned contrasts for ANOVA models
# - simple_effects()        # Simple effects analysis for interactions
# - effect_sizes()          # Cohen's d, eta-squared, omega-squared
# - marginal_effects()      # For regression models
# - pairwise_comparisons()  # Tukey, Bonferroni, etc.
# 
# Design Pattern:
# posthoc_analysis <- function(x, effect = "all", ...) UseMethod("posthoc_analysis")
# posthoc_analysis.anova_results <- function(x, effect, ...) { }
# posthoc_analysis.regression_results <- function(x, effect, ...) { }
# posthoc_analysis.glm_results <- function(x, effect, ...) { }

# ==========================================
# IMPORT STRUCTURE FOR S3 GENERICS
# ==========================================

# LOADING S3 GENERIC FILES:
# =========================
# 
# Always update import scripts when creating new S3 generics:
# 
# In testscript.R and example files:
# source("main_function.R")         # Core functionality
# source("s3_generic1.R")           # S3 generic functions
# source("s3_generic2.R")           # Additional S3 generics
# 
# Example:
# source("oneway_anova_test.R")     # Core RM-ANOVA
# source("mauchly_test.R")          # Sphericity testing
# source("emmeans.R")               # Marginal means
# source("contrasts.R")             # Planned contrasts (future)
# source("effect_sizes.R")          # Effect size calculations (future)

# ==========================================
# S3 METHOD COMPATIBILITY MATRIX
# ==========================================

# CROSS-TEST S3 METHOD SUPPORT:
# =============================
# 
# Design S3 generics to work across multiple base tests:
# 
#                     │ t_test │ anova │ rm_anova │ regression │ glm │
# ────────────────────┼────────┼───────┼──────────┼────────────┼─────┤
# levene_test()       │   ✓    │   ✓   │    ✓     │     ✓      │  ✓  │
# mauchly_test()      │   ✗    │   ✗   │    ✓     │     ✗      │  ✗  │
# emmeans()           │   ✗    │   ✓   │    ✓     │     ✓      │  ✓  │
# tukey_test()        │   ✗    │   ✓   │    ✓     │     ✗      │  ✗  │
# effect_sizes()      │   ✓    │   ✓   │    ✓     │     ✓      │  ✓  │
# contrasts()         │   ✗    │   ✓   │    ✓     │     ✓      │  ✓  │
# 
# ✓ = Should be implemented
# ✗ = Not applicable or not meaningful

# ==========================================
# ADVANCED S3 GENERIC EXAMPLES
# ==========================================

# EXAMPLE 1: NORMALITY TESTING S3 GENERIC
# ========================================
# File: normality_test.R
# 
# normality_test <- function(x, method = "shapiro", ...) {
#   UseMethod("normality_test")  
# }
# 
# normality_test.t_test_results <- function(x, method = "shapiro", ...) {
#   # Test residuals from t-test model
# }
# 
# normality_test.oneway_anova_test_results <- function(x, method = "shapiro", ...) {
#   # Test residuals from ANOVA model  
# }
# 
# normality_test.lm <- function(x, method = "shapiro", ...) {
#   # Test residuals from regression model
# }

# EXAMPLE 2: EFFECT SIZES S3 GENERIC  
# ===================================
# File: effect_sizes.R
# 
# effect_sizes <- function(x, type = "all", ...) {
#   UseMethod("effect_sizes")
# }
# 
# effect_sizes.t_test_results <- function(x, type = "all", ...) {
#   # Calculate Cohen's d, Glass's delta, etc.
# }
# 
# effect_sizes.oneway_anova_test_results <- function(x, type = "all", ...) {
#   # Calculate eta-squared, omega-squared, epsilon-squared
# }
# 
# effect_sizes.glm <- function(x, type = "all", ...) {
#   # Calculate R-squared, pseudo R-squared, etc.
# }

# ==========================================
# VALIDATION AND TESTING FOR S3 GENERICS
# ==========================================

# S3 GENERIC TESTING CHECKLIST:
# =============================
# 
# For each new S3 generic function, verify:
# 
# 1. ✓ Separate file created (function_name.R)
# 2. ✓ Complete roxygen2 documentation
# 3. ✓ Generic function with UseMethod()
# 4. ✓ At least one S3 method implementation
# 5. ✓ Print method following template standards
# 6. ✓ Error handling for unsupported classes
# 7. ✓ Parameter validation and type checking
# 8. ✓ Result object follows standard structure
# 9. ✓ Compatible with pipeline workflow (result %>% function())
# 10. ✓ Updated import statements in test scripts
# 11. ✓ Functionality testing with real data
# 12. ✓ Cross-validation with other statistical software (SPSS/R)

# INTEGRATION TESTING:
# ===================
# 
# Test S3 generics with various base test combinations:
# 
# # Basic workflow
# result <- data %>% base_test(variables, group = group)
# result %>% s3_generic()
# 
# # Grouped workflow  
# data %>% group_by(stratifier) %>% base_test(variables, group = group) %>% s3_generic()
# 
# # Weighted workflow
# data %>% base_test(variables, group = group, weights = weights) %>% s3_generic()
# 
# # Multi-variable workflow
# data %>% base_test(var1, var2, var3, group = group) %>% s3_generic()

# ==========================================
# FUTURE S3 GENERIC DEVELOPMENT ROADMAP
# ==========================================

# PLANNED S3 GENERICS (Priority Order):
# =====================================
# 
# 1. HIGH PRIORITY:
#    - effect_sizes.R      # Comprehensive effect size calculations
#    - contrasts.R         # Planned contrasts for factorial designs
#    - assumptions.R       # Unified assumption testing interface
# 
# 2. MEDIUM PRIORITY:  
#    - bootstrap.R         # Bootstrap confidence intervals
#    - power_analysis.R    # Post-hoc and prospective power analysis
#    - model_diagnostics.R # Residual analysis and model fit
# 
# 3. LOW PRIORITY:
#    - bayesian_factors.R  # Bayesian model comparison
#    - meta_analysis.R     # Meta-analytic methods
#    - machine_learning.R  # ML-based statistical inference

# BACKWARDS COMPATIBILITY PROMISE:
# ================================
# 
# All S3 generics must maintain backwards compatibility:
# - Result object structures remain stable
# - Parameter names and defaults unchanged
# - Method signatures preserved across versions
# - Deprecation warnings before removing features

# ==========================================
# RESULTS DATA FRAME STANDARDS
# ==========================================

# STANDARD COLUMNS (adapt as needed):
# - Variable: Variable name
# - [Test_Statistic]: t_stat, F_statistic, U_statistic, etc.
# - df: Degrees of freedom (df1, df2 for F-tests)
# - p_value: P-value
# - sig: Significance stars (*, **, ***)
# - [Effect_Size]: cohens_d, eta_squared, etc.
# - [Group_Info]: Group column or separate group columns
# - [Descriptives]: means, n, std_dev, etc.

# GROUPED DATA HANDLING:
# Two approaches depending on grouping method:
# 1. Direct grouping: Group column with "var=value" format
# 2. S3 method grouping: Separate columns for each group variable

# ==========================================
# PRINT METHOD STANDARDS
# ==========================================

#' Standard Print Method Template
#' @param x Test results object
#' @param digits Number of decimal places (default: 3)
#' @param ... Additional arguments
print.test_results <- function(x, digits = 3, ...) {
  
  # 1. DETERMINE TEST TYPE
  test_type <- if (!is.null(x$weight_var) || !is.null(x$weights)) {
    "Weighted [Test Name]"
  } else {
    "[Test Name]"
  }
  
  # 2. PRINT HEADER
  cat(sprintf("\n%s\n", test_type))
  cat(paste(rep("-", nchar(test_type)), collapse = ""), "\n")
  
  # 3. PRINT TEST INFORMATION
  cat("\n")
  if (!is.null(x$group_var) || !is.null(x$group)) {
    group_name <- if (!is.null(x$group_var)) x$group_var else x$group
    cat(sprintf("Grouping variable: %s\n", group_name))
  }
  if (!is.null(x$weight_var) || !is.null(x$weights)) {
    weight_name <- if (!is.null(x$weight_var)) x$weight_var else x$weights
    cat(sprintf("Weights variable: %s\n", weight_name))
  }
  # Add other test-specific information
  cat("\n")
  
  # 4. ADD SIGNIFICANCE STARS
  x$results$sig <- cut(x$results$p_value, 
                      breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                      labels = c("***", "**", "*", ""),
                      right = FALSE)
  
  # 5. HANDLE GROUPED VS UNGROUPED DATA
  is_grouped_data <- (!is.null(x$grouped) && x$grouped) || 
                     (!is.null(x$is_grouped) && x$is_grouped)
  
  if (is_grouped_data) {
    # GROUPED DATA DISPLAY
    for (i in seq_len(nrow(x$results))) {
      group_row <- x$results[i, ]
      
      # Extract group information (handle both formats)
      if ("Group" %in% names(x$results)) {
        # Direct grouped analysis: Group column like "eastwest=East"
        group_label <- gsub("=", " = ", group_row$Group)
      } else if (!is.null(x$groups) && length(x$groups) > 0) {
        # S3 method grouped analysis: separate columns
        group_parts <- sapply(x$groups, function(group_var) {
          group_value <- group_row[[group_var]]
          paste(group_var, "=", group_value)
        })
        group_label <- paste(group_parts, collapse = ", ")
      } else {
        # Fallback: check for non-standard columns
        non_standard_cols <- setdiff(names(x$results), 
                                   c("Variable", "t_stat", "F_statistic", "U_statistic",
                                     "df", "df1", "df2", "p_value", "sig", "conclusion"))
        if (length(non_standard_cols) > 0) {
          group_var <- non_standard_cols[1]
          group_value <- group_row[[group_var]]
          group_label <- paste(group_var, "=", group_value)
        } else {
          group_label <- "Unknown Group"
        }
      }
      
      # Display group
      cat(sprintf("\nGroup: %s\n", group_label))
      
      # Variable box
      var_name <- group_row$Variable
      cat(sprintf("\n┌─ %s ─┐\n", var_name))
      cat("\n")  # Blank line after variable name
      
      # Create and print results table for this group
      # [Create results_df specific to test type]
      
      # Print table with border
      header_text <- sprintf("%s:", ifelse(!is.null(x$weight_var) || !is.null(x$weights), 
                                          paste("Weighted", test_type, "Results"), 
                                          paste(test_type, "Results")))
      cat(header_text, "\n")
      
      # Dynamic border width calculation
      col_widths <- sapply(names(results_df), function(col) {
        max(nchar(as.character(results_df[[col]])), nchar(col), na.rm = TRUE)
      })
      total_width <- sum(col_widths) + length(col_widths) - 1
      border_width <- paste(rep("-", max(total_width, 50)), collapse = "")
      
      cat(border_width, "\n")
      print(results_df, row.names = FALSE)
      cat(border_width, "\n\n")
    }
    
  } else {
    # UNGROUPED DATA DISPLAY
    valid_results <- x$results[!is.na(x$results$Variable), ]
    
    for (i in seq_len(nrow(valid_results))) {
      var_name <- valid_results$Variable[i]
      
      # Variable box
      cat(sprintf("\n┌─ %s ─┐\n", var_name))
      cat("\n")  # Blank line after variable name
      
      # Create and print results table for this variable
      # [Create results_df specific to test type]
      
      # Print table with border
      header_text <- sprintf("%s:", ifelse(!is.null(x$weight_var) || !is.null(x$weights), 
                                          paste("Weighted", test_type, "Results"), 
                                          paste(test_type, "Results")))
      cat(header_text, "\n")
      
      # Dynamic border width
      col_widths <- sapply(names(results_df), function(col) {
        max(nchar(as.character(results_df[[col]])), nchar(col), na.rm = TRUE)
      })
      total_width <- sum(col_widths) + length(col_widths) - 1
      border_width <- paste(rep("-", max(total_width, 50)), collapse = "")
      
      cat(border_width, "\n")
      print(results_df, row.names = FALSE)
      cat(border_width, "\n")
      
      if (i < nrow(valid_results)) {
        cat("\n")  # Spacing between variables
      }
    }
  }
  
  # 6. STANDARD FOOTER
  cat("\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05\n")
  
  # 7. TEST-SPECIFIC INTERPRETATION
  cat("\nInterpretation:\n")
  # Add test-specific interpretation guidelines
  
  # 8. RECOMMENDATIONS (if applicable)
  if (!is.null(x$original_test) || is_grouped_data) {
    cat("\nRecommendation based on [test name]:\n")
    # Add test-specific recommendations
  }
  
  invisible(x)
}

# ==========================================
# TESTING & VALIDATION PATTERNS
# ==========================================

# STANDARD TEST CASES FOR ANY NEW FUNCTION:
test_function_comprehensive <- function() {
  
  # Create test data
  set.seed(123)
  test_data <- data.frame(
    var1 = rnorm(100, 50, 10),
    var2 = rnorm(100, 30, 5),
    group = sample(c("A", "B"), 100, replace = TRUE),
    weights = runif(100, 0.5, 2.0),
    eastwest = sample(c("East", "West"), 100, replace = TRUE)
  )
  
  # Test Case 1: Basic unweighted
  cat("=== Test Case 1: Basic Unweighted ===\n")
  result1 <- test_data %>% test_function(var1, group = group)
  print(result1)
  
  # Test Case 2: Basic weighted
  cat("\n=== Test Case 2: Basic Weighted ===\n")
  result2 <- test_data %>% test_function(var1, group = group, weights = weights)
  print(result2)
  
  # Test Case 3: Grouped unweighted
  cat("\n=== Test Case 3: Grouped Unweighted ===\n")
  result3 <- test_data %>% 
    group_by(eastwest) %>% 
    test_function(var1, group = group)
  print(result3)
  
  # Test Case 4: Grouped weighted
  cat("\n=== Test Case 4: Grouped Weighted ===\n")
  result4 <- test_data %>% 
    group_by(eastwest) %>% 
    test_function(var1, group = group, weights = weights)
  print(result4)
  
  # Test Case 5: Multiple variables
  cat("\n=== Test Case 5: Multiple Variables ===\n")
  result5 <- test_data %>% test_function(var1, var2, group = group)
  print(result5)
  
  # Test Case 6: S3 method chaining (if applicable)
  cat("\n=== Test Case 6: S3 Method Chaining ===\n")
  if (exists("other_test")) {
    other_result <- test_data %>% 
      group_by(eastwest) %>% 
      other_test(var1, group = group)
    result6 <- other_result %>% test_function()
    print(result6)
  }
}

# ==========================================
# FRAMEWORK VALIDATION FUNCTIONS
# ==========================================

#' Validate Framework Design Consistency
#' 
#' This function checks existing statistical test functions against the
#' framework design standards documented in this template.
#' 
#' @param function_files Character vector of R files to check
#' @param verbose Logical; print detailed output (default: TRUE)
#' @return List with validation results
validate_framework_design <- function(function_files = NULL, verbose = TRUE) {
  
  if (is.null(function_files)) {
    function_files <- c("t_test.R", "mann_whitney_test.R", "levene_test.R", 
                       "oneway_anova_test.R", "tukey_test.R")
  }
  
  results <- list()
  
  for (file in function_files) {
    if (!file.exists(file)) {
      if (verbose) cat("File not found:", file, "\n")
      next
    }
    
    if (verbose) cat("\n=== Validating", file, "===\n")
    
    file_content <- readLines(file, warn = FALSE)
    results[[file]] <- validate_single_function(file_content, verbose)
  }
  
  # Summary report
  if (verbose) {
         cat("\n", paste(rep("=", 60), collapse = ""), "\n")
    cat("FRAMEWORK VALIDATION SUMMARY\n")
         cat(paste(rep("=", 60), collapse = ""), "\n")
    
    for (file in names(results)) {
      cat("\n", file, ":\n")
      validation <- results[[file]]
      
      passed <- sum(sapply(validation, function(x) x$status == "PASS"))
      total <- length(validation)
      
      cat(sprintf("  Checks passed: %d/%d\n", passed, total))
      
      # Show failures
      failures <- validation[sapply(validation, function(x) x$status == "FAIL")]
      if (length(failures) > 0) {
        cat("  Issues found:\n")
        for (failure in failures) {
          cat(sprintf("    - %s: %s\n", failure$check, failure$message))
        }
      }
    }
  }
  
  invisible(results)
}

#' Validate Single Function File
#' 
#' @param file_content Character vector of file lines
#' @param verbose Logical; print detailed output
#' @return List of validation results
validate_single_function <- function(file_content, verbose = TRUE) {
  
  results <- list()
  
  # Check 1: Print method exists
  results$print_method <- check_print_method_exists(file_content, verbose)
  
  # Check 2: Group display pattern
  results$group_display <- check_group_display_pattern(file_content, verbose)
  
  # Check 3: Variable box pattern
  results$variable_box <- check_variable_box_pattern(file_content, verbose)
  
  # Check 4: Table headers
  results$table_headers <- check_table_headers(file_content, verbose)
  
  # Check 5: Significance indicators
  results$significance <- check_significance_pattern(file_content, verbose)
  
  # Check 6: Grouped data handling
  results$grouped_handling <- check_grouped_data_handling(file_content, verbose)
  
  # Check 7: Weight handling
  results$weight_handling <- check_weight_handling(file_content, verbose)
  
  return(results)
}

# Validation helper functions
check_print_method_exists <- function(file_content, verbose) {
  print_method_pattern <- "print\\.[a-z_]+_results\\s*<-\\s*function"
  has_print_method <- any(grepl(print_method_pattern, file_content))
  
  result <- list(
    check = "Print method exists",
    status = ifelse(has_print_method, "PASS", "FAIL"),
    message = ifelse(has_print_method, "Print method found", "No print method found")
  )
  
  if (verbose) cat("  ✓ Print method:", result$status, "\n")
  return(result)
}

check_group_display_pattern <- function(file_content, verbose) {
  # Look for Group: pattern without underline
  group_pattern <- 'sprintf\\("\\\\nGroup: %s\\\\n"'
  underline_pattern <- 'paste\\(rep\\("=".*collapse'
  
  has_group_pattern <- any(grepl(group_pattern, file_content))
  has_underline <- any(grepl(underline_pattern, file_content))
  
  result <- list(
    check = "Group display pattern",
    status = ifelse(has_group_pattern && !has_underline, "PASS", 
                   ifelse(has_group_pattern, "WARN", "FAIL")),
    message = ifelse(has_group_pattern && !has_underline, "Correct group pattern",
                    ifelse(has_group_pattern, "Group pattern found but may have underline",
                          "No group pattern found"))
  )
  
  if (verbose) cat("  ✓ Group pattern:", result$status, "\n")
  return(result)
}

check_variable_box_pattern <- function(file_content, verbose) {
  box_pattern <- "┌─.*─┐"
  has_box_pattern <- any(grepl(box_pattern, file_content))
  
  result <- list(
    check = "Variable box pattern",
    status = ifelse(has_box_pattern, "PASS", "FAIL"),
    message = ifelse(has_box_pattern, "Unicode box pattern found", "No variable box pattern")
  )
  
  if (verbose) cat("  ✓ Variable box:", result$status, "\n")
  return(result)
}

check_table_headers <- function(file_content, verbose) {
  # Look for weighted/unweighted header logic
  header_pattern <- 'ifelse.*Weighted.*Results'
  has_header_logic <- any(grepl(header_pattern, file_content))
  
  result <- list(
    check = "Table headers",
    status = ifelse(has_header_logic, "PASS", "WARN"),
    message = ifelse(has_header_logic, "Header logic found", "Check header implementation")
  )
  
  if (verbose) cat("  ✓ Table headers:", result$status, "\n")
  return(result)
}

check_significance_pattern <- function(file_content, verbose) {
  sig_pattern <- 'cut\\(.*p_value.*breaks.*c\\(-Inf, 0\\.001, 0\\.01, 0\\.05, Inf\\)'
  has_sig_pattern <- any(grepl(sig_pattern, file_content))
  
  result <- list(
    check = "Significance indicators",
    status = ifelse(has_sig_pattern, "PASS", "WARN"),
    message = ifelse(has_sig_pattern, "Standard significance pattern found", "Check significance implementation")
  )
  
  if (verbose) cat("  ✓ Significance:", result$status, "\n")
  return(result)
}

check_grouped_data_handling <- function(file_content, verbose) {
  # Look for both x$grouped and x$is_grouped
  grouped_pattern1 <- 'x\\$grouped'
  grouped_pattern2 <- 'x\\$is_grouped'
  
  has_grouped1 <- any(grepl(grouped_pattern1, file_content))
  has_grouped2 <- any(grepl(grouped_pattern2, file_content))
  
  result <- list(
    check = "Grouped data handling",
    status = ifelse(has_grouped1 && has_grouped2, "PASS", 
                   ifelse(has_grouped1 || has_grouped2, "WARN", "FAIL")),
    message = ifelse(has_grouped1 && has_grouped2, "Both grouped patterns found",
                    ifelse(has_grouped1 || has_grouped2, "Only one grouped pattern found",
                          "No grouped data handling found"))
  )
  
  if (verbose) cat("  ✓ Grouped handling:", result$status, "\n")
  return(result)
}

check_weight_handling <- function(file_content, verbose) {
  # Look for both x$weights and x$weight_var
  weight_pattern1 <- 'x\\$weights'
  weight_pattern2 <- 'x\\$weight_var'
  
  has_weight1 <- any(grepl(weight_pattern1, file_content))
  has_weight2 <- any(grepl(weight_pattern2, file_content))
  
  result <- list(
    check = "Weight handling",
    status = ifelse(has_weight1 && has_weight2, "PASS", 
                   ifelse(has_weight1 || has_weight2, "WARN", "FAIL")),
    message = ifelse(has_weight1 && has_weight2, "Both weight patterns found",
                    ifelse(has_weight1 || has_weight2, "Only one weight pattern found",
                          "No weight handling found"))
  )
  
  if (verbose) cat("  ✓ Weight handling:", result$status, "\n")
  return(result)
}

# ==========================================
# QUICK FRAMEWORK TEST
# ==========================================

#' Quick test of all major statistical functions
#' 
#' This function runs a standardized test across all major statistical
#' test functions to ensure they work consistently.
quick_framework_test <- function() {
  
  # Load required functions
  source("t_test.R")
  source("mann_whitney_test.R") 
  source("levene_test.R")
  source("oneway_anova_test.R")
  
  # Create standard test data
  set.seed(42)
  test_data <- data.frame(
    score = c(rnorm(50, 100, 15), rnorm(50, 105, 18)),
    group = rep(c("Control", "Treatment"), each = 50),
    region = rep(c("North", "South"), 50),
    weights = runif(100, 0.8, 1.5)
  )
  
  cat("=== QUICK FRAMEWORK TEST ===\n\n")
  
  # Test t-test
  cat("1. T-TEST:\n")
  cat("----------\n")
  try({
    result_t <- test_data %>% 
      group_by(region) %>% 
      t_test(score, group = group, weights = weights)
    print(result_t)
    cat("✓ t-test: PASS\n\n")
  }, silent = FALSE)
  
  # Test ANOVA
  cat("2. ANOVA:\n")
  cat("---------\n")
  try({
    result_anova <- test_data %>% 
      group_by(region) %>% 
      oneway_anova_test(score, group = group, weights = weights)
    print(result_anova)
    cat("✓ ANOVA: PASS\n\n")
  }, silent = FALSE)
  
  # Test Levene
  cat("3. LEVENE TEST:\n")
  cat("---------------\n")
  try({
    result_levene <- result_t %>% levene_test()
    print(result_levene)
    cat("✓ Levene: PASS\n\n")
  }, silent = FALSE)
  
  # Test Mann-Whitney
  cat("4. MANN-WHITNEY:\n")
  cat("----------------\n")
  try({
    result_mw <- test_data %>% 
      group_by(region) %>% 
      mann_whitney_test(score, group = group, weights = weights)
    print(result_mw)
    cat("✓ Mann-Whitney: PASS\n\n")
  }, silent = FALSE)
  
  cat("=== FRAMEWORK TEST COMPLETE ===\n")
}

# ==========================================
# DOCUMENTATION STANDARDS
# ==========================================

# ROXYGEN TEMPLATE:
#' Test Name
#'
#' Perform [test description] with support for weighted data, grouped analysis,
#' and multiple variables. Provides SPSS-compatible results with professional formatting.
#'
#' @param data A data frame
#' @param ... Variables to test (unquoted names or tidyselect expressions)
#' @param group Grouping variable for between-group comparisons (unquoted)
#' @param weights Weights variable (unquoted) or numeric vector
#' @param paired Logical; TRUE for paired test (default: FALSE)
#' @param var.equal Logical; TRUE assumes equal variances, FALSE uses Welch correction (default: FALSE)
#' @param alternative Character; "two.sided" (default), "less", or "greater"
#' @param conf.level Numeric; confidence level (default: 0.95)
#' @param mu Numeric; null hypothesis value (default: 0)
#'
#' @return A test_results object containing:
#'   \item{results}{Data frame with test statistics, p-values, and effect sizes}
#'   \item{method}{Test description}
#'   \item{variables}{Variable names tested}
#'   \item{group}{Grouping variable name}
#'   \item{weights}{Weights variable name}
#'   \item{grouped}{Logical indicating if data was grouped}
#'   \item{[additional fields specific to test]}
#'
#' @details
#' This function provides SPSS-compatible results for [test type]. Key features:
#' 
#' \strong{Weighted Analysis:}
#' - Uses probability weights for proper statistical inference
#' - Adjusts degrees of freedom for complex survey designs
#' - Provides effective sample sizes
#' 
#' \strong{Multiple Variables:}
#' - Tests multiple dependent variables simultaneously
#' - Maintains consistent formatting across variables
#' 
#' \strong{Grouped Analysis:}
#' - Performs separate tests for each group
#' - Groups defined by group_by() or explicit grouping variables
#' 
#' \strong{Output Format:}
#' - Professional tables with dynamic borders
#' - Variable names in decorative Unicode boxes
#' - Automatic weighted/unweighted header detection
#' - SPSS-style significance indicators (*, **, ***)
#'
#' @examples
#' # Basic test
#' data %>% test_function(variable, group = group_var)
#' 
#' # Weighted test
#' data %>% test_function(variable, group = group_var, weights = weight_var)
#' 
#' # Grouped analysis
#' data %>% group_by(region) %>% test_function(variable, group = group_var)
#' 
#' # Multiple variables
#' data %>% test_function(var1, var2, var3, group = group_var)
#'
#' @seealso \code{\link{related_function1}}, \code{\link{related_function2}}
#' @export

# ==========================================
# ERROR HANDLING STANDARDS
# ==========================================

# STANDARD ERROR MESSAGES:
# - "Grouping variable must be specified for [test name]"
# - "[Test] requires at least 2 groups, found %d"
# - "Original data not available in test results. Please use: function_name(data, variables, group = group)"
# - "[Test] is not appropriate for [other test] results.\n[Explanation of why not appropriate]"
# - "Insufficient data for [test name]: need at least %d observations per group"

# VALIDATION PATTERNS:
# 1. Check for required parameters
# 2. Validate data types and structure
# 3. Check for sufficient data (group sizes, etc.)
# 4. Validate weight vectors if provided
# 5. Check for factor/character grouping variables

# ==========================================
# COMPATIBILITY NOTES
# ==========================================

# SPSS COMPATIBILITY:
# - Use Welch's t-test as default (var.equal = FALSE)
# - Implement exact SPSS formulas for weighted statistics
# - Match SPSS significance levels and formatting
# - Use SPSS-style degrees of freedom calculations

# TIDYVERSE COMPATIBILITY:
# - Support tidyselect expressions (starts_with(), contains(), etc.)
# - Work seamlessly with group_by() and summarise()
# - Handle grouped_df objects appropriately
# - Follow tidy data principles

# BACKWARD COMPATIBILITY:
# - Maintain consistent parameter names across functions
# - Preserve existing S3 method signatures
# - Keep result object structures stable

# ==========================================
# PERFORMANCE CONSIDERATIONS
# ==========================================

# OPTIMIZATION PATTERNS:
# 1. Pre-calculate common values (avoid repeated calculations)
# 2. Use vectorized operations where possible
# 3. Minimize data copying in grouped operations
# 4. Cache expensive computations when appropriate
# 5. Use efficient algorithms for weighted calculations

# MEMORY MANAGEMENT:
# 1. Clean up large intermediate objects
# 2. Use appropriate data types (avoid unnecessary precision)
# 3. Stream processing for very large datasets
# 4. Efficient storage of results objects

# ==========================================
# VERSION CONTROL & UPDATES
# ==========================================

# CHANGELOG REQUIREMENTS:
# - Document all breaking changes
# - Note new features and parameters
# - Specify version compatibility
# - Include migration instructions for major updates

# DEPRECATION PROCESS:
# 1. Mark old functions/parameters as deprecated
# 2. Provide clear migration paths
# 3. Maintain backward compatibility for 2+ versions
# 4. Remove deprecated features with major version bumps

# ==========================================
# END OF TEMPLATE
# ==========================================

# This template should be consulted for:
# 1. Creating new statistical test functions
# 2. Updating existing functions for consistency
# 3. Debugging formatting or structural issues
# 4. Ensuring proper S3 method dispatch
# 5. Maintaining framework-wide design coherence 

# ==========================================
# PRACTICAL EXTENSION EXAMPLES
# ==========================================

# HOW TO EXTEND MAUCHLY_TEST TO OTHER TESTS:
# ==========================================

# Example 1: Extend mauchly_test to regular ANOVA
# ------------------------------------------------
# File: mauchly_test.R (add this method)
# 
# mauchly_test.t_test_results <- function(x, ...) {
#   stop("Mauchly's test is not applicable to t-test results.\nMauchly's test evaluates sphericity assumptions in repeated measures designs with 3+ time points.")
# }
# 
# mauchly_test.levene_test_results <- function(x, ...) {
#   # Chain to original test if available
#   if (!is.null(x$original_test)) {
#     return(mauchly_test(x$original_test))
#   } else {
#     stop("Original data not available in Levene test results. Please use: mauchly_test(original_anova_result)")
#   }
# }

# Example 2: Create sphericity_test as alias
# ------------------------------------------
# File: sphericity_test.R
# 
# #' Sphericity Test (Alias for Mauchly's Test)
# #' @rdname mauchly_test
# #' @export
# sphericity_test <- function(x, ...) {
#   mauchly_test(x, ...)
# }

# HOW TO EXTEND EMMEANS TO OTHER TESTS:
# =====================================

# Example 3: Extend emmeans to t-test results
# -------------------------------------------
# File: emmeans.R (add this method)
# 
# emmeans.t_test_results <- function(x, effect = "all", ...) {
#   
#   # For t-tests, only group means make sense
#   if (is.null(x$group) || is.null(x$group_var)) {
#     stop("emmeans() for t-test requires grouped comparison. Use: data %>% t_test(variable, group = group_var)")
#   }
#   
#   # Get descriptive statistics from t-test
#   if (is.null(x$descriptives)) {
#     stop("Descriptive statistics not available in t-test results. Please regenerate with current version.")
#   }
#   
#   # Calculate group means with confidence intervals
#   group_means <- x$descriptives %>%
#     mutate(
#       df = x$results$df[1],
#       t_val = qt(0.975, df = df),
#       ci_lower = mean - t_val * se,
#       ci_upper = mean + t_val * se
#     )
#   
#   result <- list(
#     group_means = group_means,
#     time_means = NULL,  # Not applicable for t-test
#     interaction_means = NULL,  # Not applicable for t-test
#     effect = "group",  # Only group effects available
#     original_test = x,
#     variables = x$variables,
#     group = x$group,
#     conf.level = x$conf.level %||% 0.95
#   )
#   
#   class(result) <- "emmeans_results"
#   return(result)
# }

# Example 4: Extend emmeans to regular ANOVA
# ------------------------------------------
# File: emmeans.R (add this method)
# 
# emmeans.oneway_anova_test_results <- function(x, effect = "all", ...) {
#   
#   # Check if this is repeated measures (already implemented)
#   if (!is.null(x$repeated) && x$repeated) {
#     # Use existing RM-ANOVA implementation
#     return(NextMethod())
#   }
#   
#   # For regular ANOVA, only group means available
#   if (is.null(x$group) || is.null(x$group_var)) {
#     stop("emmeans() requires grouping variable for ANOVA. Use: data %>% oneway_anova_test(variable, group = group_var)")
#   }
#   
#   # Calculate group means from ANOVA descriptives
#   group_means <- x$descriptives %>%
#     mutate(
#       df = x$results$df2[1],  # Error degrees of freedom
#       t_val = qt(0.975, df = df),
#       ci_lower = mean - t_val * se,
#       ci_upper = mean + t_val * se
#     )
#   
#   result <- list(
#     group_means = group_means,
#     time_means = NULL,  # Not applicable for between-subjects ANOVA
#     interaction_means = NULL,  # Only main effects in one-way ANOVA
#     effect = "group",
#     original_test = x,
#     variables = x$variables,
#     group = x$group,
#     conf.level = x$conf.level %||% 0.95
#   )
#   
#   class(result) <- "emmeans_results"
#   return(result)
# }

# ==========================================
# CREATING NEW S3 GENERICS FROM SCRATCH
# ==========================================

# EXAMPLE: EFFECT_SIZES S3 GENERIC
# =================================
# File: effect_sizes.R

# #' Effect Size Calculations
# #' 
# #' @description
# #' Calculates effect sizes for various statistical tests including Cohen's d,
# #' eta-squared, omega-squared, and other standardized effect measures.
# #' 
# #' @param x Statistical test results object
# #' @param type Character; effect size type(s) to calculate:
# #'   \itemize{
# #'     \item \code{"all"} (default): All applicable effect sizes
# #'     \item \code{"cohen"}: Cohen's d family (d, g, delta)
# #'     \item \code{"eta"}: Eta-squared family (eta, partial eta, omega)
# #'     \item \code{"r"}: Correlation-based effect sizes
# #'   }
# #' @param ... Additional arguments passed to methods
# #' 
# #' @export
# effect_sizes <- function(x, type = "all", ...) {
#   UseMethod("effect_sizes")
# }
# 
# #' @export  
# effect_sizes.t_test_results <- function(x, type = "all", ...) {
#   
#   # Calculate Cohen's d
#   cohens_d <- calculate_cohens_d(x)
#   
#   # Calculate r equivalent
#   r_equivalent <- cohens_d / sqrt(cohens_d^2 + 4)
#   
#   results_df <- data.frame(
#     Variable = x$variables,
#     Cohens_d = cohens_d,
#     r_equivalent = r_equivalent,
#     Effect_Size_Category = categorize_effect_size(cohens_d),
#     stringsAsFactors = FALSE
#   )
#   
#   result <- list(
#     results = results_df,
#     type = type,
#     method = "Effect Sizes for t-test",
#     original_test = x
#   )
#   
#   class(result) <- "effect_sizes_results"
#   return(result)
# }
# 
# #' @export
# effect_sizes.oneway_anova_test_results <- function(x, type = "all", ...) {
#   
#   # Extract F-statistics and df
#   f_stats <- x$results$F
#   df1 <- x$results$df1
#   df2 <- x$results$df2
#   
#   # Calculate eta-squared
#   eta_squared <- (f_stats * df1) / (f_stats * df1 + df2)
#   
#   # Calculate omega-squared
#   omega_squared <- (f_stats - 1) * df1 / (f_stats * df1 + df2 + 1)
#   
#   results_df <- data.frame(
#     Variable = x$variables,
#     eta_squared = eta_squared,
#     omega_squared = omega_squared,
#     Effect_Size_Category = categorize_eta_squared(eta_squared),
#     stringsAsFactors = FALSE
#   )
#   
#   result <- list(
#     results = results_df,
#     type = type,
#     method = "Effect Sizes for ANOVA",
#     original_test = x
#   )
#   
#   class(result) <- "effect_sizes_results"
#   return(result)
# }

# EXAMPLE: ASSUMPTIONS S3 GENERIC
# ================================
# File: assumptions.R

# #' Statistical Assumptions Testing
# #' 
# #' @description
# #' Comprehensive assumption testing for statistical models including
# #' normality, homogeneity of variance, linearity, and independence.
# #' 
# #' @param x Statistical test results object
# #' @param tests Character vector; which assumptions to test:
# #'   \itemize{
# #'     \item \code{"all"} (default): All applicable assumptions
# #'     \item \code{"normality"}: Shapiro-Wilk, Anderson-Darling
# #'     \item \code{"homogeneity"}: Levene, Brown-Forsythe
# #'     \item \code{"sphericity"}: Mauchly's test (repeated measures)
# #'     \item \code{"linearity"}: Regression-specific tests
# #'   }
# #' @param ... Additional arguments passed to specific tests
# #' 
# #' @export
# assumptions <- function(x, tests = "all", ...) {
#   UseMethod("assumptions")
# }
# 
# #' @export
# assumptions.t_test_results <- function(x, tests = "all", ...) {
#   
#   results_list <- list()
#   
#   # Test normality (always applicable)
#   if ("all" %in% tests || "normality" %in% tests) {
#     results_list$normality <- test_normality(x, ...)
#   }
#   
#   # Test homogeneity (for independent samples)
#   if (("all" %in% tests || "homogeneity" %in% tests) && !x$paired) {
#     results_list$homogeneity <- levene_test(x, ...)
#   }
#   
#   result <- list(
#     tests = results_list,
#     method = "Assumption Tests for t-test",
#     original_test = x
#   )
#   
#   class(result) <- "assumptions_results" 
#   return(result)
# }
# 
# #' @export
# assumptions.oneway_anova_test_results <- function(x, tests = "all", ...) {
#   
#   results_list <- list()
#   
#   # Test normality
#   if ("all" %in% tests || "normality" %in% tests) {
#     results_list$normality <- test_normality(x, ...)
#   }
#   
#   # Test homogeneity  
#   if ("all" %in% tests || "homogeneity" %in% tests) {
#     results_list$homogeneity <- levene_test(x, ...)
#   }
#   
#   # Test sphericity (for repeated measures)
#   if (("all" %in% tests || "sphericity" %in% tests) && 
#       !is.null(x$repeated) && x$repeated) {
#     results_list$sphericity <- mauchly_test(x, ...)
#   }
#   
#   result <- list(
#     tests = results_list,
#     method = "Assumption Tests for ANOVA",
#     original_test = x
#   )
#   
#   class(result) <- "assumptions_results"
#   return(result) 
# }

# ==========================================
# USAGE EXAMPLES FOR EXTENDED S3 GENERICS
# ==========================================

# COMPREHENSIVE STATISTICAL ANALYSIS PIPELINE:
# ============================================
# 
# # Traditional workflow
# result <- data %>% 
#   oneway_anova_test(outcome, group = treatment)
# 
# # Extended analysis pipeline
# result %>% assumptions()        # Test all relevant assumptions
# result %>% effect_sizes()       # Calculate effect sizes  
# result %>% emmeans()            # Estimated marginal means
# result %>% contrasts()          # Planned contrasts (future)
# result %>% power_analysis()     # Post-hoc power (future)
# 
# # For repeated measures
# rm_result <- data %>%
#   oneway_anova_test(pre, post, group = treatment, repeated = TRUE, subject_id = id)
# 
# rm_result %>% assumptions()     # Includes sphericity testing
# rm_result %>% mauchly_test()    # Specific sphericity test
# rm_result %>% emmeans()         # RM-ANOVA marginal means
# rm_result %>% effect_sizes()    # RM-ANOVA effect sizes
# 
# # Cross-test compatibility
# t_result <- data %>% t_test(outcome, group = treatment)
# t_result %>% assumptions()      # T-test assumptions
# t_result %>% effect_sizes()     # Cohen's d
# t_result %>% emmeans()          # Group means with CIs

# ==========================================
# S3 GENERIC DEVELOPMENT BEST PRACTICES
# ==========================================

# 1. GRACEFUL DEGRADATION:
#    - Handle unsupported test types with informative error messages
#    - Provide alternative suggestions when possible
#    - Chain to original test when S3 method called on secondary results

# 2. CONSISTENT INTERFACES:
#    - Use same parameter names across related functions
#    - Maintain consistent result object structures
#    - Follow template formatting standards

# 3. EXTENSIBILITY:
#    - Design with future test types in mind
#    - Use flexible parameter structures
#    - Document extension points clearly

# 4. VALIDATION:
#    - Cross-validate against SPSS/SAS when possible
#    - Test with edge cases (small samples, missing data)
#    - Verify mathematical correctness

# 5. DOCUMENTATION:
#    - Provide clear examples for each supported test type
#    - Document limitations and assumptions
#    - Include references to statistical literature

# ========================================== 