# SPSS Validation Framework
# =============================================================
# Core validation functions for comparing SurveyStat R results
# against SPSS reference outputs with appropriate tolerances.

#' Default Tolerance Settings for SPSS Validation
#' 
#' @keywords internal
.spss_tolerances <- list(
  t_statistic = 1e-6,      # t-statistics
  p_value = 1e-8,          # p-values (most critical)
  confidence_interval = 1e-6, # CI bounds
  effect_size = 1e-6,      # Cohen's d, Hedges' g, etc.
  f_statistic = 1e-6,      # F-statistics (ANOVA, Levene)
  mean_diff = 1e-6,        # Mean differences
  descriptive = 1e-6,      # Means, SDs, etc.
  df = 1e-3                # Degrees of freedom (can be slightly different due to rounding)
)

#' Validate T-Test Results Against SPSS
#' 
#' Compares SurveyStat t_test() results with SPSS reference output
#' 
#' @param r_result Result object from SurveyStat t_test()
#' @param spss_file Path to SPSS reference TXT file
#' @param tolerance_list Named list of tolerances (optional)
#' @param var.equal Logical indicating which variance assumption to test
#' @return List with validation results and any discrepancies
#' @export
validate_t_test_against_spss <- function(r_result, spss_file, 
                                         tolerance_list = NULL, var.equal = FALSE) {
  
  # Load SPSS parser
  source("tests/spss_reference/helpers/spss_parser.R", local = TRUE)
  
  # Use default tolerances if not provided
  if (is.null(tolerance_list)) {
    tolerance_list <- .spss_tolerances
  }
  
  # Parse SPSS output
  spss_results <- parse_spss_t_test(spss_file)
  
  # Initialize validation results
  validation <- list(
    passed = TRUE,
    discrepancies = list(),
    summary = list()
  )
  
  # Select which variance assumption to validate
  spss_test <- if (var.equal) spss_results$equal_variances else spss_results$unequal_variances
  r_test_results <- r_result$results
  
  # Get first row of results (for single variable tests)
  if (nrow(r_test_results) > 0) {
    r_row <- r_test_results[1, ]
    
    # Validate t-statistic
    if (!is.null(spss_test$t_stat) && !is.na(r_row$t_stat)) {
      t_diff <- abs(r_row$t_stat - spss_test$t_stat)
      if (t_diff > tolerance_list$t_statistic) {
        validation$passed <- FALSE
        validation$discrepancies$t_statistic <- list(
          r_value = r_row$t_stat,
          spss_value = spss_test$t_stat,
          difference = t_diff,
          tolerance = tolerance_list$t_statistic
        )
      }
    }
    
    # Validate degrees of freedom
    if (!is.null(spss_test$df) && !is.na(r_row$df)) {
      df_diff <- abs(r_row$df - spss_test$df)
      if (df_diff > tolerance_list$df) {
        validation$passed <- FALSE
        validation$discrepancies$df <- list(
          r_value = r_row$df,
          spss_value = spss_test$df,
          difference = df_diff,
          tolerance = tolerance_list$df
        )
      }
    }
    
    # Validate p-value
    if (!is.null(spss_test$p_value) && !is.na(r_row$p_value)) {
      p_diff <- abs(r_row$p_value - spss_test$p_value)
      if (p_diff > tolerance_list$p_value) {
        validation$passed <- FALSE
        validation$discrepancies$p_value <- list(
          r_value = r_row$p_value,
          spss_value = spss_test$p_value,
          difference = p_diff,
          tolerance = tolerance_list$p_value
        )
      }
    }
    
    # Validate mean difference
    if (!is.null(spss_test$mean_diff) && !is.na(r_row$mean_diff)) {
      mean_diff_diff <- abs(r_row$mean_diff - spss_test$mean_diff)
      if (mean_diff_diff > tolerance_list$mean_diff) {
        validation$passed <- FALSE
        validation$discrepancies$mean_diff <- list(
          r_value = r_row$mean_diff,
          spss_value = spss_test$mean_diff,
          difference = mean_diff_diff,
          tolerance = tolerance_list$mean_diff
        )
      }
    }
    
    # Validate confidence intervals
    if (!is.null(spss_test$ci_lower) && !is.na(r_row$CI_lower)) {
      ci_lower_diff <- abs(r_row$CI_lower - spss_test$ci_lower)
      if (ci_lower_diff > tolerance_list$confidence_interval) {
        validation$passed <- FALSE
        validation$discrepancies$ci_lower <- list(
          r_value = r_row$CI_lower,
          spss_value = spss_test$ci_lower,
          difference = ci_lower_diff,
          tolerance = tolerance_list$confidence_interval
        )
      }
    }
    
    if (!is.null(spss_test$ci_upper) && !is.na(r_row$CI_upper)) {
      ci_upper_diff <- abs(r_row$CI_upper - spss_test$ci_upper)
      if (ci_upper_diff > tolerance_list$confidence_interval) {
        validation$passed <- FALSE
        validation$discrepancies$ci_upper <- list(
          r_value = r_row$CI_upper,
          spss_value = spss_test$ci_upper,
          difference = ci_upper_diff,
          tolerance = tolerance_list$confidence_interval
        )
      }
    }
  }
  
  # Validate Levene test (if available)
  if (!is.null(spss_results$levene_test$F_statistic)) {
    # Run Levene test on R result to compare
    levene_result <- levene_test(r_result)
    if (!is.null(levene_result$results) && nrow(levene_result$results) > 0) {
      levene_row <- levene_result$results[1, ]
      
      # Compare F-statistic
      f_diff <- abs(levene_row$F_statistic - spss_results$levene_test$F_statistic)
      if (f_diff > tolerance_list$f_statistic) {
        validation$passed <- FALSE
        validation$discrepancies$levene_f <- list(
          r_value = levene_row$F_statistic,
          spss_value = spss_results$levene_test$F_statistic,
          difference = f_diff,
          tolerance = tolerance_list$f_statistic
        )
      }
      
      # Compare p-value
      levene_p_diff <- abs(levene_row$p_value - spss_results$levene_test$p_value)
      if (levene_p_diff > tolerance_list$p_value) {
        validation$passed <- FALSE
        validation$discrepancies$levene_p <- list(
          r_value = levene_row$p_value,
          spss_value = spss_results$levene_test$p_value,
          difference = levene_p_diff,
          tolerance = tolerance_list$p_value
        )
      }
    }
  }
  
  # Create summary
  validation$summary$total_comparisons <- length(validation$discrepancies) + 
    sum(validation$passed) * 6  # Approximate number of successful comparisons
  validation$summary$failed_comparisons <- length(validation$discrepancies)
  validation$summary$pass_rate <- 1 - (validation$summary$failed_comparisons / max(1, validation$summary$total_comparisons))
  
  return(validation)
}

#' Validate Descriptive Statistics Against SPSS
#' 
#' Compares SurveyStat describe() results with SPSS DESCRIPTIVES output
#' 
#' @param r_result Result object from SurveyStat describe()
#' @param spss_file Path to SPSS reference TXT file
#' @param tolerance_list Named list of tolerances (optional)
#' @return List with validation results and any discrepancies
#' @export
validate_descriptives_against_spss <- function(r_result, spss_file, tolerance_list = NULL) {
  
  # Load SPSS parser
  source("tests/spss_reference/helpers/spss_parser.R", local = TRUE)
  
  # Use default tolerances if not provided
  if (is.null(tolerance_list)) {
    tolerance_list <- .spss_tolerances
  }
  
  # Parse SPSS output
  spss_results <- parse_spss_descriptives(spss_file)
  
  # Initialize validation results
  validation <- list(
    passed = TRUE,
    discrepancies = list(),
    summary = list()
  )
  
  # Compare each variable
  for (var_name in names(spss_results)) {
    spss_var <- spss_results[[var_name]]
    
    # Find corresponding R results
    # Look for variable-specific columns in R results
    mean_col <- paste0(var_name, "_Mean")
    sd_col <- paste0(var_name, "_SD")
    n_col <- paste0(var_name, "_N")
    min_col <- paste0(var_name, "_Min")
    max_col <- paste0(var_name, "_Max")
    
    r_results <- r_result$results
    
    # Validate mean
    if (mean_col %in% names(r_results) && !is.null(spss_var$mean)) {
      r_mean <- r_results[[mean_col]][1]
      mean_diff <- abs(r_mean - spss_var$mean)
      if (mean_diff > tolerance_list$descriptive) {
        validation$passed <- FALSE
        validation$discrepancies[[paste0(var_name, "_mean")]] <- list(
          r_value = r_mean,
          spss_value = spss_var$mean,
          difference = mean_diff,
          tolerance = tolerance_list$descriptive
        )
      }
    }
    
    # Validate standard deviation
    if (sd_col %in% names(r_results) && !is.null(spss_var$sd)) {
      r_sd <- r_results[[sd_col]][1]
      sd_diff <- abs(r_sd - spss_var$sd)
      if (sd_diff > tolerance_list$descriptive) {
        validation$passed <- FALSE
        validation$discrepancies[[paste0(var_name, "_sd")]] <- list(
          r_value = r_sd,
          spss_value = spss_var$sd,
          difference = sd_diff,
          tolerance = tolerance_list$descriptive
        )
      }
    }
    
    # Validate sample size
    if (n_col %in% names(r_results) && !is.null(spss_var$n)) {
      r_n <- r_results[[n_col]][1]
      n_diff <- abs(r_n - spss_var$n)
      if (n_diff > 0) {  # Sample sizes should match exactly
        validation$passed <- FALSE
        validation$discrepancies[[paste0(var_name, "_n")]] <- list(
          r_value = r_n,
          spss_value = spss_var$n,
          difference = n_diff,
          tolerance = 0
        )
      }
    }
  }
  
  # Create summary
  validation$summary$variables_tested <- length(spss_results)
  validation$summary$failed_comparisons <- length(validation$discrepancies)
  validation$summary$total_comparisons <- validation$summary$variables_tested * 3  # mean, sd, n
  validation$summary$pass_rate <- 1 - (validation$summary$failed_comparisons / max(1, validation$summary$total_comparisons))
  
  return(validation)
}

#' Validate Levene Test Against SPSS
#' 
#' Compares SurveyStat levene_test() results with SPSS output
#' 
#' @param r_result Result object from SurveyStat levene_test()
#' @param spss_file Path to SPSS reference TXT file
#' @param tolerance_list Named list of tolerances (optional)
#' @return List with validation results and any discrepancies
#' @export
validate_levene_against_spss <- function(r_result, spss_file, tolerance_list = NULL) {
  
  # Load SPSS parser
  source("tests/spss_reference/helpers/spss_parser.R", local = TRUE)
  
  # Use default tolerances if not provided
  if (is.null(tolerance_list)) {
    tolerance_list <- .spss_tolerances
  }
  
  # Parse SPSS output
  spss_results <- parse_spss_levene(spss_file)
  
  # Initialize validation results
  validation <- list(
    passed = TRUE,
    discrepancies = list(),
    summary = list()
  )
  
  # Get R results
  if (!is.null(r_result$results) && nrow(r_result$results) > 0) {
    r_row <- r_result$results[1, ]
    
    # Validate F-statistic
    if (!is.null(spss_results$F_statistic) && !is.na(r_row$F_statistic)) {
      f_diff <- abs(r_row$F_statistic - spss_results$F_statistic)
      if (f_diff > tolerance_list$f_statistic) {
        validation$passed <- FALSE
        validation$discrepancies$F_statistic <- list(
          r_value = r_row$F_statistic,
          spss_value = spss_results$F_statistic,
          difference = f_diff,
          tolerance = tolerance_list$f_statistic
        )
      }
    }
    
    # Validate degrees of freedom
    if (!is.null(spss_results$df1) && !is.na(r_row$df1)) {
      df1_diff <- abs(r_row$df1 - spss_results$df1)
      if (df1_diff > tolerance_list$df) {
        validation$passed <- FALSE
        validation$discrepancies$df1 <- list(
          r_value = r_row$df1,
          spss_value = spss_results$df1,
          difference = df1_diff,
          tolerance = tolerance_list$df
        )
      }
    }
    
    if (!is.null(spss_results$df2) && !is.na(r_row$df2)) {
      df2_diff <- abs(r_row$df2 - spss_results$df2)
      if (df2_diff > tolerance_list$df) {
        validation$passed <- FALSE
        validation$discrepancies$df2 <- list(
          r_value = r_row$df2,
          spss_value = spss_results$df2,
          difference = df2_diff,
          tolerance = tolerance_list$df
        )
      }
    }
    
    # Validate p-value
    if (!is.null(spss_results$p_value) && !is.na(r_row$p_value)) {
      p_diff <- abs(r_row$p_value - spss_results$p_value)
      if (p_diff > tolerance_list$p_value) {
        validation$passed <- FALSE
        validation$discrepancies$p_value <- list(
          r_value = r_row$p_value,
          spss_value = spss_results$p_value,
          difference = p_diff,
          tolerance = tolerance_list$p_value
        )
      }
    }
  }
  
  # Create summary
  validation$summary$total_comparisons <- 4  # F, df1, df2, p-value
  validation$summary$failed_comparisons <- length(validation$discrepancies)
  validation$summary$pass_rate <- 1 - (validation$summary$failed_comparisons / validation$summary$total_comparisons)
  
  return(validation)
}

#' Create SPSS Validation Report
#' 
#' Generates a comprehensive validation report
#' 
#' @param validation_results List of validation results from various functions
#' @param output_file Optional file path to save report
#' @return Character vector with formatted report
#' @export
create_validation_report <- function(validation_results, output_file = NULL) {
  
  report_lines <- c(
    "===============================================",
    "SPSS VALIDATION REPORT",
    "===============================================",
    paste("Generated:", Sys.time()),
    "",
    "SUMMARY:",
    paste("Total functions tested:", length(validation_results)),
    ""
  )
  
  # Overall statistics
  total_passed <- sum(sapply(validation_results, function(x) x$passed))
  overall_pass_rate <- total_passed / length(validation_results) * 100
  
  report_lines <- c(report_lines,
    paste("Overall pass rate:", sprintf("%.1f%%", overall_pass_rate)),
    paste("Functions passed:", total_passed, "/", length(validation_results)),
    ""
  )
  
  # Detailed results for each function
  for (func_name in names(validation_results)) {
    result <- validation_results[[func_name]]
    
    report_lines <- c(report_lines,
      paste("FUNCTION:", func_name),
      paste("Status:", if (result$passed) "PASSED" else "FAILED"),
      ""
    )
    
    if (!result$passed && length(result$discrepancies) > 0) {
      report_lines <- c(report_lines, "Discrepancies:")
      
      for (disc_name in names(result$discrepancies)) {
        disc <- result$discrepancies[[disc_name]]
        report_lines <- c(report_lines,
          paste("  -", disc_name, ":"),
          paste("    R value:", disc$r_value),
          paste("    SPSS value:", disc$spss_value),
          paste("    Difference:", disc$difference),
          paste("    Tolerance:", disc$tolerance),
          ""
        )
      }
    }
    
    if (!is.null(result$summary)) {
      report_lines <- c(report_lines,
        paste("Summary: Pass rate", sprintf("%.1f%%", result$summary$pass_rate * 100)),
        ""
      )
    }
    
    report_lines <- c(report_lines, "-----------------------------------------------", "")
  }
  
  # Save to file if requested
  if (!is.null(output_file)) {
    writeLines(report_lines, output_file)
    cat("Validation report saved to:", output_file, "\n")
  }
  
  return(report_lines)
}