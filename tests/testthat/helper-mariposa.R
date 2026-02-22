# =============================================================================
# SHARED TEST HELPERS FOR MARIPOSA
# =============================================================================
# This file is automatically loaded by testthat before running any test file.
# It provides common setup, utilities, and SPSS validation helpers.

# Load commonly used packages
library(dplyr)

# =============================================================================
# SPSS VALIDATION TOLERANCES
# =============================================================================
# Centralized tolerance definitions for SPSS comparison tests.
# Reference: SPSS 29.0.0.0

spss_tolerance <- list(
  # Exact match
  count       = 0,


  # Rounding differences
  percentage  = 0.1,
  p_value     = 0.001,

  # Calculation precision
  statistic   = 0.00001,
  mean        = 0.01,
  sd          = 0.01,
  variance    = 1,
  se          = 0.001,
  skewness    = 0.01,
  kurtosis    = 0.01,
  median      = 0.5,
  quantile    = 0.5,
  correlation = 0.0001,
  conf_int    = 0.01,

  # Weighted totals
  weighted_n  = 1
)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Skip test if SPSS reference data is unavailable
#'
#' @param file_name Name of the SPSS reference file
skip_without_spss_ref <- function(file_name) {
  path <- file.path("../../tests/spss_reference/outputs", file_name)
  if (!file.exists(path)) {
    testthat::skip(paste("SPSS reference file not found:", file_name))
  }
}

#' Extract a single group from grouped results
#'
#' @param result A mariposa result object with grouped results
#' @param group_var Name of the grouping variable
#' @param group_value Value of the group to extract
#' @return The subset of results for the specified group
extract_group_result <- function(result, group_var, group_value) {
  if (is.null(result$group_results)) return(NULL)
  for (gr in result$group_results) {
    if (identical(as.character(gr$group_info[[group_var]]), as.character(group_value))) {
      return(gr)
    }
  }
  NULL
}
