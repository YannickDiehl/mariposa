# =============================================================================
# SHARED TEST HELPERS FOR MARIPOSA
# =============================================================================
# This file is automatically loaded by testthat before running any test file.
# It provides common setup, utilities, and SPSS validation helpers.

# Load commonly used packages
library(dplyr)

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
