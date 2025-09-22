# Setup for all testthat tests
# This file runs before all tests to ensure package is properly loaded

# Load the package for testing
library(SurveyStat)

# Ensure our datasets are available
tryCatch({
  data(survey_data)
  data(longitudinal_data) 
  data(longitudinal_data_wide)
}, error = function(e) {
  cat("Warning: Could not load datasets:", e$message, "\n")
})

# Set standard test options
options(
  testthat.progress.max_fails = 10,
  warn = 1
)

# ============================================================================
# Setup for automatic validation report generation
# ============================================================================

# Reset validation tracking at the start of test suite
if (exists("reset_validation_tracking")) {
  reset_validation_tracking()
}

# Check if we should generate validation reports
# (disable during R CMD check to speed up package checking)
if (!isTRUE(as.logical(Sys.getenv("_R_CHECK_PACKAGE_NAME_", "FALSE")))) {
  # Set environment variable to enable report generation
  Sys.setenv(SURVEYSTAT_GENERATE_REPORTS = "TRUE")
} else {
  Sys.setenv(SURVEYSTAT_GENERATE_REPORTS = "FALSE")
}

# Message about report generation status
if (isTRUE(as.logical(Sys.getenv("SURVEYSTAT_GENERATE_REPORTS", "TRUE")))) {
  cat("\nℹ️ SPSS validation report generation is ENABLED\n")
  cat("   Reports will be generated after validation tests complete.\n")
  cat("   To disable: Sys.setenv(SURVEYSTAT_GENERATE_REPORTS = 'FALSE')\n\n")
}