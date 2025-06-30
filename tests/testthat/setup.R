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