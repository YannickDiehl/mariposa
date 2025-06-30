# Test SPSS Parser Functions
# ===============================================
# Validates parser functions with mock SPSS reference files

# Load parsers
source('tests/spss_reference/helpers/spss_parser.R')
source('tests/spss_reference/helpers/validation_framework.R')

cat("==============================================\n")
cat("TESTING SPSS PARSERS WITH MOCK DATA\n")
cat("==============================================\n\n")

# Test t_test parser
cat("1. Testing t_test parser...\n")
cat("   File: t_test_basic.txt\n")

tryCatch({
  t_result <- parse_spss_t_test('tests/spss_reference/outputs/t_test/t_test_basic.txt')
  cat("   ✓ Parser executed successfully\n")
  
  # Check structure
  cat("   Structure check:\n")
  cat("     - Equal variances:", !is.null(t_result$equal_variances), "\n")
  cat("     - Unequal variances:", !is.null(t_result$unequal_variances), "\n")
  cat("     - Group statistics:", !is.null(t_result$group_statistics), "\n")
  cat("     - Levene test:", !is.null(t_result$levene_test), "\n")
  
  # Check values
  if (!is.null(t_result$equal_variances$t_stat)) {
    cat("     - Equal var t-stat:", t_result$equal_variances$t_stat, "\n")
  }
  if (!is.null(t_result$levene_test$F_statistic)) {
    cat("     - Levene F-stat:", t_result$levene_test$F_statistic, "\n")
  }
  
}, error = function(e) {
  cat("   ✗ Parser failed:", e$message, "\n")
})

cat("\n")

# Test describe parser  
cat("2. Testing describe parser...\n")
cat("   File: describe_basic.txt\n")

tryCatch({
  desc_result <- parse_spss_descriptives('tests/spss_reference/outputs/describe/describe_basic.txt')
  cat("   ✓ Parser executed successfully\n")
  
  # Check structure
  cat("   Variables found:", length(desc_result), "\n")
  for (var_name in names(desc_result)) {
    var_data <- desc_result[[var_name]]
    cat("     -", var_name, ": N=", var_data$n, ", Mean=", var_data$mean, "\n")
  }
  
}, error = function(e) {
  cat("   ✗ Parser failed:", e$message, "\n")
})

cat("\n")

# Test levene parser
cat("3. Testing levene parser...\n")
cat("   File: levene_test_basic.txt\n")

tryCatch({
  levene_result <- parse_spss_levene('tests/spss_reference/outputs/levene_test/levene_test_basic.txt')
  cat("   ✓ Parser executed successfully\n")
  
  # Check values
  if (!is.null(levene_result$F_statistic)) {
    cat("     - F-statistic:", levene_result$F_statistic, "\n")
    cat("     - df1:", levene_result$df1, "\n") 
    cat("     - df2:", levene_result$df2, "\n")
    cat("     - p-value:", levene_result$p_value, "\n")
  } else {
    cat("     - No Levene statistics found\n")
  }
  
}, error = function(e) {
  cat("   ✗ Parser failed:", e$message, "\n")
})

cat("\n==============================================\n")
cat("PARSER TESTING COMPLETE\n")
cat("==============================================\n")