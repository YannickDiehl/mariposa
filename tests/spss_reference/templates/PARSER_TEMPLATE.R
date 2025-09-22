# ============================================================================
# SPSS Output Parser Template for [FUNCTION_NAME]() Validation
# ============================================================================

#' Extract [function] values from SPSS output
#'
#' @param file_path Path to SPSS output text file
#' @param test_number Test section number to parse
#' @param variable Optional variable name for multi-variable tests
#' @return List of extracted statistics
extract_[function]_values <- function(file_path, test_number = 1, variable = NULL) {
  # Read file
  lines <- readLines(file_path, warn = FALSE)
  
  # Find test section
  test_pattern <- paste0("TEST ", test_number, ":")
  test_start <- grep(test_pattern, lines)[1]
  
  if (is.na(test_start)) {
    return(NULL)
  }
  
  # Find the relevant table
  # ADJUST: Change pattern to match your SPSS output table header
  table_pattern <- "[Your Table Header]"  # e.g., "Descriptive Statistics"
  table_start <- grep(table_pattern, lines[test_start:length(lines)])[1]
  
  if (is.na(table_start)) {
    return(NULL)
  }
  
  # Adjust for actual position
  table_start <- test_start + table_start - 1
  
  # Extract data line
  # ADJUST: Common offsets are 4-8 lines from table header
  data_line_offset <- 6  # Adjust based on your output
  data_line <- lines[table_start + data_line_offset]
  
  # Clean and extract numbers
  # Use improved regex for decimals like .819
  numbers <- as.numeric(unlist(regmatches(data_line, 
                                          gregexpr("-?\\d*\\.?\\d+", data_line))))
  
  # Map to statistics based on your SPSS output order
  # ADJUST: Map positions to your specific statistics
  result <- list()
  if (length(numbers) >= 2) {
    result$statistic1 <- numbers[1]  # e.g., mean
    result$statistic2 <- numbers[2]  # e.g., sd
    # Add more mappings as needed
  }
  
  return(result)
}

# ============================================================================
# DEBUG HELPER - Use this to find correct line offsets
# ============================================================================

debug_[function]_output <- function(file_path, test_number = 1) {
  lines <- readLines(file_path, warn = FALSE)
  
  # Find test section
  test_pattern <- paste0("TEST ", test_number, ":")
  test_start <- grep(test_pattern, lines)[1]
  
  if (is.na(test_start)) {
    cat("Test", test_number, "not found\n")
    return()
  }
  
  # Show lines around test marker
  cat("Lines from TEST", test_number, "marker:\n")
  cat("=====================================\n")
  for (i in 0:30) {
    line_num <- test_start + i
    if (line_num <= length(lines)) {
      cat(sprintf("%3d: %s\n", i, lines[line_num]))
    }
  }
}

# ============================================================================
# COMMON LINE OFFSETS BY TABLE TYPE
# ============================================================================
# Descriptive Statistics: usually +6 lines after header
# Frequencies: usually +5 lines after header  
# T-Test: usually +4 lines after "Independent Samples Test"
# Chi-Square: usually +3 lines after "Chi-Square Tests"
# ANOVA: usually +5 lines after "ANOVA" header