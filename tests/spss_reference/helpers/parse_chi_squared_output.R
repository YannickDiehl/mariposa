# ============================================================================
# CHI-SQUARED OUTPUT PARSER FOR SPSS VALIDATION
# ============================================================================
# Purpose: Parse SPSS chi-squared crosstabs output for validation testing
# Created: 2025-01-23
# ============================================================================

#' Parse SPSS Chi-Squared Output
#' 
#' Extracts chi-squared test results from SPSS output text file
#' 
#' @param file_path Path to SPSS output text file
#' @return List with parsed test results organized by test scenario
parse_chi_squared_output <- function(file_path) {
  
  # Read the output file
  lines <- readLines(file_path, warn = FALSE)
  
  # Initialize results structure
  results <- list()
  
  # Find test sections
  test_markers <- grep("=========== Test", lines)
  
  for (i in seq_along(test_markers)) {
    start_idx <- test_markers[i]
    end_idx <- if (i < length(test_markers)) test_markers[i + 1] - 1 else length(lines)
    
    # Extract test section
    test_section <- lines[start_idx:end_idx]
    
    # Identify test name from marker
    test_name <- gsub("=", "", lines[start_idx])
    test_name <- trimws(test_name)
    
    # Parse the test based on content
    if (grepl("Test 1a", test_name)) {
      results$unweighted_ungrouped_gender_region <- parse_single_test(test_section)
    } else if (grepl("Test 1b", test_name)) {
      results$unweighted_ungrouped_education_employment <- parse_single_test(test_section)
    } else if (grepl("Test 1c", test_name)) {
      results$unweighted_ungrouped_gender_education <- parse_single_test(test_section)
    } else if (grepl("Test 2a", test_name)) {
      results$weighted_ungrouped_gender_region <- parse_single_test(test_section)
    } else if (grepl("Test 2b", test_name)) {
      results$weighted_ungrouped_education_employment <- parse_single_test(test_section)
    } else if (grepl("Test 2c", test_name)) {
      results$weighted_ungrouped_gender_education <- parse_single_test(test_section)
    } else if (grepl("Test 3a", test_name)) {
      results$unweighted_grouped_gender_education <- parse_grouped_test(test_section)
    } else if (grepl("Test 3b", test_name)) {
      results$unweighted_grouped_gender_employment <- parse_grouped_test(test_section)
    } else if (grepl("Test 4a", test_name)) {
      results$weighted_grouped_gender_education <- parse_grouped_test(test_section)
    } else if (grepl("Test 4b", test_name)) {
      results$weighted_grouped_gender_employment <- parse_grouped_test(test_section)
    }
  }
  
  return(results)
}

#' Parse a single (ungrouped) chi-squared test
#' 
#' @param section Lines containing the test output
#' @return List with crosstab and test statistics
parse_single_test <- function(section) {
  result <- list()
  
  # Find crosstabulation table
  crosstab_start <- grep("Crosstabulation", section)[1]
  if (!is.na(crosstab_start)) {
    result$crosstab <- parse_crosstab(section, crosstab_start)
  }
  
  # Find chi-square tests table
  chi_start <- grep("Chi-Square Tests", section)[1]
  if (!is.na(chi_start)) {
    result$chi_squared <- parse_chi_squared_stats(section, chi_start)
  }
  
  # Find symmetric measures (Cramer's V)
  sym_start <- grep("Symmetric Measures", section)[1]
  if (!is.na(sym_start)) {
    result$cramers_v <- parse_cramers_v(section, sym_start)
  }
  
  return(result)
}

#' Parse a grouped chi-squared test
#' 
#' @param section Lines containing the test output
#' @return List with results by group
parse_grouped_test <- function(section) {
  result <- list()
  
  # For the new format, grouped tests have region-specific results in Chi-Square Tests table
  chi_start <- grep("Chi-Square Tests", section)[1]
  
  if (!is.na(chi_start)) {
    # Look for East and West lines in the chi-square section
    chi_section <- section[chi_start:length(section)]
    
    # Find East results
    east_chi_line <- grep("^\\s*East\\s+Pearson Chi-Square", chi_section)
    if (length(east_chi_line) > 0) {
      result$east <- list()
      # Extract chi-squared statistics for East
      east_line <- chi_section[east_chi_line[1]]
      east_values <- extract_chi_squared_values_grouped(east_line)
      result$east$chi_squared <- list(
        chi_squared = east_values$chi_squared,
        df = east_values$df,
        p_value = east_values$p_value
      )
      
      # Find Cramer's V for East in Symmetric Measures
      sym_start <- grep("Symmetric Measures", section)[1]
      if (!is.na(sym_start)) {
        sym_section <- section[sym_start:length(section)]
        east_cramers_line <- grep("^\\s*East.*Cramer's V", sym_section)
        if (length(east_cramers_line) > 0) {
          cramers_line <- sym_section[east_cramers_line[1]]
          numbers <- extract_numbers(cramers_line)
          if (length(numbers) > 0) {
            result$east$cramers_v <- numbers[1]
          }
        }
      }
    }
    
    # Find West results
    west_chi_line <- grep("^\\s*West\\s+Pearson Chi-Square", chi_section)
    if (length(west_chi_line) > 0) {
      result$west <- list()
      # Extract chi-squared statistics for West
      west_line <- chi_section[west_chi_line[1]]
      west_values <- extract_chi_squared_values_grouped(west_line)
      result$west$chi_squared <- list(
        chi_squared = west_values$chi_squared,
        df = west_values$df,
        p_value = west_values$p_value
      )
      
      # Find Cramer's V for West in Symmetric Measures
      sym_start <- grep("Symmetric Measures", section)[1]
      if (!is.na(sym_start)) {
        sym_section <- section[sym_start:length(section)]
        west_cramers_line <- grep("^\\s*West.*Cramer's V", sym_section)
        if (length(west_cramers_line) > 0) {
          cramers_line <- sym_section[west_cramers_line[1]]
          numbers <- extract_numbers(cramers_line)
          if (length(numbers) > 0) {
            result$west$cramers_v <- numbers[1]
          }
        }
      }
    }
  }
  
  return(result)
}

#' Parse crosstabulation table
#' 
#' @param section Test section lines
#' @param start_idx Starting index of crosstab
#' @return Data frame with counts and percentages
parse_crosstab <- function(section, start_idx) {
  # Find the count lines (they contain "Count")
  count_lines <- grep("Count", section[start_idx:length(section)])
  count_lines <- count_lines[count_lines <= 20] # Limit search range
  
  if (length(count_lines) < 2) return(NULL)
  
  # Initialize result
  crosstab <- list()
  
  # Parse each row of counts
  for (i in seq_along(count_lines)) {
    line_idx <- start_idx + count_lines[i] - 1
    count_line <- section[line_idx]
    
    # Extract numbers from count line
    numbers <- extract_numbers(count_line)
    
    # Store based on position
    if (i == 1) {
      crosstab$row1_counts <- numbers
    } else if (i == 2) {
      crosstab$row2_counts <- numbers
    } else if (i == 3) {
      crosstab$total_counts <- numbers
    }
    
    # Also extract percentages
    if (line_idx + 1 <= length(section)) {
      pct_line <- section[line_idx + 1]
      if (grepl("%", pct_line)) {
        pct_numbers <- extract_percentages(pct_line)
        if (i == 1) {
          crosstab$row1_row_pct <- pct_numbers
        } else if (i == 2) {
          crosstab$row2_row_pct <- pct_numbers
        }
      }
    }
    
    # Column percentages
    if (line_idx + 2 <= length(section)) {
      col_pct_line <- section[line_idx + 2]
      if (grepl("%", col_pct_line)) {
        col_pct_numbers <- extract_percentages(col_pct_line)
        if (i == 1) {
          crosstab$row1_col_pct <- col_pct_numbers
        } else if (i == 2) {
          crosstab$row2_col_pct <- col_pct_numbers
        }
      }
    }
  }
  
  return(crosstab)
}

#' Parse chi-squared test statistics
#' 
#' @param section Test section lines
#' @param start_idx Starting index of chi-square tests table
#' @return List with chi-square value, df, and p-value
parse_chi_squared_stats <- function(section, start_idx) {
  # Look for Pearson Chi-Square line
  pearson_line <- section[start_idx + 2] # Usually 2 lines after header
  
  if (!grepl("Pearson Chi-Square", pearson_line)) {
    # Search for it
    for (i in 1:10) {
      if (start_idx + i <= length(section)) {
        if (grepl("Pearson Chi-Square", section[start_idx + i])) {
          pearson_line <- section[start_idx + i]
          break
        }
      }
    }
  }
  
  # Extract values
  values <- extract_chi_squared_values(pearson_line)
  
  return(list(
    chi_squared = values$chi_squared,
    df = values$df,
    p_value = values$p_value
  ))
}

#' Parse Cramer's V from symmetric measures
#' 
#' @param section Test section lines
#' @param start_idx Starting index of symmetric measures table
#' @return Numeric Cramer's V value
parse_cramers_v <- function(section, start_idx) {
  # Look for Cramer's V line
  cramers_line <- NULL
  for (i in 1:10) {
    if (start_idx + i <= length(section)) {
      if (grepl("Cramer's V", section[start_idx + i])) {
        cramers_line <- section[start_idx + i]
        break
      }
    }
  }
  
  if (!is.null(cramers_line)) {
    # Extract the value (first number after "Cramer's V")
    numbers <- extract_numbers(cramers_line)
    if (length(numbers) > 0) {
      return(numbers[1])
    }
  }
  
  return(NA)
}

#' Extract numbers from a line
#' 
#' @param line Character string
#' @return Numeric vector of extracted numbers
extract_numbers <- function(line) {
  # Remove commas used as thousands separators
  line <- gsub(",", "", line)
  
  # Look for patterns that match numbers, including those starting with decimal point
  # This pattern handles: 123, 12.34, .456, 0.789
  pattern <- "(?:(?<=\\s)|(?<=^))(?:\\d+\\.\\d*|\\.\\d+|\\d+)(?=\\s|$|\\()"
  matches <- gregexpr(pattern, line, perl = TRUE)
  numbers <- regmatches(line, matches)[[1]]
  
  # Convert to numeric
  numeric_values <- suppressWarnings(as.numeric(numbers))
  numeric_values <- numeric_values[!is.na(numeric_values)]
  
  return(numeric_values)
}

#' Extract percentages from a line
#' 
#' @param line Character string
#' @return Numeric vector of extracted percentages
extract_percentages <- function(line) {
  # Extract numbers followed by %
  matches <- gregexpr("[0-9]+\\.?[0-9]*%", line)
  percentages <- regmatches(line, matches)[[1]]
  # Remove % and convert to numeric
  percentages <- gsub("%", "", percentages)
  return(as.numeric(percentages))
}

#' Extract chi-squared test values from line
#' 
#' @param line Character string containing Pearson Chi-Square
#' @return List with chi_squared, df, and p_value
extract_chi_squared_values <- function(line) {
  # Remove footnote markers like (a)
  line <- gsub("\\([a-z]\\)", "", line)
  
  # Extract all numbers
  numbers <- extract_numbers(line)
  
  # Typically: first number is chi-squared, second is df, third is p-value
  result <- list(
    chi_squared = if (length(numbers) >= 1) numbers[1] else NA,
    df = if (length(numbers) >= 2) as.integer(numbers[2]) else NA,
    p_value = if (length(numbers) >= 3) numbers[3] else NA
  )
  
  return(result)
}

#' Extract chi-squared test values from grouped line
#' 
#' @param line Character string containing region-specific Pearson Chi-Square
#' @return List with chi_squared, df, and p_value
extract_chi_squared_values_grouped <- function(line) {
  # Remove region label and footnote markers
  line <- gsub("^\\s*(East|West)\\s+", "", line)
  line <- gsub("Pearson Chi-Square\\s+", "", line)
  line <- gsub("\\([a-z]\\)", "", line)
  
  # Extract all numbers
  numbers <- extract_numbers(line)
  
  # First number is chi-squared, second is df, third is p-value
  result <- list(
    chi_squared = if (length(numbers) >= 1) numbers[1] else NA,
    df = if (length(numbers) >= 2) as.integer(numbers[2]) else NA,
    p_value = if (length(numbers) >= 3) numbers[3] else NA
  )
  
  return(result)
}