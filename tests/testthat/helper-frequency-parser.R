# ============================================================================
# COMPREHENSIVE FREQUENCY TABLE PARSER FOR SPSS OUTPUT
# ============================================================================

#' Parse frequency table from SPSS output
#' @param lines Character vector of output lines
#' @param test_identifier Test marker to find
#' @param variable Variable name to look for
#' @return List with frequency table components
parse_spss_frequency_table <- function(lines, test_identifier, variable = NULL) {
  
  # Find test section
  test_patterns <- c(
    paste0("### ", test_identifier),
    paste0(test_identifier, ":"),
    paste0("==== ", test_identifier)
  )
  
  test_start <- NA
  for (pattern in test_patterns) {
    test_idx <- grep(pattern, lines, fixed = TRUE)[1]
    if (!is.na(test_idx)) {
      test_start <- test_idx
      break
    }
  }
  
  if (is.na(test_start)) {
    warning(sprintf("Could not find test identifier: %s", test_identifier))
    return(NULL)
  }
  
  # Find frequency table header
  # Look for patterns like "Frequency", "Percent", "Valid Percent", "Cumulative Percent"
  freq_header_idx <- NA
  search_limit <- min(test_start + 50, length(lines))
  
  for (i in (test_start + 1):search_limit) {
    if (grepl("Frequency.*Percent", lines[i])) {
      freq_header_idx <- i
      break
    }
  }
  
  if (is.na(freq_header_idx)) {
    warning(sprintf("Could not find frequency table header for test: %s", test_identifier))
    return(NULL)
  }
  
  # Extract frequency table data
  # Tables typically have format:
  # Value Label | Frequency | Percent | Valid Percent | Cumulative Percent
  
  result <- list(
    test = test_identifier,
    variable = variable,
    frequencies = list(),
    totals = list(),
    statistics = list()
  )
  
  # Parse table rows (start after header)
  row_idx <- freq_header_idx + 1
  table_data <- list()
  
  # Skip separator line if present
  if (grepl("^\\s*-+\\s*$", lines[row_idx])) {
    row_idx <- row_idx + 1
  }
  
  # Parse data rows until we hit "Total" or empty line
  while (row_idx <= length(lines)) {
    line <- lines[row_idx]
    
    # Stop conditions
    if (grepl("^\\s*Total", line)) {
      # Parse total row
      total_values <- extract_numeric_values(line)
      if (length(total_values) >= 2) {
        result$totals <- list(
          frequency = total_values[1],
          percent = if (length(total_values) >= 2) total_values[2] else NA
        )
      }
      break
    }
    
    if (grepl("^\\s*$", line) || grepl("^\\s*-+\\s*$", line)) {
      row_idx <- row_idx + 1
      next
    }
    
    # Parse data row
    # Could be: "Valid   1 Male     1234    49.4    49.4    49.4"
    # Or:       "        2 Female   1266    50.6    50.6   100.0"
    
    # Extract all numeric values
    numeric_values <- extract_numeric_values(line)
    
    if (length(numeric_values) >= 4) {
      # Try to extract value label
      # Remove "Valid" or "Missing" prefix if present
      clean_line <- gsub("^\\s*(Valid|Missing)\\s+", "", line)
      
      # Extract category value and label
      # Pattern: number followed by optional label, then frequencies
      if (grepl("^\\s*\\d+", clean_line)) {
        # Extract category value
        category_match <- regmatches(clean_line, regexpr("^\\s*\\d+", clean_line))
        category_value <- as.numeric(gsub("\\s+", "", category_match))
        
        # Extract label (text between category and first frequency)
        label_part <- sub("^\\s*\\d+\\s*", "", clean_line)
        # Find where numbers start
        first_num_pos <- regexpr("\\d+", label_part)[1]
        if (first_num_pos > 1) {
          label <- trimws(substr(label_part, 1, first_num_pos - 1))
        } else {
          label <- ""
        }
        
        # Store row data
        row_data <- list(
          value = category_value,
          label = label,
          frequency = numeric_values[length(numeric_values) - 3],
          percent = numeric_values[length(numeric_values) - 2],
          valid_percent = numeric_values[length(numeric_values) - 1],
          cumulative_percent = numeric_values[length(numeric_values)]
        )
        
        table_data[[length(table_data) + 1]] <- row_data
      }
    }
    
    row_idx <- row_idx + 1
  }
  
  result$frequencies <- table_data
  
  # Look for statistics section (Mode, Median, etc.)
  stats_start <- grep("Statistics", lines[test_start:min(test_start + 100, length(lines))], 
                      ignore.case = TRUE)[1]
  if (!is.na(stats_start)) {
    stats_idx <- test_start + stats_start - 1
    
    # Look for common statistics
    for (i in stats_idx:(min(stats_idx + 20, length(lines)))) {
      line <- lines[i]
      
      if (grepl("Mode", line, ignore.case = TRUE)) {
        mode_val <- extract_last_numeric(line)
        if (!is.na(mode_val)) result$statistics$mode <- mode_val
      }
      
      if (grepl("Median", line, ignore.case = TRUE)) {
        median_val <- extract_last_numeric(line)
        if (!is.na(median_val)) result$statistics$median <- median_val
      }
      
      if (grepl("Mean", line, ignore.case = TRUE)) {
        mean_val <- extract_last_numeric(line)
        if (!is.na(mean_val)) result$statistics$mean <- mean_val
      }
      
      if (grepl("Std.*Deviation", line, ignore.case = TRUE)) {
        sd_val <- extract_last_numeric(line)
        if (!is.na(sd_val)) result$statistics$sd <- sd_val
      }
    }
  }
  
  return(result)
}

#' Parse crosstab from SPSS output
#' @param lines Character vector of output lines
#' @param test_identifier Test marker to find
#' @return List with crosstab components
parse_spss_crosstab <- function(lines, test_identifier) {
  
  # Find test section
  test_start <- grep(test_identifier, lines, fixed = TRUE)[1]
  
  if (is.na(test_start)) {
    warning(sprintf("Could not find test identifier: %s", test_identifier))
    return(NULL)
  }
  
  result <- list(
    test = test_identifier,
    cells = list(),
    row_totals = list(),
    column_totals = list(),
    grand_total = NA,
    chi_squared = list()
  )
  
  # Find crosstabulation table
  # Look for "Crosstabulation" keyword
  crosstab_idx <- grep("Crosstabulation", 
                       lines[test_start:min(test_start + 50, length(lines))], 
                       ignore.case = TRUE)[1]
  
  if (!is.na(crosstab_idx)) {
    table_start <- test_start + crosstab_idx - 1
    
    # Parse table structure
    # Crosstabs have format:
    # |         | Col1 | Col2 | Total |
    # | Row1    | n    | n    | n     |
    # | Row2    | n    | n    | n     |
    # | Total   | n    | n    | N     |
    
    # Implementation would continue here...
    # This is a simplified version for demonstration
  }
  
  # Find chi-squared statistics
  chi_idx <- grep("Chi-Square Tests", 
                  lines[test_start:min(test_start + 100, length(lines))], 
                  ignore.case = TRUE)[1]
  
  if (!is.na(chi_idx)) {
    chi_start <- test_start + chi_idx - 1
    
    # Look for Pearson Chi-Square line
    for (i in chi_start:(min(chi_start + 10, length(lines)))) {
      if (grepl("Pearson Chi-Square", lines[i])) {
        chi_values <- extract_numeric_values(lines[i])
        if (length(chi_values) >= 3) {
          result$chi_squared <- list(
            statistic = chi_values[1],
            df = chi_values[2],
            p_value = chi_values[3]
          )
        }
        break
      }
    }
  }
  
  return(result)
}

#' Extract numeric values from a line
#' @param line Character string
#' @return Numeric vector of values
extract_numeric_values <- function(line) {
  # Clean the line
  clean_line <- gsub("\\s+", " ", line)
  # Handle SPSS decimal format (.123 instead of 0.123)
  clean_line <- gsub("\\s\\.", " 0.", clean_line)
  clean_line <- gsub("^\\.", "0.", clean_line)
  clean_line <- gsub("-\\.", "-0.", clean_line)
  
  # Extract all numeric values
  matches <- regmatches(clean_line, gregexpr("-?\\d*\\.?\\d+", clean_line))
  if (length(matches) > 0 && length(matches[[1]]) > 0) {
    return(as.numeric(matches[[1]]))
  }
  return(numeric())
}

#' Extract last numeric value from a line
#' @param line Character string
#' @return Single numeric value or NA
extract_last_numeric <- function(line) {
  values <- extract_numeric_values(line)
  if (length(values) > 0) {
    return(values[length(values)])
  }
  return(NA)
}

#' Compare frequency tables between SPSS and R
#' @param spss_table SPSS frequency table from parser
#' @param r_result R frequency result object
#' @param tolerance Numeric tolerance for comparisons
#' @return Comparison data frame
compare_frequency_tables <- function(spss_table, r_result, tolerance = 0.01) {
  
  if (is.null(spss_table) || is.null(r_result)) {
    warning("Missing table data for comparison")
    return(NULL)
  }
  
  comparisons <- list()
  
  # Extract R frequency data
  # This depends on the structure of the R frequency_results object
  r_freq_data <- r_result$results
  
  # For each row in SPSS table
  for (i in seq_along(spss_table$frequencies)) {
    spss_row <- spss_table$frequencies[[i]]
    
    # Find corresponding R row by value
    r_row_idx <- which(r_freq_data$value == spss_row$value)
    
    if (length(r_row_idx) > 0) {
      r_row <- r_freq_data[r_row_idx[1], ]
      
      # Compare frequencies
      comp <- data.frame(
        Value = spss_row$value,
        Label = spss_row$label,
        Metric = c("Frequency", "Percent", "Valid %", "Cumulative %"),
        SPSS = c(
          spss_row$frequency,
          spss_row$percent,
          spss_row$valid_percent,
          spss_row$cumulative_percent
        ),
        R = c(
          r_row$n,
          r_row$percent,
          r_row$valid_percent,
          r_row$cumulative
        ),
        stringsAsFactors = FALSE
      )
      
      comp$Difference <- abs(comp$SPSS - comp$R)
      comp$Pass <- comp$Difference <= tolerance
      comp$Status <- ifelse(comp$Pass, "✅ PASS", "❌ FAIL")
      
      comparisons[[length(comparisons) + 1]] <- comp
    }
  }
  
  # Combine all comparisons
  if (length(comparisons) > 0) {
    return(do.call(rbind, comparisons))
  }
  
  return(NULL)
}

#' Debug helper to show frequency table structure
#' @param file_path Path to SPSS output file
#' @param test_identifier Test marker
#' @param show_lines Number of lines to show
debug_frequency_table <- function(file_path, test_identifier = "TEST 1", show_lines = 50) {
  lines <- readLines(file_path, warn = FALSE)
  
  test_idx <- grep(test_identifier, lines, fixed = TRUE)[1]
  
  if (!is.na(test_idx)) {
    cat(sprintf("Test '%s' found at line %d\n\n", test_identifier, test_idx))
    
    end_idx <- min(test_idx + show_lines, length(lines))
    
    for (i in test_idx:end_idx) {
      # Highlight important lines
      if (grepl("Frequency|Percent|Total|Statistics", lines[i], ignore.case = TRUE)) {
        cat(sprintf("[%4d] >>> %s <<<\n", i, lines[i]))
      } else {
        cat(sprintf("[%4d] %s\n", i, lines[i]))
      }
    }
  } else {
    cat(sprintf("Test '%s' not found\n", test_identifier))
  }
}