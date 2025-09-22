# ============================================================================
# SPSS Output Parser for Test Validation
# ============================================================================
#
# Purpose: Parse SPSS output text files to extract statistical values
#          for automated test validation
#
# This helper file provides functions to read and parse SPSS output files
# to extract statistical values for comparison with R results
# ============================================================================

#' Parse SPSS DESCRIPTIVES output table
#'
#' Extracts statistics from SPSS DESCRIPTIVES command output
#' 
#' @param lines Character vector of lines from SPSS output
#' @param start_pattern Pattern to identify start of table
#' @return Named list of statistics
parse_spss_descriptives <- function(lines, start_pattern = "Descriptive Statistics") {
  # Find the table
  start_idx <- grep(start_pattern, lines)[1]
  if (is.na(start_idx)) return(NULL)
  
  # Find the data line (usually contains the variable name)
  # Look for lines with numeric values
  data_lines <- lines[(start_idx + 5):(start_idx + 20)]
  
  # Parse the statistics line (contains the actual values)
  # SPSS format: Variable | N | Range | Min | Max | Mean | SE | SD | Var | Skew | SE | Kurt | SE
  stats_line <- NULL
  for (line in data_lines) {
    # Look for a line with multiple numeric values
    if (grepl("\\d+\\.\\d+.*\\d+\\.\\d+", line)) {
      stats_line <- line
      break
    }
  }
  
  if (is.null(stats_line)) return(NULL)
  
  # Extract numeric values from the line
  # IMPORTANT: Use improved regex that handles decimal-only numbers like .819
  # OLD: "-?\\d+\\.?\\d*" required digit before decimal, failed on .819
  # NEW: "-?\\d*\\.?\\d+" allows decimal to come first
  values <- unlist(regmatches(stats_line, gregexpr("-?\\d*\\.?\\d+", stats_line)))
  values <- as.numeric(values)
  
  # Map to standard names based on SPSS DESCRIPTIVES order
  # Typical order: N, Range, Min, Max, Mean, SE, SD, Var, Skew, SE_Skew, Kurt, SE_Kurt
  result <- list()
  if (length(values) >= 12) {
    result$n <- values[1]
    result$range <- values[2]
    result$min <- values[3]
    result$max <- values[4]
    result$mean <- values[5]
    result$se <- values[6]
    result$sd <- values[7]
    result$variance <- values[8]
    result$skewness <- values[9]
    # Skip SE of skewness (values[10])
    result$kurtosis <- values[11]
    # Skip SE of kurtosis (values[12])
  }
  
  return(result)
}

#' Parse SPSS FREQUENCIES output table
#'
#' Extracts statistics from SPSS FREQUENCIES command output
#' 
#' @param lines Character vector of lines from SPSS output
#' @param variable Variable name to look for
#' @return Named list of statistics
parse_spss_frequencies <- function(lines, variable = "age") {
  # Find Statistics table
  stats_start <- grep("^Statistics", lines)[1]
  if (is.na(stats_start)) return(NULL)
  
  # Extract the table (usually next 30-40 lines)
  table_lines <- lines[stats_start:(stats_start + 40)]
  
  # Initialize result
  result <- list()
  
  # Parse specific statistics
  patterns <- list(
    n_valid = "N\\s+Valid\\s+(\\d+)",
    n_missing = "N\\s+Missing\\s+(\\d+)",
    mean = "Mean\\s+([\\d.]+)",
    se_mean = "Std\\.\\s+Error\\s+of\\s+Mean\\s+([\\d.]+)",
    mode = "Mode\\s+([\\d.]+)",
    sd = "Std\\.\\s+Deviation\\s+([\\d.]+)",
    variance = "Variance\\s+([\\d.]+)",
    skewness = "Skewness\\s+([\\d.-]+)",
    kurtosis = "Kurtosis\\s+([\\d.-]+)",
    range = "Range\\s+([\\d.]+)",
    minimum = "Minimum\\s+([\\d.]+)",
    maximum = "Maximum\\s+([\\d.]+)"
  )
  
  for (stat_name in names(patterns)) {
    for (line in table_lines) {
      if (grepl(patterns[[stat_name]], line, ignore.case = TRUE)) {
        match <- regmatches(line, regexec(patterns[[stat_name]], line, ignore.case = TRUE))[[1]]
        if (length(match) > 1) {
          result[[stat_name]] <- as.numeric(match[2])
          break
        }
      }
    }
  }
  
  # Parse percentiles
  percentile_lines <- grep("Percentiles", table_lines)
  if (length(percentile_lines) > 0) {
    # Look for lines with percentile values
    for (i in (percentile_lines[1] + 1):min(percentile_lines[1] + 10, length(table_lines))) {
      line <- table_lines[i]
      if (grepl("25\\s+([\\d.]+)", line)) {
        result$q25 <- as.numeric(regmatches(line, regexec("25\\s+([\\d.]+)", line))[[1]][2])
      }
      if (grepl("50\\s+([\\d.]+)", line)) {
        result$median <- as.numeric(regmatches(line, regexec("50\\s+([\\d.]+)", line))[[1]][2])
      }
      if (grepl("75\\s+([\\d.]+)", line)) {
        result$q75 <- as.numeric(regmatches(line, regexec("75\\s+([\\d.]+)", line))[[1]][2])
      }
    }
  }
  
  return(result)
}

#' Parse specific test section from SPSS output
#'
#' Extracts statistics from a specific test section
#' 
#' @param file_path Path to SPSS output file
#' @param test_name Name of the test to extract (e.g., "TEST 1")
#' @return Named list of statistics
parse_spss_test_section <- function(file_path, test_name) {
  lines <- readLines(file_path, warn = FALSE)
  
  # Find the test section
  test_pattern <- paste0("====.*", test_name, ".*====")
  test_start <- grep(test_pattern, lines)[1]
  
  if (is.na(test_start)) {
    warning(paste("Test section not found:", test_name))
    return(NULL)
  }
  
  # Find the next test section or end of relevant output
  next_test <- grep("====.*TEST.*====", lines[(test_start + 1):length(lines)])[1]
  if (is.na(next_test)) {
    test_end <- min(test_start + 200, length(lines))
  } else {
    test_end <- test_start + next_test - 1
  }
  
  # Extract the section
  section_lines <- lines[test_start:test_end]
  
  # Determine the type of output and parse accordingly
  result <- NULL
  if (any(grepl("Descriptive Statistics", section_lines))) {
    result <- parse_spss_descriptives(section_lines)
  } else if (any(grepl("^Statistics", section_lines))) {
    result <- parse_spss_frequencies(section_lines)
  }
  
  return(result)
}

#' Extract grouped statistics from SPSS output
#'
#' Parses grouped analysis results (e.g., by region)
#' 
#' @param file_path Path to SPSS output file
#' @param group_var Grouping variable name
#' @param variable Variable to extract stats for
#' @return List with statistics for each group
parse_spss_grouped <- function(file_path, group_var = "Region", variable = "age") {
  lines <- readLines(file_path, warn = FALSE)
  
  # Find the table with grouped statistics
  # Look for "Region" followed by grouped data in SPSS format
  region_start <- grep("^\\s*Region\\s+N\\s+Range", lines)[1]
  
  if (is.na(region_start)) {
    # Alternative: look for split file output
    region_start <- grep("Region.*Ost/West.*N.*Range", lines)[1]
  }
  
  if (is.na(region_start)) return(NULL)
  
  result <- list()
  
  # Parse the lines following the header
  # SPSS groups data with group labels in first column
  for (i in (region_start + 3):(region_start + 50)) {
    if (i > length(lines)) break
    line <- lines[i]
    
    # Check for East group with age variable
    if (grepl("^\\s*East\\s+Alter", line)) {
      # Next line or same line might have the statistics
      stat_line <- line
      # Clean and extract values
      clean_line <- gsub("\\s+\\.", " 0.", stat_line)
      clean_line <- gsub("-\\.", "-0.", clean_line)
      
      # Extract all numeric values
      numbers <- as.numeric(unlist(regmatches(clean_line, 
                                             gregexpr("-?\\d+\\.?\\d*", clean_line))))
      
      if (length(numbers) >= 8) {
        result[["East"]] <- list(
          n = numbers[1],
          range = numbers[2],
          min = numbers[3],
          max = numbers[4],
          mean = numbers[5],
          se = numbers[6],
          sd = numbers[7],
          variance = numbers[8]
        )
      }
    }
    
    # Check for West group with age variable
    if (grepl("^\\s*West\\s+Alter", line)) {
      stat_line <- line
      # Clean and extract values
      clean_line <- gsub("\\s+\\.", " 0.", stat_line)
      clean_line <- gsub("-\\.", "-0.", clean_line)
      
      # Extract all numeric values
      numbers <- as.numeric(unlist(regmatches(clean_line, 
                                             gregexpr("-?\\d+\\.?\\d*", clean_line))))
      
      if (length(numbers) >= 8) {
        result[["West"]] <- list(
          n = numbers[1],
          range = numbers[2],
          min = numbers[3],
          max = numbers[4],
          mean = numbers[5],
          se = numbers[6],
          sd = numbers[7],
          variance = numbers[8]
        )
      }
    }
  }
  
  return(result)
}

#' Load SPSS reference values for a specific test
#'
#' Main function to load SPSS values for comparison
#' 
#' @param test_name Name of the test (e.g., "TEST 1: UNWEIGHTED SINGLE VARIABLE")
#' @param output_file Name of SPSS output file (default: "describe_basic_output.txt")
#' @return Named list of SPSS reference values
load_spss_reference <- function(test_name, 
                                output_file = "describe_basic_output.txt") {
  
  # Construct full path
  base_dir <- system.file("tests", "spss_reference", "outputs", "describe", 
                          package = "SurveyStat")
  if (base_dir == "") {
    # If not installed, try local path
    base_dir <- "tests/spss_reference/outputs/describe"
  }
  
  file_path <- file.path(base_dir, output_file)
  
  if (!file.exists(file_path)) {
    # Try alternative path
    file_path <- file.path("tests/spss_reference/outputs", output_file)
  }
  
  if (!file.exists(file_path)) {
    stop(paste("SPSS output file not found:", output_file))
  }
  
  # Parse the specific test section
  result <- parse_spss_test_section(file_path, test_name)
  
  return(result)
}

#' Extract SPSS values from raw output lines
#'
#' More robust parser for complex SPSS output
#' 
#' @param file_path Path to SPSS output file
#' @param test_number Test number to extract
#' @param variable Variable name to look for
#' @return Named list of statistics
extract_spss_values <- function(file_path, test_number, variable = "age") {
  lines <- readLines(file_path, warn = FALSE)
  
  # Find the test section
  test_pattern <- paste0("TEST ", test_number, ":")
  test_start <- grep(test_pattern, lines)[1]
  
  if (is.na(test_start)) return(NULL)
  
  # Look for the Descriptive Statistics table
  desc_start <- grep("Descriptive Statistics", lines[test_start:length(lines)])[1]
  if (!is.na(desc_start)) {
    desc_start <- test_start + desc_start - 1
    
    # Find the data row (contains variable name and statistics)
    # Look for the row with the variable name
    for (i in (desc_start + 4):(desc_start + 20)) {
      if (i > length(lines)) break
      
      line <- lines[i]
      # Check if this line contains our variable (or starts with numbers for age)
      if (grepl("Alter|Jahren|age", line, ignore.case = TRUE) || 
          grepl("^\\s*\\d+", line)) {
        
        # Need to handle SPSS format more carefully
        # SPSS uses spaces to separate columns but decimals can be confusing
        # Example line: " Alter in  2500      77.00     18.00     95.00     50.5496   .33952   16.97602  288.185   .172      .049       -.364     .098"
        
        # CRITICAL CLEANING STEPS FOR SPSS DECIMAL FORMATS:
        # SPSS outputs decimals without leading zeros (e.g., .364 instead of 0.364)
        # This causes parsing issues if not handled properly
        
        # Step 1: Replace standalone dots with 0 for proper parsing
        clean_line <- gsub("\\s+\\.", " 0.", line)    # " .364" → " 0.364"
        clean_line <- gsub("^\\.", "0.", clean_line)  # ".364" at line start → "0.364"
        
        # Step 2: Handle negative decimals like -.364
        clean_line <- gsub("-\\.", "-0.", clean_line)  # "-.364" → "-0.364"
        
        # Step 3: Extract all numbers using standard regex
        # Note: After cleaning, we can use standard pattern
        numbers <- as.numeric(unlist(regmatches(clean_line, 
                                               gregexpr("-?\\d+\\.?\\d*", clean_line))))
        
        if (length(numbers) >= 10) {
          return(list(
            n = numbers[1],
            range = numbers[2],
            min = numbers[3],
            max = numbers[4],
            mean = numbers[5],
            se = numbers[6],
            sd = numbers[7],
            variance = numbers[8],
            skewness = numbers[9],
            kurtosis = numbers[11]  # Skip SE of skewness at position 10
          ))
        }
      }
    }
  }
  
  return(NULL)
}

#' Parse SPSS Independent Samples T-Test output
#'
#' Extracts statistics from SPSS T-TEST command output
#' 
#' @param lines Character vector of lines from SPSS output
#' @param start_idx Starting index to search from
#' @return Named list of statistics
parse_spss_ttest_independent <- function(lines, start_idx = 1) {
  # IMPORTANT: This function handles complex SPSS t-test output where
  # variable names and "Equal variances assumed" text appear before numbers
  # Key fix: Split by text markers before extracting numbers
  
  result <- list()
  
  # First find Levene's Test in separate table
  levene_start <- grep("Levene's Test for Equality", lines[start_idx:length(lines)])[1]
  if (!is.na(levene_start)) {
    levene_start <- start_idx + levene_start - 1
    # Look for the data line with F and Sig values (2-3 lines down)
    for (i in (levene_start + 2):(levene_start + 5)) {
      if (i > length(lines)) break
      line <- lines[i]
      if (grepl("Equal variances assumed", line) && grepl("\\d+\\.\\d+", line)) {
        # Clean and extract just the numeric values
        clean_line <- gsub("\\s+", " ", line)
        clean_line <- gsub("\\s+\\.", " 0.", clean_line)
        clean_line <- gsub("^\\.", "0.", clean_line)
        # Extract all numbers
        numbers <- as.numeric(unlist(regmatches(clean_line, 
                                               gregexpr("-?\\d+\\.?\\d*", clean_line))))
        # The last 2 numbers should be F and Sig
        if (length(numbers) >= 2) {
          # Take the last 2 numbers as they are F and Sig
          n <- length(numbers)
          result$levene_test <- list(
            F_statistic = numbers[n-1],
            p_value = numbers[n]
          )
        }
        break
      }
    }
  }
  
  # Find the Independent Samples Test table for t-test results
  test_start <- grep("Independent Samples Test", lines[start_idx:length(lines)])[1]
  if (is.na(test_start)) return(NULL)
  test_start <- start_idx + test_start - 1
  
  # Find t-test results (Equal variances assumed line)
  for (i in (test_start + 3):(test_start + 15)) {
    if (i > length(lines)) break
    if (grepl("Equal variances assumed", lines[i])) {
      equal_line <- lines[i]
      # Clean the line
      clean_line <- gsub("\\s+", " ", equal_line)
      clean_line <- gsub("\\s+\\.", " 0.", clean_line)
      clean_line <- gsub("-\\.", "-0.", clean_line)
      
      # Extract numbers - Now just t-test values without Levene's
      # Format: t, df, one-sided p, two-sided p, mean diff, SE, CI lower, CI upper
      numbers <- as.numeric(unlist(regmatches(clean_line, 
                                             gregexpr("-?\\d+\\.?\\d*", clean_line))))
      
      if (length(numbers) >= 6) {
        result$equal_variances <- list(
          t_stat = numbers[1],
          df = numbers[2],
          p_value_one = numbers[3],
          p_value = numbers[4],  # two-sided p-value
          mean_diff = numbers[5],
          se_diff = numbers[6],
          ci_lower = if(length(numbers) >= 7) numbers[7] else NA,
          ci_upper = if(length(numbers) >= 8) numbers[8] else NA
        )
      }
      
      # Parse unequal variances line (usually next or 2 lines down due to wrapping)
      for (j in 1:3) {
        if ((i + j) > length(lines)) break
        if (grepl("Equal variances not assumed", lines[i + j])) {
          unequal_line <- lines[i + j]
          clean_line <- gsub("\\s+", " ", unequal_line)
          clean_line <- gsub("\\s+\\.", " 0.", clean_line)
          clean_line <- gsub("-\\.", "-0.", clean_line)
          
          numbers <- as.numeric(unlist(regmatches(clean_line, 
                                                 gregexpr("-?\\d+\\.?\\d*", clean_line))))
          
          if (length(numbers) >= 6) {
            result$unequal_variances <- list(
              t_stat = numbers[1],
              df = numbers[2],
              p_value_one = numbers[3],
              p_value = numbers[4],  # two-sided p-value
              mean_diff = numbers[5],
              se_diff = numbers[6],
              ci_lower = if(length(numbers) >= 7) numbers[7] else NA,
              ci_upper = if(length(numbers) >= 8) numbers[8] else NA
            )
          }
          break
        }
      }
      break
    }
  }
  
  return(result)
}

#' Parse SPSS One-Sample T-Test output
#'
#' Extracts statistics from SPSS one-sample T-TEST output
#' 
#' @param lines Character vector of lines from SPSS output
#' @param start_idx Starting index to search from
#' @return Named list of statistics
parse_spss_ttest_onesample <- function(lines, start_idx = 1) {
  result <- list()
  
  # Find the One-Sample Test table
  test_start <- grep("One-Sample Test", lines[start_idx:length(lines)])[1]
  if (is.na(test_start)) return(NULL)
  test_start <- start_idx + test_start - 1
  
  # Look for data lines (usually 4-6 lines after header)
  for (i in (test_start + 4):(test_start + 15)) {
    if (i > length(lines)) break
    line <- lines[i]
    
    # Look for lines with numeric values
    if (grepl("\\d+\\.\\d+", line)) {
      # Clean the line
      clean_line <- gsub("\\s+", " ", line)
      clean_line <- gsub("\\s+\\.", " 0.", clean_line)
      clean_line <- gsub("-\\.", "-0.", clean_line)
      
      # Extract variable name (first non-numeric element)
      var_name <- trimws(gsub("^\\s*([A-Za-z_]+).*", "\\1", line))
      
      # Extract numbers
      numbers <- as.numeric(unlist(regmatches(clean_line, 
                                             gregexpr("-?\\d+\\.?\\d*", clean_line))))
      
      if (length(numbers) >= 6) {
        result[[var_name]] <- list(
          t_stat = numbers[1],
          df = numbers[2],
          p_value = numbers[3],
          mean_diff = numbers[4],
          ci_lower = numbers[5],
          ci_upper = numbers[6]
        )
      }
    }
  }
  
  return(result)
}

#' Parse SPSS Group Statistics table
#'
#' Extracts group means and SDs for effect size calculations
#' 
#' @param lines Character vector of lines from SPSS output
#' @param start_idx Starting index to search from
#' @return Named list of group statistics
parse_spss_group_statistics <- function(lines, start_idx = 1) {
  result <- list()
  
  # Find the Group Statistics table
  stats_start <- grep("Group Statistics", lines[start_idx:length(lines)])[1]
  if (is.na(stats_start)) return(NULL)
  stats_start <- start_idx + stats_start - 1
  
  # Parse the table - look for data lines
  current_var <- NULL
  
  for (i in (stats_start + 4):(stats_start + 30)) {
    if (i > length(lines)) break
    line <- lines[i]
    
    # Skip empty lines
    if (nchar(trimws(line)) == 0) next
    
    # Check if this is a variable name line or data line
    if (grepl("^[A-Za-z]", trimws(line))) {
      # Extract variable name
      current_var <- trimws(strsplit(line, "\\s{2,}")[[1]][1])
      
      # This line might also contain group 1 data
      if (grepl("\\d+\\.\\d+", line)) {
        clean_line <- gsub("\\s+", " ", line)
        clean_line <- gsub("\\s+\\.", " 0.", clean_line)
        clean_line <- gsub("-\\.", "-0.", clean_line)
        
        # Extract group label and numbers
        parts <- strsplit(clean_line, "\\s{2,}")[[1]]
        if (length(parts) >= 2) {
          group_label <- trimws(parts[2])
          numbers <- as.numeric(unlist(regmatches(clean_line, 
                                                 gregexpr("-?\\d+\\.?\\d*", clean_line))))
          
          if (length(numbers) >= 4) {
            if (!current_var %in% names(result)) {
              result[[current_var]] <- list()
            }
            result[[current_var]]$group1 <- list(
              label = group_label,
              n = numbers[1],
              mean = numbers[2],
              sd = numbers[3],
              se = numbers[4]
            )
          }
        }
      }
    } else if (!is.null(current_var) && grepl("\\d+", line)) {
      # This is a group 2 data line
      clean_line <- gsub("\\s+", " ", line)
      clean_line <- gsub("\\s+\\.", " 0.", clean_line)
      clean_line <- gsub("-\\.", "-0.", clean_line)
      
      # Extract group label and numbers
      parts <- trimws(strsplit(line, "\\s{2,}")[[1]])
      if (length(parts) >= 1) {
        group_label <- parts[1]
        numbers <- as.numeric(unlist(regmatches(clean_line, 
                                               gregexpr("-?\\d+\\.?\\d*", clean_line))))
        
        if (length(numbers) >= 4) {
          if (!current_var %in% names(result)) {
            result[[current_var]] <- list()
          }
          result[[current_var]]$group2 <- list(
            label = group_label,
            n = numbers[1],
            mean = numbers[2],
            sd = numbers[3],
            se = numbers[4]
          )
        }
      }
    }
  }
  
  return(result)
}

#' Extract T-Test values from SPSS output file
#'
#' Main function to extract t-test statistics from SPSS output
#' 
#' @param file_path Path to SPSS output file
#' @param test_number Test number to extract
#' @param test_type Type of test ("independent", "onesample")
#' @return Named list of statistics
extract_spss_ttest_values <- function(file_path, test_number, test_type = "independent") {
  lines <- readLines(file_path, warn = FALSE)
  
  # Find the test section
  test_pattern <- paste0("TEST ", test_number, ":")
  test_start <- grep(test_pattern, lines)[1]
  
  if (is.na(test_start)) return(NULL)
  
  # Determine end of test section
  next_test <- grep("TEST \\d+:", lines[(test_start + 1):length(lines)])[1]
  if (!is.na(next_test)) {
    test_end <- test_start + next_test - 1
  } else {
    test_end <- min(test_start + 200, length(lines))
  }
  
  result <- list()
  
  # Parse based on test type
  if (test_type == "independent") {
    # Get group statistics first
    result$group_stats <- parse_spss_group_statistics(lines[test_start:test_end], 1)
    
    # Get t-test results using the new fixed parser
    parsed <- parse_spss_ttest_independent(lines[test_start:test_end], 1)
    # Restructure to match old format for compatibility
    result$test_stats <- list(
      equal_variances = parsed$equal_variances,
      unequal_variances = parsed$unequal_variances,
      levene_test = parsed$levene_test
    )
    
  } else if (test_type == "onesample") {
    # Get one-sample test results
    result$test_stats <- parse_spss_ttest_onesample(lines[test_start:test_end], 1)
  }
  
  return(result)
}

#' Calculate effect sizes from group statistics
#'
#' Calculates Cohen's d, Hedges' g, and Glass' Delta
#' 
#' @param group1_mean Mean of group 1
#' @param group1_sd SD of group 1
#' @param group1_n N of group 1
#' @param group2_mean Mean of group 2
#' @param group2_sd SD of group 2
#' @param group2_n N of group 2
#' @return Named list of effect sizes
calculate_effect_sizes <- function(group1_mean, group1_sd, group1_n,
                                 group2_mean, group2_sd, group2_n) {
  # Cohen's d with pooled SD
  pooled_sd <- sqrt(((group1_n - 1) * group1_sd^2 + (group2_n - 1) * group2_sd^2) / 
                   (group1_n + group2_n - 2))
  cohens_d <- (group1_mean - group2_mean) / pooled_sd
  
  # Hedges' g (bias correction)
  hedges_j <- 1 - (3 / (4 * (group1_n + group2_n - 2) - 1))
  hedges_g <- cohens_d * hedges_j
  
  # Glass' Delta (uses first group SD only)
  glass_delta <- (group1_mean - group2_mean) / group1_sd
  
  return(list(
    cohens_d = cohens_d,
    hedges_g = hedges_g,
    glass_delta = glass_delta
  ))
}

# Helper function to extract values from a SPSS output line
# This is used by the test file directly
extract_values_from_line <- function(line, var_name = NULL) {
  if (is.na(line) || is.null(line)) return(NULL)
  
  # Clean the line for parsing
  clean_line <- gsub("\\s+\\.", " 0.", line)
  clean_line <- gsub("^\\.", "0.", clean_line)
  clean_line <- gsub("-\\.", "-0.", clean_line)
  
  # Extract all numeric values from the line
  numbers <- as.numeric(unlist(regmatches(clean_line, 
                                         gregexpr("-?\\d+\\.?\\d*", clean_line))))
  
  if (length(numbers) >= 8) {
    # Standard SPSS DESCRIPTIVES format
    return(list(
      n = numbers[1],
      range = numbers[2], 
      min = numbers[3],
      max = numbers[4],
      mean = numbers[5],
      se = numbers[6],
      sd = numbers[7],
      variance = numbers[8],
      skewness = if(length(numbers) > 8) numbers[9] else NA,
      kurtosis = if(length(numbers) > 10) numbers[11] else NA
    ))
  }
  
  return(NULL)
}

# ============================================================================
# Chi-Squared Test Parsing Functions
# ============================================================================

#' Parse SPSS Chi-Square Tests output
#' 
#' Extracts chi-square statistic, df, p-value, and effect sizes from SPSS CROSSTABS output
#' 
#' @param lines Character vector of lines from SPSS output
#' @param start_idx Starting index for search
#' @return Named list of statistics
parse_spss_chi_squared <- function(lines, start_idx = 1) {
  result <- list()
  
  # Find Chi-Square Tests table
  chi_start <- grep("Chi-Square Tests", lines[start_idx:length(lines)])[1]
  if (is.na(chi_start)) return(NULL)
  chi_start <- start_idx + chi_start - 1
  
  # Look for Pearson Chi-Square line (usually 2-3 lines after header)
  for (i in (chi_start + 2):(chi_start + 10)) {
    if (i > length(lines)) break
    line <- lines[i]
    
    if (grepl("Pearson Chi-Square", line)) {
      # Clean and extract numbers
      clean_line <- gsub("\\(.*?\\)", "", line)  # Remove footnote markers like (a)
      
      # Extract all numbers from the line
      numbers <- as.numeric(unlist(regmatches(clean_line, 
                                             gregexpr("-?\\d*\\.?\\d+", clean_line))))
      
      if (length(numbers) >= 3) {
        result$chi_squared <- numbers[1]
        result$df <- numbers[2]
        result$p_value <- numbers[3]
      }
      break
    }
  }
  
  # Find Symmetric Measures table for effect sizes
  sym_start <- grep("Symmetric Measures", lines[chi_start:length(lines)])[1]
  if (!is.na(sym_start)) {
    sym_start <- chi_start + sym_start - 1
    
    # Look for Phi, Cramer's V, and Contingency Coefficient
    for (i in (sym_start + 2):(sym_start + 20)) {
      if (i > length(lines)) break
      line <- lines[i]
      
      if (grepl("^\\s*Nominal\\s+Phi", line) || grepl("^Phi", line)) {
        numbers <- as.numeric(unlist(regmatches(line, 
                                               gregexpr("-?\\d*\\.?\\d+", line))))
        if (length(numbers) >= 1) {
          result$phi <- numbers[1]
        }
      } else if (grepl("Cramer's V", line)) {
        numbers <- as.numeric(unlist(regmatches(line, 
                                               gregexpr("-?\\d*\\.?\\d+", line))))
        if (length(numbers) >= 1) {
          result$cramers_v <- numbers[1]
        }
      } else if (grepl("Contingency Coefficient", line)) {
        numbers <- as.numeric(unlist(regmatches(line, 
                                               gregexpr("-?\\d*\\.?\\d+", line))))
        if (length(numbers) >= 1) {
          result$contingency_c <- numbers[1]
        }
      }
    }
  }
  
  return(result)
}

#' Parse SPSS Crosstabulation table
#' 
#' Extracts observed and expected frequencies from SPSS crosstabulation
#' 
#' @param lines Character vector of lines from SPSS output  
#' @param start_idx Starting index for search
#' @return Named list with observed and expected frequencies
parse_spss_crosstab <- function(lines, start_idx = 1) {
  result <- list()
  
  # Find Crosstabulation table
  cross_start <- grep("Crosstabulation", lines[start_idx:length(lines)])[1]
  if (is.na(cross_start)) return(NULL)
  cross_start <- start_idx + cross_start - 1
  
  # Initialize matrices for observed and expected
  observed <- list()
  expected <- list()
  
  # Parse the table rows
  # Look for Count and Expected Count lines
  for (i in (cross_start + 2):(cross_start + 50)) {
    if (i > length(lines)) break
    line <- lines[i]
    
    if (grepl("Count", line) && !grepl("Expected", line)) {
      # This is an observed count line
      numbers <- as.numeric(unlist(regmatches(line, 
                                             gregexpr("-?\\d*\\.?\\d+", line))))
      if (length(numbers) > 0) {
        observed[[length(observed) + 1]] <- numbers
      }
    } else if (grepl("Expected Count", line)) {
      # This is an expected count line  
      numbers <- as.numeric(unlist(regmatches(line, 
                                             gregexpr("-?\\d*\\.?\\d+", line))))
      if (length(numbers) > 0) {
        expected[[length(expected) + 1]] <- numbers
      }
    }
    
    # Stop at Total line
    if (grepl("^\\s*Total", line)) break
  }
  
  result$observed <- observed
  result$expected <- expected
  
  return(result)
}

#' Extract chi-squared test values from SPSS output file
#' 
#' Main function to extract chi-squared statistics from SPSS output
#' 
#' @param file_path Path to SPSS output file
#' @param test_number Test number to extract
#' @return Named list of statistics
extract_spss_chi_squared_values <- function(file_path, test_number) {
  lines <- readLines(file_path, warn = FALSE)
  
  # Find the test section
  test_pattern <- paste0("TEST ", test_number, ":")
  test_start <- grep(test_pattern, lines)[1]
  
  if (is.na(test_start)) return(NULL)
  
  # Determine end of test section
  next_test <- grep("TEST \\d+:", lines[(test_start + 1):length(lines)])[1]
  if (!is.na(next_test)) {
    test_end <- test_start + next_test - 1
  } else {
    test_end <- min(test_start + 200, length(lines))
  }
  
  result <- list()
  
  # Get chi-squared test results
  chi_results <- parse_spss_chi_squared(lines[test_start:test_end])
  if (!is.null(chi_results)) {
    result <- c(result, chi_results)
  }
  
  # Get crosstabulation (observed/expected frequencies)
  cross_results <- parse_spss_crosstab(lines[test_start:test_end])
  if (!is.null(cross_results)) {
    result$crosstab <- cross_results
  }
  
  # Extract total N from Case Processing Summary
  case_start <- grep("Case Processing Summary", lines[test_start:test_end])[1]
  if (!is.na(case_start)) {
    case_start <- test_start + case_start - 1
    # Look for N in Valid column
    for (i in (case_start + 3):(case_start + 10)) {
      if (i > length(lines)) break
      line <- lines[i]
      numbers <- as.numeric(unlist(regmatches(line, 
                                             gregexpr("\\d+", line))))
      if (length(numbers) > 0) {
        result$n <- numbers[1]
        break
      }
    }
  }
  
  return(result)
}

# ============================================================================
# ONEWAY ANOVA Parser Functions
# ============================================================================

#' Parse SPSS ONEWAY ANOVA output
#'
#' Extracts statistics from SPSS ONEWAY command output
#' 
#' @param file_path Path to SPSS output file
#' @param test_number Test number to extract
#' @return Named list of ANOVA statistics
parse_spss_oneway <- function(file_path, test_number) {
  lines <- readLines(file_path, warn = FALSE)
  
  # Find test section
  test_pattern <- paste0("TEST ", test_number, ":")
  test_start <- grep(test_pattern, lines)[1]
  
  if (is.na(test_start)) return(NULL)
  
  # Find next test or use reasonable end
  next_test <- grep("TEST \\d+:", lines[(test_start + 1):length(lines)])[1]
  if (!is.na(next_test)) {
    test_end <- test_start + next_test - 1
  } else {
    test_end <- min(test_start + 300, length(lines))
  }
  
  test_lines <- lines[test_start:test_end]
  result <- list()
  
  # Parse Descriptives table for group statistics
  desc_start <- grep("^Descriptives", test_lines)[1]
  if (!is.na(desc_start)) {
    # Find the data lines (skip header lines)
    # Look for lines with group names and statistics
    for (i in (desc_start + 4):(desc_start + 20)) {
      if (i > length(test_lines)) break
      line <- test_lines[i]
      
      # Check if this is a group line (starts with a word, not "Total")
      if (grepl("^\\s*[A-Za-z]", line) && !grepl("^\\s*Total", line)) {
        # Extract numbers from the line
        numbers <- as.numeric(unlist(regmatches(line, 
                                               gregexpr("-?\\d*\\.?\\d+", line))))
        
        # Extract group name (first word(s) before numbers)
        group_name <- trimws(sub("\\s+\\d+.*", "", line))
        
        if (length(numbers) >= 3) {
          # Store group statistics
          # Typical order: N, Mean, SD, SE, CI_lower, CI_upper, Min, Max
          group_key <- paste0("group_", group_name)
          result[[group_key]] <- list(
            name = group_name,
            n = numbers[1],
            mean = numbers[2],
            sd = numbers[3],
            se = if(length(numbers) >= 4) numbers[4] else NA
          )
        }
      } else if (grepl("^\\s*Total", line)) {
        # Parse total line
        numbers <- as.numeric(unlist(regmatches(line, 
                                               gregexpr("-?\\d*\\.?\\d+", line))))
        if (length(numbers) >= 3) {
          result$total <- list(
            n = numbers[1],
            mean = numbers[2],
            sd = numbers[3]
          )
        }
      }
    }
  }
  
  # Parse ANOVA table
  anova_start <- grep("^ANOVA", test_lines)[1]
  if (!is.na(anova_start)) {
    # Look for Between Groups line
    between_line <- grep("Between Groups", test_lines[anova_start:length(test_lines)])[1]
    if (!is.na(between_line)) {
      between_idx <- anova_start + between_line - 1
      line <- test_lines[between_idx]
      
      # Extract values: SS, df, MS, F, Sig
      numbers <- as.numeric(unlist(regmatches(line, 
                                             gregexpr("-?\\d*\\.?\\d+", line))))
      
      if (length(numbers) >= 5) {
        result$between_groups <- list(
          ss = numbers[1],
          df = numbers[2],
          ms = numbers[3],
          f_statistic = numbers[4],
          p_value = numbers[5]
        )
      }
    }
    
    # Look for Within Groups line
    within_line <- grep("Within Groups", test_lines[anova_start:length(test_lines)])[1]
    if (!is.na(within_line)) {
      within_idx <- anova_start + within_line - 1
      line <- test_lines[within_idx]
      
      numbers <- as.numeric(unlist(regmatches(line, 
                                             gregexpr("-?\\d*\\.?\\d+", line))))
      
      if (length(numbers) >= 3) {
        result$within_groups <- list(
          ss = numbers[1],
          df = numbers[2],
          ms = numbers[3]
        )
      }
    }
    
    # Look for Total line
    total_line <- grep("^\\s*Total", test_lines[anova_start:length(test_lines)])[1]
    if (!is.na(total_line)) {
      total_idx <- anova_start + total_line - 1
      line <- test_lines[total_idx]
      
      numbers <- as.numeric(unlist(regmatches(line, 
                                             gregexpr("-?\\d*\\.?\\d+", line))))
      
      if (length(numbers) >= 2) {
        result$total_ss <- numbers[1]
        result$total_df <- numbers[2]
      }
    }
  }
  
  # Parse Levene's Test
  levene_start <- grep("Tests of Homogeneity", test_lines)[1]
  if (!is.na(levene_start)) {
    # Look for "Based on Mean" line
    mean_line <- grep("Based on Mean", test_lines[levene_start:length(test_lines)])[1]
    if (!is.na(mean_line)) {
      mean_idx <- levene_start + mean_line - 1
      line <- test_lines[mean_idx]
      
      numbers <- as.numeric(unlist(regmatches(line, 
                                             gregexpr("-?\\d*\\.?\\d+", line))))
      
      if (length(numbers) >= 4) {
        result$levene <- list(
          statistic = numbers[1],
          df1 = numbers[2],
          df2 = numbers[3],
          p_value = numbers[4]
        )
      }
    }
  }
  
  # Parse Welch and Brown-Forsythe tests
  robust_start <- grep("Robust Tests of Equality", test_lines)[1]
  if (!is.na(robust_start)) {
    # Welch test
    welch_line <- grep("Welch", test_lines[robust_start:length(test_lines)])[1]
    if (!is.na(welch_line)) {
      welch_idx <- robust_start + welch_line - 1
      line <- test_lines[welch_idx]
      
      numbers <- as.numeric(unlist(regmatches(line, 
                                             gregexpr("-?\\d*\\.?\\d+", line))))
      
      if (length(numbers) >= 4) {
        result$welch <- list(
          statistic = numbers[1],
          df1 = numbers[2],
          df2 = numbers[3],
          p_value = numbers[4]
        )
      }
    }
    
    # Brown-Forsythe test
    bf_line <- grep("Brown-Forsythe", test_lines[robust_start:length(test_lines)])[1]
    if (!is.na(bf_line)) {
      bf_idx <- robust_start + bf_line - 1
      line <- test_lines[bf_idx]
      
      numbers <- as.numeric(unlist(regmatches(line, 
                                             gregexpr("-?\\d*\\.?\\d+", line))))
      
      if (length(numbers) >= 4) {
        result$brown_forsythe <- list(
          statistic = numbers[1],
          df1 = numbers[2],
          df2 = numbers[3],
          p_value = numbers[4]
        )
      }
    }
  }
  
  # Parse Post-Hoc tests (Tukey)
  posthoc_start <- grep("Multiple Comparisons", test_lines)[1]
  if (!is.na(posthoc_start)) {
    result$posthoc <- list()
    
    # Look for Tukey HSD results
    tukey_start <- grep("Tukey HSD", test_lines[posthoc_start:length(test_lines)])[1]
    if (!is.na(tukey_start)) {
      # Parse pairwise comparisons
      # This is complex as it involves multiple rows
      # For now, we'll just note that post-hoc tests are present
      result$posthoc$tukey_present <- TRUE
    }
  }
  
  return(result)
}

#' Extract ONEWAY ANOVA values for validation
#'
#' Wrapper function for SPSS ONEWAY validation tests
#' 
#' @param file_path Path to SPSS output file
#' @param test_number Test number to extract
#' @return Named list of statistics formatted for validation
extract_spss_oneway_values <- function(file_path, test_number) {
  raw_result <- parse_spss_oneway(file_path, test_number)
  
  if (is.null(raw_result)) return(NULL)
  
  # Format results for easy comparison
  result <- list(
    # Main ANOVA statistics
    f_statistic = raw_result$between_groups$f_statistic,
    df_between = raw_result$between_groups$df,
    df_within = raw_result$within_groups$df,
    p_value = raw_result$between_groups$p_value,
    
    # Sum of squares
    ss_between = raw_result$between_groups$ss,
    ss_within = raw_result$within_groups$ss,
    ss_total = raw_result$total_ss,
    
    # Mean squares
    ms_between = raw_result$between_groups$ms,
    ms_within = raw_result$within_groups$ms,
    
    # Levene's test
    levene_statistic = raw_result$levene$statistic,
    levene_p = raw_result$levene$p_value,
    
    # Robust tests (if present)
    welch_f = if (!is.null(raw_result$welch)) raw_result$welch$statistic else NA,
    welch_p = if (!is.null(raw_result$welch)) raw_result$welch$p_value else NA,
    brown_forsythe_f = if (!is.null(raw_result$brown_forsythe)) raw_result$brown_forsythe$statistic else NA,
    brown_forsythe_p = if (!is.null(raw_result$brown_forsythe)) raw_result$brown_forsythe$p_value else NA,
    
    # Group statistics
    groups = raw_result[grep("^group_", names(raw_result))],
    
    # Total statistics
    total_n = raw_result$total$n,
    total_mean = raw_result$total$mean,
    total_sd = raw_result$total$sd
  )
  
  return(result)
}

# ============================================================================
# FREQUENCY PARSING FUNCTIONS
# ============================================================================

#' Parse SPSS frequency output
#' 
#' @param lines Character vector of SPSS output lines
#' @param start_idx Starting index to search from
#' @return List with frequency statistics
parse_spss_frequency <- function(lines, start_idx = 1) {
  result <- list()
  
  # Find the Statistics table first
  stats_start <- grep("^Statistics", lines[start_idx:length(lines)])[1]
  if (!is.na(stats_start)) {
    stats_start <- start_idx + stats_start - 1
    
    # Extract N Valid and N Missing
    for (i in (stats_start + 1):(stats_start + 10)) {
      if (i > length(lines)) break
      line <- lines[i]
      
      if (grepl("N Valid", line)) {
        # Extract the number after "N Valid"
        numbers <- as.numeric(unlist(regmatches(line, 
                                               gregexpr("-?\\d*\\.?\\d+", line))))
        if (length(numbers) > 0) {
          result$n_valid <- numbers[1]
        }
      }
      
      if (grepl("Missing", line) && !grepl("Definition", line)) {
        # Extract the number for missing
        numbers <- as.numeric(unlist(regmatches(line, 
                                               gregexpr("-?\\d*\\.?\\d+", line))))
        if (length(numbers) > 0) {
          result$n_missing <- numbers[1]
        }
      }
      
      if (grepl("Mode", line)) {
        # Extract the mode value
        numbers <- as.numeric(unlist(regmatches(line, 
                                               gregexpr("-?\\d*\\.?\\d+", line))))
        if (length(numbers) > 0) {
          result$mode <- numbers[1]
        }
      }
    }
  }
  
  # Find the frequency table
  # Look for a line that starts with "Valid" followed by category names
  freq_start <- NA
  for (i in start_idx:length(lines)) {
    if (grepl("^\\s*Valid\\s+", lines[i]) && !grepl("Percent", lines[i])) {
      freq_start <- i
      break
    }
  }
  
  if (!is.na(freq_start)) {
    # Parse frequency table rows
    result$frequencies <- list()
    row_num <- 1
    
    for (i in freq_start:(min(freq_start + 20, length(lines)))) {
      line <- lines[i]
      
      # Skip header and total rows
      if (grepl("Frequency.*Percent", line) || grepl("^\\s*$", line)) next
      if (grepl("Total", line)) {
        # Parse total row
        numbers <- as.numeric(unlist(regmatches(line, 
                                               gregexpr("-?\\d*\\.?\\d+", line))))
        if (length(numbers) >= 2) {
          result$total_freq <- numbers[1]
          result$total_percent <- numbers[2]
        }
        break
      }
      
      # Parse data rows (category rows)
      if (grepl("Valid", line) || grepl("Male|Female|East|West", line)) {
        # Extract category name and numbers
        # Remove "Valid" prefix if present
        clean_line <- gsub("^\\s*Valid\\s+", "", line)
        
        # Split by multiple spaces to separate category from numbers
        parts <- strsplit(clean_line, "\\s{2,}")[[1]]
        
        if (length(parts) >= 2) {
          category <- trimws(parts[1])
          
          # Extract numbers from the remaining part
          numbers_part <- paste(parts[-1], collapse = " ")
          numbers <- as.numeric(unlist(regmatches(numbers_part, 
                                                 gregexpr("-?\\d*\\.?\\d+", numbers_part))))
          
          if (length(numbers) >= 4) {
            result$frequencies[[category]] <- list(
              frequency = numbers[1],
              percent = numbers[2],
              valid_percent = numbers[3],
              cumulative_percent = numbers[4]
            )
          }
        }
      }
    }
  }
  
  return(result)
}

#' Extract frequency values from SPSS output file for a specific test
#' 
#' @param file_path Path to SPSS output file
#' @param test_identifier Test identifier (e.g., "TEST 1.1", "TEST 2.3", or numeric)
#' @param variable Variable name (optional, for clarity)
#' @return List with parsed frequency statistics
extract_spss_frequency_values <- function(file_path, test_identifier = "TEST 1", variable = NULL) {
  lines <- readLines(file_path, warn = FALSE)
  
  # Handle both old format (TEST 1:) and new format (TEST 1.1:, ### TEST 1.1)
  # Try different patterns
  patterns <- c(
    paste0("### ", test_identifier, " "),  # New format with ###
    paste0("### ", test_identifier, ":"),  # New format variant
    paste0(test_identifier, ":"),          # Old format
    paste0("==== ", test_identifier)       # Alternative format
  )
  
  test_start <- NA
  for (pattern in patterns) {
    matches <- grep(pattern, lines, fixed = TRUE)
    if (length(matches) > 0) {
      test_start <- matches[1]
      break
    }
  }
  
  if (is.na(test_start)) {
    # Try numeric format for backward compatibility
    if (is.numeric(test_identifier) || grepl("^[0-9]+$", as.character(test_identifier))) {
      test_pattern <- paste0("TEST ", test_identifier, ":")
      test_start <- grep(test_pattern, lines)[1]
    }
    
    if (is.na(test_start)) {
      return(NULL)
    }
  }
  
  # Find next test or section marker
  test_end <- length(lines)
  
  # Look for next test/section markers
  end_markers <- c(
    "### TEST",
    "==== TEST", 
    "SECTION",
    "================================================================================",
    "END OF FREQUENCY VALIDATION"
  )
  
  for (i in (test_start + 1):length(lines)) {
    if (any(sapply(end_markers, function(m) grepl(m, lines[i], fixed = TRUE)))) {
      test_end <- i - 1
      break
    }
  }
  
  # Parse the frequency output in this test section
  result <- parse_spss_frequency(lines, test_start)
  
  return(result)
}