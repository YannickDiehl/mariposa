# SPSS Output Parser Functions
# =============================================================
# Core infrastructure for parsing SPSS TXT outputs and validating
# against SurveyStat R package results.

#' Parse SPSS T-Test Output
#' 
#' Extracts key statistical values from SPSS Independent Samples T-Test output
#' 
#' @param spss_txt_file Path to SPSS output TXT file
#' @return List with extracted statistical values
#' @keywords internal
parse_spss_t_test <- function(spss_txt_file) {
  if (!file.exists(spss_txt_file)) {
    stop("SPSS reference file not found: ", spss_txt_file)
  }
  
  # Read SPSS output
  spss_lines <- readLines(spss_txt_file, warn = FALSE)
  spss_text <- paste(spss_lines, collapse = "\n")
  
  # Initialize results list
  results <- list(
    equal_variances = list(),
    unequal_variances = list(),
    group_statistics = list(),
    levene_test = list()
  )
  
  # Extract Levene's Test for Equality of Variances
  # Pattern: F statistic and Sig. (p-value)
  levene_pattern <- "Levene's Test.*?F\\s+(\\d+\\.\\d+).*?Sig\\.\\s+(\\d+\\.\\d+)"
  levene_match <- regexpr(levene_pattern, spss_text, perl = TRUE)
  if (levene_match > 0) {
    levene_captures <- attr(levene_match, "capture.start")
    if (length(levene_captures) >= 2) {
      f_start <- levene_captures[1]
      f_length <- attr(levene_match, "capture.length")[1]
      p_start <- levene_captures[2] 
      p_length <- attr(levene_match, "capture.length")[2]
      
      results$levene_test$F_statistic <- as.numeric(substr(spss_text, f_start, f_start + f_length - 1))
      results$levene_test$p_value <- as.numeric(substr(spss_text, p_start, p_start + p_length - 1))
    }
  }
  
  # Extract Group Statistics (descriptive statistics by group)
  # Pattern: Group, N, Mean, Std. Deviation, Std. Error Mean
  group_stats_pattern <- "Group Statistics.*?([A-Za-z]+)\\s+(\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)"
  group_matches <- gregexpr(group_stats_pattern, spss_text, perl = TRUE)
  
  if (length(group_matches[[1]]) > 0 && group_matches[[1]][1] > 0) {
    # Extract first group
    captures <- attr(group_matches[[1]], "capture.start")[1, ]
    lengths <- attr(group_matches[[1]], "capture.length")[1, ]
    
    if (length(captures) >= 5) {
      results$group_statistics$group1 <- list(
        name = substr(spss_text, captures[1], captures[1] + lengths[1] - 1),
        n = as.numeric(substr(spss_text, captures[2], captures[2] + lengths[2] - 1)),
        mean = as.numeric(substr(spss_text, captures[3], captures[3] + lengths[3] - 1)),
        sd = as.numeric(substr(spss_text, captures[4], captures[4] + lengths[4] - 1)),
        se = as.numeric(substr(spss_text, captures[5], captures[5] + lengths[5] - 1))
      )
    }
    
    # Extract second group if available
    if (length(group_matches[[1]]) > 1) {
      captures2 <- attr(group_matches[[1]], "capture.start")[2, ]
      lengths2 <- attr(group_matches[[1]], "capture.length")[2, ]
      
      if (length(captures2) >= 5) {
        results$group_statistics$group2 <- list(
          name = substr(spss_text, captures2[1], captures2[1] + lengths2[1] - 1),
          n = as.numeric(substr(spss_text, captures2[2], captures2[2] + lengths2[2] - 1)),
          mean = as.numeric(substr(spss_text, captures2[3], captures2[3] + lengths2[3] - 1)),
          sd = as.numeric(substr(spss_text, captures2[4], captures2[4] + lengths2[4] - 1)),
          se = as.numeric(substr(spss_text, captures2[5], captures2[5] + lengths2[5] - 1))
        )
      }
    }
  }
  
  # Extract Independent Samples Test results
  # Pattern for Equal variances assumed: t, df, Sig. (2-tailed), Mean Difference, 95% CI
  equal_var_pattern <- "Equal variances assumed\\s+([-]?\\d+\\.\\d+)\\s+(\\d+)\\s+(\\d+\\.\\d+)\\s+([-]?\\d+\\.\\d+)\\s+([-]?\\d+\\.\\d+)\\s+([-]?\\d+\\.\\d+)"
  equal_match <- regexpr(equal_var_pattern, spss_text, perl = TRUE)
  
  if (equal_match > 0) {
    captures <- attr(equal_match, "capture.start")
    lengths <- attr(equal_match, "capture.length")
    
    if (length(captures) >= 6) {
      results$equal_variances <- list(
        t_stat = as.numeric(substr(spss_text, captures[1], captures[1] + lengths[1] - 1)),
        df = as.numeric(substr(spss_text, captures[2], captures[2] + lengths[2] - 1)),
        p_value = as.numeric(substr(spss_text, captures[3], captures[3] + lengths[3] - 1)),
        mean_diff = as.numeric(substr(spss_text, captures[4], captures[4] + lengths[4] - 1)),
        ci_lower = as.numeric(substr(spss_text, captures[5], captures[5] + lengths[5] - 1)),
        ci_upper = as.numeric(substr(spss_text, captures[6], captures[6] + lengths[6] - 1))
      )
    }
  }
  
  # Pattern for Equal variances not assumed (Welch's t-test)
  unequal_var_pattern <- "Equal variances not assumed\\s+([-]?\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+([-]?\\d+\\.\\d+)\\s+([-]?\\d+\\.\\d+)\\s+([-]?\\d+\\.\\d+)"
  unequal_match <- regexpr(unequal_var_pattern, spss_text, perl = TRUE)
  
  if (unequal_match > 0) {
    captures <- attr(unequal_match, "capture.start")
    lengths <- attr(unequal_match, "capture.length")
    
    if (length(captures) >= 6) {
      results$unequal_variances <- list(
        t_stat = as.numeric(substr(spss_text, captures[1], captures[1] + lengths[1] - 1)),
        df = as.numeric(substr(spss_text, captures[2], captures[2] + lengths[2] - 1)),
        p_value = as.numeric(substr(spss_text, captures[3], captures[3] + lengths[3] - 1)),
        mean_diff = as.numeric(substr(spss_text, captures[4], captures[4] + lengths[4] - 1)),
        ci_lower = as.numeric(substr(spss_text, captures[5], captures[5] + lengths[5] - 1)),
        ci_upper = as.numeric(substr(spss_text, captures[6], captures[6] + lengths[6] - 1))
      )
    }
  }
  
  return(results)
}

#' Parse SPSS Descriptive Statistics Output
#' 
#' Extracts descriptive statistics from SPSS DESCRIPTIVES output
#' 
#' @param spss_txt_file Path to SPSS output TXT file
#' @return List with extracted descriptive statistics
#' @keywords internal
parse_spss_descriptives <- function(spss_txt_file) {
  if (!file.exists(spss_txt_file)) {
    stop("SPSS reference file not found: ", spss_txt_file)
  }
  
  spss_lines <- readLines(spss_txt_file, warn = FALSE)
  spss_text <- paste(spss_lines, collapse = "\n")
  
  results <- list()
  
  # Pattern for descriptive statistics: Variable, N, Min, Max, Mean, Std. Deviation
  desc_pattern <- "([A-Za-z_]+)\\s+(\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)"
  desc_matches <- gregexpr(desc_pattern, spss_text, perl = TRUE)
  
  if (length(desc_matches[[1]]) > 0 && desc_matches[[1]][1] > 0) {
    for (i in seq_along(desc_matches[[1]])) {
      captures <- attr(desc_matches[[1]], "capture.start")[i, ]
      lengths <- attr(desc_matches[[1]], "capture.length")[i, ]
      
      if (length(captures) >= 6) {
        var_name <- substr(spss_text, captures[1], captures[1] + lengths[1] - 1)
        
        results[[var_name]] <- list(
          n = as.numeric(substr(spss_text, captures[2], captures[2] + lengths[2] - 1)),
          min = as.numeric(substr(spss_text, captures[3], captures[3] + lengths[3] - 1)),
          max = as.numeric(substr(spss_text, captures[4], captures[4] + lengths[4] - 1)),
          mean = as.numeric(substr(spss_text, captures[5], captures[5] + lengths[5] - 1)),
          sd = as.numeric(substr(spss_text, captures[6], captures[6] + lengths[6] - 1))
        )
      }
    }
  }
  
  return(results)
}

#' Parse SPSS Levene Test Output
#' 
#' Extracts Levene test statistics from SPSS output
#' 
#' @param spss_txt_file Path to SPSS output TXT file
#' @return List with Levene test statistics
#' @keywords internal
parse_spss_levene <- function(spss_txt_file) {
  if (!file.exists(spss_txt_file)) {
    stop("SPSS reference file not found: ", spss_txt_file)
  }
  
  spss_lines <- readLines(spss_txt_file, warn = FALSE)
  spss_text <- paste(spss_lines, collapse = "\n")
  
  results <- list()
  
  # Pattern for Levene test: F statistic, df1, df2, Sig.
  levene_pattern <- "Levene Statistic\\s+(\\d+\\.\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+\\.\\d+)"
  levene_match <- regexpr(levene_pattern, spss_text, perl = TRUE)
  
  if (levene_match > 0) {
    captures <- attr(levene_match, "capture.start")
    lengths <- attr(levene_match, "capture.length")
    
    if (length(captures) >= 4) {
      results <- list(
        F_statistic = as.numeric(substr(spss_text, captures[1], captures[1] + lengths[1] - 1)),
        df1 = as.numeric(substr(spss_text, captures[2], captures[2] + lengths[2] - 1)),
        df2 = as.numeric(substr(spss_text, captures[3], captures[3] + lengths[3] - 1)),
        p_value = as.numeric(substr(spss_text, captures[4], captures[4] + lengths[4] - 1))
      )
    }
  }
  
  return(results)
}