#' Count How Many People Chose Each Option
#'
#' @description
#' \code{frequency()} helps you understand categorical data by showing how many people
#' chose each option. It's perfect for survey questions with fixed choices like
#' education level, yes/no questions, or rating scales.
#'
#' Think of it as creating a summary table that shows:
#' - How many people chose each option
#' - What percentage that represents
#' - Running totals to see cumulative patterns
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... The categorical variables you want to analyze. You can list multiple
#'   variables separated by commas, or use helpers like \code{starts_with("trust")}
#' @param weights Optional survey weights to make results representative of your
#'   population. Without weights, you get sample frequencies. With weights, you
#'   get population estimates.
#' @param sort.frq How to order the results:
#'   \itemize{
#'     \item \code{"none"} (default): Keep original order
#'     \item \code{"asc"}: Sort from lowest to highest frequency
#'     \item \code{"desc"}: Sort from highest to lowest frequency
#'   }
#' @param show.na Include missing values in the table? (Default: TRUE)
#' @param show.prc Show raw percentages including missing values? (Default: TRUE)
#' @param show.valid Show percentages excluding missing values? (Default: TRUE)
#' @param show.sum Show cumulative totals? (Default: TRUE)
#' @param show.labels Show category labels if available? (Default: "auto" - shows
#'   labels when they exist)
#'
#' @return A frequency table showing counts and percentages for each category
#'
#' @details
#' ## Understanding the Output
#'
#' The frequency table shows:
#' - **Freq**: Number of responses in each category
#' - **%**: Percentage including missing values (use for "response rate")
#' - **Valid %**: Percentage excluding missing values (use for "among those who answered")
#' - **Cum %**: Running total percentage (helps identify cutoff points)
#'
#' ## When to Use This
#'
#' Use \code{frequency()} when you have:
#' - Categorical variables (gender, region, education level)
#' - Yes/No questions
#' - Rating scales (satisfied/neutral/dissatisfied)
#' - Any question with a fixed set of options
#'
#' ## Weights Make a Difference
#'
#' Without weights, you're describing your sample. With weights, you're estimating
#' population values. Always use weights for population inference.
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#' 
#' # Basic categorical analysis
#' survey_data %>% frequency(gender)
#' 
#' # Multiple variables with weights
#' survey_data %>% frequency(gender, region, weights = sampling_weight)
#' 
#' # Grouped analysis by region
#' survey_data %>% 
#'   group_by(region) %>% 
#'   frequency(gender, weights = sampling_weight)
#' 
#' # Education levels with sorting
#' survey_data %>% frequency(education, sort.frq = "desc")
#' 
#' # Employment status with custom display options
#' survey_data %>% frequency(employment, weights = sampling_weight, 
#'                          show.na = TRUE, show.sum = TRUE)
#'
#' @export
frequency <- function(data, ..., weights = NULL, sort.frq = "none",
                     show.na = TRUE, show.prc = TRUE, show.valid = TRUE, show.sum = TRUE, show.labels = "auto") {
  
  if (!is.data.frame(data)) stop("data must be a data frame")
  
  # Check grouping and get variable names
  is_grouped <- inherits(data, "grouped_df")
  group_vars <- if (is_grouped) group_vars(data) else NULL
  
  vars <- eval_select(expr(c(!!!enquos(...))), data = data)
  var_names <- names(vars)
  
  w_name <- if (!quo_is_null(enquo(weights))) {
    names(eval_select(expr(!!enquo(weights)), data = data))
  } else NULL
  
  # Handle show.labels logic: auto-detect or use explicit user setting
  if (show.labels == "auto") {
    has_meaningful_labels <- any(sapply(var_names, function(var) {
      x <- data[[var]]
      
      # For factors, check if levels are different from their numeric representation
      if (is.factor(x)) {
        levels_x <- levels(x)
        # Check if factor levels are just numbers or provide meaningful labels
        numeric_levels <- suppressWarnings(as.numeric(levels_x))
        return(!all(!is.na(numeric_levels) & numeric_levels == seq_along(levels_x)))
      }
      
      # For variables with sjlabelled value labels
      if (!is.null(attr(x, "labels"))) {
        value_labels <- attr(x, "labels")
        
        # Get actually occurring values in the data (excluding NA)
        actual_values <- unique(x[!is.na(x)])
        
        # Check if any of the actual values have meaningful labels
        # (i.e., labels that are different from the values themselves)
        actual_values_with_labels <- actual_values[actual_values %in% value_labels]
        
        if (length(actual_values_with_labels) > 0) {
          # Check if labels for actual values are different from the values
          actual_label_names <- names(value_labels)[value_labels %in% actual_values_with_labels]
          actual_label_values <- as.character(actual_values_with_labels)
          return(!all(actual_label_names == actual_label_values))
        } else {
          # No labels for actually occurring values
          return(FALSE)
        }
      }
      
      # Variables with only variable labels (attr "label") but no value labels
      # are not considered as having meaningful value labels for display
      # This is just a variable description, not value labels
      
      return(FALSE)
    }))
    
    # Set show.labels based on auto-detection
    show.labels <- has_meaningful_labels
  } else {
    # Use explicit user setting (TRUE or FALSE)
    show.labels <- as.logical(show.labels)
  }
  
  # Calculate frequencies
  if (is_grouped) {
    results <- calculate_grouped_frequencies(data, var_names, w_name, sort.frq, show.na)
  } else {
    results <- calculate_ungrouped_frequencies(data, var_names, w_name, sort.frq, show.na)
  }
  
  # Create S3 object
  structure(list(
    results = results$frequencies,
    stats = results$stats,
    variables = var_names,
    weights = w_name,
    groups = group_vars,
    is_grouped = is_grouped,
    options = list(show.na = show.na, show.prc = show.prc, show.valid = show.valid, show.sum = show.sum, show.labels = show.labels),
    labels = sapply(var_names, function(var) attr(data[[var]], "label") %||% var)
  ), class = "frequency_results")
}

# Helper function: Get value labels
get_value_labels <- function(x, freq_names) {
  if (is.factor(x)) {
    factor_levels <- levels(x)
    ifelse(is.na(freq_names), NA, factor_levels[match(freq_names, factor_levels)] %||% as.character(freq_names))
  } else if (!is.null(attr(x, "labels"))) {
    value_labels <- attr(x, "labels")
    ifelse(is.na(freq_names), NA, names(value_labels)[match(freq_names, value_labels)] %||% as.character(freq_names))
  } else {
    # For variables without value labels, return empty strings instead of duplicating values
    rep("", length(freq_names))
  }
}

# Helper function: Adjust rounded frequencies using largest remainder method
# Ensures sum of rounded frequencies equals the rounded sum
adjust_rounded_frequencies <- function(raw_freqs, target_sum = NULL) {
  if (is.null(target_sum)) {
    target_sum <- round(sum(raw_freqs))
  }
  
  # Start with floor of all values
  floored <- floor(raw_freqs)
  remainders <- raw_freqs - floored
  
  # How many values need to be rounded up?
  n_to_round_up <- target_sum - sum(floored)
  
  if (n_to_round_up > 0) {
    # Sort by remainder size (descending) and round up the largest remainders
    order_idx <- order(remainders, decreasing = TRUE)
    adjusted <- floored
    for(i in 1:min(n_to_round_up, length(order_idx))) {
      adjusted[order_idx[i]] <- adjusted[order_idx[i]] + 1
    }
    return(adjusted)
  } else if (n_to_round_up < 0) {
    # This shouldn't happen with proper rounding, but handle it
    # Round down the smallest remainders
    order_idx <- order(remainders, decreasing = FALSE)
    adjusted <- floored
    for(i in 1:min(abs(n_to_round_up), length(order_idx))) {
      if (adjusted[order_idx[i]] > 0) {
        adjusted[order_idx[i]] <- adjusted[order_idx[i]] - 1
      }
    }
    return(adjusted)
  } else {
    # Sum already matches
    return(floored)
  }
}

# Helper function: Calculate frequency statistics for a single variable
calculate_single_frequency <- function(x, w = NULL, sort.frq = "none", show.na = TRUE) {
  if (is.null(w)) {
    # Unweighted frequencies
    freq_table <- table(x, useNA = if (show.na) "ifany" else "no")
    total <- sum(freq_table)
    na_idx <- is.na(names(freq_table))
    valid_total <- sum(freq_table[!na_idx])
    
    # For unweighted: if show.na is TRUE, total includes missing
    # Raw % should always include missing in denominator
    total_all <- if (show.na) total else (total + sum(is.na(x)))
    
    prc <- as.numeric(freq_table) / total_all * 100
    # Valid % calculation - only for non-NA values
    valid_prc <- rep(NA, length(freq_table))
    if (any(!na_idx)) {
      valid_prc[!na_idx] <- as.numeric(freq_table[!na_idx]) / valid_total * 100
    }
    # Cumulative % based on valid %
    cum_prc <- rep(NA, length(freq_table))
    if (any(!na_idx)) {
      cum_prc[!na_idx] <- cumsum(valid_prc[!na_idx])
    }
    
    # Handle factors differently than numeric variables
    if (is.factor(x)) {
      value_col <- names(freq_table)
    } else {
      value_col <- suppressWarnings(as.numeric(names(freq_table)))
      # If conversion fails, keep as character
      if (all(is.na(value_col))) {
        value_col <- names(freq_table)
      }
    }
    
    result <- data.frame(
      value = value_col,
      label = get_value_labels(x, names(freq_table)),
      freq = as.numeric(freq_table),
      prc = prc,
      valid_prc = valid_prc,
      cum_freq = cumsum(as.numeric(freq_table)),
      cum_prc = cum_prc,
      stringsAsFactors = FALSE
    )
  } else {
    # Weighted frequencies
    valid_idx <- !is.na(x) & !is.na(w)
    if (!any(valid_idx) && !show.na) {
      return(data.frame(value = numeric(0), label = character(0), freq = numeric(0),
                       prc = numeric(0), valid_prc = numeric(0), cum_freq = numeric(0),
                       cum_prc = numeric(0), n_eff = numeric(0), stringsAsFactors = FALSE))
    }
    
    # Calculate frequencies for valid values
    if (any(valid_idx)) {
      x_valid <- x[valid_idx]
      w_valid <- w[valid_idx]
      unique_vals <- sort(unique(x_valid))
      freq_weighted <- sapply(unique_vals, function(val) sum(w_valid[x_valid == val]))
      n_eff <- (sum(w_valid))^2 / sum(w_valid^2)
    } else {
      unique_vals <- numeric(0)
      freq_weighted <- numeric(0)
      n_eff <- NA
    }
    
    # Add NA values if show.na = TRUE and there are missing values
    if (show.na && any(is.na(x))) {
      na_freq <- sum(w[is.na(x) & !is.na(w)])
      if (na_freq > 0 || any(is.na(x))) {  # Include NA row if there are any NA values
        unique_vals <- c(unique_vals, NA)
        freq_weighted <- c(freq_weighted, na_freq)
      }
    }
    
    # Calculate totals
    total_valid_weighted <- sum(freq_weighted[!is.na(unique_vals)])
    total_all_weighted <- sum(w[!is.na(w)])  # Sum of all weights, including those with missing x
    
    # Calculate percentages
    # Raw % uses total including missing, Valid % uses only valid total
    prc <- freq_weighted / total_all_weighted * 100  # Raw % based on ALL observations
    
    # Valid % - only for non-NA values
    valid_prc <- rep(NA, length(freq_weighted))
    non_na_idx <- !is.na(unique_vals)
    if (any(non_na_idx) && total_valid_weighted > 0) {
      valid_prc[non_na_idx] <- freq_weighted[non_na_idx] / total_valid_weighted * 100
    }
    
    # Cumulative frequencies and percentages (only for non-NA values)
    cum_freq <- rep(NA, length(freq_weighted))
    cum_prc <- rep(NA, length(freq_weighted))
    if (any(non_na_idx)) {
      cum_freq[non_na_idx] <- cumsum(freq_weighted[non_na_idx])
      cum_prc[non_na_idx] <- cumsum(valid_prc[non_na_idx])
    }
    # Set cumulative values for NA row if it exists
    if (any(is.na(unique_vals))) {
      na_pos <- which(is.na(unique_vals))
      cum_freq[na_pos] <- NA
      cum_prc[na_pos] <- NA
    }
    
    result <- data.frame(
      value = unique_vals,
      label = get_value_labels(x, as.character(unique_vals)),
      freq = freq_weighted,
      prc = prc,
      valid_prc = valid_prc,
      cum_freq = cum_freq,
      cum_prc = cum_prc,
      n_eff = rep(n_eff, length(unique_vals)),
      stringsAsFactors = FALSE
    )
  }
  
  # Sort if requested
  if (sort.frq %in% c("asc", "desc")) {
    result <- result[order(result$value, decreasing = (sort.frq == "desc")), ]
  }
  
  return(result)
}

# Helper function: Calculate descriptive statistics
calculate_single_stats <- function(x, w = NULL) {
  if (is.null(w)) {
    x_valid <- x[!is.na(x)]
    n <- length(x_valid)
    
    # For numeric variables, calculate mean and sd
    if (is.numeric(x_valid)) {
      mean_val <- mean(x_valid)
      sd_val <- sd(x_valid)
      skewness <- if (n > 2 && sd_val > 0) (sum((x_valid - mean_val)^3) / n) / (sd_val^3) else NA
    } else {
      # For factors or other non-numeric variables
      mean_val <- NA
      sd_val <- NA
      skewness <- NA
    }
  } else {
    valid_idx <- !is.na(x) & !is.na(w)
    if (!any(valid_idx)) {
      return(list(mean = NA, sd = NA, total_n = length(x), valid_n = 0, skewness = NA))
    }
    
    x_valid <- x[valid_idx]
    w_valid <- w[valid_idx]
    
    # For numeric variables, calculate weighted mean and sd
    if (is.numeric(x_valid)) {
      w_norm <- w_valid / sum(w_valid)
      mean_val <- sum(x_valid * w_norm)
      sd_val <- sqrt(sum(w_norm * (x_valid - mean_val)^2))
      skewness <- if (length(x_valid) > 2 && sd_val > 0) {
        sum(w_norm * ((x_valid - mean_val) / sd_val)^3)
      } else NA
    } else {
      # For factors or other non-numeric variables
      mean_val <- NA
      sd_val <- NA
      skewness <- NA
    }
  }
  
  list(mean = mean_val, sd = sd_val, 
       total_n = if (is.null(w)) length(x) else sum(w[!is.na(w)]), 
       valid_n = if (is.null(w)) length(x_valid) else sum(w_valid), 
       skewness = skewness)
}

# Helper function: Process variables for a dataset
process_variables <- function(data, var_names, w_name, sort.frq, show.na = TRUE, group_info = NULL) {
  frequencies_list <- list()
  stats_list <- list()
  
  for (var_name in var_names) {
    x <- data[[var_name]]
    w <- if (!is.null(w_name)) data[[w_name]] else NULL
    
    # Calculate frequencies and stats
    freq_result <- calculate_single_frequency(x, w, sort.frq, show.na)
    freq_result$Variable <- var_name
    
    stats <- calculate_single_stats(x, w)
    stats_df <- data.frame(Variable = var_name, mean = stats$mean, sd = stats$sd,
                          total_n = stats$total_n, valid_n = stats$valid_n,
                          skewness = stats$skewness, stringsAsFactors = FALSE)
    
    # Add group information if provided
    if (!is.null(group_info) && nrow(freq_result) > 0) {
      group_info_expanded <- group_info[rep(1, nrow(freq_result)), , drop = FALSE]
      freq_result <- cbind(group_info_expanded, freq_result)
      stats_df <- cbind(group_info, stats_df)
    }
    
    frequencies_list[[var_name]] <- freq_result
    stats_list[[var_name]] <- stats_df
  }
  
  list(frequencies = do.call(rbind, frequencies_list), stats = do.call(rbind, stats_list))
}

# Helper function: Calculate frequencies for ungrouped data
calculate_ungrouped_frequencies <- function(data, var_names, w_name, sort.frq, show.na = TRUE) {
  process_variables(data, var_names, w_name, sort.frq, show.na)
}

# Helper function: Calculate frequencies for grouped data
calculate_grouped_frequencies <- function(data, var_names, w_name, sort.frq, show.na = TRUE) {
  data_list <- group_split(data)
  group_keys <- group_keys(data)
  
  results_list <- lapply(seq_along(data_list), function(i) {
    process_variables(data_list[[i]], var_names, w_name, sort.frq, show.na, group_keys[i, , drop = FALSE])
  })
  
  list(
    frequencies = do.call(rbind, lapply(results_list, `[[`, "frequencies")),
    stats = do.call(rbind, lapply(results_list, `[[`, "stats"))
  )
}

#' Print method for frequency_results objects
#'
#' @description
#' Prints formatted frequency statistics with ASCII tables.
#'
#' @param x An object of class "frequency_results"
#' @param digits Number of decimal places to display (default: 3)
#' @param ... Additional arguments passed to print
#'
#' @export
print.frequency_results <- function(x, digits = 3, ...) {
  # Helper functions for formatting
  format_num <- function(x, width = 6) sprintf(paste0("%-", width, ".2f"), ifelse(is.na(x), NA, x))
  format_int <- function(x, width = 6) sprintf(paste0("%-", width, ".0f"), ifelse(is.na(x), NA, round(x)))
  format_str <- function(x, width) sprintf(paste0("%-", width, "s"), substr(as.character(x), 1, width))
  
  print_line <- function(widths) {
    cat("+", paste(sapply(widths, function(w) paste(rep("-", w), collapse = "")), collapse = "+"), "+\n", sep = "")
  }
  
  print_row <- function(values, widths) {
    cat("|", paste(mapply(format_str, values, widths), collapse = "|"), "|\n", sep = "")
  }
  
  col_widths <- c(Value = 6, Label = 20, N = 8, Raw = 8, Valid = 8, Cum = 8)
  
  # Determine test type
  weights_name <- x$weight_var %||% x$weights
  title <- get_standard_title("Frequency Analysis", weights_name, "Results")
  print_header(title)

  # Print results for each variable
  for (var in x$variables) {
    var_label <- x$labels[var]
    cat(sprintf("\n%s\n", format_variable_name(var, var_label)))
    
    if (x$is_grouped) {
      unique_groups <- unique(x$results[x$groups])
      
      for (i in seq_len(nrow(unique_groups))) {
        group_values <- unique_groups[i, , drop = FALSE]
        # Group info not needed here since print_group_header handles it
        
        # Filter results for current group and variable
        group_results <- x$results
        for (g in names(group_values)) {
          group_results <- group_results[group_results[[g]] == group_values[[g]], ]
        }
        group_results <- group_results[group_results$Variable == var, ]
        
        if (nrow(group_results) == 0) next
        
        # Get stats
        group_stats <- x$stats
        for (g in names(group_values)) {
          group_stats <- group_stats[group_stats[[g]] == group_values[[g]], ]
        }
        stats <- group_stats[group_stats$Variable == var, ]
        
        # Print group header using standardized helper
        print_group_header(group_values)
        cat(sprintf("# total N=%.0f valid N=%.0f mean=%.2f sd=%.2f skewness=%.2f\n\n",
                    stats$total_n, stats$valid_n, stats$mean, stats$sd, stats$skewness))
        
        print_table(group_results, col_widths, print_line, print_row, format_int, format_num, x$options)
      }
    } else {
      # Ungrouped results
      var_results <- x$results[x$results$Variable == var, ]
      stats <- x$stats[x$stats$Variable == var, ]
      
      cat(sprintf("# total N=%.0f valid N=%.0f mean=%.2f sd=%.2f skewness=%.2f\n\n",
                  stats$total_n, stats$valid_n, stats$mean, stats$sd, stats$skewness))
      
      print_table(var_results, col_widths, print_line, print_row, format_int, format_num, x$options)
    }
  }
}

# Helper function to print frequency table
print_table <- function(results, col_widths, print_line, print_row, format_int, format_num, options) {
  # Determine which columns to show
  headers <- c("Value")
  width_names <- c("Value")
  
  if (options$show.labels) {
    headers <- c(headers, "Label")
    width_names <- c(width_names, "Label")
  }
  
  headers <- c(headers, "N")
  width_names <- c(width_names, "N")
  
  if (options$show.prc) {
    headers <- c(headers, "Raw %")
    width_names <- c(width_names, "Raw")
  }
  
  if (options$show.valid) {
    headers <- c(headers, "Valid %")
    width_names <- c(width_names, "Valid")
  }
  
  if (options$show.sum) {
    headers <- c(headers, "Cum. %")
    width_names <- c(width_names, "Cum")
  }
  
  # Adjust column widths based on actually shown columns
  active_widths <- col_widths[width_names]
  
  print_line(active_widths)
  print_row(headers, active_widths)
  print_line(active_widths)
  
  for (i in seq_len(nrow(results))) {
    row <- results[i, ]
    # Only use actual labels, don't fall back to value if label is empty
    display_label <- if (is.na(row$label) || row$label == "") "" else as.character(row$label)
    
    values <- c(as.character(row$value))
    if (options$show.labels) values <- c(values, display_label)
    values <- c(values, format_int(row$freq))
    if (options$show.prc) values <- c(values, format_num(row$prc))
    if (options$show.valid) values <- c(values, ifelse(is.na(row$valid_prc), "NA", format_num(row$valid_prc)))
    if (options$show.sum) values <- c(values, ifelse(is.na(row$cum_prc), "NA", format_num(row$cum_prc)))
    
    print_row(values, active_widths)
  }
  
  print_line(active_widths)
  cat("\n")
}