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
#' @param weights Optional survey weights for population-representative results.
#'   Without weights, you get sample frequencies. With weights, you get
#'   population estimates.
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
#' @param show.unused Show all defined value labels, even those with zero
#'   observations? (Default: FALSE). When TRUE, values that have labels defined
#'   (e.g., from statistical software files) but no cases in the data are
#'   included with frequency 0. This is useful for labelled datasets where
#'   unused categories should still appear in the output. Automatically enables
#'   label display.
#'
#' @return A frequency table showing counts and percentages for each category
#'
#' @details
#' ## Understanding the Results
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
#' ## Tagged Missing Values
#'
#' When data is imported with tagged NAs (e.g., via [read_spss()] with
#' `tag.na = TRUE`, or [read_stata()], [read_sas()], [read_xpt()] with the
#' `tag.na` parameter), `frequency()` automatically expands the missing value
#' section to show each missing type individually (with its original missing
#' value code and label), plus summary rows for **Total Valid** and **Total
#' Missing**.
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
#' @seealso
#' \code{\link[base]{table}} for base R frequency tables.
#'
#' \code{\link{crosstab}} for cross-tabulation of two variables.
#'
#' \code{\link{chi_square}} for testing relationships between categories.
#'
#' \code{\link{describe}} for numeric variable summaries.
#'
#' @family descriptive
#' @export
frequency <- function(data, ..., weights = NULL, sort.frq = "none",
                     show.na = TRUE, show.prc = TRUE, show.valid = TRUE, show.sum = TRUE, show.labels = "auto", show.unused = FALSE) {
  
  if (!is.data.frame(data)) cli_abort("{.arg data} must be a data frame.")

  # Check grouping and get variable names
  is_grouped <- inherits(data, "grouped_df")
  grp_vars <- if (is_grouped) dplyr::group_vars(data) else NULL

  # Select variables using centralized helper
  vars <- .process_variables(data, ...)
  var_names <- names(vars)

  # Process weights using centralized helper
  weights_info <- .process_weights(data, rlang::enquo(weights))
  w_name <- weights_info$name
  
  # When show.unused is TRUE, force labels on (unused labels without label column make no sense)
  if (show.unused && show.labels == "auto") {
    show.labels <- TRUE
  }

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
    results <- calculate_grouped_frequencies(data, var_names, w_name, sort.frq, show.na, show.unused)
  } else {
    results <- calculate_ungrouped_frequencies(data, var_names, w_name, sort.frq, show.na, show.unused)
  }
  
  # Create S3 object
  structure(list(
    results = results$frequencies,
    stats = results$stats,
    variables = var_names,
    weights = w_name,
    groups = grp_vars,
    is_grouped = is_grouped,
    options = list(show.na = show.na, show.prc = show.prc, show.valid = show.valid, show.sum = show.sum, show.labels = show.labels, show.unused = show.unused),
    labels = sapply(var_names, function(var) {
      lbl <- attr(data[[var]], "label")
      if (is.null(lbl)) var else paste(as.character(lbl), collapse = " | ")
    })
  ), class = "frequency")
}

# Note: get_value_labels() moved to helpers.R for shared use across the package

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
calculate_single_frequency <- function(x, w = NULL, sort.frq = "none", show.na = TRUE, show.unused = FALSE) {
  # Check for tagged NAs (from read_spss, read_stata, read_sas, etc.)
  has_tagged_na <- !is.null(attr(x, "na_tag_map")) &&
    requireNamespace("haven", quietly = TRUE)

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

  # --- Tagged NA expansion: split single NA row into per-tag rows + total ---
  if (has_tagged_na && show.na && any(is.na(x))) {
    result <- .expand_tagged_na_rows(result, x, w)
  }

  # Inject unused value labels as rows with freq=0
  # (skip labels that are tagged NAs - those are already expanded above)
  if (show.unused && !is.null(attr(x, "labels"))) {
    all_labels <- attr(x, "labels")  # named numeric: c("LINKS" = 1, "RECHTS" = 10, ...)

    # Filter out NA labels (tagged NAs) - they are handled by tagged NA expansion
    valid_labels <- all_labels[!is.na(all_labels)]
    observed_values <- result$value[!is.na(result$value)]
    unused_labels <- valid_labels[!valid_labels %in% observed_values]

    if (length(unused_labels) > 0) {
      has_n_eff <- "n_eff" %in% names(result)
      unused_rows <- data.frame(
        value = as.numeric(unused_labels),
        label = names(unused_labels),
        freq = 0,
        prc = 0,
        valid_prc = 0,
        cum_freq = NA,
        cum_prc = NA,
        stringsAsFactors = FALSE
      )
      if (has_n_eff) {
        unused_rows$n_eff <- result$n_eff[1]
      }
      if ("is_na_row" %in% names(result)) {
        unused_rows$is_na_row <- FALSE
      }

      # Separate NA rows, merge non-NA rows, re-sort, re-append NA rows
      na_rows <- result[is.na(result$value), , drop = FALSE]
      non_na_rows <- result[!is.na(result$value), , drop = FALSE]
      combined <- rbind(non_na_rows, unused_rows)
      combined <- combined[order(combined$value), , drop = FALSE]

      # Recalculate cumulative % for all non-NA rows
      combined$cum_prc <- cumsum(combined$valid_prc)
      combined$cum_freq <- cumsum(combined$freq)

      # Re-append NA rows
      if (nrow(na_rows) > 0) {
        result <- rbind(combined, na_rows)
      } else {
        result <- combined
      }
      rownames(result) <- NULL
    }
  }

  # Sort if requested
  if (sort.frq %in% c("asc", "desc")) {
    result <- result[order(result$value, decreasing = (sort.frq == "desc")), ]
  }

  return(result)
}

# Helper: Expand the single NA row into per-tag rows + a total row
# Called when tagged NAs are detected (any format: SPSS, Stata, SAS)
.expand_tagged_na_rows <- function(result, x, w) {
  tag_map <- attr(x, "na_tag_map")  # named numeric (SPSS/tag.na): c("a" = -42), or character (native): c("a" = ".a")
  labels  <- attr(x, "labels")
  has_n_eff <- "n_eff" %in% names(result)

  # Find the existing aggregate NA row
  na_row_idx <- which(is.na(result$value))
  if (length(na_row_idx) == 0L) return(result)

  na_total_freq <- result$freq[na_row_idx]
  na_total_prc  <- result$prc[na_row_idx]

  # Get tags for each NA observation
  na_mask <- is.na(x)
  na_tags <- vapply(x[na_mask], haven::na_tag, character(1))

  # Count per tag (unweighted or weighted)
  unique_tags <- sort(unique(na_tags[!is.na(na_tags)]))

  # Also handle system NAs (untagged)
  n_system_na <- sum(is.na(na_tags))

  # Build a full-length tag vector (empty string for non-NA, tag char for tagged NA, NA for system NA)
  all_tags <- rep("", length(x))
  all_tags[na_mask] <- ifelse(is.na(na_tags), NA_character_, na_tags)

  tag_rows <- list()
  for (tag in unique_tags) {
    tag_match <- !is.na(all_tags) & all_tags == tag
    tag_mask <- na_mask & tag_match

    if (is.null(w)) {
      tag_freq <- sum(tag_mask)
    } else {
      tag_freq <- sum(w[tag_mask & !is.na(w)])
    }

    # Total for Raw % denominator
    if (is.null(w)) {
      total_all <- length(x)
    } else {
      total_all <- sum(w[!is.na(w)])
    }
    tag_prc <- tag_freq / total_all * 100

    # Find label for this tag
    tag_label <- ""
    if (!is.null(labels)) {
      na_labels <- labels[is.na(labels)]
      label_tags <- vapply(na_labels, haven::na_tag, character(1))
      match_idx <- which(label_tags == tag)
      if (length(match_idx) > 0L) {
        tag_label <- names(na_labels)[match_idx[1]]
      }
    }

    # Display value: show original missing value code
    display_val <- if (tag %in% names(tag_map)) as.character(tag_map[tag]) else paste0("NA(", tag, ")")

    row_data <- data.frame(
      value = NA_real_,
      label = tag_label,
      freq = tag_freq,
      prc = tag_prc,
      valid_prc = NA_real_,
      cum_freq = NA_real_,
      cum_prc = NA_real_,
      stringsAsFactors = FALSE
    )
    if (has_n_eff) {
      row_data$n_eff <- result$n_eff[1]
    }
    row_data$is_na_row <- TRUE
    row_data$na_display_value <- display_val

    tag_rows <- c(tag_rows, list(row_data))
  }

  # Add system NA row if present
  if (n_system_na > 0L) {
    # System NAs are positions where na_mask is TRUE but all_tags is NA (no tag)
    sys_mask <- na_mask & is.na(all_tags)
    if (is.null(w)) {
      sys_freq <- sum(sys_mask)
      total_all <- length(x)
    } else {
      sys_freq <- sum(w[sys_mask & !is.na(w)])
      total_all <- sum(w[!is.na(w)])
    }
    row_data <- data.frame(
      value = NA_real_,
      label = "",
      freq = sys_freq,
      prc = sys_freq / total_all * 100,
      valid_prc = NA_real_,
      cum_freq = NA_real_,
      cum_prc = NA_real_,
      stringsAsFactors = FALSE
    )
    if (has_n_eff) {
      row_data$n_eff <- result$n_eff[1]
    }
    row_data$is_na_row <- TRUE
    row_data$na_display_value <- "NA"

    tag_rows <- c(tag_rows, list(row_data))
  }

  # Build the NA total (sum) row
  total_row <- data.frame(
    value = NA_real_,
    label = "Total Missing",
    freq = na_total_freq,
    prc = na_total_prc,
    valid_prc = NA_real_,
    cum_freq = NA_real_,
    cum_prc = NA_real_,
    stringsAsFactors = FALSE
  )
  if (has_n_eff) {
    total_row$n_eff <- result$n_eff[1]
  }
  total_row$is_na_row <- TRUE
  total_row$na_display_value <- "NA(total)"

  # Replace the original aggregate NA row with the expanded rows
  non_na_result <- result[-na_row_idx, , drop = FALSE]
  # Add is_na_row column to non-NA rows
  if (nrow(non_na_result) > 0L) {
    non_na_result$is_na_row <- FALSE
    non_na_result$na_display_value <- NA_character_
  } else {
    non_na_result$is_na_row <- logical(0)
    non_na_result$na_display_value <- character(0)
  }

  # Build the Valid total (sum) row
  valid_total_freq <- sum(non_na_result$freq)
  valid_total_prc  <- sum(non_na_result$prc)
  valid_total_row <- data.frame(
    value = NA_real_,
    label = "Total Valid",
    freq = valid_total_freq,
    prc = valid_total_prc,
    valid_prc = 100,
    cum_freq = NA_real_,
    cum_prc = NA_real_,
    stringsAsFactors = FALSE
  )
  if (has_n_eff) {
    valid_total_row$n_eff <- result$n_eff[1]
  }
  valid_total_row$is_na_row <- FALSE
  valid_total_row$na_display_value <- "Total"

  expanded_na <- do.call(rbind, tag_rows)
  result <- rbind(non_na_result, valid_total_row, expanded_na, total_row)
  rownames(result) <- NULL

  result
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
process_variables <- function(data, var_names, w_name, sort.frq, show.na = TRUE, show.unused = FALSE, group_info = NULL) {
  frequencies_list <- list()
  stats_list <- list()
  
  for (var_name in var_names) {
    x <- data[[var_name]]
    w <- if (!is.null(w_name)) data[[w_name]] else NULL
    
    # Calculate frequencies and stats
    freq_result <- calculate_single_frequency(x, w, sort.frq, show.na, show.unused)
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

  # Normalize columns before rbind: some variables may have tagged-NA columns

  # (is_na_row, na_display_value) while others do not
  if (length(frequencies_list) > 1L) {
    all_cols <- unique(unlist(lapply(frequencies_list, names)))
    frequencies_list <- lapply(frequencies_list, function(df) {
      missing_cols <- setdiff(all_cols, names(df))
      for (mc in missing_cols) {
        df[[mc]] <- if (mc == "is_na_row") FALSE else NA
      }
      df[all_cols]
    })
  }

  list(frequencies = do.call(rbind, frequencies_list), stats = do.call(rbind, stats_list))
}

# Helper function: Calculate frequencies for ungrouped data
calculate_ungrouped_frequencies <- function(data, var_names, w_name, sort.frq, show.na = TRUE, show.unused = FALSE) {
  process_variables(data, var_names, w_name, sort.frq, show.na, show.unused)
}

# Helper function: Calculate frequencies for grouped data
calculate_grouped_frequencies <- function(data, var_names, w_name, sort.frq, show.na = TRUE, show.unused = FALSE) {
  data_list <- dplyr::group_split(data)
  group_keys <- dplyr::group_keys(data)

  results_list <- lapply(seq_along(data_list), function(i) {
    process_variables(data_list[[i]], var_names, w_name, sort.frq, show.na, show.unused, group_keys[i, , drop = FALSE])
  })
  
  freq_parts <- lapply(results_list, `[[`, "frequencies")

  # Normalize columns across groups (tagged-NA columns may differ)
  if (length(freq_parts) > 1L) {
    all_cols <- unique(unlist(lapply(freq_parts, names)))
    freq_parts <- lapply(freq_parts, function(df) {
      missing_cols <- setdiff(all_cols, names(df))
      for (mc in missing_cols) {
        df[[mc]] <- if (mc == "is_na_row") FALSE else NA
      }
      df[all_cols]
    })
  }

  list(
    frequencies = do.call(rbind, freq_parts),
    stats = do.call(rbind, lapply(results_list, `[[`, "stats"))
  )
}

#' Print method for frequency objects
#'
#' @description
#' Prints formatted frequency statistics with ASCII tables.
#'
#' @param x An object of class "frequency"
#' @param digits Number of decimal places to display (default: 3)
#' @param ... Additional arguments passed to print
#'
#' @return Invisibly returns the input object \code{x}.
#' @export
print.frequency <- function(x, digits = 3, ...) {
  # Helper functions for formatting
  format_num <- function(x, width = 6) {
    s <- sprintf("%.2f", ifelse(is.na(x), NA, x))
    pad_utf8(paste0(s, " "), width, align = "right")
  }
  format_int <- function(x, width = 6) {
    s <- sprintf("%.0f", ifelse(is.na(x), NA, round(x)))
    pad_utf8(paste0(s, " "), width, align = "right")
  }
  format_str <- function(x, width) {
    s <- as.character(x)
    if (length(s) == 0 || is.na(s[1])) s <- "NA"
    nc <- nchar(s)
    if (!is.na(nc) && nc > width - 1) s <- paste0(substr(s, 1, width - 4), "...")
    pad_utf8(paste0(" ", s), width)
  }

  print_line <- function(widths) {
    cat("+", paste(sapply(widths, function(w) paste(rep("-", w), collapse = "")), collapse = "+"), "+\n", sep = "")
  }

  print_row <- function(values, widths, aligns = NULL) {
    if (is.null(aligns)) aligns <- rep("left", length(values))
    cells <- mapply(function(v, w, a) {
      s <- as.character(v)
      if (length(s) == 0 || is.na(s[1])) s <- "NA"
      nc <- nchar(s)
      if (!is.na(nc) && nc > w - 1) s <- paste0(substr(s, 1, w - 4), "...")
      if (a == "right") {
        pad_utf8(paste0(s, " "), w, align = "right")
      } else {
        pad_utf8(paste0(" ", s), w)
      }
    }, values, widths, aligns, SIMPLIFY = TRUE)
    cat("|", paste(cells, collapse = "|"), "|\n", sep = "")
  }

  # Dynamic widths for Value and Label based on actual content
  calc_col_width <- function(values, min_w, max_w) {
    if (length(values) == 0) return(min_w)
    content_max <- max(nchar(as.character(values)), na.rm = TRUE)
    min(max(content_max + 2, min_w), max_w)
  }

  # Include na_display_value in width calculation when tagged NAs are present
  all_values <- as.character(x$results$value)
  if ("na_display_value" %in% names(x$results)) {
    na_disp <- x$results$na_display_value[!is.na(x$results$na_display_value)]
    all_values <- c(all_values, as.character(na_disp))
  }
  all_labels <- as.character(x$results$label[!is.na(x$results$label) & x$results$label != ""])

  # Include summary row labels ("Total Valid", "Total Missing") in width calculation
  # when any variable has missing values
  has_any_na <- any(is.na(x$results$value))
  if (has_any_na) {
    all_labels <- c(all_labels, "Total Valid", "Total Missing")
  }

  value_w <- calc_col_width(all_values, min_w = 6, max_w = 40)
  label_w <- if (length(all_labels) > 0) calc_col_width(all_labels, min_w = 5, max_w = 40) else 20

  col_widths <- c(Value = value_w, Label = label_w, N = 8, Raw = 8, Valid = 8, Cum = 8)
  
  # Determine test type
  weights_name <- x$weights
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

  invisible(x)
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

  # Build alignment vector: all columns right-aligned
  aligns <- c("right")  # Value
  if (options$show.labels) aligns <- c(aligns, "right")  # Label
  aligns <- c(aligns, "right")  # N
  if (options$show.prc) aligns <- c(aligns, "right")  # Raw %
  if (options$show.valid) aligns <- c(aligns, "right")  # Valid %
  if (options$show.sum) aligns <- c(aligns, "right")  # Cum. %

  # Check if we have tagged NA rows (expanded missing types)
  has_tagged_rows <- "na_display_value" %in% names(results) &&
    any(!is.na(results$na_display_value))

  # Helper: render a single data row
  render_row <- function(row) {
    display_label <- if (is.na(row$label) || row$label == "") "" else as.character(row$label)

    if (has_tagged_rows && !is.na(row$na_display_value)) {
      display_value <- row$na_display_value
    } else {
      display_value <- as.character(row$value)
    }

    freq_str <- sprintf("%.0f", ifelse(is.na(row$freq), NA, round(row$freq)))
    prc_str <- sprintf("%.2f", ifelse(is.na(row$prc), NA, row$prc))
    valid_str <- if (is.na(row$valid_prc)) "NA" else sprintf("%.2f", row$valid_prc)
    cum_str <- if (is.na(row$cum_prc)) "NA" else sprintf("%.2f", row$cum_prc)

    values <- c(display_value)
    if (options$show.labels) values <- c(values, display_label)
    values <- c(values, freq_str)
    if (options$show.prc) values <- c(values, prc_str)
    if (options$show.valid) values <- c(values, valid_str)
    if (options$show.sum) values <- c(values, cum_str)

    print_row(values, active_widths, aligns)
  }

  # Helper: render a summary row (Total Valid / Total Missing / Total)
  render_summary_row <- function(label, freq, prc, valid_prc_str = "NA", cum_str = "", sublabel = "") {
    values <- c(label)
    if (options$show.labels) values <- c(values, sublabel)
    values <- c(values, sprintf("%.0f", round(freq)))
    if (options$show.prc) values <- c(values, sprintf("%.2f", prc))
    if (options$show.valid) values <- c(values, valid_prc_str)
    if (options$show.sum) values <- c(values, cum_str)
    print_row(values, active_widths, aligns)
  }

  print_line(active_widths)
  print_row(headers, active_widths, aligns)
  print_line(active_widths)

  if (has_tagged_rows) {
    # --- Tagged NA layout: already has Total Valid / per-tag rows / Total Missing in data ---
    prev_was_na_row <- FALSE
    for (i in seq_len(nrow(results))) {
      row <- results[i, ]

      if (!is.na(row$na_display_value)) {
        if (row$na_display_value == "Total" ||
            row$na_display_value == "NA(total)" ||
            (isTRUE(row$is_na_row) && !prev_was_na_row)) {
          print_line(active_widths)
        }
      }

      prev_was_na_row <- isTRUE(row$is_na_row)
      render_row(row)
    }
  } else {
    # --- Standard layout: unified format with Total Valid / NA / Total Missing ---
    # Separate valid rows from NA rows
    na_idx <- is.na(results$value)
    valid_rows <- results[!na_idx, , drop = FALSE]
    na_rows <- results[na_idx, , drop = FALSE]
    has_na <- nrow(na_rows) > 0 && options$show.na

    # Print valid data rows
    for (i in seq_len(nrow(valid_rows))) {
      render_row(valid_rows[i, ])
    }

    # Totals section
    valid_freq <- sum(valid_rows$freq, na.rm = TRUE)
    valid_prc <- sum(valid_rows$prc, na.rm = TRUE)
    na_freq <- if (has_na) sum(na_rows$freq, na.rm = TRUE) else 0
    na_prc <- if (has_na) sum(na_rows$prc, na.rm = TRUE) else 0

    if (has_na) {
      # Layout with NAs: Total Valid → NA row(s) → Total Missing
      print_line(active_widths)
      render_summary_row("Total", valid_freq, valid_prc, "100.00", "", "Total Valid")
      print_line(active_widths)

      for (i in seq_len(nrow(na_rows))) {
        render_row(na_rows[i, ])
      }

      print_line(active_widths)
      render_summary_row("Total", na_freq, na_prc, "NA", "", "Total Missing")
    } else {
      # No NAs: simple total row
      print_line(active_widths)
      render_summary_row("Total", valid_freq, valid_prc, "100.00", "")
    }
  }

  print_line(active_widths)
  cat("\n")
}

#' @rdname frequency
#' @usage fre(data, ..., weights = NULL, sort.frq = "none", show.na = TRUE,
#'   show.prc = TRUE, show.valid = TRUE, show.sum = TRUE, show.labels = "auto",
#'   show.unused = FALSE)
#' @export
fre <- frequency

