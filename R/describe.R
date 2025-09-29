
#' Get to Know Your Numeric Data
#'
#' @description
#' \code{describe()} gives you a complete summary of numeric variables - like age,
#' income, or satisfaction scores. It's your first step in any analysis, helping you
#' understand what's typical, what's unusual, and how spread out your data is.
#'
#' Think of it as a health check for your data that reveals:
#' - What's the average value?
#' - What's the middle value?
#' - How spread out are the responses?
#' - Are there unusual patterns or outliers?
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... The numeric variables you want to summarize. List them separated by
#'   commas, or use helpers like \code{starts_with("trust")}
#' @param weights Optional survey weights to get population-representative statistics.
#'   Without weights, you describe your sample. With weights, you describe the population.
#' @param show Which statistics to display:
#'   \itemize{
#'     \item \code{"short"} (default): Essential stats (mean, median, SD, range, IQR, skewness)
#'     \item \code{"all"}: Everything including variance, kurtosis, mode, quantiles
#'     \item Custom list: Choose specific stats like \code{c("mean", "sd", "range")}
#'   }
#' @param probs For quantiles, which percentiles to show (default: 25th, 50th, 75th)
#' @param na.rm Remove missing values before calculating? (Default: TRUE)
#' @param excess For kurtosis, show excess kurtosis? (Default: TRUE, easier to interpret)
#'
#' @return A summary table with descriptive statistics for each variable
#'
#' @details
#' ## Understanding the Output
#'
#' Key statistics and what they tell you:
#' - **n**: How many valid responses (watch for too many missing)
#' - **Mean**: The average value
#' - **Median**: The middle value (half above, half below)
#' - **SD**: Standard deviation - how spread out values are
#' - **Range**: The minimum and maximum values
#' - **IQR**: Interquartile range - the middle 50% of values
#' - **Skewness**: Whether data leans left (negative) or right (positive)
#' - **Kurtosis**: Whether you have unusual outliers
#'
#' ## When to Use This
#'
#' Always start here! Use \code{describe()} to:
#' - Check data quality (impossible values?)
#' - Understand distributions before testing
#' - Spot outliers that might affect analyses
#' - Compare groups side by side
#'
#' ## Interpreting Patterns
#'
#' - **Mean ≈ Median**: Data is roughly symmetric
#' - **Mean > Median**: Right-skewed (tail extends right)
#' - **Mean < Median**: Left-skewed (tail extends left)
#' - **Large SD**: Responses vary widely
#' - **Small SD**: Responses are similar
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#' 
#' # Basic unweighted analysis
#' survey_data %>% describe(age)
#' 
#' # Weighted analysis
#' survey_data %>% describe(age, weights = sampling_weight)
#' 
#' # Multiple variables with custom statistics
#' survey_data %>% describe(age, income, life_satisfaction, 
#'                         weights = sampling_weight, 
#'                         show = c("mean", "sd", "skew"))
#' 
#' # Grouped analysis
#' survey_data %>% 
#'   group_by(region) %>% 
#'   describe(age, weights = sampling_weight)
#'
#' @export
describe <- function(data, ..., weights = NULL, 
                     show = "short", 
                     probs = c(0.25, 0.5, 0.75),
                     na.rm = TRUE, 
                     excess = TRUE) {
  
  # ============================================================================
  # INPUT VALIDATION AND SETUP
  # ============================================================================
  
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  # Process show parameter - expand shortcuts to full statistic names
  if ("all" %in% show) {
    show <- c("mean", "median", "sd", "se", "range", "iqr", "skew", "kurtosis", "var", "mode", "quantiles")
  } else if ("short" %in% show) {
    show <- c("mean", "median", "sd", "range", "iqr", "skew")
  }
  
  # Get variable names using tidyselect
  vars <- .process_variables(data, ...)
  
  # Process weights parameter
  weights_info <- .process_weights(data, rlang::enquo(weights))
  
  # Check if data is grouped
  is_grouped <- inherits(data, "grouped_df")
  
  # ============================================================================
  # MAIN CALCULATION LOGIC
  # ============================================================================
  
  if (is_grouped) {
    results_df <- .calculate_grouped_stats(data, vars, weights_info, show, probs, na.rm, excess)
  } else {
    results_df <- .calculate_ungrouped_stats(data, vars, weights_info, show, probs, na.rm, excess)
  }
  
  # ============================================================================
  # CREATE RESULT OBJECT
  # ============================================================================
  
  result <- list(
    results = results_df,
    variables = names(vars),
    weights = weights_info$name,
    grouped = is_grouped,
    group_vars = if (is_grouped) dplyr::group_vars(data) else NULL,
    show = show,
    probs = probs
  )
  
  class(result) <- "describe"
  return(result)
}

# ============================================================================
# HELPER FUNCTIONS FOR INPUT PROCESSING
# ============================================================================

#' Process variable selection using tidyselect
#' @keywords internal
.process_variables <- function(data, ...) {
  vars <- rlang::enquos(...)
  if (length(vars) == 0) {
    stop("At least one variable must be specified")
  }
  
  selected_vars <- tidyselect::eval_select(rlang::expr(c(...)), data)
  
  if (length(selected_vars) == 0) {
    stop("No variables selected")
  }
  
  # Validate that all selected variables are numeric
  for (var_name in names(selected_vars)) {
    if (!is.numeric(data[[var_name]])) {
      stop("Variable '", var_name, "' is not numeric")
    }
  }
  
  return(selected_vars)
}

#' Process weights parameter and validate
#' @keywords internal
.process_weights <- function(data, weights_quo) {
  if (rlang::quo_is_null(weights_quo)) {
    return(list(vector = NULL, name = NULL))
  }
  
  weights_name <- rlang::as_name(weights_quo)
  if (weights_name %in% names(data)) {
    return(list(vector = data[[weights_name]], name = weights_name))
  } else {
    warning(sprintf("Weights variable '%s' not found in data. Using unweighted calculation.", weights_name))
    return(list(vector = NULL, name = NULL))
  }
}

# ============================================================================
# MAIN CALCULATION FUNCTIONS
# ============================================================================

#' Calculate statistics for ungrouped data
#' @keywords internal
.calculate_ungrouped_stats <- function(data, vars, weights_info, show, probs, na.rm, excess) {
  results_list <- list()
  
  for (i in seq_along(vars)) {
    var_name <- names(vars)[i]
    x <- data[[var_name]]
    w <- weights_info$vector
    
    # Calculate all statistics for this variable
    var_stats <- .calculate_variable_stats(x, w, var_name, show, probs, na.rm, excess)
    
    # Add to results with variable prefix for column names
    for (stat_name in names(var_stats)) {
      col_name <- paste0(var_name, "_", stat_name)
      results_list[[col_name]] <- var_stats[[stat_name]]
    }
  }
  
  return(tibble::tibble(!!!results_list))
}

#' Calculate statistics for grouped data
#' @keywords internal
.calculate_grouped_stats <- function(data, vars, weights_info, show, probs, na.rm, excess) {
  results <- data %>%
    dplyr::group_modify(~ {
      group_data <- .x
      results_list <- list()
      
      for (i in seq_along(vars)) {
        var_name <- names(vars)[i]
        x <- group_data[[var_name]]
        w <- if (!is.null(weights_info$name)) group_data[[weights_info$name]] else NULL
        
        # Calculate all statistics for this variable in this group
        var_stats <- .calculate_variable_stats(x, w, var_name, show, probs, na.rm, excess)
        
        # Add to results with variable prefix for column names
        for (stat_name in names(var_stats)) {
          col_name <- paste0(var_name, "_", stat_name)
          results_list[[col_name]] <- var_stats[[stat_name]]
        }
      }
      
      tibble::tibble(!!!results_list)
    })
  
  return(results %>% dplyr::ungroup())
}

#' Calculate all requested statistics for a single variable
#' @keywords internal
.calculate_variable_stats <- function(x, w, var_name, show, probs, na.rm, excess) {
  stats_list <- list()
  
  # Calculate missing values count
  n_missing <- sum(is.na(x))
  
  # Calculate each requested statistic
  if ("mean" %in% show) stats_list$Mean <- .w_mean(x, w, na.rm)
  if ("median" %in% show) stats_list$Median <- .w_median(x, w, na.rm)
  if ("sd" %in% show) stats_list$SD <- .w_sd(x, w, na.rm)
  if ("se" %in% show) stats_list$SE <- .w_se(x, w, na.rm)
  if ("var" %in% show) stats_list$Variance <- .w_var(x, w, na.rm)
  if ("range" %in% show) stats_list$Range <- .w_range(x, w, na.rm)
  if ("iqr" %in% show) stats_list$IQR <- .w_iqr(x, w, na.rm)
  if ("skew" %in% show) stats_list$Skewness <- .w_skew(x, w, na.rm)
  if ("kurtosis" %in% show) stats_list$Kurtosis <- .w_kurtosis(x, w, na.rm, excess)
  if ("mode" %in% show) stats_list$Mode <- .w_mode(x, w, na.rm)
  
  # Handle quantiles - can be multiple values
  if ("quantiles" %in% show) {
    quantiles <- .w_quantile(x, w, probs = probs, na.rm = na.rm)
    for (i in seq_along(probs)) {
      prob_name <- paste0("Q", probs[i] * 100)
      stats_list[[prob_name]] <- quantiles[i]
    }
  }
  
  # Add sample size information
  if (is.null(w)) {
    stats_list$N <- sum(!is.na(x))
  } else {
    # Only sum weights where x is not missing (like frequency.R does)
    stats_list$N <- sum(w[!is.na(x)], na.rm = TRUE)
    # Effective N should also only consider weights for non-missing x
    stats_list$Effective_N <- .effective_n(w[!is.na(x)])
  }
  
  stats_list$Missing <- n_missing
  
  return(stats_list)
}

# ============================================================================
# STATISTICAL CALCULATION FUNCTIONS
# ============================================================================

#' Validate and clean weights - central validation function
#' @keywords internal
.validate_and_clean_weights <- function(x, weights, na.rm = TRUE) {
  if (is.null(weights)) {
    return(list(x = x, weights = NULL, valid = TRUE))
  }
  
  # Basic validation checks
  if (length(weights) != length(x)) {
    warning("Length of weights does not match length of data. Using unweighted calculation.")
    return(list(x = x, weights = NULL, valid = FALSE))
  }
  
  if (any(weights < 0, na.rm = TRUE)) {
    warning("Negative weights found. Using unweighted calculation.")
    return(list(x = x, weights = NULL, valid = FALSE))
  }
  
  if (all(is.na(weights))) {
    warning("All weights are missing. Using unweighted calculation.")
    return(list(x = x, weights = NULL, valid = FALSE))
  }
  
  # Remove missing values if requested
  if (na.rm) {
    complete_cases <- !is.na(x) & !is.na(weights)
    x_clean <- x[complete_cases]
    weights_clean <- weights[complete_cases]
  } else {
    x_clean <- x
    weights_clean <- weights
  }
  
  return(list(x = x_clean, weights = weights_clean, valid = TRUE))
}

#' Calculate effective sample size for weighted data
#' @keywords internal
.effective_n <- function(weights) {
  if (is.null(weights)) return(length(weights))
  sum_w <- sum(weights, na.rm = TRUE)
  sum_w2 <- sum(weights^2, na.rm = TRUE)
  return(sum_w^2 / sum_w2)
}

#' Weighted mean
#' @keywords internal
.w_mean <- function(x, weights = NULL, na.rm = TRUE) {
  cleaned <- .validate_and_clean_weights(x, weights, na.rm)
  
  if (!cleaned$valid || is.null(cleaned$weights)) {
    return(mean(cleaned$x, na.rm = na.rm))
  }
  
  if (sum(cleaned$weights, na.rm = TRUE) == 0) {
    return(NA_real_)
  }
  
  return(sum(cleaned$x * cleaned$weights, na.rm = na.rm) / sum(cleaned$weights, na.rm = na.rm))
}

#' Weighted median using cumulative weights approach
#' @keywords internal
.w_median <- function(x, weights = NULL, na.rm = TRUE) {
  cleaned <- .validate_and_clean_weights(x, weights, na.rm)
  
  if (!cleaned$valid || is.null(cleaned$weights)) {
    return(median(cleaned$x, na.rm = na.rm))
  }
  
  x_clean <- cleaned$x
  w_clean <- cleaned$weights
  
  if (length(x_clean) == 0) return(NA_real_)
  
  # Sort by x values and calculate cumulative weights
  ord <- order(x_clean)
  x_sorted <- x_clean[ord]
  w_sorted <- w_clean[ord]
  cum_w <- cumsum(w_sorted)
  total_w <- sum(w_sorted)
  median_pos <- total_w / 2
  
  # Find median value using cumulative weights
  if (any(cum_w == median_pos)) {
    idx <- which(cum_w == median_pos)[1]
    if (idx < length(x_sorted)) {
      return((x_sorted[idx] + x_sorted[idx + 1]) / 2)
    } else {
      return(x_sorted[idx])
    }
  } else {
    idx <- which(cum_w > median_pos)[1]
    return(x_sorted[idx])
  }
}

#' Weighted variance with Bessel's correction
#' @keywords internal
.w_var <- function(x, weights = NULL, na.rm = TRUE) {
  cleaned <- .validate_and_clean_weights(x, weights, na.rm)
  
  if (!cleaned$valid || is.null(cleaned$weights)) {
    return(var(cleaned$x, na.rm = na.rm))
  }
  
  x_clean <- cleaned$x
  w_clean <- cleaned$weights
  
  if (length(x_clean) <= 1) return(NA_real_)
  
  # Calculate weighted variance using SPSS formula
  # SPSS uses: variance = sum(w * (x - w_mean)^2) / (V1 - 1)
  # where V1 = sum of weights
  w_mean <- .w_mean(x_clean, w_clean, na.rm = na.rm)
  V1 <- sum(w_clean, na.rm = na.rm)  # Sum of weights
  
  if (V1 <= 1) return(NA_real_)  # Need V1 > 1 for denominator
  
  numerator <- sum(w_clean * (x_clean - w_mean)^2, na.rm = na.rm)
  denominator <- V1 - 1  # SPSS formula: V1 - 1
  
  return(numerator / denominator)
}

#' Weighted standard deviation
#' @keywords internal
.w_sd <- function(x, weights = NULL, na.rm = TRUE) {
  return(sqrt(.w_var(x, weights, na.rm)))
}

#' Weighted standard error using SPSS formula
#' @keywords internal
.w_se <- function(x, weights = NULL, na.rm = TRUE) {
  cleaned <- .validate_and_clean_weights(x, weights, na.rm)
  
  if (!cleaned$valid || is.null(cleaned$weights)) {
    n <- sum(!is.na(x))
    return(sd(x, na.rm = na.rm) / sqrt(n))
  }
  
  # Calculate weighted standard error using SPSS formula
  # SPSS uses: SE = SD / sqrt(V1) where V1 = sum of weights
  V1 <- sum(cleaned$weights, na.rm = na.rm)
  
  if (V1 <= 0) return(NA_real_)
  
  w_sd <- .w_sd(cleaned$x, cleaned$weights, na.rm = na.rm)
  return(w_sd / sqrt(V1))  # SPSS formula: SD / sqrt(V1)
}

#' Weighted range (uses actual data range)
#' @keywords internal
.w_range <- function(x, weights = NULL, na.rm = TRUE) {
  cleaned <- .validate_and_clean_weights(x, weights, na.rm)
  
  if (!cleaned$valid || is.null(cleaned$weights)) {
    return(diff(range(cleaned$x, na.rm = na.rm)))
  }
  
  # For weighted range, we still use the actual range of values
  return(diff(range(cleaned$x, na.rm = na.rm)))
}

#' Weighted IQR using weighted quantiles
#' @keywords internal
.w_iqr <- function(x, weights = NULL, na.rm = TRUE) {
  q75 <- .w_quantile(x, weights, probs = 0.75, na.rm = na.rm)
  q25 <- .w_quantile(x, weights, probs = 0.25, na.rm = na.rm)
  return(q75 - q25)
}

#' Weighted quantiles using cumulative weights
#' @keywords internal
.w_quantile <- function(x, weights = NULL, probs = 0.5, na.rm = TRUE) {
  cleaned <- .validate_and_clean_weights(x, weights, na.rm)
  
  if (!cleaned$valid || is.null(cleaned$weights)) {
    return(quantile(cleaned$x, probs = probs, na.rm = na.rm))
  }
  
  x_clean <- cleaned$x
  w_clean <- cleaned$weights
  
  if (length(x_clean) == 0) return(rep(NA_real_, length(probs)))
  
  # Sort by x values and calculate cumulative weights
  ord <- order(x_clean)
  x_sorted <- x_clean[ord]
  w_sorted <- w_clean[ord]
  cum_w <- cumsum(w_sorted)
  total_w <- sum(w_sorted)
  
  # Calculate quantiles for each probability
  result <- numeric(length(probs))
  for (i in seq_along(probs)) {
    target_w <- probs[i] * total_w
    
    if (target_w <= cum_w[1]) {
      result[i] <- x_sorted[1]
    } else if (target_w >= cum_w[length(cum_w)]) {
      result[i] <- x_sorted[length(x_sorted)]
    } else {
      idx <- which(cum_w >= target_w)[1]
      if (cum_w[idx] == target_w && idx < length(x_sorted)) {
        result[i] <- (x_sorted[idx] + x_sorted[idx + 1]) / 2
      } else {
        result[i] <- x_sorted[idx]
      }
    }
  }
  
  names(result) <- paste0(probs * 100, "%")
  return(result)
}

#' Weighted skewness with bias correction
#' @keywords internal
.w_skew <- function(x, weights = NULL, na.rm = TRUE) {
  cleaned <- .validate_and_clean_weights(x, weights, na.rm)
  
  if (!cleaned$valid || is.null(cleaned$weights)) {
    # Unweighted skewness with SPSS formula
    if (any(ina <- is.na(x)))
      x <- x[!ina]

    n <- length(x)

    if (n < 3) return(NA_real_)

    mean_x <- mean(x)
    sd_x <- sd(x)  # Uses n-1 divisor

    if (sd_x <= 0) return(NA_real_)

    # SPSS formula for skewness with bias correction
    skew_spss <- n * sum((x - mean_x)^3) / ((n - 1) * (n - 2) * sd_x^3)
    return(skew_spss)
  }
  
  # Weighted skewness calculation
  x_clean <- cleaned$x
  w_clean <- cleaned$weights

  if (length(x_clean) < 3) return(NA_real_)

  # Use SPSS-compatible simple weighted formula (as in frequency.R)
  # SPSS treats weights as frequency weights without bias correction
  w_norm <- w_clean / sum(w_clean, na.rm = na.rm)
  w_mean <- sum(x_clean * w_norm, na.rm = na.rm)

  # Calculate weighted standard deviation
  w_sd <- sqrt(sum(w_norm * (x_clean - w_mean)^2, na.rm = na.rm))

  if (w_sd <= 0) return(NA_real_)

  # Simple weighted skewness without bias correction (SPSS approach)
  skew_weighted <- sum(w_norm * ((x_clean - w_mean) / w_sd)^3, na.rm = na.rm)

  return(skew_weighted)
}

#' Weighted kurtosis with bias correction
#' @keywords internal
.w_kurtosis <- function(x, weights = NULL, na.rm = TRUE, excess = TRUE) {
  cleaned <- .validate_and_clean_weights(x, weights, na.rm)
  
  if (!cleaned$valid || is.null(cleaned$weights)) {
    # Unweighted kurtosis with bias correction
    x_clean <- if (na.rm) x[!is.na(x)] else x
    if (length(x_clean) < 4) return(NA_real_)
    
    n <- length(x_clean)
    mean_x <- mean(x_clean)
    var_x <- var(x_clean)  # Uses n-1 divisor

    if (var_x <= 0) return(NA_real_)

    # SPSS formula for kurtosis (already excess kurtosis)
    kurt_spss <- (n * (n + 1) * sum((x_clean - mean_x)^4)) /
                 ((n - 1) * (n - 2) * (n - 3) * var_x^2) -
                 (3 * (n - 1)^2) / ((n - 2) * (n - 3))

    return(if (excess) kurt_spss else kurt_spss + 3)
  }
  
  # Weighted kurtosis calculation
  x_clean <- cleaned$x
  w_clean <- cleaned$weights

  if (length(x_clean) < 4) return(NA_real_)

  # Use SPSS-compatible simple weighted formula (as in frequency.R)
  # SPSS treats weights as frequency weights without bias correction
  w_norm <- w_clean / sum(w_clean, na.rm = na.rm)
  w_mean <- sum(x_clean * w_norm, na.rm = na.rm)

  # Calculate weighted standard deviation
  w_sd <- sqrt(sum(w_norm * (x_clean - w_mean)^2, na.rm = na.rm))

  if (w_sd <= 0) return(NA_real_)

  # Simple weighted kurtosis (standardized 4th moment)
  kurt_raw <- sum(w_norm * ((x_clean - w_mean) / w_sd)^4, na.rm = na.rm)

  return(if (excess) kurt_raw - 3 else kurt_raw)
}

#' Weighted mode (most frequent value by weight)
#' @keywords internal
.w_mode <- function(x, weights = NULL, na.rm = TRUE) {
  cleaned <- .validate_and_clean_weights(x, weights, na.rm)
  
  if (!cleaned$valid || is.null(cleaned$weights)) {
    # Unweighted mode
    x_clean <- if (na.rm) x[!is.na(x)] else x
    if (length(x_clean) == 0) return(NA_real_)
    
    freq_table <- table(x_clean)
    mode_val <- as.numeric(names(freq_table)[which.max(freq_table)])
    return(mode_val)
  }
  
  # Weighted mode
  x_clean <- cleaned$x
  w_clean <- cleaned$weights
  
  if (length(x_clean) == 0) return(NA_real_)
  
  unique_vals <- unique(x_clean)
  weighted_freqs <- numeric(length(unique_vals))
  
  for (i in seq_along(unique_vals)) {
    weighted_freqs[i] <- sum(w_clean[x_clean == unique_vals[i]], na.rm = na.rm)
  }
  
  mode_val <- unique_vals[which.max(weighted_freqs)]
  return(mode_val)
}

# ============================================================================
# PRINT METHOD AND FORMATTING
# ============================================================================

#' Print method for describe objects
#'
#' @description
#' Prints formatted descriptive statistics with professional layout.
#' Automatically adjusts output based on whether analysis was weighted or unweighted.
#'
#' @param x A describe object
#' @param ... Additional arguments (currently unused)
#' @export
print.describe <- function(x, ...) {

  # Determine if weighted analysis
  is_weighted <- !is.null(x$weights)

  # Print header using standardized helper
  title <- get_standard_title("Descriptive", x$weights, "Statistics")
  print_header(title)

  # Print results based on grouping
  if (x$grouped) {
    .print_grouped_results(x, is_weighted)
  } else {
    .print_ungrouped_results(x, is_weighted)
  }

  invisible(x)
}

# Note: .print_header function removed - using standardized print_header from print_helpers.R

#' Print results for ungrouped data
#' @keywords internal
.print_ungrouped_results <- function(x, is_weighted) {
  output_df <- .create_output_df(x$results, x$variables, x$show, is_weighted)
  print(output_df, row.names = FALSE)
  
  # Print footer border
  print_separator(get_table_width(output_df))
}

#' Print results for grouped data
#' @keywords internal
.print_grouped_results <- function(x, is_weighted) {
  group_vars <- x$group_vars
  results_df <- x$results
  
  # Get unique group combinations
  group_combinations <- results_df %>%
    dplyr::select(dplyr::all_of(group_vars)) %>%
    dplyr::distinct()
  
  for (i in 1:nrow(group_combinations)) {
    # Filter data for this group
    group_filter <- group_combinations[i, , drop = FALSE]
    filter_condition <- TRUE
    for (gvar in group_vars) {
      filter_condition <- filter_condition & (results_df[[gvar]] == group_filter[[gvar]])
    }
    group_data <- results_df[filter_condition, , drop = FALSE]
    
    # Print group header using standardized helper
    print_group_header(group_filter)
    
    # Create and print output
    temp_output <- .create_output_df(group_data, x$variables, x$show, is_weighted)
    print_separator(get_table_width(temp_output))
    print(temp_output, row.names = FALSE)
    print_separator(get_table_width(temp_output))
  }
}

#' Create formatted output data frame for printing
#' @keywords internal
.create_output_df <- function(results_df, variables, show, is_weighted) {
  output_rows <- list()
  
  for (var_name in variables) {
    row_data <- list(Variable = var_name)
    
    # Extract statistics for this variable
    for (stat in show) {
      col_name <- paste0(var_name, "_", 
                        switch(stat,
                               "mean" = "Mean", "median" = "Median", "sd" = "SD",
                               "se" = "SE", "var" = "Variance", "range" = "Range",
                               "iqr" = "IQR", "skew" = "Skewness", "kurtosis" = "Kurtosis",
                               "mode" = "Mode", "quantiles" = "Q25"))
      
      if (stat == "quantiles") {
        # Handle quantiles specially - can be multiple columns
        q_cols <- grep(paste0("^", var_name, "_Q"), names(results_df), value = TRUE)
        for (q_col in q_cols) {
          q_name <- gsub(paste0("^", var_name, "_"), "", q_col)
          if (q_col %in% names(results_df)) {
            row_data[[q_name]] <- round(results_df[[q_col]], 3)
          }
        }
      } else if (col_name %in% names(results_df)) {
        value <- results_df[[col_name]]
        if (is.numeric(value)) {
          stat_name <- switch(stat, "mean" = "Mean", "median" = "Median", "sd" = "SD",
                             "se" = "SE", "var" = "Variance", "range" = "Range",
                             "iqr" = "IQR", "skew" = "Skewness", "kurtosis" = "Kurtosis",
                             "mode" = "Mode")
          row_data[[stat_name]] <- round(value, 3)
        } else {
          row_data[["Mode"]] <- value
        }
      }
    }
    
    # Add sample size information - different for weighted vs unweighted
    if (is_weighted) {
      # For weighted analysis, show effective sample size
      eff_n_col <- paste0(var_name, "_Effective_N")
      if (eff_n_col %in% names(results_df)) {
        row_data$Effective_N <- round(results_df[[eff_n_col]], 1)
      }
    } else {
      # For unweighted analysis, show regular N and missing count
      n_col <- paste0(var_name, "_N")
      if (n_col %in% names(results_df)) {
        row_data$N <- round(results_df[[n_col]], 0)
      }
      
      missing_col <- paste0(var_name, "_Missing")
      if (missing_col %in% names(results_df)) {
        row_data$Missing <- round(results_df[[missing_col]], 0)
      }
    }
    
    output_rows[[length(output_rows) + 1]] <- row_data
  }
  
  # Convert to data frame
  output_df <- do.call(rbind, lapply(output_rows, function(x) {
    data.frame(x, stringsAsFactors = FALSE)
  }))
  
  return(output_df)
}

# Note: .get_border function removed - using standardized get_table_width from print_helpers.R
 