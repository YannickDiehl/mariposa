
#' Weighted Standard Error
#'
#' Calculate weighted standard error for numeric variables, with support for 
#' grouped data and multiple variables simultaneously.
#'
#' @param data A data frame, or a numeric vector when used in summarise() context
#' @param ... Variable names (unquoted) or tidyselect expressions
#' @param weights Name of the weights variable (unquoted), or a numeric vector of weights
#' @param na.rm Logical; if TRUE, missing values are removed
#'
#' @return A w_se object (list) containing results and metadata, or numeric values in summarise context
#' 
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#' 
#' # Basic weighted standard error
#' survey_data %>% w_se(age, weights = sampling_weight)
#' 
#' # Multiple variables
#' survey_data %>% w_se(age, income, life_satisfaction, weights = sampling_weight)
#' 
#' # Grouped data
#' survey_data %>% group_by(region) %>% w_se(age, weights = sampling_weight)
#' 
#' # In summarise context
#' survey_data %>% summarise(se_age = w_se(age, weights = sampling_weight))
#' 
#' # Unweighted (for comparison)
#' survey_data %>% w_se(age)
#'
#' @export
w_se <- function(data, ..., weights = NULL, na.rm = TRUE) {
  
  # Handle the case where data is a vector (e.g., in summarise context)
  if (is.numeric(data) && !is.data.frame(data)) {
    # This is the summarise() context - data is actually the variable values
    x <- data
    weights_arg <- substitute(weights)
    
    # Use helper function to evaluate weights
    weights_vec <- .evaluate_weights(weights_arg, parent.frame())
    
    # Calculate weighted or unweighted standard error
    if (!.are_weights(weights_vec) || !.validate_weights(weights_vec, verbose = FALSE)) {
      # Unweighted calculation
      if (na.rm) {
        x <- x[!is.na(x)]
      }
      result <- sd(x, na.rm = na.rm) / sqrt(length(x[!is.na(x)]))
    } else {
      # Weighted calculation
      if (na.rm) {
        valid <- !is.na(x) & !is.na(weights_vec)
        x <- x[valid]
        weights_vec <- weights_vec[valid]
      }
      
      if (length(x) == 0) {
        result <- NA_real_
      } else {
        # Weighted standard error calculation (matches sjstats exactly)
        weighted_mean <- sum(x * weights_vec) / sum(weights_vec)
        # Use sample-based variance (denominator: sum(w) - 1)
        weighted_var_sample <- sum(weights_vec * (x - weighted_mean)^2) / (sum(weights_vec) - 1)
        # SE = sqrt(sample_var / n)
        result <- sqrt(weighted_var_sample / length(x))
      }
    }
    
    return(result)
  }
  
  # Original data frame handling code continues here...
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  # Get variable names using tidyselect
  vars <- tidyselect::eval_select(rlang::expr(c(...)), data)
  if (length(vars) == 0) {
    stop("No variables selected")
  }
  
  # Get weights
  weights_quo <- rlang::enquo(weights)
  if (rlang::quo_is_null(weights_quo)) {
    weights_vec <- NULL
    weights_name <- NULL
  } else {
    weights_name <- rlang::as_name(weights_quo)
    if (weights_name %in% names(data)) {
      weights_vec <- data[[weights_name]]
    } else {
      stop("Weights variable '", weights_name, "' not found in data")
    }
  }
  
  # Check if data is grouped
  is_grouped <- inherits(data, "grouped_df")
  group_vars <- if (is_grouped) dplyr::group_vars(data) else NULL
  
  # Helper function to calculate SE for a single variable
  calculate_se <- function(data, var_name, weight_name = NULL) {
    x_values <- data[[var_name]]
    
    if (is.null(weight_name)) {
      # Unweighted calculation
      if (na.rm) {
        x_values <- x_values[!is.na(x_values)]
      }
      se_val <- sd(x_values, na.rm = na.rm) / sqrt(length(x_values[!is.na(x_values)]))
      n_val <- length(x_values[!is.na(x_values)])
      effective_n <- n_val
    } else {
      # Weighted calculation
      w_values <- data[[weight_name]]
      if (na.rm) {
        valid <- !is.na(x_values) & !is.na(w_values)
        x_values <- x_values[valid]
        w_values <- w_values[valid]
      }
      
      if (length(x_values) == 0) {
        se_val <- NA_real_
        n_val <- 0
        effective_n <- 0
      } else {
        weighted_mean <- sum(x_values * w_values) / sum(w_values)
        # Use sample-based variance (denominator: sum(w) - 1) 
        weighted_var_sample <- sum(w_values * (x_values - weighted_mean)^2) / (sum(w_values) - 1)
        # SE = sqrt(sample_var / n)
        se_val <- sqrt(weighted_var_sample / length(x_values))
        n_val <- length(x_values)
        effective_n <- sum(w_values)^2 / sum(w_values^2)
      }
    }
    
    data.frame(
      Variable = var_name,
      SE = se_val,
      N = n_val,
      Effective_N = effective_n
    )
  }

  if (is_grouped) {
    # Split data by groups
    data_list <- group_split(data)
    group_keys <- group_keys(data)
    
    # Calculate SE for each group
    results_list <- lapply(seq_along(data_list), function(i) {
      group_data <- data_list[[i]]
      group_info <- group_keys[i, , drop = FALSE]
      
      # Calculate SE for each variable in this group
      group_results <- lapply(names(vars), function(var_name) {
        tryCatch({
          result <- calculate_se(group_data, var_name, weights_name)
          cbind(group_info, result)
        }, error = function(e) {
          data.frame(
            group_info,
            Variable = var_name,
            SE = NA,
            N = NA,
            Effective_N = NA
          )
        })
      })
      
      # Combine results for this group
      do.call(rbind, group_results)
    })
    
    # Combine all group results
    results_df <- do.call(rbind, results_list)
    
    # Remove rows with NA values
    results_df <- results_df[!is.na(results_df$SE), ]
    
    # Remove rows with NA values in group variables (if grouped)
    for (g in group_vars) {
      results_df <- results_df[!is.na(results_df[[g]]), ]
    }
  } else {
    # Calculate SE for each variable (ungrouped)
    results_list <- lapply(names(vars), function(var_name) {
      tryCatch({
        calculate_se(data, var_name, weights_name)
      }, error = function(e) {
        stop(e$message)  # Re-throw the error instead of returning NA values
      })
    })
    
    # Create result data frame
    results_df <- do.call(rbind, results_list)
    
    # Remove rows with NA values
    results_df <- results_df[!is.na(results_df$SE), ]
  }
  
  # Create hybrid results structure for test compatibility
  hybrid_results <- results_df
  
  # Add test-compatible column names based on whether analysis is weighted
  stat_prefix <- if (!is.null(weights_name)) "weighted_" else ""
  
  # w_se uses long format with SE column, not variable-specific columns
  if ("SE" %in% names(results_df)) {
    # Add test-compatible columns while preserving existing structure
    hybrid_results[[paste0(stat_prefix, "se")]] <- results_df[["SE"]]
    
    # Add effective n columns
    if ("Effective_N" %in% names(results_df)) {
      hybrid_results[[paste0(stat_prefix, "n")]] <- results_df[["Effective_N"]]
      hybrid_results[["effective_n"]] <- results_df[["Effective_N"]]  # Test-specific name
    }
  }

  # Create S3 object with hybrid compatibility
  result <- list(
    results = hybrid_results,
    variables = names(vars),
    weights = weights_name,          # Current field name
    weight_var = weights_name,       # Test-compatible field name
    groups = group_vars,             # Original field
    group_vars = group_vars,         # Alternative name
    is_grouped = is_grouped,         # Original field
    grouped = is_grouped             # Alternative name
  )
  
  class(result) <- "w_se"
  return(result)
}

#' Print method for w_se objects
#'
#' @description
#' Prints a formatted summary of weighted standard error calculations, including standard errors,
#' sample sizes, and weights (if applicable). For grouped data, results are
#' displayed by group.
#'
#' @param x An object of class "w_se"
#' @param digits Number of decimal places to display (default: 3)
#' @param ... Additional arguments passed to print
#'
#' @keywords internal
#' @export
print.w_se <- function(x, digits = 3, ...) {
  # Determine if this is weighted or unweighted
  is_weighted <- !is.null(x$weights)
  
  if (is_weighted) {
    .print_header("Weighted Standard Error")
  } else {
    .print_header("Standard Error")
  }
  
  if (x$is_grouped) {
    # Get unique groups
    groups <- unique(x$results[x$groups])
    
    # Print results for each group
    for (i in seq_len(nrow(groups))) {
      # Get current group values
      group_values <- groups[i, , drop = FALSE]
      
      # Format group info with factor levels if available
      group_info <- sapply(names(group_values), function(g) {
        val <- group_values[[g]]
        if (is.factor(val)) {
          paste(g, "=", levels(val)[val])
        } else {
          paste(g, "=", val)
        }
      })
      group_info <- paste(group_info, collapse = ", ")
      
      # Filter results for current group
      group_results <- x$results
      for (g in names(group_values)) {
        group_results <- group_results[group_results[[g]] == group_values[[g]], ]
      }
      
      # Remove rows with NA in Variable column
      group_results <- group_results[!is.na(group_results$Variable), ]
      
      # Skip empty groups
      if (nrow(group_results) == 0) next
      
      # Create results table for this group including group variables
      results_df <- group_results[, c(x$groups, "Variable"), drop = FALSE]
      results_df$SE <- round(group_results$SE, digits)
      results_df$N <- group_results$N
      
      if (!is.null(x$weights)) {
        # Only add Effective_N for weighted calculations
        if (is_weighted) {
          results_df$Effective_N <- group_results$Effective_N
        }
        results_df$Weights <- x$weights
      }
      
      # Print group header and results
      cat(sprintf("\nGroup: %s\n", group_info))
      .print_border(results_df)
      print(results_df, row.names = FALSE)
      .print_border(results_df)
    }
  } else {
    # Print ungrouped results
    # Remove rows with NA in Variable column
    x$results <- x$results[!is.na(x$results$Variable), ]
    
    # Skip if no results
    if (nrow(x$results) == 0) {
      cat("\nNo valid results to display.\n")
      return(invisible(x))
    }
    
    results_df <- data.frame(
      Variable = x$results$Variable,
      SE = round(x$results$SE, digits),
      N = x$results$N
    )
    
    if (!is.null(x$weights)) {
      # Only add Effective_N for weighted calculations
      if (is_weighted) {
        results_df$Effective_N <- x$results$Effective_N
      }
      results_df$Weights <- x$weights
    }
    
    print(results_df, row.names = FALSE)
    .print_border(results_df)
  }
}


