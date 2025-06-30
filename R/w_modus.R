
#' Weighted Mode (Modus)
#'
#' Calculate weighted mode for variables, with support for 
#' grouped data and multiple variables simultaneously.
#'
#' @param data A data frame, or a vector when used in summarise() context
#' @param ... Variable names (unquoted) or tidyselect expressions
#' @param weights Name of the weights variable (unquoted), or a numeric vector of weights
#' @param na.rm Logical; if TRUE, missing values are removed
#'
#' @return A w_modus object (list) containing results and metadata, or values in summarise context
#' @export
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#' 
#' # Basic weighted mode (most frequent value)
#' survey_data %>% w_modus(gender, weights = sampling_weight)
#' 
#' # Multiple variables (works best with categorical/discrete data)
#' survey_data %>% w_modus(gender, region, education, weights = sampling_weight)
#' 
#' # Grouped data
#' survey_data %>% group_by(region) %>% w_modus(gender, weights = sampling_weight)
#' 
#' # In summarise context
#' survey_data %>% summarise(mode_gender = w_modus(gender, weights = sampling_weight))
#' 
#' # Unweighted (for comparison)
#' survey_data %>% w_modus(gender)
w_modus <- function(data, ..., weights = NULL, na.rm = TRUE) {
  
  # Handle the case where data is a vector (e.g., in summarise context)
  if (!is.data.frame(data)) {
    # This is the summarise() context - data is actually the variable values
    x <- data
    weights_arg <- substitute(weights)
    
    # Use helper function to evaluate weights
    weights_vec <- .evaluate_weights(weights_arg, parent.frame())
    
    # Calculate weighted or unweighted mode
    if (!.are_weights(weights_vec) || !.validate_weights(weights_vec, verbose = FALSE)) {
      # Unweighted calculation
      if (na.rm) {
        x <- x[!is.na(x)]
      }
      
      if (length(x) == 0) {
        result <- NA
      } else {
        # Calculate unweighted mode
        freq_table <- table(x)
        max_freq <- max(freq_table)
        modes <- names(freq_table)[freq_table == max_freq]
        
        # Return first mode if multiple exist
        result <- modes[1]
        
        # Convert back to original type if numeric
        if (is.numeric(data)) {
          result <- as.numeric(result)
        }
      }
    } else {
      # Weighted calculation
      if (na.rm) {
        valid <- !is.na(x) & !is.na(weights_vec)
        x <- x[valid]
        weights_vec <- weights_vec[valid]
      }
      
      if (length(x) == 0) {
        result <- NA
      } else {
        # Calculate weighted mode
        unique_vals <- unique(x)
        weighted_freqs <- sapply(unique_vals, function(val) {
          sum(weights_vec[x == val])
        })
        
        max_weight <- max(weighted_freqs)
        modes <- unique_vals[weighted_freqs == max_weight]
        
        # Return first mode if multiple exist
        result <- modes[1]
      }
    }
    
    return(result)
  }
  
  # Original data frame handling code
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  # Get variable names using tidyselect
  vars <- .process_variables(data, ...)
  
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
  
  if (is_grouped) {
    # Handle grouped data
    group_vars <- dplyr::group_vars(data)
    
    results <- data %>%
      dplyr::group_modify(~ {
        group_data <- .x
        results_list <- list()
        
        for (i in seq_along(vars)) {
          var_name <- names(vars)[i]
          x <- group_data[[var_name]]
          
          if (is.null(weights_vec)) {
            # Unweighted calculation
            if (na.rm) {
              x_clean <- x[!is.na(x)]
            } else {
              x_clean <- x
            }
            
            if (length(x_clean) == 0) {
              mode_val <- NA
              n_val <- 0
              n_eff <- 0
            } else {
              # Calculate unweighted mode
              freq_table <- table(x_clean)
              max_freq <- max(freq_table)
              modes <- names(freq_table)[freq_table == max_freq]
              mode_val <- modes[1]
              
              # Convert back to original type if numeric
              if (is.numeric(x)) {
                mode_val <- as.numeric(mode_val)
              }
              
              n_val <- length(x_clean)
              n_eff <- n_val
            }
          } else {
            # Weighted calculation
            w <- group_data[[weights_name]]
            if (na.rm) {
              valid <- !is.na(x) & !is.na(w)
              x_clean <- x[valid]
              w_clean <- w[valid]
            } else {
              x_clean <- x
              w_clean <- w
            }
            
            if (length(x_clean) == 0) {
              mode_val <- NA
              n_val <- 0
              n_eff <- 0
            } else {
              # Calculate weighted mode
              unique_vals <- unique(x_clean)
              weighted_freqs <- sapply(unique_vals, function(val) {
                sum(w_clean[x_clean == val])
              })
              
              max_weight <- max(weighted_freqs)
              modes <- unique_vals[weighted_freqs == max_weight]
              mode_val <- modes[1]
              
              n_val <- sum(w_clean)
              n_eff <- .effective_n(w_clean)
            }
          }
          
          results_list[[paste0(var_name, "_value")]] <- mode_val
          results_list[[paste0(var_name, "_n")]] <- n_val
          results_list[[paste0(var_name, "_n_eff")]] <- n_eff
        }
        
        tibble::tibble(!!!results_list)
      })
    
  } else {
    # Handle ungrouped data
    results_list <- list()
    
    for (i in seq_along(vars)) {
      var_name <- names(vars)[i]
      x <- data[[var_name]]
      
      if (is.null(weights_vec)) {
        # Unweighted calculation
        if (na.rm) {
          x_clean <- x[!is.na(x)]
        } else {
          x_clean <- x
        }
        
        if (length(x_clean) == 0) {
          mode_val <- NA
          n_val <- 0
          n_eff <- 0
        } else {
          # Calculate unweighted mode
          freq_table <- table(x_clean)
          max_freq <- max(freq_table)
          modes <- names(freq_table)[freq_table == max_freq]
          mode_val <- modes[1]
          
          # Convert back to original type if numeric
          if (is.numeric(x)) {
            mode_val <- as.numeric(mode_val)
          }
          
          n_val <- length(x_clean)
          n_eff <- n_val
        }
      } else {
        # Weighted calculation
        if (na.rm) {
          valid <- !is.na(x) & !is.na(weights_vec)
          x_clean <- x[valid]
          w_clean <- weights_vec[valid]
        } else {
          x_clean <- x
          w_clean <- weights_vec
        }
        
        if (length(x_clean) == 0) {
          mode_val <- NA
          n_val <- 0
          n_eff <- 0
        } else {
          # Calculate weighted mode
          unique_vals <- unique(x_clean)
          weighted_freqs <- sapply(unique_vals, function(val) {
            sum(w_clean[x_clean == val])
          })
          
          max_weight <- max(weighted_freqs)
          modes <- unique_vals[weighted_freqs == max_weight]
          mode_val <- modes[1]
          
          n_val <- sum(w_clean)
          n_eff <- .effective_n(w_clean)
        }
      }
      
      results_list[[paste0(var_name, "_value")]] <- mode_val
      results_list[[paste0(var_name, "_n")]] <- n_val
      results_list[[paste0(var_name, "_n_eff")]] <- n_eff
    }
    
    results_df <- tibble::tibble(!!!results_list)
  }
  
  # Create hybrid results structure for test compatibility
  hybrid_results <- if (is_grouped) results else results_df
  
  # Add test-compatible column names based on whether analysis is weighted
  stat_prefix <- if (!is.null(weights_name)) "weighted_" else ""
  
  # For multiple variables, create long format with Variable column
  if (length(vars) > 1 && !is_grouped) {
    # Long format for multiple variables
    long_results <- list()
    for (i in seq_along(vars)) {
      var <- names(vars)[i]
      value_col <- paste0(var, "_value")
      if (value_col %in% names(results_df)) {
        row_data <- data.frame(
          Variable = var,
          stringsAsFactors = FALSE
        )
        
        # Add the statistic value
        row_data[[paste0(stat_prefix, "mode")]] <- results_df[[value_col]]
        
        # Add effective n
        n_eff_col <- paste0(var, "_n_eff")
        if (n_eff_col %in% names(results_df)) {
          row_data[[paste0(stat_prefix, "n")]] <- results_df[[n_eff_col]]
          row_data[["effective_n"]] <- results_df[[n_eff_col]]  # Test-specific name
        }
        
        long_results[[i]] <- row_data
      }
    }
    hybrid_results <- do.call(rbind, long_results)
  } else {
    # Single variable or grouped: add test-compatible columns
    for (var in names(vars)) {
      value_col <- paste0(var, "_value")
      if (value_col %in% names(results_df)) {
        # Add test-compatible columns while preserving variable-based access
        hybrid_results[[paste0(stat_prefix, "mode")]] <- results_df[[value_col]]
        
        # Add effective n columns
        n_eff_col <- paste0(var, "_n_eff")
        if (n_eff_col %in% names(results_df)) {
          hybrid_results[[paste0(stat_prefix, "n")]] <- results_df[[n_eff_col]]
          hybrid_results[["effective_n"]] <- results_df[[n_eff_col]]  # Test-specific name
        }
      }
    }
  }

  # Create S3 object with hybrid compatibility
  result <- list(
    results = hybrid_results,
    variables = names(vars),
    weights = weights_name,          # Current field name
    weight_var = weights_name,       # Test-compatible field name
    grouped = is_grouped,            # Current field
    is_grouped = is_grouped,         # Test-compatible field
    group_vars = if (is_grouped) dplyr::group_vars(data) else NULL,  # Current field
    groups = if (is_grouped) dplyr::group_vars(data) else NULL        # Test-compatible field
  )
  
  class(result) <- "w_modus"
  return(result)
}

#' Print method for w_modus objects
#'
#' @description
#' Prints a formatted summary of weighted mode calculations, including mode values,
#' sample sizes, and weights (if applicable). For grouped data, results are
#' displayed by group.
#'
#' @param x An object of class "w_modus"
#' @param digits Number of decimal places to display (default: 3)
#' @param ... Additional arguments passed to print
#'
#' @return Invisibly returns the input object
#' @keywords internal
#' @export
print.w_modus <- function(x, digits = 3, ...) {
  # Determine if this is weighted or unweighted
  is_weighted <- !is.null(x$weights)
  
  if (is_weighted) {
    .print_header("Weighted Mode")
  } else {
    .print_header("Mode")
  }
  
  if (x$grouped) {
    # Get unique groups
    groups <- unique(x$results[x$group_vars])
    
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
      
      # Skip empty groups
      if (nrow(group_results) == 0) next
      
      # Create results table for this group - one row per variable
      results_rows <- list()
      
      for (var in x$variables) {
        value_col <- paste0(var, "_value")
        if (value_col %in% names(group_results)) {
          mode_value <- group_results[[value_col]]
          
          # Format mode value appropriately
          if (is.numeric(mode_value)) {
            mode_formatted <- round(mode_value, digits)
          } else {
            mode_formatted <- as.character(mode_value)
          }
          
          var_row <- data.frame(
            Variable = var,
            Mode = mode_formatted,
            stringsAsFactors = FALSE
          )
          
          # Add N and Effective_N for this variable
          n_col <- paste0(var, "_n")
          n_eff_col <- paste0(var, "_n_eff")
          if (n_col %in% names(group_results)) {
            var_row$N <- round(group_results[[n_col]], 1)
          }
          if (n_eff_col %in% names(group_results)) {
            # Only add Effective_N for weighted calculations
            if (is_weighted) {
              var_row$Effective_N <- round(group_results[[n_eff_col]], 1)
            }
          }
          
          if (!is.null(x$weights)) {
            var_row$Weights <- x$weights
          }
          
          results_rows[[var]] <- var_row
        }
      }
      
      results_df <- do.call(rbind, results_rows)
      
      # Print group header and results
      cat(sprintf("\nGroup: %s\n", group_info))
      results_df_print <- as.data.frame(results_df)
      .print_border(results_df_print)
      print(results_df_print, row.names = FALSE)
      .print_border(results_df_print)
    }
  } else {
    # Print ungrouped results
    # Skip if no results
    if (nrow(x$results) == 0) {
      cat("\nNo valid results to display.\n")
      return(invisible(x))
    }
    
    results_df <- data.frame(
      Variable = x$variables,
      stringsAsFactors = FALSE
    )
    
    # Add the statistic values for each variable
    for (var in x$variables) {
      value_col <- paste0(var, "_value")
      if (value_col %in% names(x$results)) {
        mode_value <- x$results[[value_col]]
        
        # Format mode value appropriately
        if (is.numeric(mode_value)) {
          results_df$Mode[results_df$Variable == var] <- round(mode_value, digits)
        } else {
          results_df$Mode[results_df$Variable == var] <- as.character(mode_value)
        }
        
        # Add N and Effective_N for this variable
        n_col <- paste0(var, "_n")
        n_eff_col <- paste0(var, "_n_eff")
        if (n_col %in% names(x$results)) {
          results_df$N[results_df$Variable == var] <- round(x$results[[n_col]], 1)
        }
        if (n_eff_col %in% names(x$results)) {
          # Only add Effective_N for weighted calculations
          if (is_weighted) {
            results_df$Effective_N[results_df$Variable == var] <- round(x$results[[n_eff_col]], 1)
          }
        }
      }
    }
    
    if (!is.null(x$weights)) {
      results_df$Weights <- x$weights
    }
    
    results_df_print <- as.data.frame(results_df)
    print(results_df_print, row.names = FALSE)
    .print_border(results_df_print)
  }
  
  invisible(x)
} 


