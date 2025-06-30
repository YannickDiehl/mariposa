
#' Weighted Kurtosis
#'
#' Calculate weighted kurtosis for numeric variables, with support for 
#' grouped data and multiple variables simultaneously.
#'
#' @param data A data frame, or a numeric vector when used in summarise() context
#' @param ... Variable names (unquoted) or tidyselect expressions
#' @param weights Name of the weights variable (unquoted), or a numeric vector of weights
#' @param na.rm Logical; if TRUE, missing values are removed
#' @param excess Logical; if TRUE, returns excess kurtosis (kurtosis - 3), if FALSE returns raw kurtosis (default: TRUE)
#'
#' @return A w_kurtosis object (list) containing results and metadata, or numeric values in summarise context
#' @export
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#' 
#' # Basic weighted kurtosis (excess kurtosis, default)
#' survey_data %>% w_kurtosis(age, weights = sampling_weight)
#' 
#' # Multiple variables
#' survey_data %>% w_kurtosis(age, income, life_satisfaction, weights = sampling_weight)
#' 
#' # Grouped data
#' survey_data %>% group_by(region) %>% w_kurtosis(age, weights = sampling_weight)
#' 
#' # Raw kurtosis (not excess)
#' survey_data %>% w_kurtosis(age, weights = sampling_weight, excess = FALSE)
#' 
#' # In summarise context
#' survey_data %>% summarise(kurt_age = w_kurtosis(age, weights = sampling_weight))
#' 
#' # Unweighted (for comparison)
#' survey_data %>% w_kurtosis(age)
w_kurtosis <- function(data, ..., weights = NULL, na.rm = TRUE, excess = TRUE) {
  
  # Handle the case where data is a vector (e.g., in summarise context)
  if (is.numeric(data) && !is.data.frame(data)) {
    # This is the summarise() context - data is actually the variable values
    x <- data
    weights_arg <- substitute(weights)
    
    # Use helper function to evaluate weights
    weights_vec <- .evaluate_weights(weights_arg, parent.frame())
    
    # Calculate weighted or unweighted kurtosis
    if (!.are_weights(weights_vec) || !.validate_weights(weights_vec, verbose = FALSE)) {
      # Unweighted calculation using moments package formula
      if (na.rm) {
        x <- x[!is.na(x)]
      }
      
      if (length(x) < 4) {
        result <- NA_real_
      } else {
        n <- length(x)
        mean_x <- mean(x)
        m2 <- sum((x - mean_x)^2) / n
        m4 <- sum((x - mean_x)^4) / n
        raw_kurtosis <- m4 / (m2^2)
        result <- if (excess) raw_kurtosis - 3 else raw_kurtosis
      }
    } else {
      # Weighted calculation
      if (na.rm) {
        valid <- !is.na(x) & !is.na(weights_vec)
        x <- x[valid]
        weights_vec <- weights_vec[valid]
      }
      
      if (length(x) < 4) {
        result <- NA_real_
      } else {
        # Weighted kurtosis calculation
        w_sum <- sum(weights_vec)
        w_mean <- sum(x * weights_vec) / w_sum
        
        # Weighted second and fourth central moments
        m2 <- sum(weights_vec * (x - w_mean)^2) / w_sum
        m4 <- sum(weights_vec * (x - w_mean)^4) / w_sum
        
        raw_kurtosis <- m4 / (m2^2)
        result <- if (excess) raw_kurtosis - 3 else raw_kurtosis
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
            
            if (length(x_clean) < 4) {
              kurt_val <- NA_real_
              n_val <- length(x_clean)
              n_eff <- n_val
            } else {
              n <- length(x_clean)
              mean_x <- mean(x_clean)
              m2 <- sum((x_clean - mean_x)^2) / n
              m4 <- sum((x_clean - mean_x)^4) / n
              raw_kurtosis <- m4 / (m2^2)
              kurt_val <- if (excess) raw_kurtosis - 3 else raw_kurtosis
              n_val <- n
              n_eff <- n
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
            
            if (length(x_clean) < 4) {
              kurt_val <- NA_real_
              n_val <- 0
              n_eff <- 0
            } else {
              # Weighted kurtosis calculation
              w_sum <- sum(w_clean)
              w_mean <- sum(x_clean * w_clean) / w_sum
              
              # Weighted second and fourth central moments
              m2 <- sum(w_clean * (x_clean - w_mean)^2) / w_sum
              m4 <- sum(w_clean * (x_clean - w_mean)^4) / w_sum
              
              raw_kurtosis <- m4 / (m2^2)
              kurt_val <- if (excess) raw_kurtosis - 3 else raw_kurtosis
              n_val <- w_sum
              n_eff <- .effective_n(w_clean)
            }
          }
          
          results_list[[paste0(var_name, "_value")]] <- kurt_val
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
        
        if (length(x_clean) < 4) {
          kurt_val <- NA_real_
          n_val <- length(x_clean)
          n_eff <- n_val
        } else {
          n <- length(x_clean)
          mean_x <- mean(x_clean)
          m2 <- sum((x_clean - mean_x)^2) / n
          m4 <- sum((x_clean - mean_x)^4) / n
          raw_kurtosis <- m4 / (m2^2)
          kurt_val <- if (excess) raw_kurtosis - 3 else raw_kurtosis
          n_val <- n
          n_eff <- n
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
        
        if (length(x_clean) < 4) {
          kurt_val <- NA_real_
          n_val <- 0
          n_eff <- 0
        } else {
          # Weighted kurtosis calculation
          w_sum <- sum(w_clean)
          w_mean <- sum(x_clean * w_clean) / w_sum
          
          # Weighted second and fourth central moments
          m2 <- sum(w_clean * (x_clean - w_mean)^2) / w_sum
          m4 <- sum(w_clean * (x_clean - w_mean)^4) / w_sum
          
          raw_kurtosis <- m4 / (m2^2)
          kurt_val <- if (excess) raw_kurtosis - 3 else raw_kurtosis
          n_val <- w_sum
          n_eff <- .effective_n(w_clean)
        }
      }
      
      results_list[[paste0(var_name, "_value")]] <- kurt_val
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
        row_data[[paste0(stat_prefix, "kurtosis")]] <- results_df[[value_col]]
        
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
        hybrid_results[[paste0(stat_prefix, "kurtosis")]] <- results_df[[value_col]]
        
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
    groups = if (is_grouped) dplyr::group_vars(data) else NULL,       # Test-compatible field
    excess = excess
  )
  
  class(result) <- "w_kurtosis"
  return(result)
}

#' Print method for w_kurtosis objects
#'
#' @description
#' Prints a formatted summary of weighted kurtosis calculations, including kurtosis values,
#' sample sizes, and weights (if applicable). For grouped data, results are
#' displayed by group.
#'
#' @param x An object of class "w_kurtosis"
#' @param digits Number of decimal places to display (default: 3)
#' @param ... Additional arguments passed to print
#'
#' @return Invisibly returns the input object
#' @keywords internal
#' @export
print.w_kurtosis <- function(x, digits = 3, ...) {
  # Determine if this is weighted or unweighted
  is_weighted <- !is.null(x$weights)
  
  kurtosis_type <- if (x$excess) "Excess Kurtosis" else "Kurtosis"
  
  if (is_weighted) {
    header <- sprintf("Weighted %s Statistics", kurtosis_type)
  } else {
    header <- sprintf("%s Statistics", kurtosis_type)
  }
  
  cat(sprintf("\n%s\n", header))
  cat(paste(rep("-", nchar(header)), collapse = ""), "\n")
  
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
          var_row <- data.frame(
            Variable = var,
            Kurtosis = round(group_results[[value_col]], digits),
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
        results_df$Kurtosis[results_df$Variable == var] <- round(x$results[[value_col]], digits)
        
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


