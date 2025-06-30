
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
  
  # Data frame handling
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  # Get variables and weights
  vars <- .process_variables(data, ...)
  var_names <- names(vars)
  
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
  
  is_grouped <- inherits(data, "grouped_df")
  
  if (is_grouped) {
    # Handle grouped data
    group_vars <- dplyr::group_vars(data)
    
    results <- data %>%
      dplyr::group_modify(~ {
        group_data <- .x
        result_cols <- list()
        
        for (var_name in var_names) {
          x <- group_data[[var_name]]
          
          if (is.null(weights_vec)) {
            # Unweighted calculation
            if (na.rm) x <- x[!is.na(x)]
            
            if (length(x) == 0) {
              stat_val <- NA
              n_val <- 0
              eff_n <- 0
            } else {
              freq_table <- table(x)
              max_freq <- max(freq_table)
              modes <- names(freq_table)[freq_table == max_freq]
              stat_val <- modes[1]
              
              # Convert back to original type if numeric
              if (is.numeric(group_data[[var_name]])) {
                stat_val <- as.numeric(stat_val)
              }
              
              n_val <- length(x)
              eff_n <- n_val
            }
          } else {
            # Weighted calculation
            w <- group_data[[weights_name]]
            if (na.rm) {
              valid <- !is.na(x) & !is.na(w)
              x <- x[valid]
              w <- w[valid]
            }
            
            if (length(x) == 0) {
              stat_val <- NA
              n_val <- 0
              eff_n <- 0
            } else {
              unique_vals <- unique(x)
              weighted_freqs <- sapply(unique_vals, function(val) {
                sum(w[x == val])
              })
              max_weight <- max(weighted_freqs)
              modes <- unique_vals[weighted_freqs == max_weight]
              stat_val <- modes[1]
              
              n_val <- length(x)
              eff_n <- sum(w)^2 / sum(w^2)  # Effective sample size
            }
          }
          
          result_cols[[var_name]] <- stat_val
          result_cols[[paste0(var_name, "_n")]] <- n_val
          result_cols[[paste0(var_name, "_eff_n")]] <- eff_n
        }
        
        tibble::tibble(!!!result_cols)
      }) %>%
      dplyr::ungroup()
    
  } else {
    # Handle ungrouped data
    result_cols <- list()
    
    for (var_name in var_names) {
      x <- data[[var_name]]
      
      if (is.null(weights_vec)) {
        # Unweighted calculation
        if (na.rm) x <- x[!is.na(x)]
        
        if (length(x) == 0) {
          stat_val <- NA
          n_val <- 0
          eff_n <- 0
        } else {
          freq_table <- table(x)
          max_freq <- max(freq_table)
          modes <- names(freq_table)[freq_table == max_freq]
          stat_val <- modes[1]
          
          # Convert back to original type if numeric
          if (is.numeric(data[[var_name]])) {
            stat_val <- as.numeric(stat_val)
          }
          
          n_val <- length(x)
          eff_n <- n_val
        }
      } else {
        # Weighted calculation
        if (na.rm) {
          valid <- !is.na(x) & !is.na(weights_vec)
          x <- x[valid]
          w <- weights_vec[valid]
        } else {
          w <- weights_vec
        }
        
        if (length(x) == 0) {
          stat_val <- NA
          n_val <- 0
          eff_n <- 0
        } else {
          unique_vals <- unique(x)
          weighted_freqs <- sapply(unique_vals, function(val) {
            sum(w[x == val])
          })
          max_weight <- max(weighted_freqs)
          modes <- unique_vals[weighted_freqs == max_weight]
          stat_val <- modes[1]
          
          n_val <- length(x)
          eff_n <- sum(w)^2 / sum(w^2)
        }
      }
      
      result_cols[[var_name]] <- stat_val
      result_cols[[paste0(var_name, "_n")]] <- n_val
      result_cols[[paste0(var_name, "_eff_n")]] <- eff_n
    }
    
    results <- tibble::tibble(!!!result_cols)
  }
  
  # Create test-compatible results structure
  if (length(var_names) > 1) {
    # Multiple variables: Long format
    results_long <- list()
    for (i in seq_along(var_names)) {
      var_name <- var_names[i]
      
      if (is_grouped) {
        # Grouped multi-variable (complex case)
        group_combinations <- results %>% 
          dplyr::select(dplyr::all_of(group_vars)) %>% 
          dplyr::distinct()
        
        for (j in 1:nrow(group_combinations)) {
          group_filter <- group_combinations[j, , drop = FALSE]
          group_results <- results
          for (grp in names(group_filter)) {
            group_results <- group_results[group_results[[grp]] == group_filter[[grp]], ]
          }
          
          if (nrow(group_results) > 0) {
            row_data <- group_filter
            row_data$Variable <- var_name
            
            if (!is.null(weights_name)) {
              row_data$weighted_mode <- group_results[[var_name]][1]
              row_data$effective_n <- group_results[[paste0(var_name, "_eff_n")]][1]
            } else {
              row_data$mode <- group_results[[var_name]][1]
              row_data$n <- group_results[[paste0(var_name, "_n")]][1]
            }
            
            results_long[[length(results_long) + 1]] <- row_data
          }
        }
      } else {
        # Ungrouped multi-variable
        row_data <- tibble::tibble(Variable = var_name)
        
        if (!is.null(weights_name)) {
          row_data$weighted_mode <- results[[var_name]][1]
          row_data$effective_n <- results[[paste0(var_name, "_eff_n")]][1]
        } else {
          row_data$mode <- results[[var_name]][1]
          row_data$n <- results[[paste0(var_name, "_n")]][1]
        }
        
        results_long[[i]] <- row_data
      }
    }
    
    final_results <- dplyr::bind_rows(results_long)
  } else {
    # Single variable: Direct mapping
    var_name <- var_names[1]
    
    if (!is.null(weights_name)) {
      final_results <- results %>%
        dplyr::mutate(
          weighted_mode = !!rlang::sym(var_name),
          effective_n = !!rlang::sym(paste0(var_name, "_eff_n"))
        )
    } else {
      final_results <- results %>%
        dplyr::mutate(
          mode = !!rlang::sym(var_name),
          n = !!rlang::sym(paste0(var_name, "_n"))
        )
    }
  }
  
  # Create S3 object with test-compatible structure
  result <- list(
    results = final_results,
    variables = var_names,
    weight_var = weights_name,        # Test-compatible field name
    weights = weights_name,           # Alternative field name
    is_grouped = is_grouped,          # Test-compatible field
    grouped = is_grouped,             # Alternative field
    groups = if (is_grouped) dplyr::group_vars(data) else NULL
  )
  
  class(result) <- "w_modus"
  return(result)
}

#' Print method for w_modus objects
#' @export
#' @method print w_modus
print.w_modus <- function(x, digits = 3, ...) {
  test_type <- if (!is.null(x$weights)) {
    "Weighted Mode Statistics"
  } else {
    "Mode Statistics"
  }
  
  cat(sprintf("\n%s\n", test_type))
  cat(paste(rep("-", nchar(test_type)), collapse = ""), "\n")
  
  is_grouped_data <- !is.null(x$is_grouped) && x$is_grouped
  
  if (is_grouped_data) {
    # Handle grouped results
    for (group_val in unique(x$results[[x$groups[1]]])) {
      group_results <- x$results[x$results[[x$groups[1]]] == group_val, ]
      
      cat(sprintf("\nGroup: %s = %s\n", x$groups[1], group_val))
      
      print_df <- group_results
      if (!is.null(x$weights)) {
        print_df <- print_df %>%
          dplyr::select(dplyr::any_of(c("Variable", "weighted_mode", "effective_n")))
        # Format mode appropriately
        if (is.numeric(print_df$weighted_mode)) {
          print_df$weighted_mode <- round(print_df$weighted_mode, digits)
        }
        print_df$effective_n <- round(print_df$effective_n, 1)
      } else {
        print_df <- print_df %>%
          dplyr::select(dplyr::any_of(c("Variable", "mode", "n")))
        # Format mode appropriately
        if (is.numeric(print_df$mode)) {
          print_df$mode <- round(print_df$mode, digits)
        }
      }
      
      print(print_df, row.names = FALSE)
    }
  } else {
    # Handle ungrouped results
    print_df <- x$results
    
    if (!is.null(x$weights)) {
      if ("Variable" %in% names(print_df)) {
        print_df <- print_df %>%
          dplyr::select(dplyr::any_of(c("Variable", "weighted_mode", "effective_n")))
      } else {
        mode_val <- print_df$weighted_mode[1]
        # Format mode appropriately
        if (is.numeric(mode_val)) {
          mode_val <- round(mode_val, digits)
        }
        print_df <- tibble::tibble(
          Variable = x$variables[1],
          weighted_mode = mode_val,
          effective_n = round(print_df$effective_n[1], 1)
        )
      }
      # Format mode appropriately
      if (is.numeric(print_df$weighted_mode)) {
        print_df$weighted_mode <- round(print_df$weighted_mode, digits)
      }
      print_df$effective_n <- round(print_df$effective_n, 1)
    } else {
      if ("Variable" %in% names(print_df)) {
        print_df <- print_df %>%
          dplyr::select(dplyr::any_of(c("Variable", "mode", "n")))
      } else {
        mode_val <- print_df$mode[1]
        # Format mode appropriately
        if (is.numeric(mode_val)) {
          mode_val <- round(mode_val, digits)
        }
        print_df <- tibble::tibble(
          Variable = x$variables[1],
          mode = mode_val,
          n = round(print_df$n[1], 0)
        )
      }
      # Format mode appropriately
      if (is.numeric(print_df$mode)) {
        print_df$mode <- round(print_df$mode, digits)
      }
    }
    
    print(print_df, row.names = FALSE)
  }
  
  cat("\n")
  invisible(x)
} 


