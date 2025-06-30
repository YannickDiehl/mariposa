
#' Weighted Quantiles
#'
#' Calculate weighted quantiles for numeric variables, with support for 
#' grouped data and multiple variables simultaneously.
#'
#' @param data A data frame, or a numeric vector when used in summarise() context
#' @param ... Variable names (unquoted) or tidyselect expressions
#' @param weights Name of the weights variable (unquoted), or a numeric vector of weights
#' @param probs Numeric vector of probabilities (default: c(0, 0.25, 0.5, 0.75, 1))
#' @param na.rm Logical; if TRUE, missing values are removed
#'
#' @return A w_quantile object (list) containing results and metadata, or numeric values in summarise context
#' 
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#' 
#' # Basic weighted quantiles (0%, 25%, 50%, 75%, 100%)
#' survey_data %>% w_quantile(age, weights = sampling_weight)
#' 
#' # Custom quantiles
#' survey_data %>% w_quantile(income, weights = sampling_weight, probs = c(0.1, 0.5, 0.9))
#' 
#' # Multiple variables
#' survey_data %>% w_quantile(age, income, weights = sampling_weight)
#' 
#' # Grouped data  
#' survey_data %>% group_by(region) %>% w_quantile(age, weights = sampling_weight)
#' 
#' # Unweighted (for comparison)
#' survey_data %>% w_quantile(age)
#'
#' @export
w_quantile <- function(data, ..., weights = NULL, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE) {
  
  # Handle the case where data is a vector (e.g., in summarise context)
  if (is.numeric(data) && !is.data.frame(data)) {
    # This is the summarise() context - data is actually the variable values
    x <- data
    weights_arg <- substitute(weights)
    
    # Use helper function to evaluate weights
    weights_vec <- .evaluate_weights(weights_arg, parent.frame())
    
    # Create consistent quantile labels
    quantile_labels <- paste0(probs * 100, "%")
    quantile_labels[quantile_labels == "0%"] <- "Min"
    quantile_labels[quantile_labels == "100%"] <- "Max"
    
    # Calculate weighted or unweighted quantiles
    if (!.are_weights(weights_vec) || !.validate_weights(weights_vec, verbose = FALSE)) {
      # Unweighted calculation
      if (na.rm) {
        x <- x[!is.na(x)]
      }
      result <- quantile(x, probs = probs, na.rm = na.rm)
      names(result) <- quantile_labels
    } else {
      # Weighted calculation
      if (na.rm) {
        valid <- !is.na(x) & !is.na(weights_vec)
        x <- x[valid]
        weights_vec <- weights_vec[valid]
      }
      
      if (length(x) == 0) {
        result <- rep(NA_real_, length(probs))
        names(result) <- quantile_labels
      } else {
        # Weighted quantile calculation
        order_idx <- order(x)
        x_sorted <- x[order_idx]
        w_sorted <- weights_vec[order_idx]
        
        cumsum_w <- cumsum(w_sorted)
        total_w <- sum(w_sorted)
        
        result <- numeric(length(probs))
        names(result) <- quantile_labels
        
        for (i in seq_along(probs)) {
          p <- probs[i]
          target_pos <- p * total_w
          
          if (p == 0) {
            result[i] <- x_sorted[1]
          } else if (p == 1) {
            result[i] <- x_sorted[length(x_sorted)]
          } else {
            # Find the index where cumulative weight >= target position
            idx <- which(cumsum_w >= target_pos)[1]
            
            if (is.na(idx)) {
              result[i] <- x_sorted[length(x_sorted)]
            } else if (idx == 1 || cumsum_w[idx] == target_pos) {
              result[i] <- x_sorted[idx]
            } else {
              # Linear interpolation
              w_below <- cumsum_w[idx - 1]
              w_above <- cumsum_w[idx]
              x_below <- x_sorted[idx - 1]
              x_above <- x_sorted[idx]
              
              # Interpolation factor
              alpha <- (target_pos - w_below) / (w_above - w_below)
              result[i] <- x_below + alpha * (x_above - x_below)
            }
          }
        }
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
  
  # Create quantile labels
  quantile_labels <- paste0(probs * 100, "%")
  quantile_labels[quantile_labels == "0%"] <- "Min"
  quantile_labels[quantile_labels == "100%"] <- "Max"
  
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
              x <- x[!is.na(x)]
            }
            quantiles <- quantile(x, probs = probs, na.rm = na.rm)
            names(quantiles) <- quantile_labels
            n_eff <- length(x[!is.na(x)])
          } else {
            # Weighted calculation
            w <- group_data[[weights_name]]
            if (na.rm) {
              valid <- !is.na(x) & !is.na(w)
              x <- x[valid]
              w <- w[valid]
            }
            
            if (length(x) == 0) {
              quantiles <- rep(NA_real_, length(probs))
              names(quantiles) <- quantile_labels
              n_eff <- 0
            } else {
              # Weighted quantile calculation
              order_idx <- order(x)
              x_sorted <- x[order_idx]
              w_sorted <- w[order_idx]
              
              cumsum_w <- cumsum(w_sorted)
              total_w <- sum(w_sorted)
              
              quantiles <- numeric(length(probs))
              names(quantiles) <- quantile_labels
              
              for (j in seq_along(probs)) {
                p <- probs[j]
                target_pos <- p * total_w
                
                if (p == 0) {
                  quantiles[j] <- x_sorted[1]
                } else if (p == 1) {
                  quantiles[j] <- x_sorted[length(x_sorted)]
                } else {
                  # Find the index where cumulative weight >= target position
                  idx <- which(cumsum_w >= target_pos)[1]
                  
                  if (is.na(idx)) {
                    quantiles[j] <- x_sorted[length(x_sorted)]
                  } else if (idx == 1 || cumsum_w[idx] == target_pos) {
                    quantiles[j] <- x_sorted[idx]
                  } else {
                    # Linear interpolation
                    w_below <- cumsum_w[idx - 1]
                    w_above <- cumsum_w[idx]
                    x_below <- x_sorted[idx - 1]
                    x_above <- x_sorted[idx]
                    
                    # Interpolation factor
                    alpha <- (target_pos - w_below) / (w_above - w_below)
                    quantiles[j] <- x_below + alpha * (x_above - x_below)
                  }
                }
              }
              
              n_eff <- sum(w)^2 / sum(w^2)
            }
          }
          
          # Add quantiles to results
          for (j in seq_along(quantiles)) {
            col_name <- paste0(var_name, "_", names(quantiles)[j])
            results_list[[col_name]] <- quantiles[j]
          }
          results_list[[paste0(var_name, "_n_eff")]] <- n_eff
        }
        
        tibble::tibble(!!!results_list)
      })
    
    # Extract group information for metadata
    group_info <- results %>%
      dplyr::select(dplyr::all_of(group_vars)) %>%
      dplyr::distinct()
    
    # Create results data frame
    results_df <- results %>%
      dplyr::ungroup()
    
  } else {
    # Handle ungrouped data
    results_list <- list()
    
    for (i in seq_along(vars)) {
      var_name <- names(vars)[i]
      x <- data[[var_name]]
      
      if (is.null(weights_vec)) {
        # Unweighted calculation
        if (na.rm) {
          x <- x[!is.na(x)]
        }
        quantiles <- quantile(x, probs = probs, na.rm = na.rm)
        names(quantiles) <- quantile_labels
        n_eff <- length(x[!is.na(x)])
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
          quantiles <- rep(NA_real_, length(probs))
          names(quantiles) <- quantile_labels
          n_eff <- 0
        } else {
          # Weighted quantile calculation
          order_idx <- order(x)
          x_sorted <- x[order_idx]
          w_sorted <- w[order_idx]
          
          cumsum_w <- cumsum(w_sorted)
          total_w <- sum(w_sorted)
          
          quantiles <- numeric(length(probs))
          names(quantiles) <- quantile_labels
          
          for (j in seq_along(probs)) {
            p <- probs[j]
            target_pos <- p * total_w
            
            if (p == 0) {
              quantiles[j] <- x_sorted[1]
            } else if (p == 1) {
              quantiles[j] <- x_sorted[length(x_sorted)]
            } else {
              # Find the index where cumulative weight >= target position
              idx <- which(cumsum_w >= target_pos)[1]
              
              if (is.na(idx)) {
                quantiles[j] <- x_sorted[length(x_sorted)]
              } else if (idx == 1 || cumsum_w[idx] == target_pos) {
                quantiles[j] <- x_sorted[idx]
              } else {
                # Linear interpolation
                w_below <- cumsum_w[idx - 1]
                w_above <- cumsum_w[idx]
                x_below <- x_sorted[idx - 1]
                x_above <- x_sorted[idx]
                
                # Interpolation factor
                alpha <- (target_pos - w_below) / (w_above - w_below)
                quantiles[j] <- x_below + alpha * (x_above - x_below)
              }
            }
          }
          
          n_eff <- sum(w)^2 / sum(w^2)
        }
      }
      
      # Add quantiles to results
      for (j in seq_along(quantiles)) {
        col_name <- paste0(var_name, "_", names(quantiles)[j])
        results_list[[col_name]] <- quantiles[j]
      }
      results_list[[paste0(var_name, "_n_eff")]] <- n_eff
    }
    
    results_df <- tibble::tibble(!!!results_list)
    group_info <- NULL
  }
  
  # Create S3 object
  result <- list(
    results = results_df,
    variables = names(vars),
    weights = weights_name,
    probs = probs,
    grouped = is_grouped,
    group_vars = if (is_grouped) dplyr::group_vars(data) else NULL
  )
  
  class(result) <- "w_quantile"
  return(result)
}

#' Print method for w_quantile objects
#'
#' @description
#' Prints a formatted summary of weighted quantile calculations, including quantiles,
#' sample sizes, and weights (if applicable). For grouped data, results are
#' displayed by group.
#'
#' @param x An object of class "w_quantile"
#' @param digits Number of decimal places to display (default: 3)
#' @param ... Additional arguments passed to print
#'
#' @keywords internal
#' @export
print.w_quantile <- function(x, digits = 3, ...) {
  # Determine if this is weighted or unweighted
  is_weighted <- !is.null(x$weights)
  
  if (is_weighted) {
    .print_header("Weighted Quantile")
  } else {
    .print_header("Quantile")
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
      
      # Print group header
      cat(sprintf("\nGroup: %s\n", group_info))
      
      # Create formatted table for this group - handle all variables
      all_results <- list()
      
      for (var_name in x$variables) {
        # Get quantile columns for this variable
        var_quantile_cols <- names(group_results)[grepl(paste0("^", var_name, "_"), names(group_results))]
        var_quantile_cols <- var_quantile_cols[!grepl("_n_eff$", var_quantile_cols)]
        
        if (length(var_quantile_cols) > 0) {
          # Get quantile values
          quantile_values <- as.numeric(group_results[1, var_quantile_cols])
          
          # Create clean quantile names
          quantile_names <- gsub(paste0(var_name, "_"), "", var_quantile_cols)
          
                     # Create display table for this variable
           var_results <- data.frame(
             Variable = rep(var_name, length(quantile_names)),
             Quantile = quantile_names,
             Value = round(quantile_values, digits),
             stringsAsFactors = FALSE
           )
          
          # Add sample size info
          n_eff_col <- paste0(var_name, "_n_eff")
          if (n_eff_col %in% names(group_results)) {
            var_results$N <- rep(round(group_results[[n_eff_col]][1], 1), nrow(var_results))
            # Only add Effective_N for weighted calculations
            if (is_weighted) {
              var_results$Effective_N <- rep(round(group_results[[n_eff_col]][1], 1), nrow(var_results))
            }
          }
          
          # Add weights info
          if (!is.null(x$weights)) {
            var_results$Weights <- rep(x$weights, nrow(var_results))
          }
          
          all_results[[var_name]] <- var_results
        }
      }
      
      if (length(all_results) > 0) {
        results_df <- do.call(rbind, all_results)
        results_df_print <- as.data.frame(results_df)
        .print_border(results_df_print)
        print(results_df_print, row.names = FALSE)
        .print_border(results_df_print)
      }
    }
  } else {
    # Print ungrouped results - handle all variables
    all_results <- list()
    
    for (var_name in x$variables) {
      # Get quantile columns for this variable
      var_quantile_cols <- names(x$results)[grepl(paste0("^", var_name, "_"), names(x$results))]
      var_quantile_cols <- var_quantile_cols[!grepl("_n_eff$", var_quantile_cols)]
      
      if (length(var_quantile_cols) > 0) {
        # Get quantile values
        quantile_values <- as.numeric(x$results[1, var_quantile_cols])
        
        # Create clean quantile names
        quantile_names <- gsub(paste0(var_name, "_"), "", var_quantile_cols)
        
        # Create display table for this variable
        var_results <- data.frame(
          Variable = rep(var_name, length(quantile_names)),
          Quantile = quantile_names,
          Value = round(quantile_values, digits),
          stringsAsFactors = FALSE
        )
        
        # Add sample size info
        n_eff_col <- paste0(var_name, "_n_eff")
        if (n_eff_col %in% names(x$results)) {
          var_results$N <- rep(round(x$results[[n_eff_col]][1], 1), nrow(var_results))
          # Only add Effective_N for weighted calculations
          if (is_weighted) {
            var_results$Effective_N <- rep(round(x$results[[n_eff_col]][1], 1), nrow(var_results))
          }
        }
        
        # Add weights info
        if (!is.null(x$weights)) {
          var_results$Weights <- rep(x$weights, nrow(var_results))
        }
        
        all_results[[var_name]] <- var_results
      }
    }
    
    if (length(all_results) > 0) {
      results_df <- do.call(rbind, all_results)
      results_df_print <- as.data.frame(results_df)
      print(results_df_print, row.names = FALSE)
      .print_border(results_df_print)
    } else {
      cat("No results to display.\n")
      cat("------------------------\n")
    }
  }
} 

