#' Weighted Mean
#'
#' Calculate weighted mean for numeric variables, with support for 
#' grouped data and multiple variables simultaneously. Provides professional
#' output formatting with smart headers and dynamic borders.
#'
#' @param data A data frame, or a numeric vector when used in summarise() context
#' @param ... Variable names (unquoted) or tidyselect expressions (e.g., starts_with(), where())
#' @param weights Name of the weights variable (unquoted), or a numeric vector of weights
#' @param na.rm Logical; if TRUE, missing values are removed (default: TRUE)
#'
#' @return A w_mean object (list) containing results and metadata, or numeric values in summarise context
#' 
#' @details
#' The function automatically detects whether calculations are weighted or unweighted and
#' adjusts the output format accordingly:
#' - Unweighted: Shows "Mean Statistics" header with Variable, Mean, N columns
#' - Weighted: Shows "Weighted Mean Statistics" header with Variable, Mean, N, Effective_N, Weights columns
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#' 
#' # Basic weighted usage
#' survey_data %>% w_mean(age, weights = sampling_weight)
#' 
#' # Multiple variables
#' survey_data %>% w_mean(age, income, life_satisfaction, weights = sampling_weight)
#' 
#' # Grouped data
#' survey_data %>% group_by(region) %>% w_mean(age, weights = sampling_weight)
#' 
#' # In summarise context
#' survey_data %>% summarise(mean_age = w_mean(age, weights = sampling_weight))
#' 
#' # Unweighted (for comparison)
#' survey_data %>% w_mean(age)
#'
#' @export
w_mean <- function(data, ..., weights = NULL, na.rm = TRUE) {
  
  # Handle the case where data is a vector (e.g., in summarise context)
  if (is.numeric(data) && !is.data.frame(data)) {
    # This is the summarise() context - data is actually the variable values
    x <- data
    weights_arg <- substitute(weights)
    
    # Use helper function to evaluate weights
    weights_vec <- .evaluate_weights(weights_arg, parent.frame())
    
    # Calculate weighted or unweighted mean
    if (!.are_weights(weights_vec) || !.validate_weights(weights_vec, verbose = FALSE)) {
      # Unweighted calculation
      if (na.rm) {
        x <- x[!is.na(x)]
      }
      result <- mean(x, na.rm = na.rm)
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
        # Weighted mean calculation
        result <- sum(x * weights_vec) / sum(weights_vec)
      }
    }
    
    return(result)
  }
  
  # Data frame handling code
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  # Get variable names using tidyselect
  vars <- .process_variables(data, ...)
  var_names <- names(vars)
  
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
        
        # Calculate for each variable
        result_cols <- list()
        for (var_name in var_names) {
          x <- group_data[[var_name]]
          
          if (is.null(weights_vec)) {
            # Unweighted calculation
            if (na.rm) {
              x <- x[!is.na(x)]
            }
            mean_val <- mean(x, na.rm = na.rm)
            n_val <- length(x[!is.na(x)])
            eff_n <- n_val
          } else {
            # Weighted calculation
            w <- group_data[[weights_name]]
            if (na.rm) {
              valid <- !is.na(x) & !is.na(w)
              x <- x[valid]
              w <- w[valid]
            }
            
            if (length(x) == 0) {
              mean_val <- NA_real_
              n_val <- 0
              eff_n <- 0
            } else {
              mean_val <- sum(x * w) / sum(w)
              n_val <- length(x)
              eff_n <- sum(w)^2 / sum(w^2)  # Effective sample size
            }
          }
          
          result_cols[[var_name]] <- mean_val
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
        if (na.rm) {
          x <- x[!is.na(x)]
        }
        mean_val <- mean(x, na.rm = na.rm)
        n_val <- length(x[!is.na(x)])
        eff_n <- n_val
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
          mean_val <- NA_real_
          n_val <- 0
          eff_n <- 0
        } else {
          mean_val <- sum(x * w) / sum(w)
          n_val <- length(x)
          eff_n <- sum(w)^2 / sum(w^2)  # Effective sample size
        }
      }
      
      result_cols[[var_name]] <- mean_val
      result_cols[[paste0(var_name, "_n")]] <- n_val
      result_cols[[paste0(var_name, "_eff_n")]] <- eff_n
    }
    
    results <- tibble::tibble(!!!result_cols)
  }
  
  # Create standardized results structure for test compatibility
  if (length(var_names) > 1) {
    # Multiple variables: Create long format with Variable column
    results_long <- list()
    for (i in seq_along(var_names)) {
      var_name <- var_names[i]
      
      # Handle grouped vs ungrouped data
      if (is_grouped) {
        # For grouped data, extract unique combinations
        group_combinations <- results %>% 
          dplyr::select(dplyr::all_of(group_vars)) %>% 
          dplyr::distinct()
        
        for (j in 1:nrow(group_combinations)) {
          group_filter <- group_combinations[j, , drop = FALSE]
          
          # Filter results for this group
          group_results <- results
          for (grp in names(group_filter)) {
            group_results <- group_results[group_results[[grp]] == group_filter[[grp]], ]
          }
          
          if (nrow(group_results) > 0) {
            row_data <- group_filter
            row_data$Variable <- var_name
            
            # Add weighted/unweighted columns
            if (!is.null(weights_name)) {
              row_data$weighted_mean <- group_results[[var_name]][1]
              row_data$effective_n <- group_results[[paste0(var_name, "_eff_n")]][1]
            } else {
              row_data$mean <- group_results[[var_name]][1]
              row_data$n <- group_results[[paste0(var_name, "_n")]][1]
            }
            
            results_long[[length(results_long) + 1]] <- row_data
          }
        }
      } else {
        # Ungrouped data
        row_data <- tibble::tibble(Variable = var_name)
        
        if (!is.null(weights_name)) {
          row_data$weighted_mean <- results[[var_name]][1]
          row_data$effective_n <- results[[paste0(var_name, "_eff_n")]][1]
        } else {
          row_data$mean <- results[[var_name]][1]
          row_data$n <- results[[paste0(var_name, "_n")]][1]
        }
        
        results_long[[i]] <- row_data
      }
    }
    
    final_results <- dplyr::bind_rows(results_long)
  } else {
    # Single variable: Simplified structure
    var_name <- var_names[1]
    
    if (!is.null(weights_name)) {
      final_results <- results %>%
        dplyr::mutate(
          weighted_mean = !!rlang::sym(var_name),
          effective_n = !!rlang::sym(paste0(var_name, "_eff_n"))
        )
    } else {
      final_results <- results %>%
        dplyr::mutate(
          mean = !!rlang::sym(var_name),
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
  
  class(result) <- "w_mean"
  return(result)
}

#' Print method for w_mean objects
#' @export
#' @method print w_mean
print.w_mean <- function(x, digits = 3, ...) {
  # Determine test type based on weights
  test_type <- if (!is.null(x$weights)) {
    "Weighted Mean Statistics"
  } else {
    "Mean Statistics"
  }
  
  cat(sprintf("\n%s\n", test_type))
  cat(paste(rep("-", nchar(test_type)), collapse = ""), "\n")
  
  # Check if this is grouped data
  is_grouped_data <- !is.null(x$is_grouped) && x$is_grouped
  
  if (is_grouped_data) {
    # Handle grouped results
    for (group_val in unique(x$results[[x$groups[1]]])) {
      group_results <- x$results[x$results[[x$groups[1]]] == group_val, ]
      
      cat(sprintf("\nGroup: %s = %s\n", x$groups[1], group_val))
      
      # Process each variable with Unicode boxes
      for (var_name in x$variables) {
        cat(sprintf("\n┌─ %s ─┐\n", var_name))
        
        # Create formatted output
        if (!is.null(x$weights)) {
          output_df <- data.frame(
            Variable = var_name,
            weighted_mean = round(group_results$weighted_mean[1], digits),
            Effective_N = round(group_results$effective_n[1], 1)
          )
        } else {
          output_df <- data.frame(
            Variable = var_name,
            mean = round(group_results[[var_name]][1], digits),
            N = round(group_results[[paste0(var_name, "_n")]][1], 0)
          )
        }
        
        print(output_df, row.names = FALSE)
      }
    }
  } else {
    # Handle ungrouped results
    variables <- if ("Variable" %in% names(x$results)) {
      unique(x$results$Variable)
    } else {
      x$variables
    }
    
    for (var_name in variables) {
      cat(sprintf("\n┌─ %s ─┐\n", var_name))
      
      if (!is.null(x$weights)) {
        # Show weighted columns with proper capitalization
        if ("Variable" %in% names(x$results)) {
          var_data <- x$results[x$results$Variable == var_name, ]
          output_df <- data.frame(
            Variable = var_name,
            weighted_mean = round(var_data$weighted_mean[1], digits),
            Effective_N = round(var_data$effective_n[1], 1)
          )
        } else {
          # Single variable case
          output_df <- data.frame(
            Variable = var_name,
            weighted_mean = round(x$results$weighted_mean[1], digits),
            Effective_N = round(x$results$effective_n[1], 1)
          )
        }
      } else {
        # Show unweighted columns
        if ("Variable" %in% names(x$results)) {
          var_data <- x$results[x$results$Variable == var_name, ]
          output_df <- data.frame(
            Variable = var_name,
            mean = round(var_data$mean[1], digits),
            N = round(var_data$n[1], 0)
          )
        } else {
          # Single variable case
          output_df <- data.frame(
            Variable = var_name,
            mean = round(x$results$mean[1], digits),
            N = round(x$results$n[1], 0)
          )
        }
      }
      
      print(output_df, row.names = FALSE)
    }
  }
  
  cat("\n")
  invisible(x)
}