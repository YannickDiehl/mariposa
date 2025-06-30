
#' Weighted Standard Deviation
#'
#' Calculate weighted standard deviation for numeric variables, with support for 
#' grouped data and multiple variables simultaneously.
#'
#' @param data A data frame, or a numeric vector when used in summarise() context
#' @param ... Variable names (unquoted) or tidyselect expressions
#' @param weights Name of the weights variable (unquoted), or a numeric vector of weights
#' @param na.rm Logical; if TRUE, missing values are removed
#'
#' @return A w_sd object (list) containing results and metadata, or numeric values in summarise context
#' 
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#' 
#' # Basic weighted standard deviation
#' survey_data %>% w_sd(age, weights = sampling_weight)
#' 
#' # Multiple variables
#' survey_data %>% w_sd(age, income, life_satisfaction, weights = sampling_weight)
#' 
#' # Grouped data
#' survey_data %>% group_by(region) %>% w_sd(age, weights = sampling_weight)
#' 
#' # In summarise context
#' survey_data %>% summarise(sd_age = w_sd(age, weights = sampling_weight))
#' 
#' # Unweighted (for comparison)
#' survey_data %>% w_sd(age)
#'
#' @export
w_sd <- function(data, ..., weights = NULL, na.rm = TRUE) {
  
  # Handle the case where data is a vector (e.g., in summarise context)
  if (is.numeric(data) && !is.data.frame(data)) {
    # This is the summarise() context - data is actually the variable values
    x <- data
    weights_arg <- substitute(weights)
    
    # Use helper function to evaluate weights
    weights_vec <- .evaluate_weights(weights_arg, parent.frame())
    
    # Calculate weighted or unweighted standard deviation
    if (!.are_weights(weights_vec) || !.validate_weights(weights_vec, verbose = FALSE)) {
      # Unweighted calculation
      if (na.rm) {
        x <- x[!is.na(x)]
      }
      result <- sd(x, na.rm = na.rm)
    } else {
      # Weighted calculation
      if (na.rm) {
        valid <- !is.na(x) & !is.na(weights_vec)
        x <- x[valid]
        weights_vec <- weights_vec[valid]
      }
      
      if (length(x) < 2) {
        result <- NA_real_
      } else {
        # Weighted standard deviation calculation (matches sjstats/datawizard)
        w_mean <- sum(x * weights_vec) / sum(weights_vec)
        n_eff <- sum(weights_vec)^2 / sum(weights_vec^2)  # Effective sample size
        w_var <- sum(weights_vec * (x - w_mean)^2) / (sum(weights_vec) * (n_eff - 1) / n_eff)
        result <- sqrt(w_var)
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
            sd_val <- sd(x, na.rm = na.rm)
            n_eff <- length(x[!is.na(x)])
          } else {
            # Weighted calculation
            w <- group_data[[weights_name]]
            if (na.rm) {
              valid <- !is.na(x) & !is.na(w)
              x <- x[valid]
              w <- w[valid]
            }
            
            if (length(x) < 2) {
              sd_val <- NA_real_
              n_eff <- 0
            } else {
              w_mean <- sum(x * w) / sum(w)
              n_eff <- sum(w)^2 / sum(w^2)  # Effective sample size
              w_var <- sum(w * (x - w_mean)^2) / (sum(w) * (n_eff - 1) / n_eff)
              sd_val <- sqrt(w_var)
            }
          }
          
          results_list[[var_name]] <- sd_val
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
        sd_val <- sd(x, na.rm = na.rm)
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
        
        if (length(x) < 2) {
          sd_val <- NA_real_
          n_eff <- 0
        } else {
          w_mean <- sum(x * w) / sum(w)
          n_eff <- sum(w)^2 / sum(w^2)  # Effective sample size
          w_var <- sum(w * (x - w_mean)^2) / (sum(w) * (n_eff - 1) / n_eff)
          sd_val <- sqrt(w_var)
        }
      }
      
      results_list[[var_name]] <- sd_val
      results_list[[paste0(var_name, "_n_eff")]] <- n_eff
    }
    
    results_df <- tibble::tibble(!!!results_list)
    group_info <- NULL
  }
  
  # Create hybrid results structure for test compatibility
  hybrid_results <- results_df
  
  # Add test-compatible column names based on whether analysis is weighted
  stat_prefix <- if (!is.null(weights_name)) "weighted_" else ""
  
  # For multiple variables, create long format with Variable column
  if (length(vars) > 1 && !is_grouped) {
    # Long format for multiple variables
    long_results <- list()
    for (i in seq_along(vars)) {
      var <- names(vars)[i]
      if (var %in% names(results_df)) {
        row_data <- data.frame(
          Variable = var,
          stringsAsFactors = FALSE
        )
        
        # Add the statistic value
        row_data[[paste0(stat_prefix, "sd")]] <- results_df[[var]]
        
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
      if (var %in% names(results_df)) {
        # Add test-compatible columns while preserving variable-based access
        hybrid_results[[paste0(stat_prefix, "sd")]] <- results_df[[var]]
        
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
    grouped = is_grouped,
    group_vars = if (is_grouped) dplyr::group_vars(data) else NULL
  )
  
  class(result) <- "w_sd"
  return(result)
}

#' Print method for w_sd objects
#' @export
#' @method print w_sd
print.w_sd <- function(x, digits = 3, ...) {
  # Determine test type based on weights
  test_type <- if (!is.null(x$weights)) {
    "Weighted Standard Deviation Statistics"
  } else {
    "Standard Deviation Statistics"
  }
  
  # Check if this is grouped data
  is_grouped_data <- !is.null(x$grouped) && x$grouped
  
  if (is_grouped_data) {
    # Handle grouped results
    groups <- unique(x$results[x$group_vars])
    
    for (i in seq_len(nrow(groups))) {
      group_values <- groups[i, , drop = FALSE]
      
      # Format group info
      group_info <- sapply(names(group_values), function(g) {
        val <- group_values[[g]]
        if (is.factor(val)) {
          paste(g, "=", as.character(val))
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
      
      if (nrow(group_results) == 0) next
      
      cat(sprintf("\nGroup: %s\n", group_info))
      
      # Process each variable in this group as separate blocks
      for (var in x$variables) {
        cat(sprintf("\n┌─ %s ─┐\n", var))
        cat("\n")  # Add blank line after variable name
        
        # Create results table for this variable
        var_sd <- round(group_results[[var]], digits)
        n_eff_col <- paste0(var, "_n_eff")
        var_n <- if (n_eff_col %in% names(group_results)) round(group_results[[n_eff_col]], 1) else NA
        
        results_df <- data.frame(
          Variable = var,
          SD = var_sd,
          N = var_n,
          stringsAsFactors = FALSE
        )
        
        if (!is.null(x$weights)) {
          results_df$Effective_N <- var_n
          results_df$Weights <- x$weights
        }
        
        # Calculate border width
        col_widths <- sapply(names(results_df), function(col) {
          max(nchar(as.character(results_df[[col]])), nchar(col), na.rm = TRUE)
        })
        total_width <- sum(col_widths) + length(col_widths) - 1
        border_width <- paste(rep("-", total_width), collapse = "")
        
        cat(sprintf("%s:\n", test_type))
        cat(border_width, "\n")
        print(results_df, row.names = FALSE)
        cat(border_width, "\n")
      }
    }
  } else {
    # Handle ungrouped results
    for (var in x$variables) {
      cat(sprintf("\n┌─ %s ─┐\n", var))
      cat("\n")  # Add blank line after variable name
      
      # Create results table for this variable
      var_sd <- round(x$results[[var]], digits)
      n_eff_col <- paste0(var, "_n_eff")
      var_n <- if (n_eff_col %in% names(x$results)) round(x$results[[n_eff_col]], 1) else NA
      
      results_df <- data.frame(
        Variable = var,
        SD = var_sd,
        N = var_n,
        stringsAsFactors = FALSE
      )
      
      if (!is.null(x$weights)) {
        results_df$Effective_N <- var_n
        results_df$Weights <- x$weights
      }
      
      # Calculate border width
      col_widths <- sapply(names(results_df), function(col) {
        max(nchar(as.character(results_df[[col]])), nchar(col), na.rm = TRUE)
      })
      total_width <- sum(col_widths) + length(col_widths) - 1
      border_width <- paste(rep("-", total_width), collapse = "")
      
      cat(sprintf("%s:\n", test_type))
      cat(border_width, "\n")
      print(results_df, row.names = FALSE)
      cat(border_width, "\n")
      
      if (which(x$variables == var) < length(x$variables)) {
        cat("\n")  # Add spacing between variables
      }
    }
  }
  
  invisible(x)
}
