# Helper Functions for Weighted Statistical Functions
# ===================================================
# This file contains shared utility functions used across all w_* functions
# to ensure consistency and reduce code duplication.

#' @importFrom dplyr %>%
NULL

# Weight Validation Functions
# ---------------------------

#' Check if weights are valid (similar to datawizard)
#' @param weights Numeric vector of weights
#' @return Logical indicating if weights are valid
#' @keywords internal
.are_weights <- function(weights) {
  !is.null(weights) && is.numeric(weights) && length(weights) > 0
}

#' Validate weights (similar to datawizard)
#' @param weights Numeric vector of weights
#' @param verbose Logical; if TRUE, show warnings
#' @return Logical indicating if weights are positive
#' @keywords internal
.validate_weights <- function(weights, verbose = TRUE) {
  if (!.are_weights(weights)) {
    return(FALSE)
  }
  
  pos <- all(weights > 0, na.rm = TRUE)
  
  if (isTRUE(!pos) && isTRUE(verbose)) {
    warning("Some weights were negative or zero. Weighting not carried out.")
  }
  
  pos
}

#' Evaluate weights in summarise() context
#' @param weights_arg Quoted expression for weights
#' @param parent_env Parent environment to search for weights
#' @return Numeric vector of weights or NULL
#' @keywords internal
.evaluate_weights <- function(weights_arg, parent_env = parent.frame()) {
  tryCatch({
    eval(weights_arg, envir = parent_env)
  }, error = function(e) {
    # If weights evaluation fails, try to find it in a data frame
    if (is.name(weights_arg)) {
      weight_name <- as.character(weights_arg)
      
      # Try to find weights in parent environments
      for (i in 1:5) {  # Check up to 5 parent frames
        env <- parent.frame(i + 1)  # +1 because we're already one level deep
        if (exists(weight_name, envir = env)) {
          return(get(weight_name, envir = env))
        }
      }
      
      # If still not found, warn and return NULL
      warning(sprintf("Weights variable '%s' not found. Falling back to unweighted calculation.", weight_name))
      return(NULL)
    }
    return(NULL)
  })
}

# Data Processing Functions
# -------------------------

#' Process variables using tidyselect
#' @param data Data frame
#' @param ... Variable selection expressions
#' @return Named integer vector of selected variables
#' @keywords internal
.process_variables <- function(data, ...) {
  vars <- tidyselect::eval_select(rlang::expr(c(...)), data)
  if (length(vars) == 0) {
    stop("No variables selected")
  }
  vars
}

#' Process weights variable
#' @param data Data frame
#' @param weights Quoted weights expression
#' @return List with weights vector and name
#' @keywords internal
.process_weights <- function(data, weights) {
  weights_quo <- rlang::enquo(weights)
  if (rlang::quo_is_null(weights_quo)) {
    return(list(weights_vec = NULL, weights_name = NULL))
  }
  
  weights_name <- rlang::as_name(weights_quo)
  if (weights_name %in% names(data)) {
    weights_vec <- data[[weights_name]]
  } else {
    stop("Weights variable '", weights_name, "' not found in data")
  }
  
  list(weights_vec = weights_vec, weights_name = weights_name)
}

#' Calculate effective sample size for weighted data
#' @param weights Numeric vector of weights
#' @return Effective sample size
#' @keywords internal
.effective_n <- function(weights) {
  if (is.null(weights) || !.are_weights(weights)) {
    return(length(weights))
  }
  sum(weights)^2 / sum(weights^2)
}

# Print Helper Functions
# ----------------------

#' Create standardized results data frame for printing
#' @param results Raw results data frame
#' @param variables Character vector of variable names
#' @param statistic_name Name of the statistic (e.g., "Mean", "SD")
#' @param weights_name Name of weights variable or NULL
#' @param is_grouped Logical indicating if data was grouped
#' @param group_vars Character vector of grouping variables
#' @return Formatted data frame for printing
#' @keywords internal
.format_results_for_print <- function(results, variables, statistic_name, 
                                      weights_name, is_grouped, group_vars = NULL) {
  
  if (is_grouped && !is.null(group_vars)) {
    # For grouped data, include group variables
    group_cols <- results[group_vars]
    
    # Create formatted results
    formatted_results <- data.frame(
      group_cols,
      Variable = rep(variables, each = nrow(results) / length(variables)),
      stringsAsFactors = FALSE
    )
    
    # Add statistic column
    stat_values <- unlist(results[paste0(variables, "_value")])
    formatted_results[[statistic_name]] <- stat_values
    
    # Add N and Effective_N
    n_values <- unlist(results[paste0(variables, "_n")])
    eff_n_values <- unlist(results[paste0(variables, "_n_eff")])
    
    formatted_results$N <- n_values
    formatted_results$Effective_N <- eff_n_values
    formatted_results$Weights <- if (is.null(weights_name)) "None" else weights_name
    
  } else {
    # For ungrouped data
    formatted_results <- data.frame(
      Variable = variables,
      stringsAsFactors = FALSE
    )
    
    # Add statistic values
    for (var in variables) {
      formatted_results[[statistic_name]][formatted_results$Variable == var] <- 
        results[[paste0(var, "_value")]]
      formatted_results$N[formatted_results$Variable == var] <- 
        results[[paste0(var, "_n")]]
      formatted_results$Effective_N[formatted_results$Variable == var] <- 
        results[[paste0(var, "_n_eff")]]
    }
    
    formatted_results$Weights <- if (is.null(weights_name)) "None" else weights_name
  }
  
  formatted_results
}

#' Print header for statistics
#' @param statistic_name Name of the statistic (can include "Weighted" prefix)
#' @keywords internal
.print_header <- function(statistic_name) {
  header <- paste(statistic_name, "Statistics")
  cat("\n", header, "\n", sep = "")
  cat(paste(rep("-", nchar(header)), collapse = ""), "\n")
}

#' Calculate dynamic border width based on data frame output
#' @param df Data frame to be printed
#' @return Character string with appropriate number of dashes
#' @keywords internal
.get_border <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return("------------------------")  # Default fallback
  }
  
  # Capture the printed output to measure width
  output <- capture.output(print(df, row.names = FALSE))
  
  # Find the longest line (usually the header or a data row)
  if (length(output) > 0) {
    max_width <- max(nchar(output), na.rm = TRUE)
    # Ensure minimum width of 24 characters
    border_width <- max(24, max_width)
    return(paste(rep("-", border_width), collapse = ""))
  }
  
  # Fallback to default
  return("------------------------")
}

#' Print group header
#' @param group_info Data frame with group information
#' @param group_vars Character vector of grouping variable names
#' @keywords internal
.print_group_header <- function(group_info, group_vars) {
  group_labels <- character(length(group_vars))
  for (i in seq_along(group_vars)) {
    var_name <- group_vars[i]
    var_value <- group_info[[var_name]]
    
    # Handle factor levels
    if (is.factor(var_value)) {
      group_labels[i] <- paste0(var_name, " = ", as.character(var_value), " (", var_value, ")")
    } else {
      group_labels[i] <- paste0(var_name, " = ", var_value)
    }
  }
  
  group_info_str <- paste(group_labels, collapse = ", ")
  cat(sprintf("\nGroup: %s\n", group_info_str))
}

#' Print dynamic border based on data frame width
#' @param df Data frame that was just printed
#' @keywords internal
.print_border <- function(df) {
  cat(.get_border(df), "\n")
}

# =============================================================================
# CRITICAL MISSING HELPER FUNCTIONS (NAMESPACE CATASTROPHE FIX)
# =============================================================================

#' Process variable selection using tidyselect
#' @description
#' Central function for processing variable selection expressions using tidyselect.
#' Handles the ... expressions passed to statistical functions and returns a named
#' vector of column positions.
#' 
#' @param data A data frame
#' @param ... Variable selection expressions (tidyselect compatible)
#' @return Named integer vector of column positions
#' @keywords internal
.process_variables <- function(data, ...) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  # Use tidyselect to evaluate the variable selection
  vars <- tidyselect::eval_select(rlang::expr(c(...)), data)
  
  if (length(vars) == 0) {
    stop("No variables selected. Please specify at least one variable.")
  }
  
  return(vars)
}

#' Process weights parameter
#' @description
#' Central function for processing the weights parameter in statistical functions.
#' Handles both NULL weights (unweighted) and specified weights with validation.
#' 
#' @param data A data frame
#' @param weights_quo Quoted expression for weights (from rlang::enquo)
#' @return List with vector (numeric vector or NULL) and name (character or NULL)
#' @keywords internal
.process_weights <- function(data, weights_quo) {
  if (rlang::quo_is_null(weights_quo)) {
    return(list(vector = NULL, name = NULL))
  }
  
  # Get the weights variable name
  weights_name <- rlang::as_name(weights_quo)
  
  # Check if weights variable exists in data
  if (!weights_name %in% names(data)) {
    stop("Weights variable '", weights_name, "' not found in data")
  }
  
  # Get weights vector
  weights_vec <- data[[weights_name]]
  
  # Basic validation
  if (!is.numeric(weights_vec)) {
    stop("Weights variable '", weights_name, "' must be numeric")
  }
  
  return(list(vector = weights_vec, name = weights_name))
}