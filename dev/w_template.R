# Template for Weighted Statistical Functions - Version 2.0
# ========================================================
# This file serves as a template for creating new w_* functions with all
# Version 2.0 features: smart headers, clean output, dynamic borders, etc.
# 
# IMPORTANT: This template contains [PLACEHOLDER] text that will cause linter errors.
# This is expected - replace ALL placeholders before use!

library(tidyverse)
library(rlang)
library(tidyselect)

# Source helper functions (REQUIRED!)
source("helpers.R")

#' Weighted [STATISTIC_NAME]
#'
#' Calculate weighted [statistic_description] for numeric variables, with support for 
#' grouped data and multiple variables simultaneously. Provides professional
#' output formatting with smart headers and dynamic borders.
#'
#' @param data A data frame, or a numeric vector when used in summarise() context
#' @param ... Variable names (unquoted) or tidyselect expressions (e.g., starts_with(), where())
#' @param weights Name of the weights variable (unquoted), or a numeric vector of weights
#' @param na.rm Logical; if TRUE, missing values are removed (default: TRUE)
#' @param [additional_params] Additional parameters specific to this statistic
#'
#' @return A w_[statistic] object (list) containing results and metadata, or numeric values in summarise context
#' 
#' @details
#' The function automatically detects whether calculations are weighted or unweighted and
#' adjusts the output format accordingly:
#' - Unweighted: Shows "[Statistic_Name] Statistics" header with Variable, [Statistic], N columns
#' - Weighted: Shows "Weighted [Statistic_Name] Statistics" header with Variable, [Statistic], N, Effective_N, Weights columns
#' 
#' For grouped data, group variables appear only in the "Group:" header, not as redundant table columns.
#' Borders automatically adjust to content width for professional appearance.
#'
#' @examples
#' # Basic usage
#' data %>% w_[statistic](variable, weights = weight_var)
#' 
#' # Multiple variables
#' data %>% w_[statistic](var1, var2, var3, weights = weights)
#' 
#' # Grouped data
#' data %>% group_by(group_var) %>% w_[statistic](variable, weights = weights)
#' 
#' # In summarise context
#' data %>% summarise(stat_val = w_[statistic](variable, weights = weights))
#' 
#' # Tidyselect expressions
#' data %>% w_[statistic](starts_with("score_"), weights = weights)
#'
#' @export
w_[statistic] <- function(data, ..., weights = NULL, na.rm = TRUE) {
  
  # Handle the case where data is a vector (e.g., in summarise context)
  if (is.numeric(data) && !is.data.frame(data)) {
    # This is the summarise() context - data is actually the variable values
    x <- data
    weights_arg <- substitute(weights)
    
    # Use helper function to evaluate weights
    weights_vec <- .evaluate_weights(weights_arg, parent.frame())
    
    # Calculate weighted or unweighted statistic
    if (!.are_weights(weights_vec) || !.validate_weights(weights_vec, verbose = FALSE)) {
      # Unweighted calculation
      if (na.rm) {
        x <- x[!is.na(x)]
      }
      # MODIFY THIS: Replace with appropriate unweighted calculation
      result <- [UNWEIGHTED_CALCULATION](x, na.rm = na.rm)
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
        # MODIFY THIS: Replace with appropriate weighted calculation
        result <- [WEIGHTED_CALCULATION](x, weights_vec)
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
  weights_info <- .process_weights(data, weights)
  weights_vec <- weights_info$weights_vec
  weights_name <- weights_info$weights_name
  
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
            # MODIFY THIS: Replace with appropriate unweighted calculation
            stat_val <- [UNWEIGHTED_CALCULATION](x_clean, na.rm = na.rm)
            n_val <- length(x_clean[!is.na(x_clean)])
            n_eff <- n_val
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
              stat_val <- NA_real_
              n_val <- 0
              n_eff <- 0
            } else {
              # MODIFY THIS: Replace with appropriate weighted calculation
              stat_val <- [WEIGHTED_CALCULATION](x_clean, w_clean)
              n_val <- sum(w_clean)
              n_eff <- .effective_n(w_clean)
            }
          }
          
          results_list[[paste0(var_name, "_value")]] <- stat_val
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
        # MODIFY THIS: Replace with appropriate unweighted calculation
        stat_val <- [UNWEIGHTED_CALCULATION](x_clean, na.rm = na.rm)
        n_val <- length(x_clean[!is.na(x_clean)])
        n_eff <- n_val
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
          stat_val <- NA_real_
          n_val <- 0
          n_eff <- 0
        } else {
          # MODIFY THIS: Replace with appropriate weighted calculation
          stat_val <- [WEIGHTED_CALCULATION](x_clean, w_clean)
          n_val <- sum(w_clean)
          n_eff <- .effective_n(w_clean)
        }
      }
      
      results_list[[paste0(var_name, "_value")]] <- stat_val
      results_list[[paste0(var_name, "_n")]] <- n_val
      results_list[[paste0(var_name, "_n_eff")]] <- n_eff
    }
    
    results_df <- tibble::tibble(!!!results_list)
  }
  
  # Create S3 object
  result <- list(
    results = if (is_grouped) results else results_df,
    variables = names(vars),
    weights = weights_name,
    grouped = is_grouped,
    group_vars = if (is_grouped) dplyr::group_vars(data) else NULL
  )
  
  class(result) <- "w_[statistic]"
  return(result)
}

#' Print method for w_[statistic] objects
#'
#' @description
#' Prints a formatted summary of weighted [statistic_description] calculations with professional formatting.
#' Features smart headers (distinguishes weighted vs unweighted), clean tables (group variables
#' only in headers), dynamic borders, and conditional Effective_N column display.
#'
#' @param x An object of class "w_[statistic]"
#' @param digits Number of decimal places to display (default: 3)
#' @param ... Additional arguments passed to print
#'
#' @return Invisibly returns the input object
#' @export
print.w_[statistic] <- function(x, digits = 3, ...) {
  # Determine if this is weighted or unweighted
  is_weighted <- !is.null(x$weights)
  
  # MODIFY THIS: Change statistic name (e.g., "Variance", "Correlation", "Skewness")
  if (is_weighted) {
    .print_header("Weighted [Statistic_Name]")
  } else {
    .print_header("[Statistic_Name]")
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
          var_row <- data.frame(
            Variable = var,
            # MODIFY THIS: Change column name to match statistic (e.g., Variance, Correlation)
            [Statistic_Column] = round(group_results[[value_col]], digits),
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
        # MODIFY THIS: Change column name to match statistic
        results_df$[Statistic_Column][results_df$Variable == var] <- round(x$results[[value_col]], digits)
        
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

# ============================================================================
# TEMPLATE INSTRUCTIONS - Version 2.0
# ============================================================================
# 
# IMPORTANT: This template contains [PLACEHOLDER] text that will cause linter errors.
# This is expected - replace ALL placeholders before use!
# 
# To create a new w_* function using this template:
# 
# 1. COPY this file to w_[new_statistic].R
# 
# 2. REPLACE ALL PLACEHOLDERS:
#    - [statistic] -> your statistic name (lowercase, e.g., "variance")
#    - [Statistic_Name] -> capitalized name (e.g., "Variance") 
#    - [Statistic_Column] -> column name (e.g., "Variance")
#    - [STATISTIC_NAME] -> full caps name (e.g., "VARIANCE")
#    - [statistic_description] -> description (e.g., "sample variance")
#    - [UNWEIGHTED_CALCULATION] -> base R function (e.g., var)
#    - [WEIGHTED_CALCULATION] -> weighted formula
#    - [additional_params] -> any extra parameters (or remove if none)
# 
# 3. IMPLEMENT CALCULATIONS:
#    Replace [UNWEIGHTED_CALCULATION] and [WEIGHTED_CALCULATION] with actual formulas
# 
# 4. UPDATE DOCUMENTATION:
#    - Update @param descriptions
#    - Update @examples with realistic examples
#    - Update @details with specific information about your statistic
# 
# 5. TEST THOROUGHLY:
#    - Test with unweighted data
#    - Test with weighted data  
#    - Test with grouped data
#    - Test with multiple variables
#    - Test edge cases (NA values, empty data)
#    - Validate against established packages if available
# 
# 6. UPDATE PROJECT FILES:
#    - Add function to README.md
#    - Add to FUNCTION_OVERVIEW.md
#    - Update installation instructions
# 
# EXAMPLE for w_variance:
# ----------------------
# [statistic] -> variance
# [Statistic_Name] -> Variance  
# [Statistic_Column] -> Variance
# [STATISTIC_NAME] -> VARIANCE
# [statistic_description] -> sample variance
# [UNWEIGHTED_CALCULATION] -> var(x_clean, na.rm = na.rm)
# [WEIGHTED_CALCULATION] -> sum(w_clean * (x_clean - weighted_mean)^2) / sum(w_clean)
#
# FEATURES INCLUDED IN VERSION 2.0:
# ---------------------------------
# ✅ Smart Headers: Automatic "Statistics" vs "Weighted Statistics"
# ✅ Clean Output: Effective_N only for weighted calculations
# ✅ Dynamic Borders: Automatically adjust to content width
# ✅ Clean Grouped Data: Group variables only in headers
# ✅ Multiple Variables: Full tidyselect support
# ✅ Professional Documentation: Comprehensive roxygen2 comments
# ✅ Consistent API: Follows established w_* function patterns
# ✅ Error Handling: Robust NA and edge case handling
# 
# REMEMBER:
# - Source helpers.R first: source("helpers.R")
# - Test against established packages when possible
# - Follow the established naming conventions
# - Update all documentation files
# - Validate mathematical accuracy