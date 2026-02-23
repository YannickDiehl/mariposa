
#' Calculate Population-Representative Percentiles
#'
#' @description
#' \code{w_quantile()} calculates percentiles (quantiles) using survey weights
#' for population-representative results. Percentiles divide your data into
#' equal portions -- for example, the 25th percentile is the value below which
#' 25% of your population falls. This is essential for understanding the
#' distribution of variables like income, age, or satisfaction scores.
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... The numeric variables you want to analyze. You can list multiple
#'   variables or use helpers like \code{starts_with("income")}
#' @param weights Survey weights to make results representative of your population.
#'   Without weights, you get the simple sample quantiles.
#' @param probs Which percentiles to calculate, as proportions between 0 and 1.
#'   Default: \code{c(0, 0.25, 0.5, 0.75, 1)} for the minimum, 25th percentile,
#'   median, 75th percentile, and maximum. Use \code{c(0.1, 0.5, 0.9)} for
#'   deciles, or \code{c(0.25, 0.5, 0.75)} for quartiles only.
#' @param na.rm Remove missing values before calculating? (Default: TRUE)
#'
#' @return Population-weighted quantile(s) with sample size information,
#'   including the weighted percentile values, effective sample size (effective N),
#'   and the number of valid observations used.
#'
#' @details
#' ## Understanding the Results
#'
#' - **Percentile values**: The data values at each requested percentile
#'   in the weighted population. For example, if the weighted 25th percentile
#'   of income is 35,000, then 25% of the population earns less than that.
#' - **Effective N**: How many independent observations your weighted data
#'   represents.
#' - **N**: The actual number of observations used.
#'
#' Common percentiles and their meaning:
#' - **0% (minimum)**: The smallest observed value
#' - **25% (Q1)**: One quarter of the population falls below this value
#' - **50% (median)**: Half the population falls below this value
#' - **75% (Q3)**: Three quarters of the population falls below this value
#' - **100% (maximum)**: The largest observed value
#'
#' ## When to Use This
#'
#' Use \code{w_quantile()} when:
#' - You want to know at what values the population splits into groups
#' - You need to construct population-representative income brackets or age groups
#' - You want to identify the median or quartiles with proper weighting
#' - You need SPSS-compatible weighted percentile values
#'
#' ## Formula
#'
#' Weighted quantiles are calculated using cumulative weights. Observations
#' are sorted by value, weights are accumulated, and the requested percentile
#' is found by linear interpolation at the point where the cumulative weight
#' proportion reaches the target probability.
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
#' @seealso
#' \code{\link[stats]{quantile}} for the base R quantile function.
#'
#' \code{\link{w_median}} for the weighted median (50th percentile).
#'
#' \code{\link{w_iqr}} for the weighted interquartile range (Q3 - Q1).
#'
#' \code{\link{describe}} for comprehensive descriptive statistics including quantiles.
#'
#' @references
#' IBM Corp. (2023). IBM SPSS Statistics 29 Algorithms. IBM Corporation.
#'
#' @family weighted_statistics
#' @export
w_quantile <- function(data, ..., weights = NULL, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE) {
  
  # Handle summarise() context
  if (is.numeric(data) && !is.data.frame(data)) {
    x <- data
    weights_arg <- substitute(weights)
    weights_vec <- .evaluate_weights(weights_arg, parent.frame())
    
    if (!.are_weights(weights_vec) || !.validate_weights(weights_vec, verbose = FALSE)) {
      # Unweighted calculation
      if (na.rm) x <- x[!is.na(x)]
      return(quantile(x, probs = probs, na.rm = na.rm))
    } else {
      # Weighted calculation
      if (na.rm) {
        valid <- !is.na(x) & !is.na(weights_vec)
        x <- x[valid]
        weights_vec <- weights_vec[valid]
      }
      
      if (length(x) == 0) {
        result <- rep(NA_real_, length(probs))
        names(result) <- paste0(probs * 100, "%")
        return(result)
      } else {
        # Use internal weighted quantiles implementation
        return(.w_quantile(x, weights_vec, probs = probs, na.rm = FALSE))
      }
    }
  }
  
  # Data frame handling
  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame.")
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
      cli_abort("Weights variable {.var {weights_name}} not found in data.")
    }
  }
  
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
        result_cols <- list()
        
        for (var_name in var_names) {
          x <- group_data[[var_name]]
          
          if (is.null(weights_vec)) {
            # Unweighted calculation
            if (na.rm) x <- x[!is.na(x)]
            quantiles <- quantile(x, probs = probs, na.rm = na.rm)
            names(quantiles) <- quantile_labels
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
              quantiles <- rep(NA_real_, length(probs))
              names(quantiles) <- quantile_labels
              n_val <- 0
              eff_n <- 0
            } else {
              quantiles <- .w_quantile(x, w, probs = probs, na.rm = FALSE)
              names(quantiles) <- quantile_labels
              n_val <- length(x)
              eff_n <- sum(w)^2 / sum(w^2)  # Effective sample size
            }
          }
          
          # Add quantiles to results
          for (j in seq_along(quantiles)) {
            col_name <- paste0(var_name, "_", names(quantiles)[j])
            result_cols[[col_name]] <- quantiles[j]
          }
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
        quantiles <- quantile(x, probs = probs, na.rm = na.rm)
        names(quantiles) <- quantile_labels
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
          quantiles <- rep(NA_real_, length(probs))
          names(quantiles) <- quantile_labels
          n_val <- 0
          eff_n <- 0
        } else {
          quantiles <- .w_quantile(x, w, probs = probs, na.rm = FALSE)
          names(quantiles) <- quantile_labels
          n_val <- length(x)
          eff_n <- sum(w)^2 / sum(w^2)
        }
      }
      
      # Add quantiles to results
      for (j in seq_along(quantiles)) {
        col_name <- paste0(var_name, "_", names(quantiles)[j])
        result_cols[[col_name]] <- quantiles[j]
      }
      result_cols[[paste0(var_name, "_n")]] <- n_val
      result_cols[[paste0(var_name, "_eff_n")]] <- eff_n
    }
    
    results <- tibble::tibble(!!!result_cols)
  }
  
  # Create test-compatible results structure 
  # For w_quantile, we maintain the quantile column structure
  # but add standard test-compatible fields
  
  # Create S3 object with test-compatible structure
  result <- list(
    results = results,
    variables = var_names,
    weight_var = weights_name,        # Test-compatible field name
    weights = weights_name,           # Alternative field name
    probs = probs,
    is_grouped = is_grouped,          # Test-compatible field
    grouped = is_grouped,             # Alternative field
    groups = if (is_grouped) dplyr::group_vars(data) else NULL
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
  
  test_type <- get_standard_title("Quantile", x$weights, "Statistics")
  print_header(test_type)
  
  if (x$grouped) {
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
        print_separator(get_table_width(results_df_print))
        print(results_df_print, row.names = FALSE)
        print_separator(get_table_width(results_df_print))
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
      print_separator(get_table_width(results_df_print))
    } else {
      cat("No results to display.\n")
      cat("------------------------\n")
    }
  }
} 

