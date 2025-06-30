#' Weighted Standard Deviation
#'
#' Calculate weighted standard deviation for numeric variables, with support for 
#' grouped data and multiple variables simultaneously.
#'
#' @param data A data frame, or a numeric vector when used in summarise() context
#' @param ... Variable names (unquoted) or tidyselect expressions
#' @param weights Name of the weights variable (unquoted), or a numeric vector of weights
#' @param na.rm Logical; if TRUE, missing values are removed (default: TRUE)
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
  
  # Handle summarise() context
  if (is.numeric(data) && !is.data.frame(data)) {
    x <- data
    weights_arg <- substitute(weights)
    weights_vec <- .evaluate_weights(weights_arg, parent.frame())
    
    if (!.are_weights(weights_vec) || !.validate_weights(weights_vec, verbose = FALSE)) {
      # Unweighted calculation
      if (na.rm) x <- x[!is.na(x)]
      return(sd(x, na.rm = na.rm))
    } else {
      # Weighted calculation
      if (na.rm) {
        valid <- !is.na(x) & !is.na(weights_vec)
        x <- x[valid]
        weights_vec <- weights_vec[valid]
      }
      
      if (length(x) < 2) {
        return(NA_real_)
      } else {
        # Weighted standard deviation calculation
        w_mean <- sum(x * weights_vec) / sum(weights_vec)
        n_eff <- sum(weights_vec)^2 / sum(weights_vec^2)  # Effective sample size
        w_var <- sum(weights_vec * (x - w_mean)^2) / (sum(weights_vec) * (n_eff - 1) / n_eff)
        return(sqrt(w_var))
      }
    }
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
            stat_val <- sd(x, na.rm = na.rm)
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
            
            if (length(x) < 2) {
              stat_val <- NA_real_
              n_val <- 0
              eff_n <- 0
            } else {
              w_mean <- sum(x * w) / sum(w)
              n_val <- length(x)
              eff_n <- sum(w)^2 / sum(w^2)  # Effective sample size
              w_var <- sum(w * (x - w_mean)^2) / (sum(w) * (eff_n - 1) / eff_n)
              stat_val <- sqrt(w_var)
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
        stat_val <- sd(x, na.rm = na.rm)
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
        
        if (length(x) < 2) {
          stat_val <- NA_real_
          n_val <- 0
          eff_n <- 0
        } else {
          w_mean <- sum(x * w) / sum(w)
          n_val <- length(x)
          eff_n <- sum(w)^2 / sum(w^2)
          w_var <- sum(w * (x - w_mean)^2) / (sum(w) * (eff_n - 1) / eff_n)
          stat_val <- sqrt(w_var)
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
              row_data$weighted_sd <- group_results[[var_name]][1]
              row_data$effective_n <- group_results[[paste0(var_name, "_eff_n")]][1]
            } else {
              row_data$sd <- group_results[[var_name]][1]
              row_data$n <- group_results[[paste0(var_name, "_n")]][1]
            }
            
            results_long[[length(results_long) + 1]] <- row_data
          }
        }
      } else {
        # Ungrouped multi-variable
        row_data <- tibble::tibble(Variable = var_name)
        
        if (!is.null(weights_name)) {
          row_data$weighted_sd <- results[[var_name]][1]
          row_data$effective_n <- results[[paste0(var_name, "_eff_n")]][1]
        } else {
          row_data$sd <- results[[var_name]][1]
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
          weighted_sd = !!rlang::sym(var_name),
          effective_n = !!rlang::sym(paste0(var_name, "_eff_n"))
        )
    } else {
      final_results <- results %>%
        dplyr::mutate(
          sd = !!rlang::sym(var_name),
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
  
  class(result) <- "w_sd"
  return(result)
}

#' Print method for w_sd objects
#' @export
#' @method print w_sd
print.w_sd <- function(x, digits = 3, ...) {
  test_type <- if (!is.null(x$weights)) {
    "Weighted Standard Deviation Statistics"
  } else {
    "Standard Deviation Statistics"
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
          dplyr::select(dplyr::any_of(c("Variable", "weighted_sd", "effective_n")))
        print_df$weighted_sd <- round(print_df$weighted_sd, digits)
        print_df$effective_n <- round(print_df$effective_n, 1)
      } else {
        print_df <- print_df %>%
          dplyr::select(dplyr::any_of(c("Variable", "sd", "n")))
        print_df$sd <- round(print_df$sd, digits)
      }
      
      print(print_df, row.names = FALSE)
    }
  } else {
    # Handle ungrouped results
    print_df <- x$results
    
    if (!is.null(x$weights)) {
      if ("Variable" %in% names(print_df)) {
        print_df <- print_df %>%
          dplyr::select(dplyr::any_of(c("Variable", "weighted_sd", "effective_n")))
      } else {
        print_df <- tibble::tibble(
          Variable = x$variables[1],
          weighted_sd = round(print_df$weighted_sd[1], digits),
          effective_n = round(print_df$effective_n[1], 1)
        )
      }
      print_df$weighted_sd <- round(print_df$weighted_sd, digits)
      print_df$effective_n <- round(print_df$effective_n, 1)
    } else {
      if ("Variable" %in% names(print_df)) {
        print_df <- print_df %>%
          dplyr::select(dplyr::any_of(c("Variable", "sd", "n")))
      } else {
        print_df <- tibble::tibble(
          Variable = x$variables[1],
          sd = round(print_df$sd[1], digits),
          n = round(print_df$n[1], 0)
        )
      }
      print_df$sd <- round(print_df$sd, digits)
    }
    
    print(print_df, row.names = FALSE)
  }
  
  cat("\n")
  invisible(x)
}