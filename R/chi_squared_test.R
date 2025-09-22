
#' Chi-Squared Test for Independence
#'
#' @description
#' Performs chi-squared test for independence between categorical variables
#' with support for weights and grouped data.
#'
#' @param data A data frame containing the variables
#' @param ... Variables to test (unquoted names)
#' @param weights Optional weights variable (unquoted)
#' @param correct Logical; apply continuity correction (default: FALSE)
#'
#' @return An object of class "chi_squared_test_results"
#' 
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#' 
#' # Basic chi-squared test for independence
#' survey_data %>% chi_squared_test(gender, region)
#' 
#' # With weights
#' survey_data %>% chi_squared_test(gender, education, weights = sampling_weight)
#' 
#' # Grouped analysis
#' survey_data %>% 
#'   group_by(region) %>% 
#'   chi_squared_test(gender, employment)
#' 
#' # With continuity correction
#' survey_data %>% chi_squared_test(gender, region, correct = TRUE)
#' 
#' @export
chi_squared_test <- function(data, ..., weights = NULL, correct = FALSE) {
  
  # Get variable names
  dots <- enquos(...)
  weights_quo <- enquo(weights)
  
  # Get data structure
  is_grouped <- inherits(data, "grouped_df")
  group_vars <- if (is_grouped) dplyr::group_vars(data) else NULL
  
  # Select variables
  vars <- tidyselect::eval_select(expr(c(!!!dots)), data = data)
  var_names <- names(vars)
  
  if (length(var_names) != 2) {
    stop("Exactly two variables must be specified")
  }
  
  # Get weight variable name if provided
  if (!quo_is_null(weights_quo)) {
    w_var <- eval_select(expr(!!weights_quo), data = data)
    w_name <- names(w_var)
  } else {
    w_name <- NULL
  }
  
  # Perform chi-squared test
  if (is_grouped) {
    # Group analysis
    data_list <- group_split(data)
    group_keys <- group_keys(data)
    
    results_list <- lapply(seq_along(data_list), function(i) {
      group_data <- data_list[[i]]
      group_info <- group_keys[i, , drop = FALSE]
      
      # Test for this group
      result <- tryCatch({
        # Extract variables
        var1 <- group_data[[var_names[1]]]
        var2 <- group_data[[var_names[2]]]
        
        if (!is.null(w_name)) {
          # Weighted test - round weights to integers for SPSS compatibility
          weights_data <- group_data[[w_name]]
          tbl <- xtabs(weights_data ~ var1 + var2)
          tbl <- round(tbl)  # Round to match SPSS behavior
        } else {
          # Unweighted test
          tbl <- table(var1, var2)
        }
        
        test_result <- chisq.test(tbl, correct = correct)
        
        # Create results entry
        data.frame(
          group_info,
          chi_squared = as.numeric(test_result$statistic),
          df = as.numeric(test_result$parameter),
          p_value = test_result$p.value,
          n = sum(tbl),
          observed = I(list(test_result$observed)),
          expected = I(list(test_result$expected)),
          residuals = I(list(test_result$residuals)),
          stringsAsFactors = FALSE
        )
      }, error = function(e) {
        # Return NA results if chi-squared test fails
        data.frame(
          group_info,
          chi_squared = NA_real_,
          df = NA_integer_,
          p_value = NA_real_,
          n = NA_integer_,
          observed = I(list(NULL)),
          expected = I(list(NULL)), 
          residuals = I(list(NULL)),
          stringsAsFactors = FALSE
        )
      })
    })
    
    # Combine all results
    results_df <- do.call(rbind, results_list)
    
  } else {
    # Single analysis for whole dataset
    var1 <- data[[var_names[1]]]
    var2 <- data[[var_names[2]]]
    
    if (!is.null(w_name)) {
      # Weighted test - round weights to integers for SPSS compatibility
      weights_data <- data[[w_name]]
      tbl <- xtabs(weights_data ~ var1 + var2)
      tbl <- round(tbl)  # Round to match SPSS behavior
    } else {
      # Unweighted test
      tbl <- table(var1, var2)
    }
    
    test_result <- chisq.test(tbl, correct = correct)
    
    # Create results dataframe
    results_df <- data.frame(
      chi_squared = as.numeric(test_result$statistic),
      df = as.numeric(test_result$parameter),
      p_value = test_result$p.value,
      n = sum(tbl),
      stringsAsFactors = FALSE
    )
    
    # Add tables as list columns
    results_df$observed <- list(test_result$observed)
    results_df$expected <- list(test_result$expected)
    results_df$residuals <- list(test_result$residuals)
  }
  
  # Calculate effect sizes
  results_df$cramers_v <- NA_real_
  results_df$phi <- NA_real_
  results_df$contingency_c <- NA_real_
  
  for (i in seq_len(nrow(results_df))) {
    if (!is.null(results_df$observed[[i]]) && !is.na(results_df$chi_squared[i])) {
      # Calculate effect sizes
      n <- results_df$n[i]
      chi_squared <- results_df$chi_squared[i]
      
      # Get table dimensions
      r <- nrow(results_df$observed[[i]])
      c <- ncol(results_df$observed[[i]])
      
      # Phi coefficient
      results_df$phi[i] <- sqrt(chi_squared / n)
      
      # Cramer's V
      results_df$cramers_v[i] <- sqrt(chi_squared / (n * min(r - 1, c - 1)))
      
      # Contingency coefficient
      results_df$contingency_c[i] <- sqrt(chi_squared / (chi_squared + n))
    }
  }
  
  # Create result object
  result <- list(
    results = results_df,
    variables = var_names,
    weights = w_name,
    correct = correct,
    is_grouped = is_grouped,
    groups = group_vars,
    data = data
  )
  
  class(result) <- "chi_squared_test_results"
  return(result)
}

#' Print method for chi_squared_test_results
#' 
#' @param x Chi-squared test results object
#' @param digits Number of decimal places (default: 3)
#' @param ... Additional arguments passed to print
#' @export
print.chi_squared_test_results <- function(x, digits = 3, ...) {
  
  # Determine test type
  test_type <- if (!is.null(x$weights)) {
    "Weighted Chi-Squared Test of Independence"
  } else {
    "Chi-Squared Test of Independence"
  }
  
  cat("\n", test_type, "\n", sep = "")
  cat(paste(rep("-", nchar(test_type)), collapse = ""), "\n")
  
  # Add significance stars
  p_values <- x$results$p_value
  sig <- cut(p_values, 
            breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
            labels = c("***", "**", "*", ""),
            right = FALSE)
  
  if (!x$is_grouped) {
    # Simple test display
    cat(sprintf("\nVariables: %s x %s\n", x$variables[1], x$variables[2]))
    
    if (!is.null(x$weights)) {
      cat(sprintf("Weights variable: %s\n", x$weights))
    }
    
    if (x$correct) {
      cat("Yates' continuity correction applied\n")
    }
    
    cat("\n")
    
    # Print observed frequencies
    cat("Observed Frequencies:\n")
    print(x$results$observed[[1]])
    
    # Print expected frequencies
    cat("\nExpected Frequencies:\n")
    print(round(x$results$expected[[1]], digits))
    
    # Print chi-squared results
    results_table <- data.frame(
      Chi_squared = round(x$results$chi_squared[1], digits),
      df = x$results$df[1],
      p_value = ifelse(x$results$p_value[1] < 0.001, 
                     "<.001", round(x$results$p_value[1], digits)),
      sig = as.character(sig[1])
    )
    
    cat("\nChi-Squared Test Results:\n")
    border_width <- paste(rep("-", 50), collapse = "")
    cat(border_width, "\n")
    print(results_table, row.names = FALSE)
    cat(border_width, "\n")
    
    # Print effect sizes
    cramers_v <- x$results$cramers_v[1]
    if (!is.na(cramers_v)) {
      effect_size <- if (cramers_v < 0.1) "negligible" else
                     if (cramers_v < 0.3) "small" else
                     if (cramers_v < 0.5) "medium" else "large"
      
      effect_table <- data.frame(
        Cramers_V = round(cramers_v, digits),
        Phi = round(x$results$phi[1], digits),
        Contingency_C = round(x$results$contingency_c[1], digits)
      )
      
      cat("\nEffect Sizes:\n")
      cat(paste(rep("-", 12), collapse = ""), "\n")
      print(effect_table, row.names = FALSE)
      cat(sprintf("\nCramer's V = %.3f (%s effect)\n", cramers_v, effect_size))
    }
    
  } else {
    # Grouped tests
    cat("\nVariables tested:", paste(x$variables, collapse = " x "), "\n")
    cat("Grouped by:", paste(x$groups, collapse = ", "), "\n")
    
    # Create a unified table with all results
    results_table <- x$results
    
    # Print results for each group
    for (i in seq_len(nrow(results_table))) {
      # Format group info
      group_values <- results_table[i, x$groups, drop = FALSE]
      group_info <- sapply(seq_along(x$groups), function(j) {
        paste(x$groups[j], "=", group_values[[j]])
      })
      group_info <- paste(group_info, collapse = ", ")
      
      cat(sprintf("\n--- Group: %s ---\n", group_info))
      cat("\n")  # Blank line
      
      # Print observed frequencies
      cat("Observed Frequencies:\n")
      print(results_table$observed[[i]])
      
      # Print test results
      test_results <- data.frame(
        Chi_squared = round(results_table$chi_squared[i], digits),
        df = results_table$df[i],
        p_value = ifelse(results_table$p_value[i] < 0.001, 
                       "<.001", round(results_table$p_value[i], digits)),
        sig = as.character(sig[i])
      )
      
      cat("\nChi-Squared Test Results:\n")
      border_width <- paste(rep("-", 50), collapse = "")
      cat(border_width, "\n")
      print(test_results, row.names = FALSE)
      cat(border_width, "\n")
      
      # Print effect sizes  
      cramers_v <- results_table$cramers_v[i]
      if (!is.na(cramers_v)) {
        effect_size <- if (cramers_v < 0.1) "negligible" else
                       if (cramers_v < 0.3) "small" else
                       if (cramers_v < 0.5) "medium" else "large"
        
        cat(sprintf("\nCramer's V = %.3f (%s effect)\n", cramers_v, effect_size))
      }
    }
  }
  
  cat("\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05\n")
  
  invisible(x)
}
