#' Measure How Strongly Variables Are Related
#'
#' @description
#' \code{pearson_cor()} shows you how strongly numeric variables are related to each
#' other. For example, is age related to income? Does satisfaction increase with
#' experience? This helps you understand patterns in your data.
#'
#' The correlation tells you:
#' - **Direction**: Positive (both increase together) or negative (one increases as other decreases)
#' - **Strength**: How closely the variables move together (from 0 = no relationship to 1 = perfect relationship)
#' - **Significance**: Whether the relationship is real or could be due to chance
#'
#' @param data Your survey data (a data frame or tibble)
#' @param ... The numeric variables you want to correlate. List two for a single
#'   correlation or more for a correlation matrix.
#' @param weights Optional survey weights for population-representative results
#' @param conf.level Confidence level for intervals (Default: 0.95 = 95%)
#' @param na.rm How to handle missing values:
#'   \itemize{
#'     \item \code{"pairwise"} (default): Use all available data for each pair
#'     \item \code{"listwise"}: Only use complete cases across all variables
#'   }
#'
#' @return Correlation results showing relationships between variables, including:
#' - Correlation coefficient (r): Strength and direction of relationship
#' - P-value: Whether the relationship is statistically significant
#' - Confidence interval: Range of plausible correlation values
#' - Sample size: Number of observations used
#'
#' @details
#' ## Understanding Correlation Values
#'
#' **Correlation coefficient (r)** ranges from -1 to +1:
#' - **+1**: Perfect positive relationship (as one goes up, the other always goes up)
#' - **0**: No linear relationship
#' - **-1**: Perfect negative relationship (as one goes up, the other always goes down)
#'
#' **Interpreting strength** (absolute value of r):
#' - 0.00 - 0.10: Negligible relationship
#' - 0.10 - 0.30: Weak relationship
#' - 0.30 - 0.50: Moderate relationship
#' - 0.50 - 0.70: Strong relationship
#' - 0.70 - 0.90: Very strong relationship
#' - 0.90 - 1.00: Extremely strong relationship
#'
#' **P-value interpretation**:
#' - p < 0.001: Very strong evidence of a relationship
#' - p < 0.01: Strong evidence of a relationship
#' - p < 0.05: Moderate evidence of a relationship
#' - p ≥ 0.05: No significant relationship found
#'
#' ## When to Use Pearson Correlation
#'
#' Use this when:
#' - Both variables are numeric and continuous
#' - You expect a linear relationship
#' - Data is roughly normally distributed
#' - You want to measure strength of linear association
#'
#' Don't use when:
#' - Data has extreme outliers (consider Spearman instead)
#' - Relationship is curved/non-linear
#' - Variables are categorical (use chi-squared test)
#' - You need to establish causation (correlation ≠ causation!)
#'
#' ## Reading the Results
#'
#' A correlation of 0.65 with p < 0.001 means:
#' - Strong positive relationship (r = 0.65)
#' - As one variable increases, the other tends to increase
#' - Very unlikely to be due to chance (p < 0.001)
#' - About 42% of variation is shared (r² = 0.65² = 0.42)
#'
#' ## Tips for Success
#'
#' - Always plot your data first to check for non-linear patterns
#' - Consider both statistical significance (p-value) and practical importance (r value)
#' - Remember: correlation does not imply causation
#' - Check for outliers that might inflate or deflate correlations
#' - Use Spearman correlation for ordinal data or non-normal distributions
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#' 
#' # Basic correlation between two variables
#' survey_data %>% 
#'   pearson_cor(age, income)
#' 
#' # Correlation matrix for multiple variables
#' survey_data %>% 
#'   pearson_cor(age, income, life_satisfaction)
#' 
#' # Weighted correlations
#' survey_data %>% 
#'   pearson_cor(age, income, weights = sampling_weight)
#' 
#' # Grouped correlations
#' survey_data %>% 
#'   group_by(region) %>% 
#'   pearson_cor(age, income, life_satisfaction)
#' 
#' # Using tidyselect helpers
#' survey_data %>% 
#'   pearson_cor(where(is.numeric), weights = sampling_weight)
#' 
#' # Listwise deletion for missing data
#' survey_data %>% 
#'   pearson_cor(age, income, na.rm = "listwise")
#' 
#' # Store results for further analysis
#' result <- survey_data %>% 
#'   pearson_cor(age, income, life_satisfaction, weights = sampling_weight)
#' print(result)
#'
#' @seealso 
#' \code{\link[stats]{cor}} for the base R correlation function
#' \code{\link[stats]{cor.test}} for correlation significance testing
#'
#' @references
#' Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.). 
#' Lawrence Erlbaum Associates.
#' 
#' Fisher, R.A. (1915). Frequency distribution of the values of the correlation coefficient 
#' in samples from an indefinitely large population. Biometrika, 10(4), 507-521.
#'
#' @export
pearson_cor <- function(data, ..., weights = NULL, conf.level = 0.95, na.rm = "pairwise") {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  if (!na.rm %in% c("pairwise", "listwise")) {
    stop("na.rm must be either 'pairwise' or 'listwise'")
  }
  
  if (conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be between 0 and 1")
  }
  
  # Check if data is grouped
  is_grouped <- inherits(data, "grouped_df")
  group_vars <- if (is_grouped) dplyr::group_vars(data) else NULL
  
  # Get variable names using tidyselect
  dots <- enquos(...)
  weights_quo <- enquo(weights)
  
  # Evaluate selections
  vars <- eval_select(expr(c(!!!dots)), data = data)
  var_names <- names(vars)
  
  if (length(var_names) < 2) {
    stop("At least two variables must be specified for correlation analysis")
  }
  
  # Validate that all selected variables are numeric
  for (var_name in var_names) {
    if (!is.numeric(data[[var_name]])) {
      stop("Variable '", var_name, "' is not numeric")
    }
  }
  
  # Get weights if provided
  if (!quo_is_null(weights_quo)) {
    w_var <- eval_select(expr(!!weights_quo), data = data)
    w_name <- names(w_var)
  } else {
    w_name <- NULL
  }
  
  # Helper function to calculate weighted correlation
  calculate_weighted_cor <- function(x, y, w = NULL, conf.level = 0.95) {
    if (!is.null(w)) {
      # Remove missing values
      valid <- !is.na(x) & !is.na(y) & !is.na(w) & w > 0
      x <- x[valid]
      y <- y[valid]
      w <- w[valid]
      
      n <- length(x)
      if (n < 3) {
        return(list(
          correlation = NA_real_,
          p_value = NA_real_,
          conf_int = c(NA_real_, NA_real_),
          n = n,
          df = NA_integer_
        ))
      }
      
      # Calculate weighted means
      mean_x <- sum(w * x) / sum(w)
      mean_y <- sum(w * y) / sum(w)
      
      # Calculate weighted covariance and variances
      cov_xy <- sum(w * (x - mean_x) * (y - mean_y)) / sum(w)
      var_x <- sum(w * (x - mean_x)^2) / sum(w)
      var_y <- sum(w * (y - mean_y)^2) / sum(w)
      
      # Calculate weighted correlation
      r <- cov_xy / sqrt(var_x * var_y)
      
      # SPSS compatibility: use sum of weights for significance testing
      # Also calculate true effective sample size for reference
      n_eff_true <- sum(w)^2 / sum(w^2)  # Statistically correct effective sample size
      n_eff <- sum(w)  # SPSS uses sum of weights for df calculation
      
    } else {
      # Unweighted correlation
      valid <- !is.na(x) & !is.na(y)
      x <- x[valid]
      y <- y[valid]
      
      n <- length(x)
      if (n < 3) {
        return(list(
          correlation = NA_real_,
          p_value = NA_real_,
          conf_int = c(NA_real_, NA_real_),
          n = n,
          df = NA_integer_
        ))
      }
      
      r <- cor(x, y, method = "pearson")
      n_eff <- n
    }
    
    # Ensure correlation is within valid range
    r <- max(-1, min(1, r))
    
    # Calculate t-statistic for significance test
    df <- n_eff - 2
    if (abs(r) == 1) {
      t_stat <- if (r > 0) Inf else -Inf
      p_value <- 0
    } else {
      t_stat <- r * sqrt(df / (1 - r^2))
      p_value <- 2 * pt(-abs(t_stat), df)
    }
    
    # Calculate confidence interval using Fisher's z transformation
    if (abs(r) < 1) {
      z <- 0.5 * log((1 + r) / (1 - r))
      se_z <- 1 / sqrt(n_eff - 3)
      z_crit <- qnorm((1 + conf.level) / 2)
      
      z_lower <- z - z_crit * se_z
      z_upper <- z + z_crit * se_z
      
      # Transform back to correlation scale
      ci_lower <- (exp(2 * z_lower) - 1) / (exp(2 * z_lower) + 1)
      ci_upper <- (exp(2 * z_upper) - 1) / (exp(2 * z_upper) + 1)
    } else {
      ci_lower <- r
      ci_upper <- r
    }
    
    # For weighted correlations, report sum of weights as N (SPSS compatibility)
    # For unweighted, report actual sample size
    n_report <- if (!is.null(w)) round(n_eff) else n
    
    return(list(
      correlation = r,
      p_value = p_value,
      conf_int = c(ci_lower, ci_upper),
      n = n_report,
      df = df
    ))
  }
  
  # Helper function to calculate all pairwise correlations
  calculate_correlation_matrix <- function(data, var_names, w_name = NULL, conf.level = 0.95, na.rm = "pairwise") {
    n_vars <- length(var_names)
    
    # Initialize storage
    cor_matrix <- matrix(NA_real_, n_vars, n_vars)
    p_matrix <- matrix(NA_real_, n_vars, n_vars)
    n_matrix <- matrix(NA_integer_, n_vars, n_vars)
    ci_lower_matrix <- matrix(NA_real_, n_vars, n_vars)
    ci_upper_matrix <- matrix(NA_real_, n_vars, n_vars)
    
    rownames(cor_matrix) <- colnames(cor_matrix) <- var_names
    rownames(p_matrix) <- colnames(p_matrix) <- var_names
    rownames(n_matrix) <- colnames(n_matrix) <- var_names
    rownames(ci_lower_matrix) <- colnames(ci_lower_matrix) <- var_names
    rownames(ci_upper_matrix) <- colnames(ci_upper_matrix) <- var_names
    
    # Get weights vector if provided
    weights_vec <- if (!is.null(w_name)) data[[w_name]] else NULL
    
    # Handle listwise deletion if requested
    if (na.rm == "listwise") {
      complete_cases <- complete.cases(data[var_names])
      if (!is.null(weights_vec)) {
        complete_cases <- complete_cases & !is.na(weights_vec)
      }
      data <- data[complete_cases, ]
      if (!is.null(weights_vec)) {
        weights_vec <- weights_vec[complete_cases]
      }
    }
    
    # Calculate correlations for each pair
    for (i in 1:n_vars) {
      for (j in i:n_vars) {
        if (i == j) {
          # Diagonal: perfect correlation with self
          cor_matrix[i, j] <- 1
          p_matrix[i, j] <- 0
          # For weighted analysis, use sum of weights; for unweighted, use count
          if (!is.null(weights_vec)) {
            valid_idx <- !is.na(data[[var_names[i]]]) & !is.na(weights_vec)
            n_matrix[i, j] <- round(sum(weights_vec[valid_idx]))
          } else {
            n_matrix[i, j] <- sum(!is.na(data[[var_names[i]]]))
          }
          ci_lower_matrix[i, j] <- 1
          ci_upper_matrix[i, j] <- 1
        } else {
          # Calculate correlation
          result <- calculate_weighted_cor(
            data[[var_names[i]]], 
            data[[var_names[j]]], 
            weights_vec,
            conf.level
          )
          
          # Store results (symmetric)
          cor_matrix[i, j] <- cor_matrix[j, i] <- result$correlation
          p_matrix[i, j] <- p_matrix[j, i] <- result$p_value
          n_matrix[i, j] <- n_matrix[j, i] <- result$n
          ci_lower_matrix[i, j] <- ci_lower_matrix[j, i] <- result$conf_int[1]
          ci_upper_matrix[i, j] <- ci_upper_matrix[j, i] <- result$conf_int[2]
        }
      }
    }
    
    return(list(
      correlations = cor_matrix,
      p_values = p_matrix,
      n_obs = n_matrix,
      ci_lower = ci_lower_matrix,
      ci_upper = ci_upper_matrix
    ))
  }
  
  # Main execution logic
  if (is_grouped) {
    # Group analysis
    data_list <- group_split(data)
    group_keys <- group_keys(data)
    
    results_list <- lapply(seq_along(data_list), function(i) {
      group_data <- data_list[[i]]
      group_info <- group_keys[i, , drop = FALSE]
      
      # Calculate correlation matrix for this group
      cor_results <- calculate_correlation_matrix(
        group_data, 
        var_names, 
        w_name, 
        conf.level, 
        na.rm
      )
      
      # Convert to long format for results data frame
      long_results <- list()
      k <- 1
      
      for (i in 1:length(var_names)) {
        for (j in i:length(var_names)) {
          if (i != j) {  # Skip diagonal
            result_row <- cbind(
              group_info,
              data.frame(
                var1 = var_names[i],
                var2 = var_names[j],
                correlation = cor_results$correlations[i, j],
                p_value = cor_results$p_values[i, j],
                conf_int_lower = cor_results$ci_lower[i, j],
                conf_int_upper = cor_results$ci_upper[i, j],
                n = cor_results$n_obs[i, j],
                stringsAsFactors = FALSE
              )
            )
            long_results[[k]] <- result_row
            k <- k + 1
          }
        }
      }
      
      do.call(rbind, long_results)
    })
    
    # Combine all results
    correlations_df <- do.call(rbind, results_list)
    
    # Store matrices for each group (for print method)
    matrices_list <- lapply(seq_along(data_list), function(i) {
      group_data <- data_list[[i]]
      calculate_correlation_matrix(
        group_data, 
        var_names, 
        w_name, 
        conf.level, 
        na.rm
      )
    })
    
    n_obs <- NULL  # Will be stored in matrices_list
    
  } else {
    # Single analysis for whole dataset
    cor_results <- calculate_correlation_matrix(
      data, 
      var_names, 
      w_name, 
      conf.level, 
      na.rm
    )
    
    # Convert to long format for results data frame
    long_results <- list()
    k <- 1
    
    for (i in 1:length(var_names)) {
      for (j in i:length(var_names)) {
        if (i != j) {  # Skip diagonal
          result_row <- data.frame(
            var1 = var_names[i],
            var2 = var_names[j],
            correlation = cor_results$correlations[i, j],
            p_value = cor_results$p_values[i, j],
            conf_int_lower = cor_results$ci_lower[i, j],
            conf_int_upper = cor_results$ci_upper[i, j],
            n = cor_results$n_obs[i, j],
            stringsAsFactors = FALSE
          )
          long_results[[k]] <- result_row
          k <- k + 1
        }
      }
    }
    
    correlations_df <- do.call(rbind, long_results)
    n_obs <- cor_results$n_obs
    matrices_list <- list(cor_results)
  }
  
  # Add significance indicators
  correlations_df$sig <- cut(correlations_df$p_value,
                             breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                             labels = c("***", "**", "*", ""),
                             right = FALSE)
  
  # Calculate r-squared for effect size
  correlations_df$r_squared <- correlations_df$correlation^2
  
  # Create result object
  result <- list(
    correlations = correlations_df,
    n_obs = n_obs,
    matrices = matrices_list,
    variables = var_names,
    weights = w_name,
    conf.level = conf.level,
    na.rm = na.rm,
    is_grouped = is_grouped,
    groups = group_vars,
    group_keys = if(is_grouped) group_keys else NULL
  )
  
  class(result) <- "pearson_cor_results"
  return(result)
}

#' Internal function to print correlation matrices without cutoff
#' @keywords internal
.print_correlation_matrix <- function(mat, digits = 3, title = "Correlation Matrix:", 
                                      type = "correlation") {
  # Get current console width
  old_width <- getOption("width")
  
  # Calculate required width for the matrix
  n_vars <- ncol(mat)
  max_rowname_length <- max(nchar(rownames(mat)), na.rm = TRUE)
  
  # Determine value width based on type
  if (type == "correlation") {
    # Correlations: -1.000 to 1.000
    value_width <- digits + 4  # e.g., "-0.xxx" or " 1.000"
  } else if (type == "pvalue") {
    # P-values: 0.0000 to 1.0000
    value_width <- digits + 3  # e.g., "0.xxxx"
  } else {
    # Sample sizes: integers
    value_width <- max(nchar(format(mat, scientific = FALSE)), na.rm = TRUE) + 1
  }
  
  # Calculate total required width
  # rowname + spaces + (n_vars * (value_width + space between))
  required_width <- max_rowname_length + 2 + (n_vars * (value_width + 1))
  
  # Adjust console width if needed (cap at 200 for readability)
  width_adjusted <- FALSE
  if (required_width > old_width && required_width <= 200) {
    options(width = required_width)
    on.exit(options(width = old_width), add = TRUE)
    width_adjusted <- TRUE
  } else if (required_width > 200) {
    # For very wide matrices, use maximum reasonable width
    options(width = 200)
    on.exit(options(width = old_width), add = TRUE)
    width_adjusted <- TRUE
  }
  
  # For very large matrices (> 6 variables), use more compact formatting
  if (n_vars > 6 && type == "correlation") {
    digits <- min(digits, 2)
    value_width <- digits + 4
  }
  
  # Print title and border
  cat(paste0("\n", title, "\n"))
  border_width <- paste(rep("-", nchar(title)), collapse = "")
  cat(border_width, "\n")
  
  # Format matrix based on type
  if (type == "correlation") {
    # Format correlation matrix with consistent spacing
    formatted_mat <- format(round(mat, digits), width = value_width, nsmall = digits, justify = "right")
  } else if (type == "pvalue") {
    # Format p-value matrix
    formatted_mat <- format(round(mat, digits), width = value_width, nsmall = digits, justify = "right")
  } else {
    # Format sample size matrix (integers)
    formatted_mat <- format(mat, width = value_width, justify = "right")
  }
  
  # Print the formatted matrix
  print(formatted_mat, quote = FALSE, right = TRUE)
  cat(border_width, "\n")
  
  # If we had to adjust width for a very large matrix, add a note
  if (width_adjusted && required_width > 200) {
    cat("Note: Matrix display adjusted for console width.\n")
  }
}

#' Print method for pearson_cor_results
#'
#' @param x A pearson_cor_results object
#' @param digits Number of decimal places to display (default: 3)
#' @param ... Additional arguments passed to print
#'
#' @export
print.pearson_cor_results <- function(x, digits = 3, ...) {

  # Determine test type using standardized helper
  test_type <- get_standard_title("Pearson Correlation", x$weights, "")
  print_header(test_type)
  
  # Print test information using standardized helpers
  cat("\n")
  test_info <- list(
    "Weights variable" = x$weights,
    "Missing data handling" = paste(x$na.rm, "deletion")
  )
  print_info_section(test_info)

  # Print test parameters
  test_params <- list(conf.level = x$conf.level)
  print_test_parameters(test_params)
  cat("\n")
  
  if (x$is_grouped) {
    # Grouped analysis
    group_combinations <- unique(x$correlations[x$groups])
    
    for (i in seq_len(nrow(group_combinations))) {
      # Format group header (Template Standard)
      group_info <- sapply(names(group_combinations), function(g) {
        paste(g, "=", group_combinations[i, g])
      })
      cat(sprintf("\nGroup: %s\n", paste(group_info, collapse = ", ")))
      
      # Filter correlations for this group
      group_corrs <- x$correlations
      for (g in names(group_combinations)) {
        group_corrs <- group_corrs[group_corrs[[g]] == group_combinations[i, g], ]
      }
      
      # For each pair of variables, show results like t_test does
      if (length(x$variables) == 2) {
        # Single correlation - use variable block format
        var_pair <- paste(x$variables[1], "\u00d7", x$variables[2])
        cat(sprintf("\n--- %s ---\n\n", var_pair))
        
        # Show correlation statistics
        cat(sprintf("  Correlation: r = %.3f\n", group_corrs$correlation[1]))
        cat(sprintf("  Effect size: r-squared = %.3f\n", group_corrs$r_squared[1]))
        cat(sprintf("  Sample size: n = %d\n", group_corrs$n[1]))
        cat(sprintf("  95%% CI: [%.3f, %.3f]\n", 
                   group_corrs$conf_int_lower[1],
                   group_corrs$conf_int_upper[1]))
        cat(sprintf("  p-value: %.4f\n", group_corrs$p_value[1]))
        # Display significance on separate line for clarity
        sig_text <- if (group_corrs$sig[1] == "") "ns" else group_corrs$sig[1]
        cat(sprintf("  Significance: %s\n", sig_text))
        
      } else {
        # Multiple correlations - show matrix first, then detailed results
        # Use the smart matrix printer for all matrices
        cor_matrix <- x$matrices[[i]]$correlations
        .print_correlation_matrix(cor_matrix, digits = digits, 
                                  title = "Correlation Matrix:", 
                                  type = "correlation")
        
        # Print p-value matrix
        p_matrix <- x$matrices[[i]]$p_values
        .print_correlation_matrix(p_matrix, digits = 4, 
                                  title = "Significance Matrix (p-values):", 
                                  type = "pvalue")
        
        # Print sample size matrix
        n_matrix <- x$matrices[[i]]$n_obs
        .print_correlation_matrix(n_matrix, digits = 0, 
                                  title = "Sample Size Matrix:", 
                                  type = "n")
        
        # Detailed pairwise results
        cat("\nPairwise Results:\n")
        border_width <- paste(rep("-", 16), collapse = "")
        cat(border_width, "\n")
        
        # Create results table
        output_df <- data.frame(
          Variable_Pair = paste(group_corrs$var1, "×", group_corrs$var2),
          r = round(group_corrs$correlation, digits),
          r_squared = round(group_corrs$r_squared, digits),
          p_value = round(group_corrs$p_value, 4),
          CI_95 = sprintf("[%.3f, %.3f]", 
                         group_corrs$conf_int_lower,
                         group_corrs$conf_int_upper),
          n = group_corrs$n,
          sig = group_corrs$sig,
          stringsAsFactors = FALSE
        )
        
        print(output_df, row.names = FALSE)
        cat(border_width, "\n")
      }
    }
    
  } else {
    # Ungrouped analysis
    
    if (length(x$variables) == 2) {
      # Single correlation - use variable block format like t_test
      var_pair <- paste(x$variables[1], "×", x$variables[2])
      cat(sprintf("\n--- %s ---\n\n", var_pair))
      
      # Show correlation statistics
      cat(sprintf("  Correlation: r = %.3f\n", x$correlations$correlation[1]))
      cat(sprintf("  Effect size: r² = %.3f\n", x$correlations$r_squared[1]))
      cat(sprintf("  Sample size: n = %d\n", x$correlations$n[1]))
      cat(sprintf("  95%% CI: [%.3f, %.3f]\n", 
                 x$correlations$conf_int_lower[1],
                 x$correlations$conf_int_upper[1]))
      cat(sprintf("  p-value: %.4f\n", x$correlations$p_value[1]))
      # Display significance on separate line for clarity
      sig_text <- if (x$correlations$sig[1] == "") "ns" else x$correlations$sig[1]
      cat(sprintf("  Significance: %s\n", sig_text))
      
    } else {
      # Multiple correlations - show matrix then detailed results
      # Use the smart matrix printer for all matrices
      cor_matrix <- x$matrices[[1]]$correlations
      .print_correlation_matrix(cor_matrix, digits = digits, 
                                title = "Correlation Matrix:", 
                                type = "correlation")
      
      p_matrix <- x$matrices[[1]]$p_values
      .print_correlation_matrix(p_matrix, digits = 4, 
                                title = "Significance Matrix (p-values):", 
                                type = "pvalue")
      
      n_matrix <- x$matrices[[1]]$n_obs
      .print_correlation_matrix(n_matrix, digits = 0, 
                                title = "Sample Size Matrix:", 
                                type = "n")
      
      # Detailed pairwise results
      cat("\nPairwise Results:\n")
      border_width <- paste(rep("-", 16), collapse = "")
      cat(border_width, "\n")
      
      # Create results table
      output_df <- data.frame(
        Variable_Pair = paste(x$correlations$var1, "×", x$correlations$var2),
        r = round(x$correlations$correlation, digits),
        r_squared = round(x$correlations$r_squared, digits),
        p_value = round(x$correlations$p_value, 4),
        CI_95 = sprintf("[%.3f, %.3f]", 
                       x$correlations$conf_int_lower,
                       x$correlations$conf_int_upper),
        n = x$correlations$n,
        sig = x$correlations$sig,
        stringsAsFactors = FALSE
      )
      
      print(output_df, row.names = FALSE)
      cat(border_width, "\n")
    }
  }
  
  # Footer section with interpretation (Template Standard)
  cat("\n")
  cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05\n")
  cat("\nCorrelation Strength Interpretation:\n")
  cat("  |r| < 0.30:        Weak correlation\n")
  cat("  0.30 ≤ |r| < 0.70: Moderate correlation\n")
  cat("  |r| ≥ 0.70:        Strong correlation\n")
  cat("\nr² represents the proportion of variance explained\n")
  
  invisible(x)
}